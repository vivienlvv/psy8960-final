# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

library(tm)
library(tidytext)
library(qdap)
library(textstem)
library(RWeka)

library(doParallel)
library(ldatuning)
library(topicmodels)

# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")



# Analysis 

## Purpose: Build a ML model that predicts turnover (which I believe is the "Attrition column"?), binary 

### STEP 1: Feature Engineering 

#### 1.1 Creating language features from "pros" and "cons"

##### 1.1.1 Creating corpus
corpus_pros = VCorpus(VectorSource(final_tbl$pros))
corpus_cons = VCorpus(VectorSource(final_tbl$cons))


##### 1.1.2 Begin actual corpus cleaning 
##### This is a function that allows cleanign text corpus- can be used for pros and cons responses
##### Input: corpus object 
cleaning_corpus = function(corpus_obj){
  cleaned_corpus = corpus_obj %>%
    tm_map(content_transformer(str_replace_all), pattern = "’", replacement = "'") %>%
    # tm_map(content_transformer(str_replace_all), pattern = "—|“|”|‘", replacement = "") %>%
    # This step was performed to replace any abbreviations before converting text to lower case because the abbreviation library is case sensitive 
    tm_map(content_transformer(str_replace_all), pattern = "\\/", replacement = " ") %>%
    tm_map(content_transformer(replace_abbreviation)) %>% 
    tm_map(content_transformer(replace_contraction)) %>% 
    tm_map(content_transformer(str_to_lower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace) %>% 
    # This step is added to remove leading and ending spaces
    tm_map(content_transformer(trimws)) %>% 
    # This step is added because instructions require a "lemmatized" corpus
    tm_map(content_transformer(lemmatize_strings))
  return(cleaned_corpus)
}

##### Apply cleaning function to the two corpora 
cleaned_corpus_pros = cleaning_corpus(corpus_pros)
cleaned_corpus_cons = cleaning_corpus(corpus_cons)

corpus_pros[[1]]$content
cleaned_corpus_pros[[1]]$content
corpus_cons[[1]]$content
cleaned_corpus_cons[[1]]$content


##### 1.1.3 Creating n-gram features from document-term matrix 
######  Creating Bigram DTM 
bigram_filter = function(x){
  bool = nchar(stripWhitespace(x$content)[[1]]) > 0 & !is.na(x$content[[1]])
  return(bool)
}
# post_retained = sapply(cleaned_corpus_pros, bigram_filter)
# temp = final_tbl %>% select(employeeID, pros, cons) 
# temp[!post_retained, ]

## Creating bigram tokenizer using Weka
bigram_Tokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}


## Creating bigram DTM for pros and cons corpora 

pros_dtm = cleaned_corpus_pros %>%
  tm_filter(bigram_filter) %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) 

cons_dtm = cleaned_corpus_cons %>% 
  tm_filter(bigram_filter) %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) 



##### 1.1.4 LDA topics (NEED TO RECONSIDER THIS PART; NOT INCORPORATED FOR NOW)


###### LDA Tuning: Finding optimal number of topics  
## Turning on parallelization
local_cluster = makeCluster(detectCores() - 1)   
registerDoParallel(local_cluster)

## Finding # of topics
lda_tuning = function(dtm){
  FindTopicsNumber(dtm,
                   topics = seq(2, 10, 1),
                   metrics = c("Griffiths2004",
                               "CaoJuan2009",
                               "Arun2010",
                               "Deveaud2014"),
                   control = list(seed = 2023),
                   verbose = TRUE)
}
FindTopicsNumber_plot(lda_tuning(pros_dtm))
FindTopicsNumber_plot(lda_tuning(cons_dtm))


## Turning off parallelization
stopCluster(local_cluster)
registerDoSEQ()


###### LDA topic model building  

# Actual fitting of LDA model 
lda_results = LDA(pros_dtm, 5,
                       control = list(seed = 2023))

## Posterior probabilities representing the probability that a word belongs to a topic
lda_betas = tidy(lda_results, matrix = "beta")

## Posterior probabilities of documents about each topic
lda_gammas = tidy(lda_results, matrix = "gamma")



#### 1.2 Combining features 

##### Getting n-gram features in matrix form to be merged with original dataset 
pros_dtm_mat = as.matrix(pros_dtm) 
pros_dtm_mat = data.frame(employeeID = rownames(pros_dtm_mat), pros_dtm_mat)

cons_dtm_mat = as.matrix(cons_dtm)
cons_dtm_mat = data.frame(employeeID = rownames(cons_dtm_mat), cons_dtm_mat)

####
ml_tbl = final_tbl %>% 
  select(-pros, -cons) %>%
  left_join(y = pros_dtm_mat, by = "employeeID") %>% 
  left_join(y = cons_dtm_mat, by = "employeeID")
  



### STEP 2: Building predictive models 

#################################################################################
# NEED TO TWEAK THE CODE BELOW
#################################################################################

## Creating train and test splits 
index = createDataPartition(gss_tbl$workhours, p = 0.75, list = FALSE)
gss_tbl_train = gss_tbl[index,]
gss_tbl_test = gss_tbl[-index,]

## Creating 10 folds used in cross-validation from training set
### Need to do it on y otherwise there might be fold overlap
training_folds = createFolds(gss_tbl_train$workhours, 10)

## Creating reusable trainControl object for all 4 models 
reuseControl = trainControl( method = "cv", number = 10, search = "grid", 
                             indexOut = training_folds, verboseIter = TRUE)




# Visualization
# Publication