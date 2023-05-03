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

library(caret)

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

#### Tibble without language features
ml_bg_tbl =  final_tbl %>% select(-pros, -cons, # Remove text entries
                               -EmployeeCount, # Remove these 3 variables with no variance
                               -StandardHours,
                               -Over18)
ml_tbl = ml_bg_tbl %>% select(-employeeID)

#### Tibble  with numeric and language features
ml_combined_tbl = ml_bg_tbl %>% 
  left_join(y = pros_dtm_mat, by = "employeeID") %>% 
  left_join(y = cons_dtm_mat, by = "employeeID") %>%
  select(-employeeID)
  



### STEP 2: Building predictive models 

#################################################################################
# NEED TO TWEAK THE CODE BELOW: ABSTRACTION FOR ML_COMBINED_TBL TOO
#################################################################################

## Creating train and test splits 
index = createDataPartition(ml_tbl$Attrition, p = 0.75, list = FALSE)
ml_tbl_train = ml_tbl[index,]
ml_tbl_test = ml_tbl[-index,]

## Creating 10 folds used in cross-validation from training set
### Need to do it on y otherwise there might be fold overlap
training_folds = createFolds(ml_tbl_train$Attrition, 10)

## Creating reusable trainControl object for all models
reuseControl = trainControl( method = "cv", number = 10, search = "grid", 
                             indexOut = training_folds, verboseIter = TRUE)


## Creating vector containing info for each method
mod_vec = c("glm" , "glmnet", "ranger", "xgbTree", "svmRadialCost")



## Turning on parallelization
local_cluster = makeCluster(detectCores() - 1)   
registerDoParallel(local_cluster)


## Model Training
mod_ls = list()

for(i in 1:length(mod_vec)){
  
  method = mod_vec[i]
  
  # Getting pre-processing options based on method used
  if(method == "glm" | method == "glmnet"){
    pre_process = c("center", "scale", "nzv", "medianImpute")
  }else{
    pre_process = "medianImpute"
  }
  
  # Training model 
  mod = train(Attrition ~ .,
              data = ml_tbl_train,
              method = method,
              metric = "Accuracy",
              na.action = na.pass,
              trControl = reuseControl,
              preProcess = pre_process)
  
  # Saving model from each iteration to the pre-defined list 
  mod_ls[[i]] = mod
}

## Turning off parallelization
stopCluster(local_cluster)
registerDoSEQ()


## Output
results = function(train_mod = mod_ls[[1]], test_data = ml_tbl_test){
  algo = train_mod$method
  cv_accuracy = str_remove(format(round(max(train_mod$results$Accuracy), 2), nsmall = 2), "^\\d")
  
  preds = predict(train_mod, newdata = test_data, na.action = na.pass)
  ho_accuracy = str_remove(format(round(sum(preds == ml_tbl_test$Attrition)/nrow(test_data), 2), nsmall = 2), "^\\d")
  return(c("algo" = algo, "cv_accuracy" = cv_accuracy, "ho_accuracy" = ho_accuracy ))
}



# Visualization
# Publication