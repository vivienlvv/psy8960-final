# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

library(tm)
library(tidytext)
library(qdap)
library(textstem)

library(RWeka)

# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")





# Analysis 

## Purpose: Build a ML model that predicts turnover (which I believe is the "Attrition column"?), binary 

### STEP 1: Feature Engineering 

#### 1.1 Creating language features from "pros" and "cons"

##### 1.1.1 Creating corpus
pro_tbl = final_tbl %>% select(doc_id = employeeID, text = pros)
con_tbl = final_tbl %>% select(doc_id = employeeID, text = cons)

corpus_pros = VCorpus(VectorSource(pro_tbl))
corpus_cons = VCorpus(VectorSource(con_tbl))




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

# corpus_pros[[2]]$content[2]
# cleaned_corpus_pros[[2]]$content[2]
# 
# corpus_cons[[2]]$content[2]
# cleaned_corpus_cons[[2]]$content[2]


##### 1.1.3 Creating n-gram features from document-term matrix 

######  Creating Bigram DTM 

## Creating bigram tokenizer using Weka
bigram_Tokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min=1, max=2))
}
## Creating bigram DTM for pros and cons corpora 
pros_dtm = cleaned_corpus_pros %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) 
cons_dtm = cleaned_corpus_cons %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) 

############################################################################  
# NEED TO DEAL WITH ERROR WITH RWEKA
############################################################################                                      




### STEP 2: Building predictive models 




# Visualization
# Publication