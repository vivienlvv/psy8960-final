# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

library(tm)
library(tidytext)
library(qdap)
library(textstem)
library(RWeka)

library(doParallel)
library(caret)

# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")




## Purpose: Build a ML model that predicts turnover ("Attrition"), i.e., a binary classification model 

### STEP 1: Feature Engineering 

#### 1.1 Creating language features from "pros" and "cons" of working at the company

##### 1.1.1 Creating corpus
corpus_pros = VCorpus(VectorSource(final_tbl$pros))
corpus_cons = VCorpus(VectorSource(final_tbl$cons))


##### 1.1.2 Begin actual corpus cleaning 
##### This is a function that allows cleanign text corpus- can be used for pros and cons responses
##### Input: corpus object 
cleaning_corpus = function(corpus_obj){
  cleaned_corpus = corpus_obj %>%
    tm_map(content_transformer(replace_abbreviation)) %>% 
    tm_map(content_transformer(replace_contraction)) %>% 
    tm_map(content_transformer(str_to_lower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, "[^[:alnum: ]]") %>%
    tm_map(removeNumbers) %>%
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

# corpus_pros[[1]]$content
# cleaned_corpus_pros[[1]]$content
# corpus_cons[[1]]$content
# cleaned_corpus_cons[[1]]$content


##### 1.1.3 Creating n-gram features from document-term matrix 
######  Creating Bigram DTM 

## Creating a filter function that filters out empty and NA responses
bigram_filter = function(x){
  bool = nchar(stripWhitespace(x$content)[[1]]) > 0 & !is.na(x$content[[1]])
  return(bool)
}


## Creating bigram tokenizer using Weka
bigram_Tokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min = 1, max = 2))
}

## Creating bigram DTM for pros and cons corpora 
### 1. I filtered out comments with only empty and NA responses using bigram_filter
### 2. I also used removeSparseTerms to get rid of n-grams with too little frequency 
pros_dtm = cleaned_corpus_pros %>%
  tm_filter(bigram_filter) %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) %>% 
  removeSparseTerms(sparse = 0.997)
cons_dtm = cleaned_corpus_cons %>% 
  tm_filter(bigram_filter) %>% 
  DocumentTermMatrix(control = list(tokenize = bigram_Tokenizer)) %>% 
  removeSparseTerms(sparse = 0.997)


#### 1.2 Combining features (n-grams & employee background)

##### Getting bigram features in matrix form to be merged with original dataset 
pros_dtm_mat = as.matrix(pros_dtm) 
pros_dtm_mat = data.frame(employeeID = rownames(pros_dtm_mat), pros_dtm_mat)

cons_dtm_mat = as.matrix(cons_dtm)
cons_dtm_mat = data.frame(employeeID = rownames(cons_dtm_mat), cons_dtm_mat)

# This is the demographic tibble where I removed the text entries and variables w/o variance
ml_bg_tbl =  final_tbl %>% 
  select(-c(pros, cons, EmployeeCount, StandardHours, Over18))

##### Creating Tibbles ready for training building classifiers
# 1. ML Tibble without language features 
ml_tbl = ml_bg_tbl %>% 
  select(-employeeID)

# 2. ML Tibble  with numeric and language features
ml_combined_tbl = ml_bg_tbl %>% 
  left_join(y = pros_dtm_mat, by = "employeeID") %>% 
  left_join(y = cons_dtm_mat, by = "employeeID") %>%
  select(-employeeID)
  



# Analysis 

### STEP 2: Building predictive models 

#### 2.1 Creating a function that takes full dataset as input and give models as output
#### This is to avoid writing separate chunks of code for the 2 datasets and also for training different ML models

model_building = function(input_tbl){
  
  set.seed(2023)
  
  ## Creating train and test splits 
  index = createDataPartition(input_tbl$Attrition, p = 0.75, list = FALSE)
  train_tbl = input_tbl[index,]
  test_tbl = input_tbl[-index,]
  
  ## Creating 10 folds used in cross-validation from training set
  ### Need to do it on y otherwise there might be fold overlap
  training_folds = createFolds(train_tbl$Attrition, 10)
  
  ## Creating reusable trainControl object for all models
  ### Using default grid search parameters provided by R because there is no reason
  ### for me to believe particular parameters may work better
  reuseControl = trainControl( method = "cv", number = 10, search = "grid", 
                               indexOut = training_folds, verboseIter = TRUE)
  
  
  ## Creating vector containing name for each algo
  mod_vec = c("glm" , "glmnet", "ranger", "xgbTree")
  
  
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
                data = train_tbl,
                method = method,
                metric = "Accuracy",
                na.action = na.pass,
                trControl = reuseControl,
                preProcess = pre_process)
    
    # Saving model from each iteration to the pre-defined list 
    mod_ls[[i]] = mod
  }
  
  return(list(mod_ls, test_tbl))
}


# 2.2 Model training 

## Turning on parallelization
local_cluster = makeCluster(detectCores() - 1)   
registerDoParallel(local_cluster)

## Training models with only background information
ml_tbl_output = model_building(ml_tbl)
## Training models with both background and language features
ml_combined_tbl_output = model_building(ml_combined_tbl)

## Turning off parallelization
stopCluster(local_cluster)
registerDoSEQ()




# Publication

## Answers to required questions:

### 1. My choice of final model
### Based on the table I created, I believe that the best model would be random forest if the selection criterion 
### chosen is accuracy. Random Forest has the greatest accuracy at 99% which means it is able to 99% of the time it is able to make predictions 99% of the time. Other models such as logistic regression (0.89) and elastic net (0.90) have lower accuracy. 

### 2. What characteristics of how you created the final model likely made the biggest impact in maximizing its performance? How do you know? Be sure to interpret specific numbers in the table you just created.
###


### 3.  What is the incremental predictive accuracy gained by including text data in your model versus not including text data?
### As shown in the table, there is not really incremental predictive accuracy gained by including text data in our model versus not including text data. This is likely because the text features are based on people's view of the advantages and disadvantages of working at the company which may already be highly correlated with other variables on background information such as job satisfaction. Since adding highly collinear features will not help improve model performance, we did not observe any notable incremental predictive accuracy by including text data. 


## Preparing output tables

### Creating a function that takes model list and test data as input and return 
### nicely formatted APA table 

results = function(train_mod = mod_ls[[1]], test_data = ml_tbl_test, features){
  algo = train_mod$method

  # Creating confusion matrix to get holdout metrics
  preds_y = predict(train_mod, newdata = test_data, na.action = na.pass) 
  obs_y = test_data$Attrition
  confusion_mat = confusionMatrix(preds_y, obs_y)
  
  cv_accuracy = str_remove(format(round(max(train_mod$results$Accuracy), 2), nsmall = 2), "^\\d")
  ho_accuracy = str_remove(format(round(sum(preds_y == test_data$Attrition)/nrow(test_data), 2), nsmall = 2), "^\\d")
  
  ho_precision = str_remove(format(round(confusion_mat$byClass[["Precision"]], 2), nsmall = 2), "^\\d")
  ho_recall = str_remove(format(round(confusion_mat$byClass[["Recall"]], 2), nsmall = 2), "^\\d")
  ho_f1 = str_remove(format(round(confusion_mat$byClass[["F1"]], 2), nsmall = 2), "^\\d")
    
  
  return(c("Features" = features,
           "Algorithm" = algo,
           "cv_Accuracy" = cv_accuracy,
           "ho_Accuracy" = ho_accuracy,
           "ho_Precision" = ho_precision,
           "ho_Recall" = ho_precision,
           "ho_F1" = ho_f1))
}


#### Obtaining ML output table with only background information in dataset
ml_output_tbl = as_tibble(t(sapply(ml_tbl_output[[1]], results,
                                   test_data = ml_tbl_output[[2]],
                                   features = "Background only")))

#### Obtaining ML output table with both backgroudn & language info in the dataset
ml_combined_output_tbl = as_tibble(t(sapply(ml_combined_tbl_output[[1]], results,
                                            test_data = ml_combined_tbl_output[[2]],
                                            features = "Background + Lang")))

#### Making final output table 
final_output_tbl = rbind(ml_output_tbl, ml_combined_output_tbl)
final_output_tbl


### Saving final output comparison table 
write_csv(final_output_tbl, "../out/part2_model_comparison.csv")
