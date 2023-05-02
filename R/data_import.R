# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)


# Data Import and Cleaning

## Importing data
employee_tbl = read_delim("../data/dataset.csv", delim = "+", col_names = TRUE)
review_tbl =  read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = c("pros", "cons", "employeeID"))

## Merging the two datasets by employeeID 
combined_tbl = employee_tbl %>%
  mutate(employeeID = 1:n(),
         # Turning all strings to factor 
         across(where(is.character), as.factor)) %>%
  # Performing left join to keep all employees because there are missing satisfaction reviews
  left_join(y = review_tbl, by = "employeeID")

## Saving joined data into an object
saveRDS(combined_tbl, "../data/combined_tbl.RDS")
  
