# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# ITEMS TO CHECK: H2, H3 viz and test


# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")

## Analysis 

## Hypothesis 1. There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
h1 = final_tbl%>% cor_test(MonthlyIncome, PerformanceRating)
h1

## Hypothesis 2. Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).

## h2 = aov(MonthlyIncome ~ Department, data = final_tbl)
h2 = final_tbl %>% anova_test(MonthlyIncome ~ Department)
anova_summary(h2)

### NEED TO TURN SUMMARY INTO A TABLE! AND FIGURE OTU WHICH ANOVA OPTION TO GO WITH

## Hypothesis 3. Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
h3 = lm(YearsAtCompany ~ 1 + RelationshipSatisfaction + RelationshipSatisfaction * Gender, data = final_tbl)
summary(h3)
h3_preds = fitted(h3)


# Visualization

## Hypothesis 1: Scatterplot
final_tbl %>% ggplot(aes(x = MonthlyIncome, y = PerformanceRating)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  labs(title = "Figure 1. Scatterplot for Monthly Income and Performance Ratings",
       x = "Monthly Income", y = "Performance Ratings") + 
  theme_minimal()


## Hypothesis 2: Boxplot
final_tbl %>% ggplot(aes(x = Department, y = MonthlyIncome)) + 
  geom_boxplot() +
  labs(title = "Figure 2. Boxplot for Monthly Income by Department",
       x = "Department", y = "Monthly Income") + 
  theme_minimal()

## Hypothesis 3 (VERY CONFUSED BY THIS)
h3_tbl = cbind(h3_preds, final_tbl)
h3_tbl %>% ggplot(aes(x = RelationshipSatisfaction, y = h3_preds, color = Gender, fill = Gender)) + 
  geom_point() + 
  # geom_jitter(width=.1) + 
  geom_smooth(method = "lm", se = FALSE)

h3_tbl %>% ggplot(aes(x = RelationshipSatisfaction, y = YearsAtCompany, color = Gender, fill = Gender)) + 
  geom_jitter(width=.1) + 
  geom_smooth(method = "lm", se = FALSE)


# Analysis 
# Publication