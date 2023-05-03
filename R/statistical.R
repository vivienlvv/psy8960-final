# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

# ITEMS TO CHECK: H2, H3 viz and test


# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")




# Analysis 

## Hypothesis 1. There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
h1 = final_tbl%>% cor_test(MonthlyIncome, PerformanceRating)
h1

## Hypothesis 2. Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).

## h2 = aov(MonthlyIncome ~ Department, data = final_tbl)
h2 = final_tbl %>% anova_test(MonthlyIncome ~ Department, detailed = TRUE)

### Extracting information to construct ANOVA table in publication 
component_name = c("SS_Between", "SS_within", "SS_total")

SS_between = h2$SSn
SS_within = h2$SSd

df_between = h2$DFn
df_within = h2$DFd

MS_between = SS_between/df_between
MS_within = SS_within/df_within

f_val = h2$F
p_val = h2$p


  
  
## Hypothesis 3. Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
h3 = lm(YearsAtCompany ~ 1 + RelationshipSatisfaction + RelationshipSatisfaction * Gender, data = final_tbl)
summary(h3)
h3_preds = fitted(h3)



# Visualization

## Hypothesis 1: Scatterplot
final_tbl %>% ggplot(aes(x = MonthlyIncome, y = PerformanceRating)) + 
  geom_point(position = "jitter") + 
  geom_smooth(se = FALSE) + 
  labs(title = "Figure 1. Scatterplot for Monthly Income and Performance Ratings",
       x = "Monthly Income ($)", y = "Performance Ratings") + 
  theme_minimal()


## Hypothesis 2: Boxplot
final_tbl %>% ggplot(aes(x = Department, y = MonthlyIncome)) + 
  geom_boxplot() +
  labs(title = "Figure 2. Boxplot for Monthly Income by Department",
       x = "Department", y = "Monthly Income ($)") + 
  theme_minimal()

## Hypothesis 3 (VERY CONFUSED BY THIS)
h3_tbl = cbind(h3_preds, final_tbl)
h3_tbl %>% ggplot(aes(x = RelationshipSatisfaction, y = h3_preds, color = Gender, fill = Gender)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.1, jitter.height = 0.25)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Figure 3. Scatterplot for relationship statisfaction and predicted tenure",
       x = "Relationship Statisfaction", y = "Predictd Tenure (years)") + 
  theme_minimal()


# Publication

## Publication results for H1 
### Publication ready sentence: There is a slight negative and non-significant relationship between monthly income and performance ratings, r(1468) = -.02, p = 0.512 > 0.05. This means that there is virtually no relationship between monthly income and performance ratings. This is likely due to range restriction in our dependent variable, performance ratings which have a range of 3-4. 
paste0("The correlation betweeen monthly income and performance ratings was r(",
       nrow(final_tbl)-2, 
       ") = ", 
       h1$cor,
       ", p = ",
       h1$p,
       ". This test was ",
       ifelse(h1$p <= .05, "", "not "),
       "statistically significant.")


## Publication results for H2 
### Publication ready sentence: 
data.frame(Component = component_name,
           SS = c(SS_between, SS_within, SS_between + SS_within),
           df = c(df_between, df_within, df_between + df_within),
           MS = c(MS_between, MS_within, ""),
           "F" = c(f_val, "", ""),
           "p" = c(p_val, "", ""))


## Publication results for H3 
### Publication ready sentence:

tidy(h3) %>%
  select(-std.error) # Need to come up with something to format it properly


#### Creating functions to help format all numbers properly
format_num = function(x){ # x = 0.567, 105.669
  return(str_remove(format(round(x, 2), nsmall = 2), "^0|^-0")) ## current issue: cannot deal with megative values
}


