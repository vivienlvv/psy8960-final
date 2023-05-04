# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)

#################################################################################
# NEED TO ADD IN INTERPRETATION IN PUB SECTION, display for H3 is a bit weird, need to re-save
#################################################################################

# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")


# Analysis 

## Hypothesis 1. There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
h1 = final_tbl%>% cor_test(MonthlyIncome, PerformanceRating)
h1

## Hypothesis 2. Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).

## h2 = aov(MonthlyIncome ~ Department, data = final_tbl)
h2 = final_tbl %>% anova_test(MonthlyIncome ~ Department, detailed = TRUE)
h2


## Hypothesis 3. Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
h3 = lm(YearsAtCompany ~ 1 + RelationshipSatisfaction + RelationshipSatisfaction * Gender, data = final_tbl)
H3_summary = summary(h3)
### These predicted values are used for creating scatterplot
h3_preds = fitted(h3)



# Visualization

## Hypothesis 1: Scatterplot
(final_tbl %>% ggplot(aes(x = MonthlyIncome, y = PerformanceRating)) + 
  geom_point(position = "jitter") + 
  geom_smooth(se = FALSE) + 
  labs(title = "Figure 1. Scatterplot for Monthly Income and Performance Ratings",
       x = "Monthly Income ($)", y = "Performance Ratings") + 
  theme_minimal()) %>% 
  ggsave(filename="../fig/H1.png", units="px", width=1920, height=1080)


## Hypothesis 2: Boxplot
(final_tbl %>% ggplot(aes(x = Department, y = MonthlyIncome)) + 
  geom_boxplot() +
  labs(title = "Figure 2. Boxplot for Monthly Income by Department",
       x = "Department", y = "Monthly Income ($)") + 
  theme_minimal()) %>% 
  ggsave(filename="../fig/H2.png", units="px", width=1920, height=1080)


## Hypothesis 3 (VERY CONFUSED BY THIS)
h3_tbl = cbind(h3_preds, final_tbl)
(h3_tbl %>% ggplot(aes(x = RelationshipSatisfaction, y = h3_preds, color = Gender, fill = Gender)) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.1, jitter.height = 0.25)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Figure 3. Scatterplot for relationship statisfaction and predicted tenure",
       x = "Relationship Statisfaction", y = "Predictd Tenure (years)") + 
  theme_minimal() )%>%
  ggsave(filename="../fig/H3.png", units="px", width=1920, height=1080)



# Publication

## Writing custom function to display all numbers properly 
display_num = function(number){
    # Removing leading zeroes 
    return_number = format(round(number,2), nsmall = 2)
    
    # Only changes things if these are decimals less than 1 or greater than -1 
    return_number = str_replace(return_number, "^0|^-0","")
    sign_positive = number > 0 
      # This only applies to negative numbers greater than -1 
    return_number = ifelse(sign_positive == FALSE & number > -1,
                           paste0("-",return_number),
                           return_number)
    return(return_number)
}

## Publication results for H1 
### Interpretation: The correlation between monthly income and performance ratings was r(1468) = -0.017, p = 0.512. This test was not statistically significant. This means that there is virtually no relationship between monthly income and performance ratings. Our hypothesis (1) that there is a relationship between monthly pay and performance rating is rejected using an alpha value of .05, although this is likely due to range restriction in our dependent variable, performance ratings which have a range of 3-4.
paste0("The correlation between monthly income and performance ratings was r(",
       nrow(final_tbl)-2, 
       ") = ", 
       display_num,
       ", p = ",
       display_num(h1$p),
       ". This test was ",
       ifelse(h1$p <= .05, "", "not "),
       "statistically significant.")




## Publication results for H2 
### Interpretation: 

#### Extracting information to construct ANOVA table in publication 
component_name = c("SS_Between", "SS_within", "SS_total")

SS_between = h2$SSn
SS_within = h2$SSd
SS_total = SS_between + SS_within

df_between = h2$DFn
df_within = h2$DFd
df_total = df_between + df_within

MS_between = SS_between/df_between
MS_within = SS_within/df_within

f_val = h2$F
p_val = h2$p

H2_tbl = data.frame(Component = component_name,
           SS = sapply(c(SS_between, SS_within, SS_total), display_num),
           df = sapply(c(df_between, df_within, df_total), display_num),
           MS = c(display_num(MS_between), display_num(MS_within), ""),
           "F" = c(display_num(f_val), "", ""),
           "p" = c(display_num(p_val), "", "")) 
H2_tbl
write_csv(H2_tbl, "../out/H2.csv")


## Publication results for H3 
### Interpretation: 
options(scipen = 999)

H3_tbl = tidy(h3) %>%
  select(-std.error) %>% 
  as.data.frame() %>%
  mutate(across(where(is.numeric), display_num))  #### WHY IS COEF AND T-STAT NOT WORKING??
colnames(H3_tbl) = c("regression_term,", "coefficient", "t_stat", "p_val")
H3_tbl
write_csv(H3_tbl, "../out/H3.csv")



