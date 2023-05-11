# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rstatix)
options(scipen = 999) # Turning off scientific notation




# Data Import and Cleaning
final_tbl = readRDS("../data/combined_tbl.RDS")




# Analysis 

## H1. There is a relationship between monthly pay and performance rating. Correlation and significance test, with a scatterplot and fit line.
h1 = final_tbl%>% cor_test(MonthlyIncome, PerformanceRating)
h1

## H2. Monthly pay differs by department. ANOVA and significance tests, with a boxplot split by department. Include a traditional ANOVA summary table (component name, SS, df, MS, F, p).
h2 = final_tbl %>% anova_test(MonthlyIncome ~ Department, detailed = TRUE)
h2

## H3. Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender. Regression and significance tests, with scatterplot and fit lines. Note that you’ll need to plot predicted values (i.e., marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful labels.
h3 = lm(YearsAtCompany ~ 1 + RelationshipSatisfaction + RelationshipSatisfaction * Gender, data = final_tbl)
H3_summary = summary(h3)
h3_preds = fitted(h3) # Getting predicted values for scatterplot
h3_tbl = cbind(h3_preds, final_tbl)




# Visualization

## Hypothesis 1: Scatterplot
(final_tbl %>% ggplot(aes(x = MonthlyIncome, y = PerformanceRating)) + 
  geom_point(position = "jitter", alpha = 0.5) + 
  geom_smooth(se = FALSE) + 
  labs(title = "Figure 1. Scatterplot for Monthly Income and Performance Ratings",
       x = "Monthly Income ($)", y = "Performance Ratings") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = -0.6, vjust=2))) %>% 
  ggsave(filename="../fig/H1.png", units = "px", width = 1920, height = 1080)


## Hypothesis 2: Boxplot
(final_tbl %>% ggplot(aes(x = Department, y = MonthlyIncome)) + 
  geom_boxplot() +
  labs(title = "Figure 2. Boxplot for Monthly Income by Department",
       x = "Department", y = "Monthly Income ($)") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.4, vjust=2))) %>% 
  ggsave(filename="../fig/H2.png", units = "px", width = 1920, height = 1080)


## Hypothesis 3: Scatterplot 
(h3_tbl %>% 
    ggplot(aes(x = RelationshipSatisfaction, y = h3_preds,
               color = Gender, fill = Gender)) + 
    geom_point(position = position_jitterdodge(dodge.width = 0.1, jitter.height = 0.25),
               alpha = 0.5) + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Figure 3. Scatterplot for the relationship statisfaction and predicted tenure",
       x = "Relationship Statisfaction",
       y = "Predictd Tenure (years)") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, hjust = 0.2, vjust=2))) %>% 
  ggsave(filename="../fig/H3.png", units = "px", width = 1920, height = 1080)




# Publication

## Writing custom function to display all numbers properly in APA style
display_num = function(number){
  
    # Rounding & Retaining only two decimals
    ## I added trimws() because for some reason there is an extra space in some numbers  
    return_number = trimws(format(round(number,2), nsmall = 2))
    
    # Only changes things if these are decimals less than 1 or greater than -1
    return_number = str_remove(return_number, "^0|^-0")
    sign_positive = number > 0
    # This only applies to negative numbers greater than -1
    return_number = ifelse(number < 0 & number > -1, paste0("-",return_number), return_number)

    return(return_number)
}

## Publication results for H1:
# Interpretation: The correlation between monthly income and performance ratings
# was r(1468) = -.02, p = .51. This test was not statistically significant using
# an alpha value of .05. We fail to reject our null hypothesis that there is no 
# relationship between between monthly pay and performance ratings. In other words,
# our Hypothesis 1 that there is a relationship between monthly pay and performance rating is not supported by our findings.
# *** although this is likely due to range restriction in our dependent variable, performance ratings which have a range of 3-4.
paste0("The correlation between monthly income and performance ratings was r(",
       nrow(final_tbl) - 2, 
       ") = ", 
       display_num(h1$cor),
       ", p = ",
       display_num(h1$p),
       ". This test was ",
       ifelse(h1$p <= .05, "", "not "),
       "statistically significant using an alpha value of .05. We ",
       ifelse(h1$p <= .05, "", "fail to "),
       "reject our null hypothesis that there is no relationship between between monthly pay and performance ratings. In other words, our Hypothesis 1 that there is a relationship between monthly pay and performance rating is ",
       ifelse(h1$p <= .05, "", "not "),
       "supported by our findings.")


## Publication results for H2:
#### Extracting information to construct ANOVA table in publication 
component_name = c("SS_Between", "SS_within", "SS_total")

SS_between = h2$SSn
SS_within = h2$SSd
SS_total = SS_between + SS_within

df_between = h2$DFn
df_within = h2$DFd
df_total = df_between + df_within

MS_between = display_num(SS_between/df_between)
MS_within = display_num(SS_within/df_within)

f_val = display_num(h2$F)
p_val = display_num(h2$p)

#### Building required ANOVA table for publication 
H2_tbl = data.frame(Component = component_name,
           SS = sapply(c(SS_between, SS_within, SS_total), display_num),
           df = sapply(c(df_between, df_within, df_total), display_num),
           MS = c(MS_between, MS_within, ""),
           "F" = c(f_val, "", ""),
           "p" = c(p_val, "", "")) 
H2_tbl
write_csv(H2_tbl, "../out/H2.csv")

# Interpretation (Publication ready sentence): 
# "Given the output from the ANOVA table, we can see that monthly pay is significantly different by department, F(2,1467) = 3.20, p = .04. Using alpha = .05, we reject the null hypothesis that monthly income does not differ by department. In other words, our evidence supports the Hypothesis 2 that monthly pay does differ by department."
paste0("Given the output from the ANOVA table, we can see that monthly pay is ",
       ifelse(as.numeric(p_val) <= .05, "", "not "),
       "significantly different by department, ",
       "F(", df_between, ",", df_within, ") = ", f_val, 
       ", p = ", p_val, ". Using alpha = .05, we ",
       ifelse(as.numeric(p_val) <= .05, "", "fail to "),
       "reject the null hypothesis that monthly income does not differ by department. ",
       "In other words, our evidence ",
       ifelse(as.numeric(p_val) <= .05, "supports ", "does not support "),
       "the Hypothesis 2 that monthly pay does differ by department.")


## Publication results for H3: 
### Building regression table for publication
H3_tbl = H3_summary$coefficients %>%
  as.data.frame() %>% 
  select(-`Std. Error`) %>% 
  mutate(across(where(is.numeric), display_num)) # Applying formatting
H3_tbl = cbind(term = c("Intercept",
                        "Relationship Satisfaction",
                        "Gender",
                        "Relationship Satisfaction x Gender"),
                 H3_tbl)
colnames(H3_tbl) = c("term", "coefficient", "t_stat", "p_val")
rownames(H3_tbl) = 1:4
H3_tbl
write_csv(H3_tbl, "../out/H3.csv")

# Interpretation (Publication ready sentence): 
# Given the output from the regression table, the main effect of relationship satisfaction (b = .37, p = .11) on tenure is not significant and the moderating effect of gender on such relationship (b = -.43,p = .16), is not significant at alpha = .05. In other words, the results do not provide support for Hypothesis 3 which states that tenure can be predicted from relationship satisfaction and that such relationship is moderated by gender.
paste0("Given the output from the regression table, the main effect of relationship satisfaction (b = ", H3_tbl$coefficient[2],
       ", p = ", H3_tbl$p_val[2]
       , ") on tenure is ",
       ifelse(as.numeric(H3_tbl$p_val[2]) <= .05, "", "not "), "significant ",
       "and the moderating effect of gender on such relationship (b = ",
       H3_tbl$coefficient[4], ",p = ", H3_tbl$p_val[4], "), is ",
       ifelse(as.numeric(H3_tbl$p_val[4]) <= .05, "", "not "), "significant at alpha = .05. ",
       "In other words, the results do ",
       ifelse(as.numeric(H3_tbl$p_val[2]) > .05 | as.numeric(H3_tbl$p_val[4]) > .05, "not ", " "),
       "provide support for Hypothesis 3 which states that tenure can be predicted from relationship satisfaction and that such relationship is moderated by gender.")



