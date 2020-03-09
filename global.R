####################################
#### Messy Data - Global.R ###
####################################

library(tidyverse)
library(corrplot)
library(caTools)
library(ROCR)
library(plotROC)
library(gplots)

# general data transformation 

###### tidyverse
# Read CSV into R
df1 <- read.csv(file="messy-data.csv", header=TRUE, sep=",", stringsAsFactors = F)


# set as general option that Strings are not factors
options(stringsAsFactors = FALSE)

# save all column names
old_names1 <- colnames(df1)

new_names = c("np_id","np_look_work", "np_current_work",
              "hf_A", "ac_A", 
              "hf_B", "ac_B",
              "hf_C", "ac_C", 
              "hf_D", "ac_D",
              "hf_E", "ac_E",
              "hf_F", "ac_F",
              "cs_1", "se_1",
              "cs_2", "se_2",
              "cs_3", "se_3",
              "cs_4", "se_4",
              "cs_5", "se_5",
              "cs_6", "se_6",
              "cs_7", "se_7",
              "cs_8", "se_8",
              "np_before_work",
              "np_keen_move",
              "np_city_size",
              "np_gender",
              "np_age",
              "np_education")

# rename columns with old_names and new_names vectors
df1 <- df1 %>% rename_at(vars(old_names1), ~ new_names)

# filter rows with yes for Column look_job
# code needs dplyr::filter to ensure base filter not used
df1 <- dplyr::filter(df1, np_look_work == "yes")

# match all strings ending with job and replace with 1
# .* 
df1 <- df1 %>% 
  mutate_all(funs(str_replace_all(., '.*job$', '1')))

# reorder columns
df1 <- df1 %>% select(np_id, ends_with('work'), np_before_work:np_education, starts_with('hf_'), 
                      starts_with('ac_'), starts_with('cs_'), starts_with('se_'))

# change the blanks to NA
df1 <- mutate_all(df1, funs(na_if(.,"")))


# pivot_longer  - transforms the columns in wide format starting with 'hf' and 'ac' to long format in separate columns
# names_to parameters:
# .value = contains metadata on the cell values that correspond to the original columns 
# these values are pivoted in long format and added in a new columns "hf" and "ac"
# column "group" has the original column endings (e.g. the numbers 1-6) pivoted to long format
# names_pattern = regex argument specifying character "_" where column names are to be broken up

# create key column com_level for 6 levels of commuting and move values to long format
df1 <- df1 %>% 
  tidyr::pivot_longer(cols = c(starts_with("hf"), starts_with("ac")),
                      names_to = c(".value", "com_level"), 
                      names_pattern = "(.*)_(.*)" 
  )

# create key column sal_level for 8 levels of salary and move values to long format
df1 <- df1 %>% 
  tidyr::pivot_longer(cols = c(starts_with("cs"), starts_with("se")),
                      names_to = c(".value", "sal_level"), 
                      names_pattern = "(.*)_(.*)"                      
                      # values_drop_na = TRUE
  )


# omit only those rows where columns hf, ac  are all NA 
df1 <- df1[!(is.na(df1$hf)) | !(is.na(df1$ac)),] 
# omit only those rows where columns cs, se are all NA 
df1 <- df1[!(is.na(df1$cs)) | !(is.na(df1$se)),]

# set up character vectors for old and new column names
col.from <- c("hf", "ac", "cs", "se")
col.to <- c("hf_com", "ab_com", "curr_sal", "exp_sal")

# rename old columns
df1 <- df1 %>% rename_at(vars(col.from), function(x) col.to)

################ Task 1 - distribution of the expected net income (se) in relation to the current net income (cs)  

############### create matrix for calculation of statistics ############################


# create table with frequency counts for exp_sal and curr_sal per category of level
# 
cs_es_table <- df1 %>%
  dplyr::count(sal_level, exp_sal, curr_sal) %>%
  tidyr::spread(key = sal_level,value = n) %>% 
  select(curr_sal, exp_sal, 1, 2, 3, 4, 5, 6, 7, -8) %>% # reorder columns and omit Column 8 (no answer) 
  as.data.frame()

# save all column names
old_cs_es_names <- colnames(cs_es_table)

# old_cs_es_names
# create vector with new column names for salary levels - no entries for 5001-6000 EUR or > 6000 EUR
new_sal_names1 <- c("curr_sal",
                    "exp_sal", 
                    "< 1000 EUR", 
                    "1001-1500 EUR", 
                    "2001-3000 EUR", 
                    "3001-4000 EUR", 
                    "4001-5000 EUR")


# rename columns with old_names and new_names vectors
cs_es_table <- cs_es_table %>% rename_at(vars(old_cs_es_names), ~ new_sal_names1)

# convert cs_es_table to long format and summarise exp_sal and curr_sal frequencies
cs_es_table <- cs_es_table %>% 
  gather(key, value, -curr_sal,-exp_sal) %>% # crucial step to make data long
  mutate(curr_val = ifelse(curr_sal == 1,value,NA),
         exp_val = ifelse(exp_sal == 1,value,NA)) %>% #mutate actually cleans up the data and assigns a value to each new column for 'exp' and 'curr'
  group_by(key) %>% #for your summary, because you want to sum up your previous rows which are now assigned a key in a new column
  summarise_at( .vars = vars(curr_val, exp_val), .funs = sum, na.rm = TRUE)

# move column heading to rowname
cs_es_table <- cs_es_table %>% 
  column_to_rownames(var = "key") %>% 
  as.data.frame()

# save all new column names
old_cs_es_names2 <- colnames(cs_es_table)

new_sal_names2 <- c("Current Salary",
                    "Expected Salary")

# rename columns with old_names and new_names vectors
cs_es_table <- cs_es_table %>% rename_at(vars(old_cs_es_names2), ~ new_sal_names2)

################ Task 2 - correlations desire to earn more (se) between possibility to commute longer (ab) 

############### create matrix for calculation of statistics ############################

# select the columns to check for correlations
# this counts all the exp_sal by sal_level and all the ab_com by com_level
# this is similar to the result in task 1
es_ab_table <- df1 %>%
  dplyr::count(sal_level, exp_sal, com_level, ab_com) %>%
  as.data.frame()

######## pivot salary columns wider - remove ab_com and NA
es_ab_table <- es_ab_table %>%
  pivot_wider(names_from = c(sal_level, exp_sal), values_from = n) %>%
  select(-ab_com, -ends_with("NA"))

#  need to combine and summarise rows
es_ab_table <- es_ab_table %>%
  group_by(com_level) %>%
  summarise_all(sum) %>%
  ungroup()

old_es_ab_names <- colnames(es_ab_table)

new_sal_names3 <- c("com_level",
                    "< 1000 EUR", 
                    "1001-1500 EUR", 
                    "2001-3000 EUR", 
                    "3001-4000 EUR", 
                    "4001-5000 EUR",
                    "5001-6000 EUR",
                    "> 6000 EUR",
                    "no answer")


# rename columns with old_names and new_names vectors
es_ab_table <- es_ab_table %>% rename_at(vars(old_es_ab_names), ~ new_sal_names3)

# recode values in the first column - need to specify library dplyr with recode function

es_ab_table <- es_ab_table %>% mutate(com_level=dplyr::recode(com_level, "A" = "< 5 km",
                                                              "B" = "5 - 10 km",
                                                              "C" = "11 - 20 km",
                                                              "D" = "21 - 50 km",
                                                              "E" = "51 - 100 km",
                                                              "F" = "> 100 km"))

# replace NA values with zeros and remove "no answer column"
es_ab_table <- es_ab_table %>% mutate_all(funs(replace_na(., 0))) %>%
  select(-c("no answer"))

# move column heading to rowname
es_ab_table <- es_ab_table %>% 
  column_to_rownames(var = "com_level") %>% 
  as.data.frame()

############## functions for calculating statistics and preparing graph parameters Tasks 1 and 2 #############################

# function to perform chi square test with following options:
# correct=FALSE - don't apply continuity correction when computing the test statistic for 2 by 2 tables:
# simulate.p.value = true - compute p-values by Monte Carlo simulation.

chisq_function <- function(table) {
  chisq <- chisq.test(table, correct=FALSE, simulate.p.value = TRUE)
}  

# function to prepare contingency table data for ggplot2 use
ggplot_prep_function <- function(table) {
  # 1. convert the data as a table
  dt <- as.table(as.matrix(table))
  
  # 2. Convert table into data frame so ggplot can read it
  dt1 <- as.data.frame(dt)
  return(dt1)
}

################ Task 3 - key factor that determines the tendency to move closer to the work place (keen_move) and other factors (what is the most relevant factor influencing the will to move home closer work) 

# Regression - level 2
# independent variables - all can be made into factors
# - current_work 
# - before_work,
# - city_size 
# - gender,
# - age,
# - education
# - current salary levels
# - expected salary levels
# - how far commuting
# - ability to commute

###### Preprocessing Data Level 1 #################################

# rename columns to remove `np_` prefix
df_level1 <- df1 %>%
  dplyr::rename_all(
    funs(stringr::str_replace_all(., "np_", ""))
  )

# set up vector with variables to be converted from characters to factors
chr_to_factor_vars <- 
  c("keen_move", "current_work", "before_work",
    "city_size", "gender", "age", "education")

# convert character variables to factors
df_level1 <-
  df_level1 %>%
  mutate_at(.funs = funs(as.factor), .vars = chr_to_factor_vars)

############## functions for calculating statistics - Task 3 level 2 #############################

# logistic regression function for passing x and y parameters plus the dataset
glm_function<-function(y, x, dataset){
  f <- as.formula(paste(y, x, sep="~"))
  predictorvariable <- glm(formula=f, data=dataset, family = "binomial")
  return(predictorvariable)
}

# function for basic R plot with colours and threshold labels
plot_ROC_function <- function(plot_dataset, plot_title, plot_subtitle) {
  plot(plot_dataset, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
  title(plot_title, sub = plot_subtitle,
        cex.main = 2,   font.main= 4, col.main= "blue",
        cex.sub = 0.75, font.sub = 3, col.sub = "red")
}

# specificity sensitivity function
sesp_func <- function(sesp_variable, predict_variable, threshold) {
  # sensitivity = c()
  # specificity = c()
  # confmat_train = c()
  confmat_train = table(sesp_variable, predict_variable > threshold)
  sensitivity = confmat_train[1 , 1] / sum(confmat_train[ , 1])
  specificity = confmat_train[2 , 2] / sum(confmat_train[ , 2])
  return(list(sensitivity, specificity))
}

###### Preprocessing Data Level 2 #################################
# put in code to change all NAs to zeros in df1 for logistic regression  

# convert all NAs in columns curr_sal, exp_sal, hf_com, ab_com to 0 to allow for logistic regression based on factors
df_level2 <- df1 %>% 
  mutate_at(vars(curr_sal, 
                 exp_sal,
                 hf_com,
                 ab_com), ~replace_na(., 0))

# rename columns to remove `np_` prefix
df_level2 <- df_level2 %>%
  dplyr::rename_all(
    funs(stringr::str_replace_all(., "np_", ""))
  )

# set up vector with variables to be converted from characters to factors
chr_to_factor_vars_2 <- 
  c("keen_move", "current_work", "before_work",
    "city_size", "gender", "age", "education", "com_level", "sal_level")

# convert character variables to factors
df_level2 <-
  df_level2 %>%
  mutate_at(.funs = funs(as.factor), .vars = chr_to_factor_vars_2)

# set up vector with variables to be converted from numbers to factors

num_to_factors_vars <- c("hf_com", "ab_com", "curr_sal", "exp_sal")

# convert number variables to factors
df_level2 <-
  df_level2 %>%
  mutate_at(.funs = funs(as.factor), .vars = num_to_factors_vars)

############## preparing model parameters Task 3 level 2 #############################

# set up parameters for logistic regression with keen to move as dependent factor and independent level 2 factors
y <- "keen_move"
x <- "current_work + city_size + gender + age + education + hf_com + ab_com + curr_sal + exp_sal"
dataset <- df_level2

# calculate logistic regression with keen to move as dependent factor and independent level 2 factors
model2 <- glm_function(y, x, dataset)
summary(model2)

# logistic regression with training and test
# split df_level2 into training and test datasets
split <- sample.split(df_level2$keen_move, SplitRatio = 0.75)

level2_Train = subset(df_level2, split == TRUE)
level2_Test = subset(df_level2, split == FALSE) 

# display the number of rows in each dataset  
nrow(level2_Train)
nrow(level2_Test)

# convert columns to factor based on chr_to_factor_vars_2
level2_Train <-
  level2_Train %>%
  mutate_at(.funs = funs(as.factor), .vars = chr_to_factor_vars_2)

# convert columns to factor based on num_to_factors_vars 
level2_Train <-
  level2_Train %>%
  mutate_at(.funs = funs(as.factor), .vars = num_to_factors_vars)

# set up parameters for:
# logistic regression with keen to move as dependent factor and independent level 2 factors and training set
y <- "keen_move"
x <- "current_work + city_size + gender + age + education + hf_com + ab_com + curr_sal + exp_sal"
dataset <- level2_Train

# calculate logistic regression with keen to move as dependent factor and independent level 2 factors and training set
Level2_Train_Log <- glm_function(y, x, dataset)
summary(Level2_Train_Log)

# Training set predictions from level 1 training set logistic regression
predictTrain2 <- predict(Level2_Train_Log, type="response")
summary(predictTrain2)

# apply predictions to level2_train dataset mean
tapply(predictTrain2, level2_Train$keen_move, mean)


# Confusion matrix for threshold of 0.5 for training set
Train_5_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.5)

# Confusion matrix for threshold of 0.7 for training set
Train_7_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.6)

# Confusion matrix for threshold of 0.2 for training set
Train_2_2 <- sesp_func(level2_Train$keen_move, predictTrain2, 0.2)

# calculate ROC (Receiver Operating Characteristics) predictions for training set
ROCRpred2 = prediction(predictTrain2, level2_Train$keen_move)

# Performance function
ROCRperf2 = performance(ROCRpred2, "tpr", "fpr")

#  apply Training set predictions from level 2 training set logistic regression to test dataset
predictTest2 <- predict(Level2_Train_Log, type = "response", newdata = level2_Test)

# Predict the accuracy of the model with the test set
pr <- prediction(predictTest2, level2_Test$keen_move)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]


# Confusion matrix for threshold of 0.6 for training set
Test_6_2 <- sesp_func(level2_Test$keen_move, predictTest2, 0.6)

# convert list to dataframe to round numbers
Test_6_2_df <- data.frame(matrix(unlist(Test_6_2), nrow=length(Test_6_2), byrow=T))





