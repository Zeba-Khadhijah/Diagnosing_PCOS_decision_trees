#set working directory
getwd()
setwd("C:/Users/LENOVO/Desktop/Placement/portfolio/proj 2")

#load required packages 
library(readxl)
library(readr)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(stats)
library(corrplot)
library(psych)
library(DescTools)
library(caret)
library(tree)
library(rpart)
library(rattle)

#reading the data

df <- read_excel("PCOS_data_without_infertility.xlsx", sheet = "Full_new")

#dropping redundant columns
df <- df[-c(1,45)]

#check data type
d_type <- sapply(df, class)
print(d_type)

#change character to numeric values
df[c(17, 25)]<- lapply(df[c(17, 25)], as.numeric)

#check for missing values
missing_val <- colSums(is.na(df))
print(missing_val)

#replace missing data with median value

df$"Marraige Status (Yrs)"[is.na(df$"Marraige Status (Yrs)")] <- median(
  df$"Marraige Status (Yrs)", na.rm = T)

df$`II    beta-HCG(mIU/mL)`[is.na(df$`II    beta-HCG(mIU/mL)`)]<- median(
  df$`II    beta-HCG(mIU/mL)`, na.rm = T)

df$`AMH(ng/mL)`[is.na(df$`AMH(ng/mL)`)]<- median(
  df$`AMH(ng/mL)`, na.rm = T)

df$`Fast food (Y/N)`[is.na(df$`Fast food (Y/N)`)]<- median(
  df$`Fast food (Y/N)`, na.rm = T)


#checking correlations

# choose the column to correlate with the others
outcome_var <- "PCOS (Y/N)"

# initialize an empty data frame to store the results
corr_df <- data.frame(col1 = character(), col2 = character(), correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# loop through all columns in the data frame
for (col in names(df)) {
  if (col != outcome_var) {
    # calculate the correlation and test for significance
    corr <- cor.test(df[[outcome_var]], df[[col]], method = "pearson")
    # check if the p-value is less than 0.05
    if (corr$p.value < 0.05) {
      # add the results to the data frame
      corr_df <- rbind(corr_df, data.frame(col1 = outcome_var, col2 = col, correlation = corr$estimate, p_value = corr$p.value, stringsAsFactors = FALSE))
    }
  }
}

# print the data frame
print(corr_df)

# There are 23 factors correlating with PCOS. 
# To avoid overfitting and get a comprehensive model, we will choose only
# the strongly correlated factors to make our model

new_df <-df[c(2,11,30,31,32,35,39,40)]

# calculate correlation matrix and significance values of new_df
corr_mat <- corr.test(new_df, method = "pearson")$r
p_mat <- corr.test(new_df, method = "pearson")$p

# combine correlation matrix and significance values into a single matrix
corr_p_mat <- matrix(paste(round(corr_mat, 2), ifelse(p_mat < 0.05, "*", " "), sep = ""), ncol = ncol(new_df))
rownames(corr_p_mat) <- colnames(corr_p_mat) <- colnames(new_df)
print(corr_p_mat)

#above matrix shows that the predictors are significantly correlated.

#Therefore a random forest classifier that is more robust to multicollinearity would be best. 

#Resampling

# check class distribution
table(new_df$'PCOS (Y/N)')

#change classifer to factor variable
new_df$'PCOS (Y/N)'<- factor(new_df$'PCOS (Y/N)')

# resample data using random undersampling of majority class
df_resampled <- downSample(new_df, new_df$'PCOS (Y/N)')

# split data into train and test sets
set.seed(123)
trainIndex <- createDataPartition(df_resampled$'PCOS (Y/N)', p = 0.7, list = FALSE)
train <- df_resampled[trainIndex, ]
test <- df_resampled[-trainIndex, ]

# Decision tree

# create the decision tree model
tree_model <- rpart(train$`PCOS (Y/N)` ~ ., data = train)

# Make predictions on testing set
predictions <- predict(tree_model, test, type = "class")

# Create confusion matrix
cm <- confusionMatrix(predictions, test$'PCOS (Y/N)')
print(cm)

# accuracy of 1 - all values predicted correctly



