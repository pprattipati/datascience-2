library(rpart)
library(caret)

setwd("D:/Data Science/Algorithmica/Restaurant/") # set the working directory

# reading all string variables as 'factors'
restaurant_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""), stringsAsFactors = TRUE)
dim(restaurant_train)
str(restaurant_train)

# Unable to convert factor variable to 'Date' directly; cast it to 'character' type first
# restaurant_train$Open.Date = as.Date(restaurant_train$Open.Date)
restaurant_train$Open.Date = as.character(restaurant_train$Open.Date)

# adding a level that was missing from 'train' data
levels(restaurant_train$Type)
levels(restaurant_train$Type) = c(levels(restaurant_train$Type), "MB")

# get the number of days from the reference date (the maximum date in the input)
restaurant_train$num_days = as.Date(c("31-12-2014"), format="%d-%m-%Y") - as.Date(restaurant_train$Open.Date, format="%m/%d/%Y")
restaurant_train$num_days = as.numeric(restaurant_train$num_days) # casting from 'difftime' class

restaurant_train1 = restaurant_train[,-c(1,2,3)] # filter unwanted features
dim(restaurant_train1)

# Increase number of folds since we are dealing with very less data
resample_strategy = trainControl(method="cv", number=50)
set.seed(100)
# CART can deal with categorical variables; so no need for dummy variables
cart_grid = expand.grid(.cp=0)
cart_model = train(revenue ~ ., restaurant_train1, method="rpart", trControl=resample_strategy, tuneGrid = cart_grid)
# do not set the "importance" flag for rpart. Setting it to TRUE or FALSE both give error.
varImp(cart_model)
# Although cp = 0, we have pre-puned based on the default numSplits=20 (min. num of nodes in leaf to split)

# We see that the most important variables are not necessarily the ones at the top; another approach used
# Important variables are computed by permuting a variable's column values in test data, and
# re-calculating the accuracy of test data. If accuracy goes down, the variable is important.

# CART uses a greedy approach to split at each level; hence the top values are not necessarily
# the most important.
cart_model$finalModel

cart_grid = expand.grid(.cp=0)
#  Gives error. "wrong model type for regression".

# C4.5 cannot be used for 'regression'. It can be used only for 'classification'. "C4.5 - C only for classification"
# The post-pruning strategy in C4.5 (which does not use the training data) cannot be applied on
# 'continuous' target variables. Hence C4.5 cannot be used for regression.
c4.5_model = train(revenue ~ ., restaurant_train1, method="J48", trControl=resample_strategy)

set.seed(100)
resample_strategy = trainControl(method="boot")
treebag_model = train(revenue ~ ., restaurant_train1, method="treebag", trControl=resample_strategy)
# performance not great since trees are related; most use P28, num_days as most important attributes
treebag_model
# Diff trees show diff variable importance because they are built on different random samples
treebag_model$finalModel$mtrees[[1]]
treebag_model$finalModel$mtrees[[2]]
treebag_model$finalModel$mtrees[[15]]