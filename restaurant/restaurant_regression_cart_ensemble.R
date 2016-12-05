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
# We see that the most important variables are not necessarily the ones at the top; some other approach used
cart_model$finalModel

cart_grid = expand.grid(.cp=0)
#  Gives error. "wrong model type for regression". Perhaps cannot do meaningful post-pruning for C4.5
# C4.5 cannot be used for 'regression'. It can be used only for 'classification'. "C4.5 - C only for classification"
c4.5_model = train(revenue ~ ., restaurant_train1, method="J48", trControl=resample_strategy)

set.seed(100)
resample_strategy = trainControl(method="boot")
treebag_model = train(revenue ~ ., restaurant_train1, method="treebag", trControl=resample_strategy)
treebag_model # performance not great since trees are related; most use P28, num_days as most important attributes
# Diff trees show diff variable importance because they are built on different random samples
treebag_model$finalModel$mtrees[[1]]
treebag_model$finalModel$mtrees[[2]]
treebag_model$finalModel$mtrees[[15]]


#############################
# IGNORE THE BELOW SECTION  #
#############################


# Repeat all the above 'feature engineering' work on 'test' data also
restaurant_test = read.csv("test.csv", header = TRUE, na.strings=c("NA",""), stringsAsFactors = TRUE)
dim(restaurant_test)
str(restaurant_test)
restaurant_test$Open.Date = as.character(restaurant_test$Open.Date)

# No need to add level "MB" in test data as it is already present here
levels(restaurant_test$Type)

restaurant_test$num_days = as.Date(c("31-12-2014"), format="%d-%m-%Y") - as.Date(restaurant_test$Open.Date, format="%m/%d/%Y")
restaurant_test$num_days = as.numeric(restaurant_test$num_days)

# revenue not present in test data; standardize all distances just like we did in 'train'
restaurant_test1 = restaurant_test[-c(1,2,3)]

restaurant_test2 = as.data.frame(predict(dummyVarsObj, restaurant_test1))
dim(restaurant_test2)
str(restaurant_test2)

restaurant_test3 = predict(preObj, restaurant_test2)
dim(restaurant_test3)
str(restaurant_test3)

# Predict the revenue using the standardized distances
restaurant_test3$Prediction = predict(knn_model, restaurant_test3)
restaurant_test3$Id = restaurant_test$Id

# Kaggle RMSE score is still only 1974545; implies that 'num_days/Open Date' was not so helpful afterall!
result = restaurant_test3[,c("Id", "Prediction")]
write.csv(result, "submission_v3.csv", row.names = F)

# How do we get the intuition on what attributes might be important for prediction?
# Build decision tree/forest first. Attributes that branch at the top of the tree are more important

resample_strategy = trainControl(method="boot", number=10)
set.seed(100)
rf_model = train(restaurant_train[,-c(1,2,3,43)], restaurant_train$revenue, method="rf", trControl=resample_strategy, importance=TRUE)
dim(restaurant_train)
str(restaurant_train)

# This shows that P28,P6 are the most important attributes. num_days is only 10th most important.
# City.Group is 12th and Type is 18th. This explains why our prediction did not improve by using
# these attributes. It is also bad that our kNN model gives equal weightage to all the attributes!
varImp(rf_model)
x11()
varImpPlot(rf_model$finalModel)