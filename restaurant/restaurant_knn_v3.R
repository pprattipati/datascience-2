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

# Need to standardize the distances for City.Group, Type, P1...P37, num_days (using z-score)
# Must not standardize revenue (43rd column); also ignore the unused attributes (first 3 columns)
# Collecting all columns to be standardized in restaurant_train1
restaurant_train1 = restaurant_train[,-c(1,2,3,43)] # filter unwanted features
dim(restaurant_train1)

# Some of the distances to be standardized are 'factors' (City.Group & Type); replace them with dummy variables
# fullRank = FALSE => One factor variable with 'n' levels converts to 'n' dummy variables
# fullRank = TRUE => One factor variable with 'n' levels converts to 'n-1' dummy variables (all 0s represents the one missing level)
dummyVarsObj = dummyVars(~., restaurant_train1, fullRank = TRUE)
restaurant_train2 = as.data.frame(predict(dummyVarsObj, restaurant_train1))
dim(restaurant_train2) # 42 columns. If fullRank = FALSE, it would have become 44 columns
str(restaurant_train2)

# Convert all distances to z-scores
preObj = preProcess(restaurant_train2, method=c("center", "scale"))
restaurant_train3 = predict(preObj, restaurant_train2)
dim(restaurant_train3)
str(restaurant_train3) # see the standardized z-scores

# restaurant_train3 contains all standardized distances; add 'revenue' and then build the kNN model
restaurant_train3$revenue = restaurant_train$revenue

# Increase number of folds since we are dealing with very less data
resample_strategy = trainControl(method="cv", number=50)
set.seed(100)
knn_model = train(revenue ~ ., restaurant_train3, method="knn", trControl=resample_strategy)
knn_model # different 'k' values tried; best 'k' value chosen based on accuracy


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