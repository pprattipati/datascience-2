library(rpart)
library(caret)
library(e1071)

setwd("D:/Data Science/Algorithmica/Titanic/") # set the working directory
titanic_train = read.csv("train.csv", na.strings = c("NA",""))
titanic_test = read.csv("test.csv")

# typecast the required columns
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Pclass = as.factor(titanic_train$Pclass)
titanic_test$Survived = NA
titanic_test$Pclass = as.factor(titanic_test$Pclass)

summary(titanic_train)
# Embarked - 2 missing data.
# Age - 177 missing

# Handle missing data
summary(titanic_train$Embarked)
titanic_train$Embarked[is.na(titanic_train$Embarked)] = 'S' # Majority vote is called 'mode'
summary(titanic_train$Age)
titanic_train$Age[is.na(titanic_train$Age)] = mean(titanic_train$Age, na.rm=TRUE)
titanic_test$Age[is.na(titanic_test$Age)] = mean(titanic_test$Age, na.rm=TRUE)

# kNN can be used only when all the predictor variables are 'continuous'.
# We can use the formula interface of the 'train' method to treat all predictors as 'continuous'
# knn_grid = expand.grid(.k=c(2,3,4))
resample_strategy = trainControl(method="cv", number=10)
knn_model = train(Survived ~ Sex +  Pclass + Parch + SibSp + Age + Embarked, titanic_train, method="knn", trControl=resample_strategy)
knn_model # different 'k' values tried; best 'k' value chosen based on accuracy

# Another approach to converting 'categorical' predictors into 'continuous' predictors is to replace
# the former with 'Dummy' variables. This is applicable to both train and test data. kNN computes 
# 'distances' using predictors. One can compute distances only when the values involved are 'continuous'.

# This 2-step approach to computing dummy variables helps to reuse it for both 'train' and 'test' data
dummy_obj = dummyVars(~Sex +  Pclass + Parch + SibSp + Age + Embarked, titanic_train)
titanic_train1 = as.data.frame(predict(dummy_obj, titanic_train))

titanic_test1 = as.data.frame(predict(dummy_obj, titanic_test))
titanic_train1$Survived = titanic_train$Survived

# Using dummy variables for kNN estimation
# '.' means 'all attributes'. "Survived ~ ." means, predict 'Survived' using all other attributes
knn_model = train(Survived ~ ., titanic_train1, method="knn", trControl=resample_strategy)
knn_model # different 'k' values tried; used k=5

# Predict using kNN model
# Need to eliminate all 'missing' ages in both train & test. kNN cannot handle missing data.
titanic_test$Survived = predict(knn_model, titanic_test1)
length(titanic_test$Survived)
