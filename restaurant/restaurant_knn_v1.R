library(rpart)
library(caret)

setwd("D:/Data Science/Algorithmica/Restaurant/") # set the working directory
restaurant_train = read.csv("train.csv")
dim(restaurant_train)
str(restaurant_train)
# eliminating 5th column 'Type' also since one of its levels "MB" is missing from train
# and causing issues when found in 'test alone
restaurant_train1 = restaurant_train[,-c(1,2,3,5)]

resample_strategy = trainControl(method="cv", number=10)
set.seed(100)
# Formula interface of the 'train' method implicitly treats all predictors as 'continuous'
knn_model = train(revenue ~ ., restaurant_train1, method="knn", trControl=resample_strategy)
knn_model # different 'k' values tried; best 'k' value chosen based on accuracy

restaurant_test = read.csv("test.csv", header=TRUE, na.strings = c("NA", ""))
restaurant_test1 = restaurant_test[-c(1,2,3,5)]

# Gives Kaggple RMSE score of 1957430, which is a huge improvement over the random prediction
# score of 8549523. Shows that kNN approach works for this restaurant revenue problem!
restaurant_test1$Prediction = predict(knn_model, restaurant_test1)
restaurant_test1$Id = restaurant_test$Id
result = restaurant_test1[,c("Id", "Prediction")]
write.csv(result, "submission_v1.csv", row.names = F)