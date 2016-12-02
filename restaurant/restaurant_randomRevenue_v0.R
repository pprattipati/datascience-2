library(rpart)

library(rpart)

setwd("D:/Data Science/Algorithmica/Restaurant/") # set the working directory
restaurant_train = read.csv("train.csv")
dim(restaurant_train)
str(restaurant_train)
rev_range = range(restaurant_train$revenue)

restaurant_test = read.csv("test.csv", header=TRUE, na.strings = c("NA", ""))

set.seed(100)
# Gives a very high RMSE error of 8549523 on kaggle and ranks almost at the end. Very poor approach.
restaurant_test$Prediction = sample((rev_range[1]:rev_range[2]), nrow(restaurant_test), replace=TRUE)
# Randomly predict (with replacement) a revenue from the [min,max] revenues found in train data

write.csv(restaurant_test[,c("Id","Prediction")], "submission_v0.csv", row.names = F)