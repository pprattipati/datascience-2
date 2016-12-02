library(caret)

setwd("D:/Data Science/Algorithmica/Restaurant/") # set the working directory

restaurant_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""), stringsAsFactors = FALSE)
dim(restaurant_train)
str(restaurant_train)

restaurant_train$City.Group = as.factor(restaurant_train$City.Group)
restaurant_train$Type = as.factor(restaurant_train$Type)

# adding a level that was missing from 'train' data
levels(restaurant_train$Type)
levels(restaurant_train$Type) = c(levels(restaurant_train$Type), "MB")

# How to know if 'Type' is useful? Do EDA.
xtabs(~Revenue + Type, restaurant_train)
X11()
# we see that 'Type' of restaurant matters for revenue
ggplot(restaurant_train) + geom_histogram(aes(x=revenue)) + facet_grid(Type ~ .)

# We feel that 'date of opening' of restaurant is important since the older
# restaurants usually have a larger customer base and hence generate more revenue

# get the number of days from the reference date (the maximum date in the input)
restaurant_train$num_days = as.Date(c("31-12-2014"), format="%d-%m-%Y") - as.Date(restaurant_train$Open.Date, format="%m/%d/%Y")
restaurant_train$num_days = as.numeric(restaurant_train$num_days) # casting from 'difftime' class

# eliminate unnecessary columns from kNN computations
restaurant_train1 = restaurant_train[,-c(1,2,3)]
dim(restaurant_train1)

set.seed(100)
# Increase number of folds since we are dealing with very less train data
resampling_strategy = trainControl(method="cv", number=50)
knn_model = train(revenue ~ ., restaurant_train1, method="knn", trControl = resampling_strategy)
knn_model # different 'k' values tried; best 'k' value chosen based on accuracy

restaurant_test = read.csv("test.csv", header = TRUE, na.strings=c("NA",""))
dim(restaurant_test)

# Do 'feature engineering' on test data also
restaurant_test$num_days = as.Date(c("31-12-2014"), format="%d-%m-%Y") - as.Date(restaurant_test$Open.Date, format="%m/%d/%Y")
restaurant_test$num_days = as.numeric(restaurant_test$num_days) # casting from 'difftime' class

# Ensure that test data is similar to the train data used to build the kNN model
restaurant_test1 = restaurant_test[,-c(1,2,3)] 
restaurant_test1$Prediction = predict(knn_model, restaurant_test1)
dim(restaurant_test1)
restaurant_test1$Id = restaurant_test$Id

# Using the num_days (derived from 'Open Date') and the restaurant 'Type' produced no improvement.
# kaggle RMSE score increased from 1957430 to 2045568 So, it did NOT work.

# Problem here is that the distance between 'num_days' is in 1000s but the other distances are <10.
# So, the num_days distance dominates over and suppresses the effect of other distances. Need to 
# standardize all the distances using z-score.
result = restaurant_test1[,c("Id","Prediction")]
write.csv(result,"submission_v2.csv",row.names = F)
