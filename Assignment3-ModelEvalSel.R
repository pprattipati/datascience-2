library(ggplot2)
library(caret)

# NOTE: There is an issue with the dataset for both problems 1 and 2. train() does not build meaningful
# trees using any of the resampling strategies. Even with 'cp = 0' we get meaningless decision trees.
# Our instructor (Mr.Thimma Reddy) said that he would provide better data samples

# Problem 1: Model Evaluation with different resampling schemes
# Given the following data with Age as predictor variable and Survived as target variable:
#   Id Age Survived
# 1 25 1
# 2 23 0
# 3 30 1
# 4 35 1
# 5 32 0
# 6 28 1
# 7 13 0
# 8 12 0
# Do the following:
#   a) Create a data frame with above sample data and do required type conversions
Id = 1:8
Age = c(25,23,30,35,32,28,13,12)
Survived = c(1,0,1,1,0,1,0,0)
age_train = data.frame(Id, Age, Survived) 
age_train$Survived = factor(age_train$Survived)

# b) Build tree model with age feature using caret train method (without trainControl argument)
# Why does this not work?
#tree_model = train(age_train[,c("Age")], age_train$Survived, method="rpart")
#tree_model = train(age_train[,c("Age")], age_train$Survived, method="rpart", trControl = trainControl(method="cv", number = 4))
set.seed(10)
tree_model = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", tuneGrid = expand.grid(.cp=0))
# c) Explore the object returned by train method
#    a. Model summary: Find the summary of model to understand the default evaluation strategy used and accuracy of model
tree_model # Default is "bootstrap" with 25 repetitions for each tuning parameter. Accuracy = 0.3

#    b. Final model: Find out the actual model built by accessing finalModel field of returned object and also check how many observations used in building final model
# ASK: How to 'check how many observations used in building final model'?
tree_model$finalModel
# Model says everyone is 'non-survivor'. All 8 observations were used.
# 1) root 8 4 0 (0.5000000 0.5000000) *

#    c. Bias and variance: Find out the bias and variance of finalModel across resampling iterations by accessing resample field of the returned object
# ASK: Why do all the resample values appear 3 times each?
# This is surely a bug and is worth reporting. This perhaps happens only when there is very less data
tree_model$resample


accuracy = mean(tree_model$resample$Accuracy) # 0.326
bias = 1 - accuracy # 0.674
variance = var(tree_model$resample$Accuracy) #0.094

#    d. Train and validation data: Find the details of train and validation used across resampling iterations via control$index and control$indexout fields of the returned object
tree_model$control$index # train data for best tuning parameter; indices used for training
tree_model$control$indexOut # validation data for best tuning parameter; indices used for validation
#    e. Confusion matrices: Find out more deeper details about accuracy of models across resampling iterations via resampledCM field of returned object
tree_model$resampledCM # Gives the number of true+ve, true-ve, false+ve, false-ve predictions in the 'validation' set

# d) Explore all the details expected by part-c above for each of the following options by using train control object in train method:
#   i. Repeated holdout(LGOCV) with 3 iterations and 75% train data.
resample_strategy2 = trainControl(method="LGOCV", number = 3, p = 0.75)
tree_model2 = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", trControl = resample_strategy2)
tree_model2
tree_model2$finalModel
# ASK: Why is each of the three resamples showing up 3 times?
tree_model2$resample
accuracy = mean(tree_model2$resample$Accuracy) # 0.5
bias = 1 - accuracy # 0.5
variance = var(tree_model2$resample$Accuracy) # 0
tree_model2$control$index
tree_model2$control$indexOut
#   ii. 4-fold cross validation(cv with number =4)
resample_strategy3 = trainControl(method="cv", number = 4)
tree_model3 = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", trControl = resample_strategy3)
tree_model3
tree_model3$finalModel
tree_model3$resample
accuracy = mean(tree_model3$resample$Accuracy) # 0.5
bias = 1 - accuracy # 0.5
variance = var(tree_model3$resample$Accuracy) # 0
tree_model3$control$index
tree_model3$control$indexOut

#   iii. 4-fold cross validation with 3 repeats(repeatedcv with number=4 and repeats=3)
# ASK: What is 'k-fold cross validation with repeats'? Why is it done?
resample_strategy4 = trainControl(method="repeatedcv", number = 4, repeats = 3)
tree_model4 = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", trControl = resample_strategy4)
tree_model4
tree_model4$finalModel
tree_model4$resample
accuracy = mean(tree_model4$resample$Accuracy) # 0.5
bias = 1 - accuracy # 0.5
variance = var(tree_model4$resample$Accuracy) # 0
tree_model4$control$index
tree_model4$control$indexOut
#   iv. Leave one out cross validation(LOOCV)
resample_strategy5 = trainControl(method="LOOCV")
tree_model5 = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", trControl = resample_strategy5)
# ASK: Why do we get an error here?
tree_model5
tree_model5$finalModel
tree_model5$resample
accuracy = mean(tree_model5resample$Accuracy) # ?
bias = 1 - accuracy # ?
variance = var(tree_model5$resample$Accuracy) # ?
tree_model5$control$index
tree_model5$control$indexOut
#   v. Bootstrapping with 3 iterations(boot with number = 3)
resample_strategy6 = trainControl(method="boot", number = 3)
tree_model6 = train(age_train[,c("Id","Age")], age_train$Survived, method="rpart", trControl = resample_strategy6)
tree_model6
tree_model6$finalModel
tree_model6$resample
accuracy = mean(tree_model6$resample$Accuracy) # 0.3888
bias = 1 - accuracy # 0.6111
variance = var(tree_model6$resample$Accuracy) # 0.006944
tree_model6$control$index
tree_model6$control$indexOut


# Problem 2: Model Selection
# ==========================
# Given the following data with Age, Gender, Location as predictor variables and Survived as target variable:
#   Id Age Gender Location Survived
# 1 25 F S 1
# 2 23 M Q 0
# 3 30 F Q 1
# 4 35 M C 1
# 5 32 F S 0
# 6 28 F S 1
# 7 13 M Q 0
# 8 12 F C 0
# 
# Do the following:
# a) Create a data frame with above sample data and do required type conversions
Id = 1:8 # Adding to use along with 'Age' to train. Unable to train with 'Age' alone
Age = c(25,23,30,35,32,28,13,12)
Gender = c('F','M','F','M','F','F','M','F')
Location = c('S','Q','Q','C','S','S','Q','C')
Survived = c(1,0,1,1,0,1,0,0)
person_train = data.frame(Id, Age, Gender, Location, Survived)
person_train$Survived = factor(person_train$Survived)
str(person_train)
# b) Build tree model with
#    a. Features: gender
#    b. Evalution strategy: 10-fold cross validation
strategy = trainControl(method="cv", number = 10)
tree_model = train(person_train[,c("Id", "Gender")], person_train$Survived, method="rpart", trControl = strategy)
# tree_model = train(person_train[,c("Id", "Gender")], person_train$Survived, method="rpart", trControl = strategy, tuneGrid=expand.grid(.cp=c(0)), control=rpart.control(minsplit=2))
# tree_model = train(person_train[,c("Id", "Gender")], person_train$Survived, method="rpart", trControl = strategy, control=rpart.control(minsplit=2, cp=c(0)))

# ASK: Why does model say 'none survived'? F=60% survival, M=33% survival.
# Why is model not saying 'all females survived'?
tree_model$finalModel
tree_model$resample
tree_model$control$indexOut
# c) Build tree model with
#    a. Features: gender and age
#    b. Evalution strategy: 10-fold cross validation
tree_model2 = train(person_train[,c("Age", "Gender")], person_train$Survived, method="rpart", trControl = strategy)
# tree_model2 = train(person_train[,c("Age", "Gender")], person_train$Survived, method="rpart", trControl = strategy, control=rpart.control(minsplit=2, cp=c(0)))
# ASK: Why does model again say 'none survived'?
tree_model2$finalModel
# d) Build tree model with
#    a. Features: gender, age and location
#    b. Evalution strategy: 10-fold cross validation
tree_model3 = train(person_train[,c("Age", "Gender", "Location")], person_train$Survived, method="rpart", trControl = strategy)
# ASK: Why does model again say 'none survived'?
tree_model3$finalModel

# e) Find out the bias and variance of the above 3 models. Suggest the final model for deployment.
# Bias and variance are 0 for all 3 models. Something wrong here.

# f) Use the deployed model to predict the outcomes for following test data:
#    Id Age Gender Location
#     9 26 F S
#    10 36 F Q
Id2= 9:10
Age2=c(26,36)
Gender2=c('F', 'F')
Location2=c('S', 'Q')
person_test = data.frame(Id2,Age2,Gender2,Location2)
names(person_test) = c("Id", "Age", "Gender", "Location")
# Predicts all outcomes as 'not survived'
person_test$Survived = predict(tree_model2, person_test)
