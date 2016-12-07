library(rpart)
library(caret)
# install.packages("corrplot")
library(corrplot)

setwd("D:/Data Science/Algorithmica/Restaurant/") # set the working directory
restaurant_train = read.csv("train.csv")
dim(restaurant_train)
str(restaurant_train)

restaurant_train1 = restaurant_train[,-1]

# feature engineering to filter features automatically using 'variance' and 'correlation'

numeric_attr = sapply(restaurant_train1, is.numeric)
correlations = cor(restaurant_train1[,numeric_attr])
dim(correlations)
str(correlations)
correlations
X11()
# diagonal is 'strong blue' between the correlation to oneself is always +1
corrplot(correlations)
corrplot(correlations, order="hclust")
corrplot(correlations, order="hclust", addrect=3)
# can do correlation-based filtering by removing redundant features

# shows the features that are eliminated
# use a high cut-off; eliminate columns only if they have a high redundancy; Otherwise, dangerous to eliminate
# consider only those pair of columns which have at least 0.95 correlation and REMOVE ONLY ONE of them;
# remove the one with the largest mean absolute correlation
filtered_features_correlation = findCorrelation(correlations, cutoff = 0.95)
# no need for abs() since the method implicitly considers only absolute values of pair-wise correlations
# filtered_features_correlation = findCorrelation(abs(correlations), cutoff = 0.95)

# Analyze the variance of features; and then eliminate features that have zero or near-zero variance
nzv_obj = nearZeroVar(restaurant_train1, saveMetrics = TRUE)