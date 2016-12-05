# Problem 1: Applying KNN Algorithm
# Given the following training data, predict the class of the following new example using 
# k Nearest Neighbour for k=5: age<=30, income=medium, student=yes, credit_rating=fair.

# For similarity measure use a simple match of attribute values:
#       Similarity(A,B) = sum_over_i(w_i * distance[a_i, b_i] / 4) where 
#                           distance[a_i, b_i] = 1 if a_i equals b_i
#                                              = 0 otherwise
# a_i and b_i are either age, income, student, or credit_rating.
# Weights w_i are all 1 except for income for which it is 2.

setwd("D:/Data Science/Algorithmica/Assignments/ComputerBuyer")
computer = read.csv("computerBuyer.csv", header=TRUE, stringsAsFactors = FALSE)
str(computer)
dim(computer)

equalityCheck = function(x,y)
{
  return (ifelse(x == y, 1, 0))
}

# compute the similarity score for every attribute 
age_similarity = apply(as.array(computer$age), 1, FUN = equalityCheck, "youth")
income_similarity = apply(as.array(computer$income), 1, FUN = equalityCheck, "medium")
student_similarity = apply(as.array(computer$student), 1, FUN = equalityCheck, "yes")
credit_rating_similarity = apply(as.array(computer$credit.rating), 1, FUN = equalityCheck, "fair")

# compute the overall similarity score from the test candidate (across all attributes)
# 'income' has a weightage of 2; others have a weightage of 1
total_similarity = (age_similarity + (2*income_similarity) + student_similarity + credit_rating_similarity) / 4

# find the order of the overall similarity score in decreasing order
similarity_order = order(total_similarity, decreasing = TRUE)

# list the 'computer buy' values in 'train' data in the decreasing similarity order
computer$buy[similarity_order]

# list the 'k' nearest 'computer buy' values
k = 5
computer$buy[head(similarity_order, k)] # gives the values - "no,yes,yes,yes,yes"

# kNN takes 'majority vote' for categorical varaiables on the k-nearest neighbors.
# So, majority vote("no,yes,yes,yes,yes" ) = "yes"

# kNN thus predicts "buy" for the given candidate using k=5