# Problem 1: Working with numeric vectors
# Create the following vectors using R
# a. (1,2,3,....,18,19,20)
v1 = 1:20
# b. (20,19,18,...,3,2,1)
v2 = 20:1
# c. (1,2,3,....,19,20,19,...,2,1) 
v3 = c(v1, 19:1)
# d. (4,6,3,4,6,3,....,4,6,3) - There are 10 occurences of 4
v4 = rep(c(4,6,3), 10)
# e. (4,6,3,4,6,3,....,4,6,3,4) - There are 11 occurences of 4 and 10 occurences of 6
v5 = c(v4, 4)
# f. (4,4,...,4,6,6,...,6,3,3,...,3) - 10 occurences of 4, 20 occurences of 6, and 30 occurences of 3
v6 = c(rep(4,10), rep(6,20), rep(3,30))
# g. Create a vector of values of ex. cos(x) at x=3,3.1,3.2,...,6
v7 = cos(seq(3,6,0.1))


# Problem 2: Working with character vectors
# Use the function paste to create the following character vectors of length 30
# a. ("label 1", "label 2", ..., "label 30"). Note that there is a single space between label and the number following
paste("label", 1:30, sep=" ")
# b. ("fn1", "fn2", ..., "fn30"). In this case, there is no space between fn and the number following
paste("fn", 1:30, sep="")

# Problem 3: Working with random vectors
# Execute the following lines which create two vectors of random integers which are chosen with replacement
# from the integers 0, 1, ..., 999. Both vectors have length 250.
set.seed(50)
x_vec = sample(1:999, 250, replace=T)
y_vec = sample(1:999, 250, replace=T)
# a. Suppose x = (x1, x2, ..., xn) denotes the vector x_vec and y = (y1, y2, ..., yn) denotes the vector y_vec.
# Create the vector (y2-x1, y3-x2, ..., yn-x[n-1])
result_vec = y_vec[2:250]-x_vec[1:249]
# b. Pick out the values in y_vec which are > 600.
y_vec[y_vec > 600]
# c. What are the index positions in y_vec of the values which are > 600?
y_index_mask = (y_vec > 600) * (1:250)
y_gt600_index = y_index_mask[y_index_mask > 0]
 # Easier approach
which(y_vec > 600)
# d. What are the values in x_vec which correspond to the values in y_vec which are > 600? (By correspond, we mean at the same index positions)
x_vec[y_vec > 600]
# Less elegant approach
x_vec[which(y_vec > 600)]
# e. How many values in y_vec are within 200 of the maximum value of the terms in y_vec?
bool_vec = (max(y_vec) - y_vec) < 200

length(bool_vec[bool_vec == T])
# or
sum(bool_vec)
# Alternative 1
diff_vec = max(y_vec) - y_vec
length(diff_vec[diff_vec < 200])
# or
sum(diff_vec < 200)
# Alternative 2
sum(y_vec > (max(y_vec) - 200))
# f. How many numbers in x_vec are divisible by 2? (Note that the modulo operator is denoted %%.)
length(x_vec[x_vec %%2 == 0])
# g. Sort the numbers in the vector x_vec in the order of increasing values in y_vec.
# order(vector) returns a permutation of the indexes of the vector in ascending order
x_vec[order(y_vec)]
# h. Pick out the elements in y_vec at index positions 1, 4, 7, 10, 13, ...
y_vec[seq(1,250,3)]


# Problem4: Working with Data Frames
# The data file rainfall.dat(available at algorithmica github repository) records hourly rainfall at a 
# certain location in Canada, every day from 1960 to 1980. Answer the following questions:

# a. Load the data set into R and make it a dataframe called rain.df. What command did you use?
setwd("D:/Data Science/Algorithmica/Assignments/Rainfall")
rain.df = read.table("rainfall.dat", header = FALSE, sep="", na.strings = c(-999))
# write.table(rain.df,"test2.txt", row.names = FALSE)
# b. How many rows and columns does rain.df have? How do you know? (If there are not 5070 rows and 27 columns, you did something wrong in the first part of the problem.)
str(rain.df)
dim(rain.df)
rain.df[1,]
# c. What command would you use to get the names of the columns of rain.df? What are those names?
names(rain.df)
# d. What command would you use to get the value at row 2, column 4? What is the value?
rain.df[2,4]
# e. What command would you use to display the whole second row? What is the content of that row?
rain.df[2,]
# f. What does names(rain.df) = c('year', 'month', 'day', 0:23) do?
# It reassigns the column names
names(rain.df) = c('year', 'month', 'day', 0:23)
# g. Create a new column called daily which is the sum of the 24 hourly columns
# Vectorize adding the columns for each row without manually doing, rain.df$daily = rain.df$`0` + rain.df$`1` + ....
rain.df$daily = rowSums(rain.df[,c(4:27)], na.rm=T)
# Vectorizing is much more efficient than looping
# for(i in 1:nrow(rain.df))
# {
#   rain.df$daily[i] = sum(rain.df[i,4:27])
# }
# h. Make a histogram of the daily rainfall amounts.
# Create a new entry called 'Date' by combing the 3 date fields. The draw a bar graph with 'Date' as 'X' AND 'Daily' as 'y'
rain.df$date = paste(rain.df$day,rain.df$month,rain.df$year,sep="/")
x11()
library(ggplot2)
ggplot(rain.df) + geom_bar(aes(x=date, y=daily), stat="identity")
dim(rain.df)
summary(rain.df$daily)


# Problem5: A First attempt at Predictive Analytics Problems
# Go through the kaggle problem at this link:
#   https://www.kaggle.com/c/otto-group-product-classification-challenge
# Do the following tasks:
# a. Apply random predictions to each test observation and find out how much accurate your predictions are by submitting to kaggle?
library(caret)
library()
setwd("D:/Data Science/Algorithmica/Assignments/OttoGroup")
otto_test = read.csv("test.csv", header=TRUE, sep=",")
dim(otto_test)
# 94 columns so far
str(otto_test)
# adding 9 more columns (95 to 103)
new_cols = paste("Class", 1:9, sep="_")
# Initialize all the 9 column values to zero
otto_test[, new_cols] = 0
# The below code is the same as the above
# otto_test[, c(95:103)] = 0

# Below code is less efficient, but works. How to vectorize this?
for (i in 1:nrow(otto_test))
{
  otto_test[i,sample(95:103,1)] = 1
}

# Failed attempt at vectorization
# -------------------------------
# set value 1 randomly to one of the nine added columns; column with 1 indicates the product category
# The below approach will not work. The RHS expression is a constant value and will not be computed for each row
otto_test[, new_cols] = c(rep(0,sample(0:8,1)), 1, rep(0,8))[1:9]
# Understand how the below logic works. It writes (0,1) continuously across all the chosen columns, and also
# continues the pattern from the end of one column to the start of the next column. Try c(0,0,1) for clarity.
otto_test[, c(95:103)] = c(0,1)


# Vectorization: Method 1
# -----------------------
randomVec = function(i)
{
  j = i-95
  return (c(rep(0,j), 1, rep(0,8-j)))
}

columns = as.list(sample(95:103, nrow(otto_test), replace=T)) # convert vector to list; lapply works on list only
# Use lapply 
# Each number in the 'columns' list transformed into a 'numeric vector of eight 0s and one 1'
res = lapply(columns, FUN=randomVec)
# Transform the list of vectors into a matrix of 'nrow x 9' size and assign to the nine columns
# unlist is very inefficient
otto_test[, new_cols] = matrix(unlist(res), ncol = 9, byrow=TRUE)
head(otto_test[, c(95:103)], 10)


# Vectorization: Method 2
# -----------------------
getColumnVector = function() {
  v1 = rep(0, sample(0:8,1))
  v2 = c(v1,1)
  v3 = c(v2, rep(0,9-length(v2)))
  v3
}

otto_test[,new_cols] = t(replicate(nrow(otto_test), getColumnVector()))

# Got a score of 30.68784 with this 'random guess' submission
write.csv(otto_test[,c(1,95:103)], "otto_submission.csv", row.names = F)

# b. Perform EDA on train data and write the logic based on your observations. Predict the category for test observations based on your discovered logic and submit it to kaggle?
otto_train = read.csv("train.csv", header=TRUE, sep=",")
dim(otto_train)
# Majority products are Class_2(26%), Class_6(23%), Class_8(14%), Class_3(13%), ..., Class_1(3%)
xtabs(~target, otto_train) / nrow(otto_train)
# feat_1 > 30 ==> Class_8 for sure
xtabs(~feat_1 + target, otto_train)
# Combining these two observations for the first attempt
for (i in 1:nrow(otto_test))
{
  # if feat_1 > 30, it is Class_8. Otherwise it is Class_2 (majority product)
  if (otto_test[i,"feat_1"] > 30) {
    otto_test[i,102] = 1
  }
  else {
    otto_test[i,96] = 1
  }
}
# Improved score to 25.59 with this 'majority vote' + some extra logic
write.csv(otto_test[,c(1,95:103)], "otto_submission.csv", row.names = F)

# c. Try to infer the pattern or logic with more EDA and change your logic and check how much does it improves your prediction accuracy?
# feat_2 > 30 ==> Class_8 most probably
xtabs(~feat_2 + target, otto_train)
# Analyze 'Class_2' since it is the most common. Hard to get insights from this approach
xtabs(~feat_1 + feat_2, otto_train[otto_train$target == "Class_2", ])

str(otto_train)
# This doesn't help much
tree_model1 = rpart(target ~ feat_1 + feat_2 + feat_3 + feat_4, otto_train, method="class")

tree_model2 = train(otto_train[,c(2:94)], otto_train$target, method="rpart")
# train() using bootstrapping discovered the following 'unsatisfying' logic. 
# If (feat_11 >= 4.5) OR (feat_11 < 4.5 AND feat_60 >= 3.5), it is Class_6. Otherwise, it is Class_2.
# How is human discovery supposed to do better?
tree_model2$finalModel

# Coding the machine-discovered logic
for (i in 1:nrow(otto_test))
{
  if (otto_test[i,"feat_11"] >= 4.5) {
    otto_test[i,100] = 1  # Class_6
  }
  else {
    if (otto_test[i,"feat_60"] >= 3.5) {
      otto_test[i,100] = 1  # Class_6
    }
    else {
      otto_test[i,96] = 1  # Class_2
    }
  }
}

# Improved score further to 19.44765 using the machine logic!
write.csv(otto_test[,c(1,95:103)], "otto_submission.csv", row.names = F)

tree_model3 = train(otto_train[,c(2:94)], otto_train$target, method="rpart", tuneGrid=expand.grid(.cp=0))
# Tree is too deep. Unable to even see it fully.
tree_model3$finalModel
