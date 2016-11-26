library(rpart)
library(caret)
library(ggplot2)

# Gini index is another way to measure the purity
# Gini index = 1 - sigma[p(i)^2]
gini = function(p) {
  temp = 0
  for (i in 1:length(p)) {
    temp = temp + (p[i]*p[i])
  }
  return (1 - temp)
}

# Problem 1: Applying CART, C4.5 and NaiveBayes Algorithms
# Given the following training data with 4 categorical variables and 1 target variable.
# a. Build a decision tree using CART algorithm manually without any pre and post pruning.

setwd("D:/Data Science/Algorithmica/Assignments/ComputerBuyer")
computer = read.csv("computerBuyer.csv", header=TRUE)
str(computer)

rpart(buy ~ age + income + student + credit.rating, computer,control=rpart.control(cp=0,minsplit=2))

# Create the decision tree manually using CART algorithm - gini index & binary splits
xtabs(~buy, computer)
gini_root = gini(c(9/14,5/14))

# Splitting the "root" node
xtabs(~student + buy, computer)
gini_student = gini(c(1/7,6/7))*7/14 + gini(c(4/7,3/7))*7/14

xtabs(~credit.rating + buy, computer)
gini_credit = gini(c(3/6,3/6))*6/14 + gini(c(2/8,6/8))*8/14

# Compute gini for 'age' split - 3 ways of binary split
xtabs(~age + buy, computer)
# split into youth, mid+senior
gini_age_youth = gini(c(3/5,2/5))*5/14 + gini(c(2/9,7/9))*9/14
# split into mid, youth+senior
gini_age_mid = gini(c(0/4,4/4))*4/14 + gini(c(5/10,5/10))*10/14
# split into senior, youth+mid
gini_age_senior = gini(c(2/5,3/5))*5/14 + gini(c(3/9,6/9))*9/14

# Compute gini for 'income' split - 3 ways of binary split
xtabs(~income + buy, computer)
# split into low, mid+high
gini_income_low = gini(c(1/4,3/4))*4/14 + gini(c(4/10,6/10))*10/14
# split into mid, low+high
gini_income_mid = gini(c(2/6,4/6))*6/14 + gini(c(3/8,5/8))*8/14
# split into high, low+mid
gini_income_high = gini(c(2/4,2/4))*4/14 + gini(c(3/10,7/10))*10/14
# gini_age_mid has the minimum value of 0.357, and hence max. gain from the root
# Split the root using 'age' - middleAged, youth+senior

# Splitting the "root">"age = middle-aged" node
# No need to split as the node is pure; all are 'buyers'

# Splitting the "root">"age = youth+senior" node
xtabs(~student + buy, computer[computer$age != "middle-aged",])
gini_student = gini(c(1/5,4/5))*5/10 + gini(c(4/5,1/5))*5/10

xtabs(~credit.rating + buy, computer[computer$age != "middle-aged",])
gini_credit = gini(c(3/4,1/4))*4/10 + gini(c(2/6,4/6))*6/10

# There are only two ages now, youth and senior. So, only 1 way to split
xtabs(~age + buy, computer[computer$age != "middle-aged",])
gini_age = gini(c(2/5,3/5))*5/10 + gini(c(3/5,2/5))*5/10

xtabs(~income + buy, computer[computer$age != "middle-aged",])
gini_income_low = gini(c(1/3,2/3))*3/10 + gini(c(4/7,3/7))*7/10
gini_income_mid = gini(c(2/5,3/5))*5/10 + gini(c(3/5,2/5))*5/10
gini_income_high = gini(c(2/2,0/2))*2/10 + gini(c(3/8,5/8))*8/10
# gini_student has the minimum value of 0.32; split by student now.

# Splitting the "root">"age = youth+senior">"student=yes" node
xtabs(~student + buy, computer[computer$age != "middle-aged" & computer$student == "yes",])
gini_student = gini(c(1/5,4/5)) # only the 'yes' students are present

xtabs(~credit.rating + buy, computer[computer$age != "middle-aged" & computer$student == "yes",])
gini_credit = gini(c(1/2,1/2))*2/5 + gini(c(0/3,3/3))*3/5

xtabs(~age + buy, computer[computer$age != "middle-aged" & computer$student == "yes",])
gini_age = gini(c(0/2,2/2))*2/5 + gini(c(1/3,2/3))*3/5

xtabs(~income + buy, computer[computer$age != "middle-aged" & computer$student == "yes",])
gini_income = gini(c(1/3,2/3))*3/5 + gini(c(0/2,2/2))*2/5 # only low and medium incomes present
# gini_credit has the minimum value of 0.2; split by credit now.

# Splitting the "age = youth+senior">"student=no" node
xtabs(~student + buy, computer[computer$age != "middle-aged" & computer$student == "no",])
gini_student = gini(c(1/5,4/5)) # only the 'no' students are present

xtabs(~credit.rating + buy, computer[computer$age != "middle-aged" & computer$student == "no",])
gini_credit = gini(c(2/2,0/2))*2/5 + gini(c(2/3,1/3))*3/5

xtabs(~age + buy, computer[computer$age != "middle-aged" & computer$student == "no",])
gini_age = gini(c(3/3,0/3))*3/5 + gini(c(1/2,1/2))*2/5

xtabs(~income + buy, computer[computer$age != "middle-aged" & computer$student == "no",])
gini_income = gini(c(2/3,1/3))*3/5 + gini(c(2/2,0/2))*2/5 # only medium and high incomes present
# gini_age has the minimum value of 0.2; split by age now.


# Splitting the "root">"age = youth+senior">"student=yes">"credit=excellent" node
xtabs(~student + buy, computer[computer$age != "middle-aged" & computer$student == "yes" & computer$credit.rating == "excellent",])
gini_student = gini(c(1/2,1/2)) # only the 'yes' students are present

xtabs(~credit.rating + buy, computer[computer$age != "middle-aged" & computer$student == "yes" & computer$credit.rating == "excellent",])
gini_credit = gini(c(1/2,1/2)) # only the 'excellent' ratings are present

xtabs(~age + buy, computer[computer$age != "middle-aged" & computer$student == "yes" & computer$credit.rating == "excellent",])
gini_age = gini(c(0/1,1/1))*1/2 + gini(c(1/1,0/1))*1/2

xtabs(~income + buy, computer[computer$age != "middle-aged" & computer$student == "yes" & computer$credit.rating == "excellent",])
gini_income = gini(c(0/1,1/1))*1/2 + gini(c(1/1,0/1))*1/2
# gini_age and gini_income are both 0 (pure). So, we split by the attribute that produces less leaves.
# age - youth, senior. income - low, medium. Both produce 2 leaves.
# So, let us also choose the same 'age' attribute to split

# Splitting the "root">"age = youth+senior">"student=yes">"credit=fair" node
# No need to split as all are 'buyers'

# Splitting the "age = youth+senior">"student=no">"age=senior" node
# optimizing the filter to "non-student>senior"
xtabs(~student + buy, computer[computer$student == "no" & computer$age == "senior",])
gini_student = gini(c(1/2,1/2)) # only the 'no' students are present

xtabs(~credit.rating + buy, computer[computer$student == "no" & computer$age == "senior",])
gini_credit = gini(c(0/1,1/1))*1/2 +  gini(c(1/1,0/1))*1/2

xtabs(~age + buy, computer[computer$student == "no" & computer$age == "senior",])
gini_age = gini(c(1/2,1/2)) # only seniors are present

xtabs(~income + buy, computer[computer$student == "no" & computer$age == "senior",])
gini_income = gini(c(1/2,1/2)) # only 'medium' income are present
# gini_credit has the minimum value of 0; split by credit now.

# Splitting the "age = youth+senior">"student=no">"age=youth" node
# No need to split as all are 'non-buyers'

# Splitting the "root">"age = youth+senior">"student=yes">"credit=excellent">"age=youth" node
# No need to split as all are 'buyers'

# Splitting the "root">"age = youth+senior">"student=yes">"credit=excellent">"age=senior" node
# No need to split as all are 'non-buyers'

# Splitting the "age = youth+senior">"student=no">"age=senior">"credit=excellent" node
# No need to split as all are 'non-buyers'

# Splitting the "age = youth+senior">"student=no">"age=senior">"credit=fair" node
# No need to split as all are 'buyers'

# Finally, our classification tree by manually applying the CART algorithm with any pre/post pruning is:
# ===============================================================
# 1) root 14 yes (9 yes, 5 no)
#    2) age=senior,youth 10 yes/no (5 yes, 5 no)  
#       4) student=no 5 no (1 yes, 4 no)  
#          8) age=youth 3 no (0 yes, 3 no) *
#          9) age=senior 2 yes/no (1 yes, 1 no)  
#             18) credit.rating=excellent 1 no (0 yes, 1 no) *
#             19) credit.rating=fair 1 yes (1 yes, 0 no) *
#       5) student=yes 5 yes (4 yes, 1 no)  
#          10) credit.rating=excellent 2 yes/no (1 yes, 1 no)  
#              20) age=senior 1 no (0 yes, 1 no) *
#              21) age=youth 1 yes (1 yes, 0 no) *
#          11) credit.rating=fair 3 yes (3 yes, 0 no) *
#    3) age=middle-aged 4 yes (4 yes, 0 no) *

# Note how it matches with the tree shown below generated by rpart:
tree_model = rpart(buy ~ age + income + student + credit.rating, computer,control=rpart.control(cp=0,minsplit=2))
post(tree_model, file="postPruneCART.ps", "Post pruning in CART")
# ========================================================================
#   1) root 14 5 yes (0.3571429 0.6428571)  
#      2) age=senior,youth 10 5 no (0.5000000 0.5000000)  
#         4) student=no 5 1 no (0.8000000 0.2000000)  
#            8) age=youth 3 0 no (1.0000000 0.0000000) *
#            9) age=senior 2 1 no (0.5000000 0.5000000)  
#               18) credit.rating=excellent 1 0 no (1.0000000 0.0000000) *
#               19) credit.rating=fair 1 0 yes (0.0000000 1.0000000) *
#         5) student=yes 5 1 yes (0.2000000 0.8000000)  
#            10) credit.rating=excellent 2 1 no (0.5000000 0.5000000)  
#                20) age=senior 1 0 no (1.0000000 0.0000000) *
#                21) age=youth 1 0 yes (0.0000000 1.0000000) *
#            11) credit.rating=fair 3 0 yes (0.0000000 1.0000000) *
#      3) age=middle-aged 4 0 yes (0.0000000 1.0000000) *

# b. Predict the class of following test observation using the tree you constructed in part-a:
#   age<=30, income=medium, student=yes, creditrating=fair
# [Ans:] We will reach the "root>age=youth,senior>student=yes>credit=fair" path in the tree. So,
# the outcome will be "yes"

# c. Prune the tree built in step using cp parameter for values 0.01, 0.05 and 0.08.
# Show the resulting trees you got after pruning.
# [Ans:]
# Cost of sub-tree = Sum_across_leaves(Misclassification error at node j * proportion of data instances reached to node j ) + cp*(number of leaves in sub-tree)
# See "Assignment 6 answers.docx for details"

# cp = 0.01
# =========
# The tree remains unchanged during post-pruning for cp = 0.01. Verifiable from below rpart() call.
rpart(buy ~ age + income + student + credit.rating, computer,control=rpart.control(cp=0.01,minsplit=2))

# cp = 0.05
# =========
# Tree still remains unchanged. Why?
rpart(buy ~ age + income + student + credit.rating, computer,control=rpart.control(cp=0.05,minsplit=2))

# cp = 0.08
# =========
# Tree still remains unchanged. Why?
rpart(buy ~ age + income + student + credit.rating, computer,control=rpart.control(cp=0.08,minsplit=1))


# d. Build a decision tree using C4.5 algorithm manually without any pre and post pruning.
library(RWeka)
J48(buy ~ age + income + student + credit.rating, computer)
J48(buy ~ age + income + student + credit.rating, computer,control=Weka_control(M=1, R=FALSE, C=0.999))

# Entropy is a measure of purity. Entropy = sigma(-p(i)(log_2(p(i)))
entropy = function(p) {
  ent = 0
  for (i in 1:length(p)) {
    ent = ent + (-p[i]*log2(p[i]))
  }
  return (ent)
}

# Create the decision tree manually using C4.5 algorithm - shannon entropy & multi-way splits
xtabs(~buy, computer)
entropy_root = entropy(c(9/14,5/14))

# Splitting the "root" node
xtabs(~student + buy, computer)
entropy_student_split = entropy(c(1/7,6/7))*7/14 + entropy(c(4/7,3/7))*7/14
entropy_student = entropy(c(7/14,7/14))
gain_ratio_student = (entropy_root - entropy_student_split)/entropy_student

xtabs(~credit.rating + buy, computer)
entropy_credit_split = entropy(c(3/6,3/6))*6/14 + entropy(c(2/8,6/8))*8/14
entropy_credit = entropy(c(6/14,8/14))
gain_ratio_credit = (entropy_root - entropy_credit_split)/entropy_credit

xtabs(~age + buy, computer)
entropy_age_split = entropy(c(3/5,2/5))*5/14 + entropy(c(2/5,3/5))*5/14 + entropy(c(4/4))*4/14
entropy_age = entropy(c(5/14,5/14,4/14))
gain_ratio_age = (entropy_root - entropy_age_split)/entropy_age

xtabs(~income + buy, computer)
entropy_income_split = entropy(c(1/4,3/4))*4/14 + entropy(c(2/6,4/6))*6/14 + entropy(c(2/4,2/4))*4/14
entropy_income = entropy(c(4/14,6/14,4/14))
gain_ratio_income = (entropy_root - entropy_income_split)/entropy_income
# gain_ratio_age has the maximum value of 0.1564; split by age now.

# Splitting the "age = youth" node
entropy_root = entropy(c(2/5,3/5))

xtabs(~student + buy, computer[computer$age == "youth",])
entropy_student_split = entropy(c(2/2))*2/5 + entropy(c(3/3))*3/5
entropy_student = entropy(c(2/5,3/5))
gain_ratio_student = (entropy_root - entropy_student_split)/entropy_student

xtabs(~credit.rating + buy, computer[computer$age == "youth",])
entropy_credit_split = entropy(c(1/2,1/2))*2/5 + entropy(c(2/3,1/3))*3/5
entropy_credit = entropy(c(2/5,3/5))
gain_ratio_credit = (entropy_root - entropy_credit_split)/entropy_credit

xtabs(~age + buy, computer[computer$age == "youth",])
entropy_age_split = entropy(c(3/5,2/5)) # only youth are present
entropy_age = entropy(c(1)) # entropy = 0
gain_ratio_age = (entropy_root - entropy_age_split)/entropy_age # 0 divided by 0

xtabs(~income + buy, computer[computer$age == "youth",])
entropy_income_split = entropy(c(1))*1/5 + entropy(c(1/2,1/2))*2/5 + entropy(c(1))*2/5
entropy_income = entropy(c(1/5,2/5,2/5))
gain_ratio_income = (entropy_root - entropy_income_split)/entropy_income
# gain_ratio_student has the maximum value of 1; split by student now.

# Splitting the "age = middle-aged" node
# No need to split as all are 'buyers'


# Splitting the "age = senior" node
entropy_root = entropy(c(3/5,2/5))

xtabs(~student + buy, computer[computer$age == "senior",])
entropy_student_split = entropy(c(1/3,2/3))*3/5 + entropy(c(1/2,1/2))*2/5
entropy_student = entropy(c(3/5,2/5))
gain_ratio_student = (entropy_root - entropy_student_split)/entropy_student

xtabs(~credit.rating + buy, computer[computer$age == "senior",])
entropy_credit_split = entropy(c(1))*2/5 + entropy(c(1))*3/5
entropy_credit = entropy(c(2/5,3/5))
gain_ratio_credit = (entropy_root - entropy_credit_split)/entropy_credit

xtabs(~age + buy, computer[computer$age == "senior",])
entropy_age_split = entropy(c(3/5,2/5)) # only seniors are present
entropy_age = entropy(c(1)) # entropy = 0
gain_ratio_age = (entropy_root - entropy_age_split)/entropy_age # 0 divided by 0

xtabs(~income + buy, computer[computer$age == "senior",])
entropy_income_split = entropy(c(1/2,1/2))*2/5 + entropy(c(1/3,2/3))*3/5
entropy_income = entropy(c(2/5,3/5))
gain_ratio_income = (entropy_root - entropy_income_split)/entropy_income
# gain_ratio_credit has the maximum value of 1; split by credit now.

# Splitting the "age = youth>student=yes" node
# No need to split as all are 'buyers'

# Splitting the "age = youth>student=no" node
# No need to split as all are 'non-buyers'

# Splitting the "age = senior>credit=excellent" node
# No need to split as all are 'non-buyers'

# Splitting the "age = senior>credit=fair" node
# No need to split as all are 'buyers'

# Finally, our classification tree by manually applying the C4.5 algorithm with any pre/post pruning is:
# ===============================================================
# 1) root 14 yes (9 yes, 5 no)
#    2) age=youth 5 no (2 yes, 3 no)  
#       5) student=yes 2 yes (2 yes, 0 no) *
#       6) student=no  3 no  (0 yes, 3 no) *
#    3) age=middle-aged 4 yes (4 yes, 0 no) *
#    4) age=senior 5 yes (3 yes, 2 no)
#       7) credit=excellent 2 no  (0 yes, 2 no) *
#       8) credit=fair      3 yes (3 yes, 0 no) *

# Note how it matches with the tree shown below generated by J48() method that creates C4.5 trees:
J48(buy ~ age + income + student + credit.rating, computer,control=Weka_control(M=1, R=FALSE, C=0.999))
# ========================================================================
# age = middle-aged: yes (4.0)
# age = senior
# |   credit.rating = excellent: no (2.0)
# |   credit.rating = fair: yes (3.0)
# age = youth
# |   student = no: no (3.0)
# |   student = yes: yes (2.0)

# e. Predict the class of following test observation using the tree you constructed in part-d:
#   age<=30, income=medium, student=yes, creditrating=fair
# [Ans:] Based on the path "age = youth> student=yes", the class of the test data is "yes".

 
# f. Build the conditional probability tables using Naïve Bayes algorithm manually without considering 
# any parameters.

xtabs(~buy, computer)
p(buy=yes) = 9/14
p(buy=no) = 5/14

xtabs(~student + buy, computer)
p(student=yes|buy=yes) = 6/9  ||  p(student=yes|buy=no) = 1/5
p(student=no|buy=yes)  = 3/9  ||  p(student=no|buy=no)  = 4/5

xtabs(~credit.rating + buy, computer)
p(credit.rating=excellent|buy=yes) = 3/9  ||  p(credit.rating=excellent|buy=no) = 3/5
p(credit.rating=fair|buy=yes)      = 6/9  ||  p(credit.rating=fair|buy=no) = 2/5

xtabs(~age + buy, computer)
p(age=youth|buy=yes)       = 2/9    ||  p(age=youth|buy=no)       = 3/5
p(age=middle-aged|buy=yes) = 4/9    ||  p(age=middle-aged|buy=no) = 0/5
p(age=senior|buy=yes)      = 3/9    ||  p(age=senior|buy=no)      = 2/5

xtabs(~income + buy, computer)
p(income=low|buy=yes)    = 3/9    ||  p(income=low|buy=no)    = 1/5
p(income=medium|buy=yes) = 4/9    ||  p(income=medium|buy=no) = 2/5
p(income=high|buy=yes)   = 2/9    ||  p(income=high|buy=no)   = 2/5

# We can verify the above conditional tables from the output of below code, in 'finalModel$tables'
resampling_strategy = trainControl(method="cv", number=10, p=0.8)
nb_grid = expand.grid(.usekernel=c(T,F), .fL=c(0,1), .adjust=c(0,1))
set.seed(10)
nb_model = train(computer[, c("age", "income", "student", "credit.rating")], computer$buy, method="nb", trControl = resampling_strategy, tuneGrid=nb_grid)
nb_model$finalModel$apriori
nb_model$finalModel$tables


# g. Predict the class of following test observation using the conditional tables you constructed in part-f:
#   age<=30, income=medium, student=yes, creditrating=fair

# Need to find if P(buy=yes|age=youth,income=medium,student=yes,credit.rating=fair) is greater than
# P(buy=no|age=youth,income=medium,student=yes,credit.rating=fair) or not. For this, we only need to
# compare the numerators of their expansions using Bayes rule as their denominators are equal.

# Numerator of i)  = P(age=youth,income=medium,student=yes,credit.rating=fair|buy=yes)*P(buy=yes)
#                  = P(age=youth|buy=yes)*P(income=medium|buy=yes)*P(student=yes|buy=yes)*P(credit.rating=fair|buy=yes)*P(buy=yes)
#                  = (2/9)*(4/9)*(6/9)*(6/9)*(9/14)
#                  = 16/567
#                  = 0.028
# Numerator of ii) = P(age=youth,income=medium,student=yes,credit.rating=fair|buy=no)*P(buy=no)
#                  = P(age=youth|buy=no)*P(income=medium|buy=no)*P(student=yes|buy=no)*P(credit.rating=fair|buy=no)*P(buy=no)
#                  = (3/5)*(2/5)*(1/5)*(2/5)*(5/14)
#                  = 6/875
#                  = 0.007
# Obviously, i) is greater.
# So, the naive Bayesian classifier predicts "buy = yes" for the given test sample.

# This is verifiable from the below output.
computer_test = read.csv("test_computerBuyer.csv", header=TRUE)
computer_test$buy = predict(nb_model, computer_test, type="raw")
computer_test$buy

# Why does this give the probability of "buy=yes" as 0.99? As per our calculations, it should
# be (0.028/0.028+0.007) = 0.80
# Our numbers are correct; refer naiveBayesianClassifier.pdf
computer_test$buy = predict(nb_model, computer_test, type="prob")
computer_test$buy


#####################################################
# Problems 2 and 3: Refer Assignment 6 answers.docx #
#####################################################


#########################################################################
# Problem 4: Applying Naïve Bayes Algorithm on Continuous & Categorical Data
# Given the training data in the table below (Tennis data), predict the class of the following 
# new example using Naïve Bayes classification: outlook=overcast, temperature=60, humidity=62, 
# windy=false. Assume Gaussian distribution for numerical attributes and use Laplace's Correction 
# factor while estimating likelihoods.

setwd("D:/Data Science/Algorithmica/Assignments/Tennis")
tennis = read.csv("tennis.csv", header=TRUE)
tennis$windy = as.character(tennis$windy)
str(tennis)

# This is NOT affected by Laplace correction
xtabs(~play, tennis)
# p(play=yes) = 9/14
# p(play=no)  = 5/14

# Correction needs to be applied to the below tables since one of the probabilities is 0
xtabs(~windy + play, tennis)
# p(windy=TRUE|play=yes)  = 3/9  ||  p(windy=TRUE|play=no)  = 3/5
# p(windy=FALSE|play=yes) = 6/9  ||  p(windy=FALSE|play=no) = 2/5

xtabs(~outlook + play, tennis)
# p(outlook=sunny|play=yes)    = 2/9    ||  p(outlook=sunny|play=no)    = 3/5
# p(outlook=overcast|play=yes) = 4/9    ||  p(outlook=overcast|play=no) = 0/5
# p(outlook=rainy|play=yes)    = 3/9    ||  p(outlook=rainy|play=no)    = 2/5

# Tables after applying Laplace correction of 1
# ---------------------------------------------
# p(windy=TRUE|play=yes)  = 4/11  ||  p(windy=TRUE|play=no)  = 4/7
# p(windy=FALSE|play=yes) = 7/11  ||  p(windy=FALSE|play=no) = 3/7
# 
# p(outlook=sunny|play=yes)    = 3/12    ||  p(outlook=sunny|play=no)    = 4/8
# p(outlook=overcast|play=yes) = 5/12    ||  p(outlook=overcast|play=no) = 1/8
# p(outlook=rainy|play=yes)    = 4/12    ||  p(outlook=rainy|play=no)    = 3/8

# Computing mean and standard deviation for the continuous variables
mean(tennis[tennis$play=="yes",c("temperature")])
sd(tennis[tennis$play=="yes",c("temperature")])
mean(tennis[tennis$play=="no",c("temperature")])
sd(tennis[tennis$play=="no",c("temperature")])

# temperature
#     mean     sd
# yes 73.0 6.164414
# no  74.6 7.893035

mean(tennis[tennis$play=="yes",c("humidity")])
sd(tennis[tennis$play=="yes",c("humidity")])
mean(tennis[tennis$play=="no",c("humidity")])
sd(tennis[tennis$play=="no",c("humidity")])

# humidity
#     mean     sd
# yes 79.11111 10.21573
# no  86.20000  9.731393

# We can verify the above conditional probability tables from the output of below code
# in finalModel$apriori and finalModel$tables
resampling_strategy = trainControl(method="cv", number=10, p=0.8)
nb_grid = expand.grid(.usekernel=c(T,F), .fL=c(0,1), .adjust=c(0,1))
set.seed(10)
nb_model = train(tennis[, c("outlook", "temperature", "humidity", "windy")], tennis$play, method="nb", trControl = resampling_strategy, tuneGrid=nb_grid)
nb_model$finalModel$apriori
nb_model$finalModel$tables

# Need to predict the class of "outlook=overcast, temperature=60, humidity=62, windy=false". For this, 
# we have to check which of these is greater:
#   P(play=yes|outlook=overcast, temperature=60, humidity=62, windy=false)
#   P(play=no|outlook=overcast, temperature=60, humidity=62, windy=false)

# Use Naive Bayes formula and compare only their below numerators as the denominators are identical:
#   P(outlook=overcast, temperature=60, humidity=62, windy=false|play=yes)*P(play=yes)
#   P(outlook=overcast, temperature=60, humidity=62, windy=false|play=no)*P(play=no)
# Either use this custom function or the built-in dnorm() function
gaussian = function(num, mean, sd)
{
  denom = sd*sqrt(2*3.14159265358979)
  numer = exp(-(num - mean)^2 / (2*sd^2))
  return (numer/denom)
}

dnorm(60,73.0,6.164414) # 0.007003006
gaussian(60,73.0,6.164414) # my equivalent function for dnorm()
dnorm(60,74.6,7.893035)

# Using the conditional tables, we compute:
# P(temperature=60|play=yes) = dnorm(60,73.0,6.164414) = 0.007003006
# P(temperature=60|play=no)  = dnorm(60,74.6,7.893035) = 0.009134712

dnorm(62,79.11111,10.21573)
dnorm(62,86.20000,9.731393)
# P(humidity=62|play=yes) = dnorm(62,79.11111,10.21573) = 0.009603374
# P(humidity=62|play=no)  = dnorm(62,86.20000,9.731393) = 0.001861495

# i) P(outlook=overcast, temperature=60, humidity=62, windy=false|play=yes)*P(play=yes) = 
#   P(outlook=overcast|play=yes)*P(temperature=60|play=yes)*P(humidity=62|play=yes)*P(windy=false|play=yes)*P(play=yes)
#   = (5/12)*0.007003006*0.009603374*(7/11)*(9/14)
#   = 1.146349e-05
#   
# ii) P(outlook=overcast, temperature=60, humidity=62, windy=false|play=no)*P(play=no) = 
#     P(outlook=overcast|play=no)*P(temperature=60|play=no)*P(humidity=62|play=no)*P(windy=false|play=no)*P(play=no)
#   = (1/8)*0.009134712*0.001861495*(3/7)*(5/14)
#   = 3.253359e-07
#   
# Obviously, i) is much greater than ii). 
# So, the Naive Bayesian classifier predicts "play=yes" for this test sample.

# This is verifiable from the below output.
tennis_test = read.csv("test_tennis.csv", header=TRUE)
tennis_test$windy = as.character(tennis_test$windy)
str(tennis_test)
tennis_test$play = predict(nb_model, tennis_test, type="raw")
tennis_test$play

# Why does this give the probability of "buy=yes" as 0.9595? As per our calculations, it should
# be 1.146349e-05 / (1.146349e-05 + 3.253359e-07 = 0.9724
tennis_test$play = predict(nb_model, tennis_test, type="prob")
tennis_test$play