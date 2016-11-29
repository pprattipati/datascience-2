# Problem 4: Normality Test
# Load data "income.csv" available under datasets(github of algorithmica) branch into R data frame. Do the following things:
setwd("D:/Data Science/Algorithmica/Assignments/NormalityTest")
income = read.csv("income.csv", header=TRUE)
dim(income)

# a) Obtain descriptive statistics for "income", "edu", and "expr". The statistics should include number of observations,
#    min, max, mean, median, std, skewness, kurtosis, quantile(0.25), quantile(0.75).

# Skewness is a measure of symmetry, or more precisely, the lack of symmetry. A distribution, or data set, is symmetric
# if it looks the same to the left and right of the center point. Kurtosis is a measure of whether the data are
# heavy-tailed or light-tailed relative to a normal distribution.

# The skewness for a normal distribution is zero, and any symmetric data should have a skewness near zero.
# Negative values for the skewness indicate data that are skewed left and positive values for the skewness
# indicate data that are skewed right. By skewed left, we mean that the left tail is long relative to the
# right tail. Similarly, skewed right means that the right tail is long relative to the left tail. If the
# data are multi-modal, then this may affect the sign of the skewness.

# The kurtosis for a standard normal distribution is three.

summary(income[,c("income","edu","expr")]) # gives min, max, mean, median, quantile(0.25 - 1st quantile), quantile(0.75 - 3rd quantile)
library(e1071)

# Standard deviation values = 14259.3, 4.23, 10.4
lapply(income[,c("income","edu","expr")], FUN=sd)
# Skewness values = 7.548, -0.386, 0.075
lapply(income[,c("income","edu","expr")], FUN=skewness)
# Kurtosis values = 129.09, -0.437, -0.786
lapply(income[,c("income","edu","expr")], FUN=kurtosis)

kurtosis(income$income) # 129.09
kurtosis(income$edu) # -0.437
kurtosis(income$expr) # -0.786

# b) Does income, education and expr of all people follows normal distribution individually?
# ASK: How to check for normality? skewness ~ 0 ANd Kurtosis ~ 3?
# High kurtosis for income => it is not normally distributed

# c) Does income, education and expr of male people follows normal distribution individually?
# Skewness values = 4.678, -0.39, 0.033
lapply(income[income$female == 0, c("income","edu","expr")], FUN=skewness)
# Kurtosis values = 38.2, -0.035, -0.846
lapply(income[income$female == 0, c("income","edu","expr")], FUN=kurtosis)

# For males, income - NOT normally distributed; education & experience - normally distributed

# d) Does income, education and expr of female people follows normal distribution individually?
# Skewness values = 11.288, -0.24, 0.1194
lapply(income[income$female == 1, c("income","edu","expr")], FUN=skewness)
# Kurtosis values = 253.1, -0.819, -0.719
lapply(income[income$female == 1, c("income","edu","expr")], FUN=kurtosis)

# For females, income - NOT normally distributed; education & experience - normally distributed