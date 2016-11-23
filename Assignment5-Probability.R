# Problem 8: Expected value & Standard deviation 
# II. Suppose that the probability function shown below reflects the possible lifetimes
#     (in months after emergence) for fruit flies: 
#   
#   x     1  2  3    4    5    6 
# p(x) 0.30  ?  0.20 0.15 0.10 0.05 
# 
# d.	What is the mean lifetime for a fruit fly? 

x = 1:6
prob = c(0.3,0.2,0.2,0.15,0.1,0.05)
data = data.frame(x, prob)

#install.packages("Hmisc")
library(Hmisc)
weighted.mean(data$x, data$prob)
wtd.var(data$x, data$prob, normwt=TRUE) # This gives a different variance 

(0.3*1.7*1.7) + (0.2*0.7*0.7) + (0.2*0.3*0.3) + (0.15*1.3*1.3) + (0.1*2.3*2.3) + (0.05*3.3*3.3)
sqrt(2.31)
