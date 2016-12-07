library(ggplot2)

stock_plot = function(s1, s2) {
  df = data.frame(a=s1,b=s2)
  
  ggplot(df) + geom_point(aes(x=a, y=b))
  print(cov(df$a, df$b))  # covariance
  print(cor(df$a, df$b))  # correlation
}

s1 = c(100,200,300,400)
s2 = c(10,20,30,50)
s3 = c(1,2,3,5)
s4 = c(500,600,700,800)
s5 = c(5,3,2,1)
s6 = c(800,700,600,500)

X11()
df = data.frame(s1,s2)
ggplot(df) + geom_point(aes(s1, s2))

# covariance depends on the scale of the values. So, even if there is the same linear relationship,
# covariance values will change. So, normalize the values by z-score and compute covariance on the
# z-scores; this is called correlation and its value range is [-1, +1].
# For z-score, almost 99% values fall in the [-3, +3] value range
stock_plot(s1,s2)
stock_plot(s1,s3)
stock_plot(s1,s4)
stock_plot(s1,s5)
stock_plot(s1,s6)

# See why the z-score of mean is '0'
x = c(10,20,30,40,50)
x_z = (x - mean(x))/sd(x)
df = data.frame(x, x_z)
