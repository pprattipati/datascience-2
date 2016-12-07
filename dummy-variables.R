age = c(10,20,30)
gender = factor(c("M", "F", "M"))
pclass = factor(c("1","2","3"))
passengers = data.frame(age, gender, pclass)
str(passengers)
# dummy variables are used to convert 'factors' to 'continuous' variables
##dummy variables will Convert variables from factors to discrete.. 
# variables that are already 'continuous' remain unchanged
tmp = dummyVars(~age+gender+pclass, passengers)
# use information to predict the continuous variables
predict(tmp, passengers)
tmp
