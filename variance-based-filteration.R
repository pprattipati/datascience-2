library(caret)

v1 = rep(10,10)
v2 = c(rep(20,5), rep(30,5))
df = data.frame(v1,v2)

# v1 has same value in all rows; so it has zero variance and hence near-zero variance
nzv_obj = nearZeroVar(df, saveMetrics = TRUE)
nzv_obj
# retain only those columns that don't have zero variance
df1 = df[,nzv_obj$zeroVar==F]

# nearZeroVar diagnoses predictors that have one unique value (i.e. are zero variance predictors) or predictors 
# that have both of the following characteristics: they have very few unique values relative to the numberv of
# samples (default is 10% distinct values) and the ratio of the frequency of the most common value to the 
# frequency of the second most common value is large (default is 95/5)

# digit recognizer sample has 700+ columns and many have zero variance