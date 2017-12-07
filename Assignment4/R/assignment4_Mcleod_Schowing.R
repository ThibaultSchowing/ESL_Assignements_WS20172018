# ESL - Assignment 4
#
#
#
#


rm(list=ls())

# Libraries

library(MASS)

# for subset selection
library("leaps")

# /!\ Set working directory
setwd("C:/Users/thsch/Desktop/ESL_Assignements_WS20172018/Assignment4/R")

# a:
data = read.table("prostate.txt")

# https://stackoverflow.com/questions/37730926/how-to-calculate-normalize-zero-mean-and-unit-variance
# "Column 1 is index and should not be used" -> column 0 is index,  data[,1] is lcavol

data_norm = scale(data[, 1:(length(data) - 1)])

# Check for variance and mean
round(apply(data_norm, 2, mean))
apply(data_norm, 2, sd)

# Split train-test set

train.rows = which(data$train)
test.rows = which(!data$train)

data_train = data_norm[train.rows, ]
data_test = data_norm[test.rows, ]


# b:

# ISRL p 245
# Have to convert the data (table) to dataframe
regfit.full = regsubsets(lpsa ~ ., as.data.frame(data_train))
reg.summary = summary(regfit.full)

# Display reg.summary
reg.summary

# Generate plots for R2, adjusted R2, Cp and BIC

plot(reg.summary$rsq, main = "R2", xlab = "Number of features", ylab="R2", type="l")

plot(reg.summary$adjr2, main = "Adjusted R2", xlab = "Number of features", ylab = "Adjusted RSq", type="l")

plot(reg.summary$cp, main = "Cp", xlab = "Number of features", ylab="Cp", type="l")

plot(reg.summary$bic, main = "BIC", xlab = "Number of features", ylab = "BIC", type="l")

# Check for minimum and maximum 
which.max(reg.summary$adjr2)
#7
which.min(reg.summary$cp)
#7
which.min(reg.summary$bic)
#2

# Cp and adjusted R2 select the same 7 features model. Bic select the 2 feature model because it puts more penality on models with more variables.


# In reg.summary we see that the 7 features model uses lcavol, lweight, age, lbph svi, lcp and pgg45. 




# Test and train data with the 7 features (without gleason)
x.train = data_train[, -7]
y.train = data_train[, 8]

x.test = data_test[, -7]
y.test = data_test[, 8]


# Calculate test and train MSE for this model 
# ????????????????????????????????????''''
model <- lm(y.train~x.train,  data=as.data.frame(data_train))
fit.train = predict(model,as.data.frame(x.train),as.data.frame(y.train))



# c: 

library(glmnet)

# To store vector of ridge reg. coeff.
grid=10^seq(10, -2,length =100)

ridge.mod = glmnet(x.train,y.train,alpha =0, lambda =grid)

summary(ridge.mod)

par(mfrow=c(1,1))
matplot(grid, t(coef(ridge.mod)), type="l", col=seq_len(ncol(t(coef(ridge.mod)))), 
        main = "Ridge models", xlab="lambda", ylab="Coefficient", xlim=c(0,10))

legend("topright", legend = row.names(coef(ridge.mod)), col=seq_len(ncol(t(coef(ridge.mod)))),
       cex=0.8, fill=seq_len(ncol(t(coef(ridge.mod)))))


# Completely fucked-up but funny !
#plot(ridge.mod)

















