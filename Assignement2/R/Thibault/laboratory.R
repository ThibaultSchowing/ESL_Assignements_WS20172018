### Elements of Statistical Learning - Assignment 2

# Laboratory 3.6 - ISLR

## Name:Sarah McLeod and Thibault Schowing
## Matriculation number: 2566398 and 2571837



# CAUTION: Next lines empty the environement variables

rm(list=ls())

library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

?Boston

lm.fit = lm(medv~lstat, data=Boston)
summary(lm.fit)
names(lm.fit)

confint(lm.fit)

predict(lm.fit ,data.frame(lstat=(c(5 ,10 ,15))), interval ="confidence")


attach(Boston)
plot(lstat, medv)
abline(lm.fit)

abline(lm.fit,lwd=3)
abline(lm.fit, lwd=3,col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow = c(2,2))
plot(lm.fit)


plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))


# Leverage statistic
plot(hatvalues(lm.fit))

which.max(hatvalues(lm.fit))


lm.fit = lm(medv~lstat + age, data = Boston)
summary(lm.fit)

# instead
lm.fit = lm(medv~.,data=Boston)
summary(lm.fit)
 ?summary.lm


library(car)
# variance inflation
vif(lm.fit)

# we see that age has high p-value so we exclude it

lm.fit1 = lm(medv~.-age,data=Boston)
summary(lm.fit1)


summary(lm(medv~lstat*age ,data=Boston ))

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

# With quadratic it looks better, use anova to quantify it

lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)


# polynomial fit
lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

# try log transformation
summary(lm(medv~log(rm),data=Boston))


# 3.6.6 Qualitative predictors

fix(Carseats)
names(Carseats)

lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)





























