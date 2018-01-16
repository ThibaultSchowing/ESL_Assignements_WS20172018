#Assignment 6
rm(list=ls())

library(ISLR)
attach(Hitters)

# A

#Remove NA
completedata = na.omit(Hitters, col="Salary")

#Log transform Salary
completedata["Salary"] = log(completedata["Salary"])

trainset = completedata[1:200,]
testset = completedata[201:nrow(completedata),]


# B

library(gbm)
set.seed(1)

# For one boos with the default delta (0.001):
boost.hitters = gbm(Salary~.,data=trainset, distribution = "gaussian", n.trees=1000, interaction.depth = 4)
summary(boost.hitters)
boost.hitters$train.error

par(mfrow=c(1,2))
plot(boost.hitters, i="CAtBat")
plot(boost.hitters, i="CWalks")

# Incrementing shrinkage parameter from 0 to 1 with 0.01 increment

#Table containing Delta and MSE

test_mse = array(dim = 100)
train_mse = array(dim = 100)
deltas = array(dim = 100)
i = 1

for(shrkg in seq(0, 1, 0.01)) {
  boost.hitters = gbm(Salary~.,
                      data=trainset, 
                      distribution = "gaussian", 
                      n.trees=1000, 
                      interaction.depth = 4, 
                      shrinkage=shrkg)
  
  salary.boost=predict(boost.hitters, newdata=testset, n.trees=1000)
  
  
  boost.hitters$train.error
  # Print MSE and delta
  
  MSE = mean((salary.boost - testset$Salary)^2)
  T_MSE = mean(boost.hitters$train.error)
  
  
  print(paste(shrkg, "   ", MSE))
  
  test_mse[i] <- MSE
  train_mse[i] <- T_MSE
  
  deltas[i] <- shrkg
  
  i = i+1
}

#Get the delta giving the lowest MSE

best_shrinkage = deltas[match(min(test_mse), test_mse)]
boosting_MSE = min(test_mse)


# HERE TODO: Plot test_mse, train_mse and Deltas(x axis) 
plot(deltas, test_mse,xlab="Shrinkage Parameter", col="red", ylim=c(0,1.5), ylab="Train MSE (Black) - Test MSE (Red)")
par(new=TRUE)

#lines(predict(lm(test_mse~deltas+I(deltas^2))), ylim=c(0,1.5))
par(new=TRUE)
plot(deltas, train_mse,xlab="Shrinkage Parameter", ylim=c(0,1.5), ylab="")


# Around 0.2 the train error is low and the test error hasn't gone up too fast. 


# C

# Linear regression

lm.fit = lm(Salary~., data=trainset)
summary(lm.fit)
lm.pred = predict(lm.fit,testset)

least_square_MSE = mean((testset$Salary - lm.pred)^2)

# Ridge regression

x = model.matrix(Salary~.,completedata)[,-1]
y = completedata$Salary

train=sample(1:nrow(x), nrow(x)/2)
test=(-train )

y.test = y[test]



library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x[train,],y[train], alpha=0, lambda=grid)
ridge.pred = predict(ridge.mod, s=4, newx = x[test,])
ridge_MSE = mean(((ridge.pred - y.test)^2))


print(boosting_MSE)
print(least_square_MSE)
print(ridge_MSE)

# Boosting MSE is a lot lower than linear regression or ridge (1/2)


# D

# Boost variables
summary(boost.hitters)
# Here most important variable is CAtBat

# For Ridge regression: 

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
cv.out$lambda





# For linear regression 

summary(lm.fit)$coefficients

# E




# F
library(randomForest)

# Default parameters
bag.hitters = randomForest(Salary~.,data=trainset, importance=TRUE)

# Train MSE
mean(bag.hitters$mse)

salary.bag = predict(bag.hitters, newdata=testset)
plot(unname(salary.bag), testset$Salary)
mean(((unname(salary.bag))-testset$Salary)^2)

# Predictors importance
importance(bag.hitters)













