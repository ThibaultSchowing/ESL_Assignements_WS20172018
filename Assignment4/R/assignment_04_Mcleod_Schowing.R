### Elements of Statistical Learning Exercise sheet 4

## Name:Sarah McLeod and Thibault Schowing
## Matriculation number: 2566398 and 2571837

library(MASS)
library(ISLR)
#library(leaps)
library(glmnet)
library(ggplot2)

# set the seed for the random number generator so results are repetable.
set.seed(1)

# Load and normlaize the data to mean=0 and std =1
p_data <- read.table("prostate.txt")
p_data[,1:8] <- scale(p_data[,1:8])
# verify scaling
#summary(p_data)
# check for missing values
# is.na(p_data)

# subset into train and test data
train_data <- subset(p_data,train== TRUE)
test_data <- subset(p_data, train==FALSE)

# Apply best subset selection on the training data.
regfit.full <- regsubsets(lpsa~.-train, data=train_data, nvmax = 8)
reg.summary <- summary(regfit.full)

# Plot R^2, adjusted R^2, Cp and BIC
par(mfrow=c(2,2))
plot(reg.summary$rsq, xlab="Number of Predictors", ylab="RSq",type="l")
max_rsq <- which.max(reg.summary$rsq)
points(max_rsq,reg.summary$rsq[max_rsq], col="red", cex=2,pch=20)
plot(reg.summary$adjr2, xlab="Number of Predictors", ylab = "Adjusted RSq", type="l")
# get the model with the largets adjR2
max_adjr2 <- which.max(reg.summary$adjr2)
points(max_adjr2,reg.summary$adjr2[max_adjr2], col="red", cex=2,pch=20)
plot(reg.summary$cp, xlab = "Number of Predictors", ylab = "Cp", type = "l")
min_cp <- which.min(reg.summary$cp)
points(min_cp,reg.summary$cp[min_cp], col="red",cex=2,pch=20)
min_bic <- which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l")
points(min_bic,reg.summary$bic[min_bic],col="red",cex=2,pch=20)

# plot features used in the BIC model
dev.off()
plot(regfit.full, scale = "Cp")

# Calculate training and test MSE for the data.
train.mat = model.matrix(lpsa~.-train, data=train_data)
coefi = coef(regfit.full, id=7)
pred = train.mat[,names(coefi)]%*%coefi
train_error = mean((train_data$lpsa - pred)^2)

test.mat=model.matrix(lpsa ~.-train, data=test_data)
coefi = coef(regfit.full, id=7)
pred = test.mat[,names(coefi)]%*%coefi
test_error = mean((test_data$lpsa - pred)^2)

# Fit ridge regression models on the training set.
x = model.matrix(lpsa ~ .,-train_data)[,1:8]
y = train_data$lpsa

# fit ridge regression model
grid <- 10^seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha = 0, lambda = grid)
# verify coeficients
dim(coef(ridge.mod))

# plot the standardized coeficients as a function of lambda
plot.glmnet(ridge.mod, xvar = "lambda")
L <- length(ridge.mod$lambda)
x2 <- log(ridge.mod$lambda[L])
y2 <- ridge.mod$beta[,L]
labs <- names(y2)
text(x2,y2, labels = labs)
#legend("topright", col=1:length(names(ridge.mod$beta[,1])), legend = names(ridge.mod$beta[,1]), lty = 1, cex = 0.5)
#plot.glmnet(ridge.mod)

# fit lasso model
lasso.mod = glmnet(x,y, alpha = 1, lambda = grid)
plot.glmnet(lasso.mod, xvar = "lambda")
L <- length(ridge.mod$lambda)
x2 <- log(ridge.mod$lambda[L])
y2 <- ridge.mod$beta[,L]
labs <- names(y2)
text(x2,y2, labels = labs)
#plot.glmnet(lasso.mod)

# Do 5 fold cross validation on the ridge regression model 
cv.out = cv.glmnet(x,y,alpha=0, nfolds = 5)
bestlam <- cv.out$lambda.min # training MSE is 0.10586
# get test MSE
ridge.pred <- predict(ridge.mod, s=bestlam, newx = model.matrix(lpsa ~ ., test_data)[,1:8])
mean((ridge.pred - test_data$lpsa)^2) # test MSE is 0.44623

# ridge regression coeficients for the best model. 
ind <- match(bestlam,cv.out$lambda)
rr_coef <- coef(ridge.mod)[,ind]

# Do 5-fold cross validation on lasso model
cv.out = cv.glmnet(x,y,alpha=1, nfolds = 5)
bestlam <- cv.out$lambda.min # training MSE is 0.10342
# get test MSE
lasso.pred = predict(lasso.mod, s=bestlam, newx = model.matrix(lpsa ~ ., test_data)[,1:8])
mean((lasso.pred - test_data$lpsa)^2) # test MSE is 0.4436

# lasso coedicients
ind <- match(bestlam,cv.out$lambda)
laso_coef <- coef(ridge.mod)[,ind]

# train linear regression model on all features.
reg.mod <- lm(lpsa ~.-train, data = train_data)
lm.pred <- predict(reg.mod, train_data)
mean((lm.pred - train_data$lpsa)^2) # 0.4391998

lm.pred2 <- predict(reg.mod, test_data)
mean((lm.pred2 - test_data$lpsa)^2)   # 0.521274
