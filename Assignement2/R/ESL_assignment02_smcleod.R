### Elements of Statistical Learning Exercise sheet 2 

## Name:Sarah McLeod and 
## Matriculation number: 2566398 and 

library(ISLR)
library(ggplot2)

# compute the matrix of correlations, except for the name variable
var_cors <- cor(Auto[,-9])
pairs(var_cors)

# Scatter plots for most highly correlated
# displacement v.s. cylinders
ggplot(Auto, aes(cylinders, displacement)) + geom_point(position = "jitter", shape = 21)
# displacement v.s. weight
ggplot(Auto, aes(weight, displacement)) + geom_point(shape = 21)
# displacement v.s. horsepower
ggplot(Auto, aes(horsepower, displacement)) + geom_point(shape = 21)

# Scatter plots for the most anti-correlated
# weight v.s. mpg
ggplot(Auto, aes(mpg, weight)) + geom_point(shape = 21)
# displacement v.s. mpg
ggplot(Auto, aes(mpg, displacement)) + geom_point(shape = 21)
# horsepower v.s. mpg
ggplot(Auto, aes(mpg, horsepower)) + geom_point(shape = 21) 

# Simple regression of mpg on cylinders
reg_cyl <- lm(mpg ~ cylinders, data = Auto)
# plot the risiduals v.s. fitted value
plot(predict(reg_cyl),residuals(reg_cyl))
summary(reg_cyl) # R2 = 0.6047, F-stat = 596.6, p < 2.2e-16

# Simple regression of mpg on displacement
reg_disp <- lm(mpg ~ displacement, data = Auto)
# plot the risiduals v.s. fitted value
plot(predict(reg_disp),residuals(reg_disp))
summary(reg_disp) # R2 = 0.6482, F-stat = 718.7, p < 2.2e-16

# Simple regression of mpg on horsepower
reg_hrsp <- lm(mpg ~ horsepower, data = Auto)
# plot the risiduals v.s. fitted value
plot(predict(reg_hrsp),residuals(reg_hrsp))
summary(reg_hrsp) # R2 = 0.6059, F-stat: 599.7, p < 2.2e-16

# Simple regression of mpg on year
reg_year <- lm(mpg ~ year, data = Auto)
# plot the risiduals v.s. fitted value
plot(predict(reg_year),residuals(reg_year))
summary(reg_year) # R2 = 0.337, F-stat = 198.3, p < 2.2e-16

# Linear regression for mpg ~ all other variables, except name
reg <- lm(mpg ~ .-name, data = Auto)
summary(reg)

# Plot residuals
plot(predict(reg), residuals(reg))
abline(h=0, col="red")

# Plot leverage statistics
plot(hatvalues(reg))
which.max(hatvalues(reg))

# Linear models for linear combinations of cylinders, weight, and year.
# I'm assuming this is what is asked for!
lc1_reg <- lm(mpg ~ cylinders*weight, data = Auto)
lc2_reg <- lm(mpg ~ weight*year, data = Auto)
lc3_reg <- lm(mpg ~ year*cylinders, data = Auto)
summary(lc1_reg)
summary(lc2_reg)
summary(lc3_reg)

# Regression of mpg on non-linear transformations
nl1_reg <- lm(mpg ~ displacement+log(displacement), data = Auto)
nl2_reg <- lm(mpg ~ displacement+sqrt(displacement), data = Auto)
nl3_reg <- lm(mpg ~ displacement+I(displacement^2), data = Auto)
summary(nl1_reg)
summary(nl2_reg)
summary(nl3_reg)