# lab ISLR p190
library(ISLR)
set.seed(1)
train=sample (392 ,196)

lm.fit =lm(mpg∼horsepower ,data=Auto ,subset =train )
attach (Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)

lm.fit2=lm(mpg∼poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)

lm.fit3=lm(mpg∼poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)

set.seed (2)


library (boot)
glm.fit=glm(mpg∼horsepower ,data=Auto)
cv.err =cv.glm(Auto ,glm.fit)
cv.err$delta
