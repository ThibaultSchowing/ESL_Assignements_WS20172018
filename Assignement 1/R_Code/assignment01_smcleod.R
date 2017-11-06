### Elements of Statistical Learning Exercise sheet 1 

## Name:Sarah McLeod and 
## Matriculation number: 2566398 and 

# Load the data
load("ozone.RData")
str(ozone)

# Explore the data
lapply(ozone,range)
lapply(ozone, sd)
lapply(ozone, mean)

# Get the pearson correlation coeficient
cor(ozone)
# Plots for every pair of features in the data set 
pairs(ozone)

# RSS function univariate
RSS <- function(y_true,y_predicted){
  #print(dim(y_true))
  res <- sum((y_true - y_predicted)^2)
  return(res)
}

# Now fit a linear model using lm()
t_data <- ozone[trainset,]
lm_fit <- lm(t_data$ozone ~ t_data$radiation + t_data$temp + t_data$wind)
# format test data
t_data <- ozone[testset,]
lm_predict <- predict(lm_fit, t_data[,2:4],interval="prediction")

# RSS for predictions and true responses.
RSS(t_data$ozone,lm_predict[,1])

# Pearson correlation of predictions and true responses.
cor(lm_predict[,1],t_data$ozone)

# Plot true values v.s. predicted values
plot(t_data[,1],lm_predict[,1], 
     pch = 15, col = c("red","blue"),
     xlab = "true ozone values", ylab = "predicted ozone values")
# add legend
legend(max(t_data[,1]), legend=c("true values","predicted values"),
       col = c("red","blue"),
       lty=1)

# KNN regression for k = 1...30
library(FNN)
k = 30
# Initialize to hold KNN predictions
knn_predictions_train <- matrix(nrow=k,ncol=length(trainset))
knn_predictions_test <- matrix(nrow=k,ncol=length(testset))

# KNN for k =1:30 
for(i in 1:k){
  knn_reg <- knn.reg(ozone[trainset,2:4],ozone[trainset,2:4],ozone[trainset,1], k=i)
  knn_predictions_train[i,] <- knn_reg$pred
  knn_reg2 <- knn.reg(ozone[trainset,2:4],ozone[testset,2:4],ozone[trainset,1], k=i)
  knn_predictions_test[i,] <- knn_reg2$pred
}

# RSS on training and test for every K
rss_train <- vector(length = k)
rss_test <- vector(length = k)
for(i in 1:k){
  #print(i)
  rss_train[i] <- RSS(ozone[trainset,1],knn_predictions_train[i,])
}
for(i in 1:k){
  rss_test[i] <- RSS(ozone[testset,1],knn_predictions_test[i,])
}

# Plot unnormalized train and test reusults
library(ggplot2)
plot_data <- data.frame(k=1:30,test=rss_test,train=rss_train)
ggplot(plot_data, aes(k)) + 
  geom_line(aes(y=train, color = "train")) +
  geom_line(aes(y=test, color = "test"))

# Plot normalized, since it makes me happy.
plot_data <- data.frame(k=1:30,test=rss_test/length((testset)),train=rss_train/length((trainset)))
ggplot(plot_data, aes(k)) + 
  geom_line(aes(y=train, color = "train")) +
  geom_line(aes(y=test, color = "test"))
