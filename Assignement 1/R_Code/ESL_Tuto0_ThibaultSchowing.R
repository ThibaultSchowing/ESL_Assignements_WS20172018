#
# Elements of Statistical Learning WS2017/2018
# Assignement 1
#
# Thibault Schowing & Sarah Mcleod
#
#
#

# CAUTION: Next lines empty the environement variables and set a specific working environement

rm(list=ls())


# I don't know how to automaticaly set the WD according to the source file location.. 
# so let's each have our own directory and change it whenever we want to execute the script

workingDirectory = 'C:/Users/thsch/Desktop/ESL_Assignements_WS20172018/Assignement 1/R_Code'
#workingDirectory = ''

setwd(workingDirectory)


#=========================================
#  B
#=========================================



load('ozone.RData')

ozone

ls(ozone)
str(ozone)
summary(ozone)
dim(ozone)
length(ozone)
range(ozone)
colnames(ozone)




# Ozone: continuous
# Radiation: continuous
# Temperature: continuous
# Wind: continuous







#=========================================
#  C
#=========================================

# Data sizes

length(testset)
length(trainset)
dim(ozone)


# Testset: 31
# Trainset: 80 
# Total: 111 observations


# Mean of the columns
apply(ozone, 2, mean)

# Range of the columns
apply(ozone, 2, range)

# Standart Deviation of the columns
apply(ozone, 2, sd)



#=========================================
#  D
#  Create scatterplots for every pair of 
#  features
#=========================================

# Scatterplots

plot(x = ozone[,1], 
     y = ozone[,2], 
     main = "Ozone vs Radiation", 
     xlab = "Ozone", 
     ylab = "Radiation", 
     xlim = c(0,170), 
     ylim = c(0,335)
     )



# Helpfull source: https://www.tutorialspoint.com/r/r_scatterplots.htm

# Give the chart file a name. If you don't want to save the file, ignore the png() and the dev.off().
png(file = "scatterplot_matrices.png")

pairs(ozone,
      main = "Scatterplot Matrix")

# Save the file.
dev.off()

# For a direct display in R
pairs(ozone,
      main = "Scatterplot Matrix")


# Pearson Correlation
cor(ozone)


#=========================================
#  E
#=========================================

# RSS implementation
# Source: https://en.wikipedia.org/wiki/Residual_sum_of_squares


# RSS function univariate
RSS <- function(y_true,y_predicted){
  #print(dim(y_true))
  res <- sum((y_true - y_predicted)^2)
  return(res)
}



#=========================================
#  F
#=========================================



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


#=========================================
#  G
#=========================================

# Perform KNN 

#install.packages("FNN")




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
ggplot(plot_data, aes(k, RSS)) + 
  geom_line(aes(y=train, color = "train")) +
  geom_line(aes(y=test, color = "test"))

# Plot normalized, since it makes me happy.
plot_data <- data.frame(k=1:30,test=rss_test/length((testset)),train=rss_train/length((trainset)))
ggplot(plot_data, aes(k, Normalized_RSS)) + 
  geom_line(aes(y=train, color = "train")) +
  geom_line(aes(y=test, color = "test"))























