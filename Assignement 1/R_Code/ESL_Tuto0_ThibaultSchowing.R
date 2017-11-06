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

pairs(~ozone+radiation+temperature+wind,data = ozone,
      main = "Scatterplot Matrix")

# Save the file.
dev.off()

# For a direct display in R
pairs(~ozone+radiation+temperature+wind,data = ozone,
      main = "Scatterplot Matrix")


# Pearson Correlation

df = data.frame(ozone)
cor(df)


#=========================================
#  E
#=========================================

# RSS implementation
# Source: https://en.wikipedia.org/wiki/Residual_sum_of_squares


rssCustom <- function(predicted, real){
  
  # Verify that number of predicted values and real values are the same
  if(length(predicted) != length(real)){
    print("Error: vectors are not the same size")
    return("Error")
  }
  
  RSS <- 0
  
  for(i in 1:length(predicted)){
    #WRONG
    RSS = RSS + ((real[i] - predicted[i])^2)
  }
  
  return(RSS)
}



#=========================================
#  F
#=========================================



model = lm(ozone ~ wind+temperature+radiation, data=ozone[trainset,])

summary(model)

Y_pred = predict(model, newdata = ozone[testset,])

# Report the RSS ???????????

rssValue = rssCustom(Y_pred, ozone[testset,1])
print(rssValue)

deviance(model)

sum(resid(model)^2)

anova(model)

with(summary(model), df[2] * sigma^2)

# Report the Correlation ???




#=========================================
#  G
#=========================================

# Perform KNN 


#install.packages("FNN")
library("FNN")


train = ozone[trainset,]
test = ozone[testset,]

#Response of each observation in training set
y = ozone[trainset,1]

nb_neighbour = 30

results <- matrix(nrow=nb_neighbour, ncol=2)

for (k in 1:nb_neighbour){
  
  # KNN with k neighbours
  res = knn.reg(train, test = test, y, k = k)
  
  # Calculate RSS (Verify function)
  #
  # RSS inputs: - Real values
  #             - Predicted values
  
  rss_k = rssCustom(test$ozone,res$pred)
  cat("\n\nk = " , k , "    Rss = " , rss_k)
  
  results[k,1] = k
  results[k,2] = rss_k
  
  
}

results
plot(results, xlab="k neighbours", ylab="RSS")

res = knn.reg(train, test = test, y, k = 5)


rss_k = rssCustom(test$ozone,res$pred)

























