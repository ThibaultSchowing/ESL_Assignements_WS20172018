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

# ISRL p 247




