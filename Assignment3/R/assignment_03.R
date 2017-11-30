### Elements of Statistical Learning Exercise sheet 3 

## Name:Sarah McLeod and Thibault Schowing
## Matriculation number: 2566398 and 2571837

library(MASS)
library(plyr)
phoneme = read.csv("phoneme.csv")

# split into train and test according to $speaker
train_indexes <- grep("train",phoneme$speaker, fixed = TRUE)
test_indexes <- grep("test", phoneme$speaker, fixed = TRUE)
phoneme_train <- phoneme[train_indexes,c(-1,-259)]
phoneme_test <- phoneme[test_indexes,c(-1,-259)]

# Fit an LDA model with all the log-periodograms as predictors.
model_1 = lda(g ~ ., data = phoneme_train)

# calculate the training error
get_error <- function(pred, gt){
  d <- data.frame(gt, pred)
   
  return(count(d$gt == d$pred))
}

# get the training error
pred <- predict(model_1)
err <- get_error(pred$class, phoneme_train$g)
train_err <-err$freq[!err$x]/sum(err$freq) # 0.056

# get the test error
pred <- predict(model_1, phoneme_test[,-257], interval="prediction")
err <- get_error(pred$class, phoneme_test$g)
test_err <- err$freq[!err$x]/sum(err$freq) # 0.080

# plot the linear disrimintans

#pred$x
#?plot

# fit a model for vowels aa and ao
phoneme_train_2 <- subset(phoneme_train, subset = g %in% c("aa","ao"))
phoneme_train_2$g <- droplevels(phoneme_train_2$g)
phoneme_test_2 <- subset(phoneme_test, subset = g %in% c("aa","ao"))
phoneme_test_2$g <- droplevels(phoneme_test_2$g)

model_2 = lda(g ~ ., data = phoneme_train_2)


# get train error
pred_train_2 <- predict(model_2)
err <- get_error(pred_train_2$class, phoneme_train_2$g)
train_err <- err$freq[!err$x]/sum(err$freq) # 0.1064

# get test error
pred <- predict(model_2, phoneme_test_2[,-257], interval="prediction")
err <- get_error(pred$class, phoneme_test_2$g)
test_err <- err$freq[!err$x]/sum(err$freq) # 0.2141

# repeat original model fit for QDA
# Fit an QDA model with all the log-periodograms as predictors.
model_3 = qda(g ~ ., data = phoneme_train)

# get the training error
pred <- predict(model_3)
err <- get_error(pred$class, phoneme_train$g)
train_err <- err$freq[!err$x]/sum(err$freq) # 0

# get the test error
pred <- predict(model_3, phoneme_test[,-257], interval="prediction")
err <- get_error(pred$class, phoneme_test$g)
test_err <- err$freq[!err$x]/sum(err$freq) # 0.158

# fit a model for vowels aa and ao
model_4 = qda(g ~ ., data = phoneme_train_2)

# get train error
pred_train_4 <- predict(model_4)
err <- get_error(pred_train_4$class, phoneme_train_2$g)
train_err <- err$freq[!err$x]/sum(err$freq) # 0

# get test error
pred <- predict(model_4, phoneme_test_2[,-257], interval="prediction")
err <- get_error(pred$class, phoneme_test_2$g)
test_err <- err$freq[!err$x]/sum(err$freq) # 0.3394

# create confusion matrices for the LDA and QDA models for aa and ao
table (phoneme_train_2$g,pred_train_4$class)
table(phoneme_train_2$g, pred_train_2$class)
