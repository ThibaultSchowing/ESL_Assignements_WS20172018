### Elements of Statistical Learning Exercise sheet 5

## Name:Sarah McLeod and Thibault Schowing
## Matriculation number: 2566398 and 2571837

library(pls)
library(ggplot2)
load("prostate.RData")
set.seed(2)

# fit a PCR model to the training data
pcr.fit = pcr(lpsa ~ ., data = prostate.train, ncomp = 8, validation = "CV")
summary(pcr.fit)
# plot training error
validationplot(pcr.fit, val.type = "RMSEP", legendpos = "topright")

# predict with the 
pcr.pred = predict(pcr.fit, newdata = prostate.test, ncomp = 8)
pcr_error = RMSEP(pcr.fit, newdata = prostate.test)
plot(pcr_error, legendpos = "topright")

# fit a PLS model to the training data
pls.fit = plsr(lpsa ~ ., data = prostate.train, ncomp = 8, validation = "CV")
validationplot(pls.fit, val.type = "RMSEP", legendpos = "topright")

# get the test error
error = RMSEP(pls.fit, newdata = prostate.test)
plot(error, legendpos = "topright")

# visulalize training data
plot(pcr.fit, plottype = "scores", comps = 1:4, col = ifelse(pcr.fit$model$lpsa > 2.5, "red", "blue"), asp =1)
all.fit = pcr(lpsa ~ ., data = rbind(prostate.test,prostate.train), ncomp = 8, validation = "CV")
plot(all.fit, plottype = "scores", comps = 1:4, col = ifelse(all.fit$model$lpsa > 2.5, "red", "blue"), asp =1)

plot(pls.fit, plottype = "scores", comps = 1:4, col = ifelse(pls.fit$model$lpsa > 2.5, "red", "blue"), asp =1)
all.fit = plsr(lpsa ~ ., data = rbind(prostate.test, prostate.train), ncomp = 8, validation = "CV")
plot(all.fit, plottype = "scores", comps = 1:4, col = ifelse(all.fit$model$lpsa > 2.5, "red", "blue"), asp =1)
