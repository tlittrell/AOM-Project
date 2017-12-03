setwd("~/Dropbox (MIT)/AOM Project/Dave:Tim")
library(tidyverse)

# Read in and prepare data

regressionData = read.csv("RegressionData.csv") %>%
  mutate(SpreadPosNeg = as.factor(as.numeric(Spread >= 0)))


### Regressions

model = lm(Spread ~ X1 + X2 + X3 + WinScore, data = regressionData)
summary(model)
par(mfrow = c(2,2))
plot(model)

model2 = lm(Spread ~ X1 + X2 + X3, data = regressionData)
summary(model2)
par(mfrow = c(2,2))
plot(model2)

model3 = lm(Spread ~ WinScore, data = regressionData)
summary(model3)
par(mfrow = c(2,2))
plot(model)


### Classification

# Train test split
library(caTools)
library(ROCR)
split = sample.split(regressionData$SpreadPosNeg, SplitRatio = 0.75)
regTrain = subset(regressionData, split == TRUE)
regTest = subset(regressionData, split == FALSE)

# Logistic regression
logitModel = glm(SpreadPosNeg ~ X1 + X2 + X3, data = regTrain, family = binomial)
summary(logitModel)

predict = predict(logitModel, regTest, type= 'response')
table(regTest$SpreadPosNeg, predict > 0.5)

ROCRpred = prediction(predict, regTest$SpreadPosNeg)
ROCRperf = performance(ROCRpred, 'tpr','fpr')
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE)
as.numeric(performance(ROCRpred,"auc")@y.values) #AUC

# Random forest
library(randomForest)
rfModel = randomForest(SpreadPosNeg ~ X1 + X2 + X3, data = regTrain)

predict = predict(rfModel, regTest, type= 'response')
table(regTest$SpreadPosNeg, predict)

ROCRpred = prediction(as.numeric(predict), regTest$SpreadPosNeg)
ROCRperf = performance(ROCRpred, 'tpr','fpr')
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE)
as.numeric(performance(ROCRpred,"auc")@y.values) #AUC
