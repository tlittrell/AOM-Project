rm(list=ls())
cat("\014")

setwd("C:/Users/dmeg0/Dropbox (MIT)/15.774 AOM/Project")

data = read.csv("cluster composition.csv")

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

### Training and Testing Split
set.seed(123)
spl = sample.split(data$PointMargin, SplitRatio = 0.7)
train = subset(data, spl==TRUE)
test = subset(data, spl==FALSE)

########## Linear Regression
lm = lm(PointMargin ~., data=train)
summary(lm)

### OSR2 calculation
lmPredict = predict(lm, newdata=test)
train.mean = mean(train$PointMargin)
lmOSR2 = 1 - sum((lmPredict - test$PointMargin)^2)/sum((train.mean - test$PointMargin)^2)
lmOSR2

########## CART 
library(rpart)
library(rpart.plot)

### CART Model
CARTmod = rpart(Cluster1 ~ ., 
                   data=train, 
                   method="anova",
                   minbucket = 25,
                   cp=0.002)
prp(CARTmod)

### Cross Validation Analysis
set.seed(144)
fitControl = trainControl( method = "cv", number = 10 )

CARTGrid = data.frame( .cp = seq(0.0001,0.0030,0.0001)) 

CARTCV = train(train %>% select(-PointMargin), train$PointMargin, method = "rpart", 
               trControl = fitControl, tuneGrid = CARTGrid )

CARTCV 

prp(CARTCV$finalModel)

### Calculating R2 and OSR2
CARTpredTest = predict(CARTCV$finalModel, newdata=test)
CARTpredTrain = predict(CARTCV$finalModel, newdata=train)

CARTR2 = 1 - sum((CARTpredTrain - train$PointMargin)^2)/sum((train.mean - train$PointMargin)^2)
CARTOSR2 = 1 - sum((CARTpredTest - test$PointMargin)^2)/sum((train.mean - test$PointMargin)^2)

CARTR2
CARTOSR2

