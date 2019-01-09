# Jordi Armengol i Carles Balsells
library(MASS)
library(nnet)
library(caret)
set.seed(111)
train.data <- read.csv("../data/dataset_train.csv")
summary(train.data)

## specify 5x5 CV
trc <- trainControl (method="repeatedcv", number=5, repeats=5)

(decays <- 10^seq(-3,0,by=0.05))

## WARNING: this takes some time
model.nnet <- train (medianHouseValue ~., data = train.data, method='nnet', maxit = 1500, linout = TRUE,
                        tuneGrid = expand.grid(.size=25,.decay=decays), trControl=trc)

####################################
save(model.nnet, file = "NNet_5x5CV.mod")
####################################

## We can inspect the full results
model.nnet$results

## and the best model found
model.nnet$bestTune
