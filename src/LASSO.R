library(MASS)
library(glmnet)
library(caret)
set.seed(111)
train.data <- read.csv("../data/dataset_train.csv")
summary(train.data)

library(glmnet)

t <- as.numeric(train.data[,9])
x <- as.matrix(train.data[,1:8])#8])

model.lasso <- cv.glmnet (x, t, nfolds = length(t))

####################################
save(model.lasso, file = "LASSO.mod")
####################################

plot(model.lasso)

coef(model.lasso)

# lambda.min is the value of lambda that gives minimum mean cross-validated error
model.lasso$lambda.min

