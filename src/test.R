# Jordi Armengol i Carles Balsells

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
learn.data <- read.csv("../data/dataset_train.csv")
test.data <-  read.csv("../data/dataset_test.csv")
library(randomForest)
set.seed(1457)
model.final <- randomForest(medianHouseValue ~ ., data = learn.data, ntree=1000, proximity=FALSE)
prediction <- predict(model.final,newdata=test.data)
N <- nrow(test.data)
(rmse <- sqrt(mean((test.data$medianHouseValue - prediction)^2)))
