setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
learn.data <- read.csv("../data/dataset_train.csv")
set.seed(1234)

library(e1071)

bestSVM <- tune(svm, medianHouseValue~., data = learn.data, kernel="linear",
                ranges = list(cost =10^seq(-2,3),epsilon=10^seq(-5,2)),
                tunecontrol = tune.control(sampling = "cross",nrepeat=5), scale = FALSE
)
Sys.time()


model.svm_lin <- svm(medianHouseValue ~ ., data = learn.data, scale = FALSE, epsilon = 100, cost = 1000, kernel = "linear")

prediction <- predict(model.svm_lin,newdata=learn.data)
N <- nrow(learn.data)
(rmse <- sqrt(mean((learn.data$medianHouseValue - prediction)^2)))

save(bestSVM, file = "bestSVMlinear.mod")
