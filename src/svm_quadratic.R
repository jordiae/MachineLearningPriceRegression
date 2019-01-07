
learn.data <- read.csv("../data/dataset_train.csv")
set.seed(1234)

library(e1071)

model.SVMQuad <- tune(svm, medianHouseValue~., data = learn.data, kernel="polynomial", degree=2,coef0=1,
                ranges = list(cost =10^seq(-2,3),epsilon=10^seq(-5,2)),
                tunecontrol = tune.control(sampling = "cross",nrepeat=5), scale = FALSE
)

save(model.SVMQuad, file = "bestSVMQuad.mod")
