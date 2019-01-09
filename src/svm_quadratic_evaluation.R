# Jordi Armengol i Carles Balsells

load("../models/bestSVMQuad.mod")

# Best parameters found for the quadratic SVM
model.SVMQuad$best.parameters

# Number of support vectors for the best model
 nrow(model.SVMQuad$best.model$SV)

mse <- model.SVMQuad$best.performance

# RMSE for quadratic SVM
sqrt(mse)

train.data <- read.csv("../data/dataset_train.csv")
N <- nrow(train.data)

# and the corresponding predictive R-square
(R2 <- (1 - model.mse*N/((N-1)*var(train.data$medianHouseValue)))*100)
