# Jordi Armengol i Carles Balsells
load("../models/LASSO.mod")

plot(model.lasso)

coef(model.lasso)

# lambda.min is the value of lambda that gives minimum mean cross-validated error
model.lasso$lambda.min

model.mse <- model.lasso$cvm[model.lasso$lambda == model.lasso$lambda.min]

# value for RMSE
sqrt(model.mse)

train.data <- read.csv("../data/dataset_train.csv")
N <- nrow(train.data)

# and the corresponding predictive R-square
(R2 <- (1 - model.mse*N/((N-1)*var(train.data$medianHouseValue)))*100)
