load("../data/LASSO.mod")

plot(model.lasso)

coef(model.lasso)

# lambda.min is the value of lambda that gives minimum mean cross-validated error
model.lasso$lambda.min

sqrt(model.lasso$cvm[model.lasso$lambda == model.lasso$lambda.min])
