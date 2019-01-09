load("../models/NNet_5x5CV.mod")

# Best parameters found for the MLP neural network
model.nnet$bestTune

# RMSE of the MLP neural network
min(model.nnet$results$RMSE)

# and the corresponding predictive R-square
max(model.nnet$results$Rsquared)
