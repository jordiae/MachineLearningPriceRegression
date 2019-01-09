# Jordi Armengol i Carles Balsells
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
learn.data <- read.csv("../data/dataset_train.csv")
library(randomForest)

## Now we can try to optimize the number of trees, guided by OOB:

(ntrees <- round(10^seq(1,3,by=0.2)))

# prepare the structure to store the partial results

rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"OOB"] <- 0
ii <- 1
set.seed(1234)
for (nt in ntrees)
{ 
  print(nt)
  
  model.rf <- randomForest(medianHouseValue ~ ., data = learn.data, ntree=nt, proximity=FALSE)
  
  # get the OOB
  rf.results[ii,"OOB"] <- model.rf$mse[nt]#,1]#err.rate[nt,1]
  
  ii <- ii+1
}

rf.results

# choose best value of 'ntrees'

lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
(ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

# We could also try to optimize the number of variables in the same way, though the default value works quite well in general

bestRF <- randomForest(medianHouseValue ~ ., data = learn.data, ntree=ntrees.best, proximity=FALSE)

prediction <- predict(bestRF,newdata=learn.data)
N <- nrow(learn.data)
(rmse <- sqrt(mean((learn.data$medianHouseValue - prediction)^2)))

