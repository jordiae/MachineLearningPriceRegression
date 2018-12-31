setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
varnames = c("longitude", "latitude", "housingMedianAge", "totalRooms", "totalBedrooms", "population", "households", "medianIncome", "medianHouseValue")
df <- read.table("../data/cal_housing.data", sep = ",", col.names = varnames)
dim(df)
summary(df) # all vars are continous. some transformations (ln) already applied, data seems clean and treated
sum(is.na(df)) # 0 NAs
# we want to predict medianHouseValue

plot.ts(df)
hist(df$longitude)
#mat = data.matrix(df)


# visualization

# el següent codi està copy-pastejat per plotejar tot
library(purrr)
library(tidyr)
library(ggplot2)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# apparently, no errors

# variable selection: we don't have many variables and all of them seem relevant...

# tabé copy-pastejat, intento plotejar les coordenades

#library(ggmap)
#library(ggplot2)

#devtools::install_github("dkahle/ggmap") 

# Get a map
#california <- get_map(location = c(lat = 36.7783, lon = 119.4179), zoom = 6)
#california <- get_openstreetmap(location = c(lon = 36.7783, lat = 119.4179), zoom = 6)
#get_stamenmap

#ggmap(california) +
#  geom_point(data = df, aes(x = longitude, y = latitude, fill = genus), size = 3, shape = 21)


# geographical plot (for the moment, map plotting libs of R did not work or needed gmaps API key)
plot(df$longitude,df$latitude)
# California :D
# we have more data from houses near the coastline, LA and following the Interstate 5 (I-5) road

# bivariate: all variables against medianHouseValue
plot(df,df$medianHouseValue)
# falta la llegenda, sad

# hem de fer PCA, LDA, kmeans, EM? sí, sad
# PCA/LDA podria servir per justificar que no cal variable selection
# per variable selection també es podria fer servir les importàncies de random forest
# kmeans i EM. es podria fer estàndard, o també es podria fer només amb les dades geogràfiques
# tot això ara mateix em fa una mica de pal
# respecte el preprocessing, sembla que ja està fet. l'únic que sí que faltaria és escalar per alguns mètodes


# KMEANS
# codi deel lab 2
library(cclust)
do.kmeans <- function (what.k)
{
  r <- cclust (data.matrix(df),what.k,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.matrix(df), index="calinski"))
}

res <- vector("numeric", 11)
ks <- c(2,4,8,12,16,20,24,28,32,36,40)
#for (k in 2:20)
for (k in ks)
  res[k] <- max (r <- replicate (100, do.kmeans(k)))

res[1] <- NA
plot(res, type="l", axes = FALSE, xlab="k", ylab="Calinski-Harabasz")

axis(side = 1, at = 2:20)
axis(side = 2, at = seq(0,5000,500))
grid(9, 6, lwd = 2)

# geo clustering?
geodf <- data.frame(df$longitude,df$latitude)
do.kmeans.geo <- function (what.k)
{
  r <- cclust (data.matrix(geodf),what.k,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.matrix(feodf), index="calinski"))
}

res <- vector("numeric", 10)
for (k in 2:20)
  res[k] <- max (r <- replicate (100, do.kmeans(k)))

res[1] <- NA
plot(res, type="l", axes = FALSE, xlab="k", ylab="Calinski-Harabasz")

axis(side = 1, at = 2:20)
axis(side = 2, at = seq(0,5000,500))
grid(9, 6, lwd = 2)

# conclusió: el dataset és massa complex, hauria de tenir molts més clusters. no aporta insights (Balsells dixit)
# no val la pena provar EM
# cost computacional alt


# Now we compute PCA:

#dfSenseLabel <- df
#dfSenseLabel$ medianHouseValue <- NULL
myPCA <- prcomp(df,scale=TRUE)
# > myPCA$sdev
#[1] 1.9779888 1.3866064 1.3026376 0.9540588 0.5415963 0.3775204 0.2502894 0.2110654 0.1214429

plot(d1PCA)
#abline(0,PCAslope1,col='black',lwd=2)

# Now we need to project the data in the first principal component

d1PCA <- myPCA$x[,1]
PCAslope1 <- myPCA$rotation[2,1]/myPCA$rotation[1,1]
pc1 <- myPCA
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(df)[2]]^2)/dim(df)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(df)[2]]^2)/dim(df)[2]
percInerAccum


# feature selection, agafat amb pinces 
# random forest per escollir variables. altra opció seria mutual information
library(randomForest)
model.rf <- randomForest(medianHouseValue ~ ., data = df, proximity=FALSE)
importance(model.rf)
varImpPlot(model.rf)

#                 IncNodePurity
#longitude         4.285144e+13
#latitude          4.182471e+13
#housingMedianAge  1.562922e+13
#totalRooms        1.682104e+13
#totalBedrooms     1.215556e+13
#population        1.851711e+13
#households        1.175843e+13
#medianIncome      1.082295e+14

# sembla que totes són importans? (???)
# s'hauria de normalitzar la mesura?
#library(infotheo)
#mutinformation(df$medianIncome, df$medianHouseValue )
# requereix valors discrets



### RESAMPLING

N <- nrow(df)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- df[learn.indexes,]
test.data <- df[test.indexes,]

nlearn <- length(learn.indexes)
ntest <- N - nlearn



### LINEAR METHODS


# LINEAR REGRESSION



# Noteu que glm sempre afegeix un terme "intercept" ó "offset" (un regressor constant 1) per defecte, així que tenim dues opcions:

model.linear <- glm (medianHouseValue ~ ., data=learn.data, family = gaussian)

# Els coefficients (el vector w)

model$coefficients

prediction <- predict(model,newdata = learn.data)
N <- nrow(learn.data)
#(mean.square.error <- sum((learn.data$medianHouseValue - prediction)^2)/N)
rmse <- sqrt(mean((learn.data$medianHouseValue - prediction)^2))


# LASSO

library(glmnet)

t <- as.numeric(learn.data[,9])
x <- as.matrix(learn.data[,1:8])

model.lasso <- cv.glmnet (x, t, nfolds = 10)
plot(model.lasso)

coef(model.lasso)

# lambda.min is the value of lambda that gives minimum mean cross-validated error
model.lasso$lambda.min

# Predictions can be made based on the fitted cv.glmnet object; for instance, this would be the TR error with the "optimal" lambda as chosen by LOOCV
predict (model.lasso, newx = x, s = "lambda.min")

# And this would be corresponding LOOCV
LOOCV <- model.lasso$cvm[model.lasso$lambda == model.lasso$lambda.min]

# and the corresponding predictive R-square 
(R2.LOOCV = (1 - LOOCV*N/((N-1)*var(learn.data$medianHouseValue)))*100)

prediction <- predict(model.lasso,newx=x,s="lambda.min")
N <- nrow(learn.data)
#(mean.square.error <- sum((learn.data$medianHouseValue - prediction)^2)/N)
rmse <- sqrt(mean((learn.data$medianHouseValue - prediction)^2))
### NON-LINEAR METHODS


# RBF SVM


library(e1071)

bestSVM <- tune(svm, medianHouseValue~., data = learn.data, kernel="radial",
                ranges = list(gamma = 10^seq(-5,2), cost =10^seq(-2,3),epsilon=10^seq(-5,2)),
                tunecontrol = tune.control(sampling = "cross",nrepeat=5), scale = TRUE
)


#model.rbfsvm <- svm (x,y,kernel,epsilon=0.01,gamma=200, C=100)

#model <- svm(x_train, t_train, type="C-classification", cost=myC, kernel="radial", scale = FALSE)
#lines(x,predict(model.rbfsvm,x),col="green")



# MLP


library(caret)
library(MASS)
library(nnet)
set.seed (4567)

## specify 10x10 CV
trc <- trainControl (method="repeatedcv", number=10, repeats=10,verboseIter = FALSE)
learn.data.scaled <- scale(learn.data)

(decays <- 10^seq(-3,0,by=0.05))
## WARNING: this takes some minutes
model.10x10CV <- train (medianHouseValue ~., data = learn.data.scaled, method='nnet', maxit = 500,
                        trace = FALSE, metric='RMSE', linout = TRUE,
                        tuneGrid = expand.grid(.size=30,.decay=decays), trControl=trc)

save(model.10x10CV,file="weights.mod")
load ("weights.mod")

## We can inspect the full results
model.10x10CV$results

## and the best model found
model.10x10CV$bestTune