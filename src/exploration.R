set.seed(1234)
# Llegim el dataset principal i construim el dataframe
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
varnames = c("longitude", "latitude", "housingMedianAge", "totalRooms", "totalBedrooms", "population", "households", "medianIncome", "medianHouseValue")
df <- read.table("../data/cal_housing.data", sep = ",", col.names = varnames)

# Enriquirem el dataset amb una altra taula: un conjunt de punts repartit pel litoral californià
# Els punts els hem extret manualment de Google Maps
# Calcularem la distància de cada punt del dataset principal al punt més proper de la costa
# La nova variable, oceanProximity, està inspirada per una nova variable creada a la versió
# de Kaggle del nostre dataset, però la nostra serà contínua.
# La necessitat d'aquesta variable es veu al fer el plot de les coordenades (més endavant, però el vam fer abans per veure la importància)
oceanCoords <-  read.csv("pacific_coordinates.txt",  col.names = c('latitude','longitude'))

eucDist <- function(point1x,point1y, point2x,point2y) {
  return(sqrt((point1x-point2x)*(point1x-point2x) + (point1y-point2y)*(point1y-point2y) ))
  
}

calculateOceanProximity <- function(lat, long){
  oceanValues <- mapply(eucDist, lat, long,oceanCoords$latitude,oceanCoords$longitude)
  return(min(oceanValues))
  
}
# El càlcul és bastant ràpid
df$oceanProximity <- mapply(calculateOceanProximity,df$latitude,df$longitude)

# Volem predir medianHouseValue
# Reordenarem les columnes perquè el target sigui la primera variable, per comoditat
df <- df[,c(9,1:8,10)]

# Procedim a explorar les dades.
dim(df) #  20640 files i  10 variables
summary(df) # Totes les variables són contínues. Aparentment no hi ha errors, però sembla que eserà necessari escalar
# Possibles valors incoherents: els mínims valors de population, rooms... molt petits. Pensant-ho i buscant,
# creiem que no són errors sinó que es tracta de, per exemple, zones del camp o a les afores.
# Al treball original no hem trobat que es parlés de valors incoherents/errors.
sum(is.na(df)) # 0 NAs. No haurem d'imputar missings (no hi ha altres codis de missing tipus 999 o 000)

# Procedim a visualitzar les dades per comprendre-les millors, detectar possibles outliers o veure si s'han de tractar

# Farem un histograma de totes les variables. El següent codi està extret de https://drsimonj.svbtle.com/quick-plot-of-all-variables
library(purrr)
library(tidyr)
library(ggplot2)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Observem que algunes variables (households,medianIncome,oceanProximity,population,totalBedrooms i totalRooms) semblen tenir skewness
# medianHouseValue també, però té un pic molt destacat al final pel que no no trobem massa explicacions.
# housingMedianAge també té un pic al final
# Ens guardem aquesta informació per més endanvant.

# Ja que tenim informació geogràfica, podríem visualitzar les dades geogràficament.
# Ho farem segons el preu, primer, i segons oceanProximity, després, per comprovar que s'ha calculat bé


# Preu mitja en funcio de la localitat

colvec <- rbPal(10)[as.numeric(cut(df$medianHouseValue,breaks = 10))]
plot(main="Preu medià segons les coordenades",df$longitude,df$latitude,col=colvec, xlab = "Longitud",ylab = "Latitud")
legend("topright", legend=c("Preu alt", "Preu baix"),
       col=c("blue", "red"),fill=c("blue","red"),
       title="Preu mitjà", text.font=4)

# Ocean proximity
rbPal <- colorRampPalette(c('red','blue'))
colvec <- rbPal(10)[as.numeric(cut(-log(df$oceanProximity),breaks = 10))]
plot(main="Proximitat a l'oceà segons les coordenades",df$longitude,df$latitude,col=colvec, xlab = "Longitud",ylab = "Latitud")
legend("topright", legend=c("Pròxim", "Allunyat"),
       col=c("blue", "red"),fill=c("blue","red"),
       title="Proximitat", text.font=4)


# El primer gràfic mostra clarament que la costa és més cara i justifica la necessitat d'enriquir el dataset
# amb informació geogràfica. S'intueix perfectament la forma de Califòrnia, perquè hi ha molts punts i
# relativament repartits.

# Tenint en compte que el dataset té 10 variables, també hem provat PCA i clustering per extreure'n més
# informació. Però, a diferència del plot geogràfic, no ens ha aportat massa informació.

# KMEANS
# codi del lab 2
library(cclust)
do.kmeans <- function (what.k)
{
  r <- cclust (data.matrix(df),what.k,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.matrix(df), index="calinski"))
}

res <- vector("numeric", 10)
ks <- c(2:10)
for (k in ks)
  res[k] <- max (r <- replicate (100, do.kmeans(k)))

res[1] <- NA
plot(res, type="l", axes = FALSE, xlab="k", ylab="Calinski-Harabasz", main = "K-Means amb totes les variables")

axis(side = 1, at = 2:20)
axis(side = 2, at = seq(0,5000,500))
grid(9, 6, lwd = 2)

# Hem provat també amb valors més alts, però l''optim de Calinski és massa alt.

# geo clustering?
geodf <- data.frame(df$longitude,df$latitude)
do.kmeans.geo <- function (what.k)
{
  r <- cclust (data.matrix(geodf),what.k,iter.max=100,method="kmeans",dist="euclidean")
  (clustIndex(r,data.matrix(feodf), index="calinski"))
}

res <- vector("numeric", 10)
for (k in 2:10)
  res[k] <- max (r <- replicate (100, do.kmeans(k)))

res[1] <- NA
plot(res, type="l", axes = FALSE, xlab="k", ylab="Calinski-Harabasz",main = "K-Means amb coordenades")

axis(side = 1, at = 2:20)
axis(side = 2, at = seq(0,5000,500))
grid(9, 6, lwd = 2)

# Clustering: hem provat k-means tant amb el dataset complet com amb només les coordenades.
# En els dos casos, el criteri de qualitat continua augmentant a més gran és la k.
# Provant valors més grans, en algun moment començaria a decréixer. Però no veiem que ens aporti gaire informació
# i a més el cost computacional és alt, encara que k-means sigui ràpid.
# Pel mateix motiu creiem que no val la pena provar EM.


# Interem projectar segons amb el primer component principal com a eix.
# El següent codi està basat en el del laboratori corresponent d'MD (Karina Gibert)
# https://www-eio.upc.edu/~karina/datamining/refmaterial/labos/3Visualization/ACPCode.r

pc1 <- prcomp(df, scale=TRUE)

# Percentatge d'inèrcia acumulat

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

barplot(100*cumsum(pc1$sdev[1:dim(df)[2]]^2)/dim(df)[2],names.arg  = 1:10,
        main = "Percentatge d'inèrcia acumulat segons components", xlab="Nombre Components principals inclosos", ylab="% acumulat inèrcia")
percInerAccum<-100*cumsum(pc1$sdev[1:dim(df)[2]]^2)/dim(df)[2]
percInerAccum


# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 4

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS


Psi = pc1$x[,1:nd]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(df)
etiq = names(df)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis
eix_pc1<-1
eix_pc2<-2

plot(Psi[,eix_pc1],Psi[,eix_pc2],main = "Projecció de les dades sobre els primers dos components principals", xlab = "PC1", ylab = "PC2")
text(Psi[,eix_pc1],Psi[,eix_pc2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

# Creació de noves variables

df$roomsPop <- df$totalRooms/df$population
df$bedsPop <- df$totalBedrooms/df$population
df$popHouses <- df$population/df$households
df$households <- df$households
df$housingMedianAge <- df$housingMedianAge

# Realitzem un Random Forest per tal de veure la relació entre les variables i la seva importancia

library(randomForest)
model.rf <- randomForest(medianHouseValue ~ ., data = df, proximity=FALSE)
importance(model.rf)
varImpPlot(model.rf)

# Realitzem un correlation plot per tal de veure si els indexs que hem creat son adequats
# i com es relacionen les diferents variables del problema entre si

library(corrplot)
correlation <- cor(df)#[, -1])
corrplot(corr = correlation,
         order = 'hclust',
         tl.col = 'black',
         tl.cex = 0.8)
library(caret)
scales <- list(x = list(relation = 'free'),
               y = list(relation = 'free'),
               cex = 0.6)
featurePlot(x = df[, -9],
            y = df$medianHouseValue,
            plot = 'density',
            scales = scales,
            layout = c(3, 8),
            auto.key = list(columns = 2),
            pch = '|')

#Dataset definitiu

# Tenint en compte el grafic de correlacions i importancies, es decideix que el dataset definitiu contindra
# nomes les seguents variables

newvarnames = c("latitude", "roomsPop", "bedsPop", "popHouses", "housingMedianAge", "households", "medianIncome", "oceanProximity", "medianHouseValue")

df <- df[newvarnames]

# Transformacio de variables: Apliquem la funcio logaritme a les funcions tenint en compte
# el paper original i les observacions realitzades sobre les dades

df$roomsPop <- log(df$roomsPop)
df$bedsPop <- log(df$bedsPop)
df$popHouses <- log(df$popHouses)
df$households <- log(df$households)
df$housingMedianAge <- log(df$housingMedianAge)
df$oceanProximity <- log(df$oceanProximity)

# Apliquem escalat a totes les variables excepte el preu mitja en el qual nomes restem la mitjana

df$roomsPop <- scale(df$roomsPop)
df$bedsPop <- scale(df$bedsPop)
df$popHouses <- scale(df$popHouses)
df$housingMedianAge <- scale(df$housingMedianAge)
df$households <- scale(df$households)
df$medianIncome <- scale(df$medianIncome)
df$oceanProximity <- scale(df$oceanProximity)
df$latitude <- scale(df$latitude)
df$medianHouseValue <- df$medianHouseValue - mean(df$medianHouseValue)

# Guardem el dataset definitiu

write.csv(df, file = "../data/dataset_clean.csv",row.names=FALSE)

# Inspecció del dataset definitiu
summary(df)

# Histogrames del dataset definitiu
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Generacio de datasets d'entrenament i validacio

df <- read.csv("../data/dataset_clean.csv")
set.seed(1234)
N <- nrow(df)
all.indexes <- 1:N

learn.indexes <- sample(1:N, round(2*N/3))
test.indexes <- all.indexes[-learn.indexes]

learn.data <- df[learn.indexes,]
test.data <- df[test.indexes,]

# Guardem els dos datasets generats

write.csv(learn.data, file = "../data/dataset_train.csv",row.names=FALSE)
write.csv(test.data, file = "../data/dataset_test.csv",row.names=FALSE)

