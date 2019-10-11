library(dplyr)
library(tidyr)
library(glmnet)
library(Metrics)
library(tibble)
library(reshape2)
library(BMA)
library(BMS)
library(xlsx)

# Function to extract a prepare the data

#cargamos datos

gettingTheData <- function(filePath) {
  data <- na.omit(read.table(filePath, header = TRUE, sep = ";"))
}

df <- gettingTheData("datanew_2.csv")

# exploracion de datos con todas las variables -----------------------------------------

seleccion<-c(1:50)

hist(df[,2])
shapiro.test(df[,2])

# analisis de correlaciones

M <- cor(df[,seleccion+2])
col<- colorRampPalette(c("Blue"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Blues"))

corrplot::corrplot.mixed(M, lower.col = "black", number.cex = .7)

# organizar este resultado, eliminando los duplicados, y poniendo al lado los signos.

cormat <- round(cor(df[,seleccion+2]),2)
cormat[lower.tri(cormat,diag=TRUE)] <- NA
head(cormat)
melted_cormat <- melt(cormat)
melted_cormat <- melted_cormat %>% drop_na()
head(melted_cormat)
melted_cormat <- melted_cormat %>% mutate(signo=ifelse(value<0,'-','+'))
melted_cormat <- melted_cormat %>% mutate(value=abs(value))
melted_cormat <- melted_cormat[order(-melted_cormat[,3]),]
melted_cormat %>% filter(value>=0.4)

# Particion del dataset

ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))

# 20 % validacion x y y sin estandarizar
validation <- df[-ind_train,]

# 80 % data original x y y sin estandarizar
sample <- df[ind_train,]

y.sample <- sample[,2]
x.sample <- sample[,seleccion+2]

# medias
x.sample.means <- apply(x.sample, 2, mean)

# varianzas
x.sample.variances <- apply(x.sample, 2, sd)

# scale 
x.sample.scale <- scale(x.sample)

# corremos modelos con todas las variables y con particion de train -----------------------------

mse<-matrix(0,6,1)
accuracy<-matrix(0,6,1)

# Lasso - GLM 

cv.lasso <- cv.glmnet(x.sample.scale, y.sample, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = 1,standardize = FALSE)
lasso.model <- glmnet(x.sample.scale, y.sample, family = "gaussian", lambda = cv.lasso$lambda.min, alpha = 1,standardize = FALSE)

plot(cv.lasso)

y.predict.lasso <- predict(lasso.model, x.sample.scale)

mse.lasso <- mse(y.predict.lasso, y.sample) #juli dice mirar si cv para el mse.

mse[1]<-mse.lasso

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.lasso <- unlist(lapply(as.vector(y.predict.lasso), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.lasso <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# Lasso - Accuracy

lasso.accuracy <- accuracy(y.transf.sample.lasso, y.transf.predict.lasso)

accuracy[1]<-lasso.accuracy

# Ridge - GLM 

cv.ridge <- cv.glmnet(x.sample.scale, y.sample, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = 0,standardize = FALSE)
ridge.model <- glmnet(x.sample.scale, y.sample, family = "gaussian", lambda = cv.ridge$lambda.min, alpha = 0,standardize = FALSE)

plot(cv.ridge)

y.predict.ridge <- predict(ridge.model, x.sample.scale)

mse.ridge <- mse(y.predict.ridge, y.sample)

mse[2]<-mse.ridge

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.ridge <- unlist(lapply(as.vector(y.predict.ridge), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.ridge <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# Ridge - Accuracy

ridge.accuracy <- accuracy(y.transf.sample.ridge, y.transf.predict.ridge)

accuracy[2]<-ridge.accuracy

# Elasticnet - GLM

a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x.sample.scale, y.sample, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = i,standardize = FALSE)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
}
cv.optimo <- search[search$cvm == min(search$cvm), ]
elasticnet.model <- glmnet(x.sample.scale, y.sample, family = "gaussian", lambda = cv.optimo$lambda.min, alpha = cv.optimo$alpha, standardize = FALSE)

y.predict.elasticnet <- predict(elasticnet.model, x.sample.scale)

#plot(y.predict.elasticnet,y.sample)

mse.elasticnet <- mse(y.predict.elasticnet, y.sample)

mse[3]<-mse.elasticnet

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.elasticnet <- unlist(lapply(as.vector(y.predict.elasticnet), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.elasticnet <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# Elasticnet - Accuracy

elasticnet.accuracy <- accuracy(y.transf.sample.elasticnet, y.transf.predict.elasticnet)

accuracy[3]<-elasticnet.accuracy


# BMA bic - GLM

bicbma.model<-bic.glm(x.sample.scale, y.sample, glm.family = gaussian(link = "identity"),maxCol=51,
                      strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)

summary(bicbma.model)
imageplot.bma(bicbma.model)
plot(bicbma.model)
plot(bicbma.model, e = 1e-04, mfrow = NULL, 
     include = 1:length(bicbma.model$names))
#bicbma.model

y.predict.bicbma <- cbind(matrix(1,dim(x.sample.scale)[1],1),x.sample.scale) %*% bicbma.model$postmean
y.predict.bicbma2 <- cbind(matrix(1,dim(x.sample.scale)[1],1),x.sample.scale) %*% bicbma.model$condpostmean

mse.bicbma <- mse(y.predict.bicbma, y.sample)
mse.bicbma2 <- mse(y.predict.bicbma2, y.sample)

mse[4]<-mse.bicbma
mse[5]<-mse.bicbma2

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.bicbma <- unlist(lapply(as.vector(y.predict.bicbma), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.predict.bicbma2 <- unlist(lapply(as.vector(y.predict.bicbma2), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.bicbma <- unlist(lapply(y.sample, function(x) continuous_transformation(x) ), use.names = FALSE)

# bicbma - Accuracy

bicbma.accuracy <- accuracy(y.transf.sample.bicbma, y.transf.predict.bicbma)
bicbma.accuracy2 <- accuracy(y.transf.sample.bicbma, y.transf.predict.bicbma2)

accuracy[4]<-bicbma.accuracy
accuracy[5]<-bicbma.accuracy2

# Juntando el resultado de los coeficientes

lasso.coef <- coef(lasso.model)
ridge.coef <- coef(ridge.model)
elasticnet.coef <- coef(elasticnet.model)
bicbma.coef <- bicbma.model$postmean
bicbma.coef2<- bicbma.model$condpostmean

lasso.coef@x
lasso.coef@i

models<-matrix(0,6,length(seleccion)+1)
models<-data.frame(models)
models[1,lasso.coef@i+1]<-lasso.coef@x
models[2,ridge.coef@i+1]<-ridge.coef@x
models[3,elasticnet.coef@i+1]<-elasticnet.coef@x
models[4,]<-bicbma.model$postmean
models[5,]<-bicbma.model$condpostmean
models[6,]<-c(100,bicbma.model$probne0)
colnames(models)<-c("inter",names(x.sample))
models<- models %>% mutate(MSE=mse,Acc=accuracy)
rownames(models)<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost","PIP_BMA")
models

x<-cbind(lasso.coef,ridge.coef,elasticnet.coef,bicbma.coef,bicbma.coef2)

# corremos validacion de los modelos en los datos separados para el test ---------------------------

mse.validation<-matrix(0,5,1)
accuracy.validation<-matrix(0,5,1)

validation <- df[-ind_train,]

y.validation <- validation[,2]
x.validation <- validation[,seleccion+2]
# medias
x.sample.means
# varianzas
x.sample.variances
# scale 
x.validation.scale <- sweep(x.validation, 2, x.sample.means, FUN="-")
x.validation.scale<-sweep(x.validation.scale, 2, x.sample.variances, FUN="/")
x.validation.scale<-data.matrix(x.validation.scale)
#lasso validation
y.predict.lasso.validation <- predict(lasso.model, x.validation.scale,type = "response")
mse.lasso.validation <- mse(y.predict.lasso.validation, y.validation)
mse.validation[1]<-mse.lasso.validation
continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}
y.transf.predict.lasso.validation <- unlist(lapply(as.vector(y.predict.lasso.validation), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.validation <- unlist(lapply(y.validation, function(x) continuous_transformation(x) ), use.names = FALSE)
# Lasso - Accuracy validation
lasso.accuracy.validation <- accuracy(y.transf.sample.validation, y.transf.predict.lasso.validation)
accuracy.validation[1]<-lasso.accuracy.validation

# Ridge validation
y.predict.ridge.validation <- predict(ridge.model, x.validation.scale,type = "response")
mse.ridge.validation <- mse(y.predict.ridge.validation, y.validation)
mse.validation[2]<-mse.ridge.validation
continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}
y.transf.predict.ridge.validation <- unlist(lapply(as.vector(y.predict.ridge.validation), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.validation <- unlist(lapply(y.validation, function(x) continuous_transformation(x) ), use.names = FALSE)
# Ridge - Accuracy validation
ridge.accuracy.validation <- accuracy(y.transf.sample.validation, y.transf.predict.ridge.validation)
accuracy.validation[2]<-ridge.accuracy.validation

# Elasticnet validation
y.predict.elasticnet.validation <- predict(elasticnet.model, x.validation.scale,type = "response")
mse.elasticnet.validation <- mse(y.predict.elasticnet.validation, y.validation)
mse.validation[3]<-mse.elasticnet.validation
continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}
y.transf.predict.elasticnet.validation <- unlist(lapply(as.vector(y.predict.elasticnet.validation), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.validation <- unlist(lapply(y.validation, function(x) continuous_transformation(x) ), use.names = FALSE)
# Ridge - Accuracy validation
elasticnet.accuracy.validation <- accuracy(y.transf.sample.validation, y.transf.predict.elasticnet.validation)
accuracy.validation[3]<-elasticnet.accuracy.validation

# BMA validation
y.predict.bicbma.validation <- cbind(matrix(1,dim(x.validation.scale)[1],1),x.validation.scale) %*% bicbma.model$postmean
y.predict.bicbma2.validation <- cbind(matrix(1,dim(x.validation.scale)[1],1),x.validation.scale) %*% bicbma.model$condpostmean

mse.bicbma.validation <- mse(y.predict.bicbma.validation, y.validation)
mse.bicbma2.validation <- mse(y.predict.bicbma2.validation, y.validation)

mse.validation[4]<-mse.bicbma.validation
mse.validation[5]<-mse.bicbma2.validation

continuous_transformation <- function(x) {
  if (x <= -1) {0} else {1}
}

y.transf.predict.bicbma.validation <- unlist(lapply(as.vector(y.predict.bicbma.validation), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.predict.bicbma2.validation <- unlist(lapply(as.vector(y.predict.bicbma2.validation), function(x) continuous_transformation(x) ), use.names = FALSE)
y.transf.sample.validation <- unlist(lapply(y.validation, function(x) continuous_transformation(x) ), use.names = FALSE)

# bicbma - Accuracy

bicbma.accuracy.validation <- accuracy(y.transf.sample.validation, y.transf.predict.bicbma.validation)
bicbma.accuracy2.validation <- accuracy(y.transf.sample.validation, y.transf.predict.bicbma2.validation)

accuracy.validation[4]<-bicbma.accuracy.validation
accuracy.validation[5]<-bicbma.accuracy2.validation

models.validation<- data.frame(cbind(round(mse.validation,2),round(accuracy.validation,2)))
colnames(models.validation)<-c('MSE','Accuracy')
rownames(models.validation)<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost")

models.validation<-data.frame(models.validation)
models<-format(models,digits=3)
models<-data.frame(t(models))
models
models.validation

pip<-matrix(0,50,5)
for(i in 1:5) {
  ind_train <- sample(1:nrow(df), size = round(0.8*nrow(df)))
  sample <- df[ind_train,]
  y.sample <- sample[,2]
  x.sample <- sample[,seleccion+2]
  x.sample.scale <- scale(x.sample)
  
  bicbma.model<-bic.glm(x.sample.scale, y.sample, glm.family = gaussian(link = "identity"),maxCol=51,
                        strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)
  
  pip[,i]<-bicbma.model$probne0
}

count<-apply(matrix(sapply(pip,function(x){if (x >= 50) {1} else {0}}),50,5),1,sum)
pip<- cbind(pip,count)
colnames(pip)<-c("PIP1","PIP2","PIP3","PIP4","PIP5","conteo")
rownames(pip)<-names(x.sample)
pip


# variables seleccionadas

seleccion<-c(6,7,10,11,15,16,18,49)

M <- cor(df[,seleccion+2])
col<- colorRampPalette(c("Blue"))
corrplot::corrplot(M, method = "color", order="hclust", col=RColorBrewer::brewer.pal(n=8, name="Blues"))

corrplot::corrplot.mixed(M, lower.col = "black", number.cex = .7)

cormat <- round(cor(df[,seleccion+2]),2)
cormat[lower.tri(cormat,diag=TRUE)] <- NA
head(cormat)
melted_cormat <- melt(cormat)
melted_cormat <- melted_cormat %>% drop_na()
head(melted_cormat)
melted_cormat <- melted_cormat %>% mutate(signo=ifelse(value<0,'-','+'))
melted_cormat <- melted_cormat %>% mutate(value=abs(value))
melted_cormat <- melted_cormat[order(-melted_cormat[,3]),]
melted_cormat %>% filter(value>=0.4)

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

bicbma.model.pre<-bic.glm(x.all, y.all, glm.family = gaussian(link = "identity"),maxCol=51,
                          strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)

summary(bicbma.model.pre)

cv.ridge.pre <- cv.glmnet(x.all, y.all, family = "gaussian", nfold = 5, type.measure = "mse", parallel = TRUE, alpha = 0,standardize = FALSE)
ridge.model.pre <- glmnet(x.all, y.all, family = "gaussian", lambda = cv.ridge.pre$lambda.min, alpha = 0,standardize = FALSE)

coef(ridge.model.pre)

#  variables definitivas PIP ------------------------------------------

seleccion<-c(6,7,11,15,49)

#### Cross Validation modelos definitivos

fold<-5

y.all <- df[,2]
x.all <- scale(df[,seleccion+2])

# lasso test
mse.test.lasso<-matrix(0,fold,1)
mse.test.total.lasso<-matrix(0,fold,100)


cv.lasso2 <- cv.glmnet(x.all, y.all, family = "gaussian", nfold = fold, type.measure = "mse", parallel = TRUE, alpha = 1,standardize = FALSE)
lasso.model2 <- glmnet(x.all, y.all, family = "gaussian", lambda = cv.lasso2$lambda.min, alpha = 1,standardize = FALSE)

y.predict.lasso2 <- predict(lasso.model2, x.all)
mselasso <- mse(y.predict.lasso2, y.all)

for (j in 1:100) {
  folding <- modelr::crossv_kfold(df[,c(2,seleccion+2)],k=fold)
  
  for(i in 1:fold) {
    idx.train<-folding$train[[i]]$idx
    x.train<-df[idx.train,seleccion+2]
    x.test<-df[-idx.train,seleccion+2]
    
    y.train<-df[idx.train,2]
    y.test<-df[-idx.train,2]
    
    x.train.mean <- apply(x.train, 2, mean)
    x.train.variances <- apply(x.train, 2, sd)
    
    x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
    x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
    x.train<-data.matrix(x.train)
    
    x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
    x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
    x.test<-data.matrix(x.test)
    
    lasso.model.train <- glmnet(x.train, y.train, family = "gaussian", lambda = cv.lasso2$lambda.min, alpha = 1,standardize = FALSE)
    
    y.predict.lasso.test <- predict(lasso.model.train, x.test)
    mse.test.lasso[i] <- mse(y.predict.lasso.test, y.test)
    
    mse.test.total.lasso[i,j] <- mse(y.predict.lasso.test, y.test)
  }
}

colMeans(mse.test.total.lasso)

mean(colMeans(mse.test.total.lasso))

sd(colMeans(mse.test.total.lasso))

# ridge test
mse.test.ridge<-matrix(0,fold,1)
mse.test.total.ridge<-matrix(0,fold,100)

cv.ridge2 <- cv.glmnet(x.all, y.all, family = "gaussian", nfold = fold, type.measure = "mse", parallel = TRUE, alpha = 0,standardize = FALSE)
ridge.model2 <- glmnet(x.all, y.all, family = "gaussian", lambda = cv.ridge2$lambda.min, alpha = 0,standardize = FALSE)

y.predict.ridge2 <- predict(ridge.model2, x.all)
mseridge <- mse(y.predict.ridge2, y.all)

for (j in 1: 100) {
  folding.ridge <- modelr::crossv_kfold(df[,c(2,seleccion+2)],k=fold)
    for(i in 1:fold) {
    idx.train<-folding.ridge$train[[i]]$idx
    x.train<-df[idx.train,seleccion+2]
    x.test<-df[-idx.train,seleccion+2]
    
    y.train<-df[idx.train,2]
    y.test<-df[-idx.train,2]
    
    x.train.mean <- apply(x.train, 2, mean)
    x.train.variances <- apply(x.train, 2, sd)
    
    x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
    x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
    x.train<-data.matrix(x.train)
    
    x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
    x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
    x.test<-data.matrix(x.test)
    
    
    ridge.model.train <- glmnet(x.train, y.train, family = "gaussian", lambda = cv.ridge2$lambda.min, alpha = 0,standardize = FALSE)
    
    y.predict.ridge.test <- predict(ridge.model.train, x.test)
    mse.test.ridge[i] <- mse(y.predict.ridge.test, y.test)
    mse.test.total.ridge[i,j] <- mse(y.predict.ridge.test, y.test)
  }
}

colMeans(mse.test.total.ridge)

mean(colMeans(mse.test.total.ridge))

sd(colMeans(mse.test.total.ridge))

# elasticnet test
# elasticnet test

# elasticnet test
mse.test.elasticnet<-matrix(0,fold,1)
mse.test.total.elasticnet<-matrix(0,fold,100)

a <- seq(0.1, 0.9, 0.05)
search2 <- foreach(j = a, .combine = rbind) %dopar% {
  cv2 <- cv.glmnet(x.all, y.all, family = "gaussian", nfold = fold, type.measure = "mse", parallel = TRUE, alpha = j,standardize = FALSE)
  data.frame(cvm = cv2$cvm[cv2$lambda == cv2$lambda.min], lambda.min = cv2$lambda.min, alpha = j)
}
cv.optimo2 <- search2[search2$cvm == min(search2$cvm), ]
elasticnet.model2 <- glmnet(x.all, y.all, family = "gaussian", lambda = cv.optimo2$lambda.min, alpha = cv.optimo2$alpha, standardize = FALSE)

y.predict.elasticnet2 <- predict(elasticnet.model2, x.all)
mselasticnet <- mse(y.predict.elasticnet2, y.all)

for(j in 1: 100){
  foldingelastic.net <- modelr::crossv_kfold(df[,c(2,seleccion+2)],k=fold)
  for(i in 1:fold) {
    idx.train<-foldingelastic.net$train[[i]]$idx
    x.train<-df[idx.train,seleccion+2]
    x.test<-df[-idx.train,seleccion+2]
    
    y.train<-df[idx.train,2]
    y.test<-df[-idx.train,2]
    
    x.train.mean <- apply(x.train, 2, mean)
    x.train.variances <- apply(x.train, 2, sd)
    
    x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
    x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
    x.train<-data.matrix(x.train)
    
    x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
    x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
    x.test<-data.matrix(x.test)

    elasticnet.model.train <- glmnet(x.train, y.train, family = "gaussian", lambda = cv.optimo2$lambda.min, alpha = cv.optimo2$alpha, standardize = FALSE)
    
    y.predict.elasticnet.test <- predict(elasticnet.model.train, x.test)
    mse.test.elasticnet[i] <- mse(y.predict.elasticnet.test, y.test)
    mse.test.total.elasticnet[i,j] <- mse(y.predict.elasticnet.test, y.test)
  }
}

colMeans(mse.test.total.elasticnet)

mean(colMeans(mse.test.total.elasticnet))

sd(colMeans(mse.test.total.elasticnet))


# BMA bic - GLM - test


mse.test.bicbma<-matrix(0,fold,1)
mse.test.bicbma2<-matrix(0,fold,1)

mse.test.total.bicbma2<-matrix(0,fold,100)

bicbma.model2<-bic.glm(x.all, y.all, glm.family = gaussian(link = "identity"),maxCol=51,
                       strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)

y.predict.bicbma2 <- cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$postmean
y.predict.bicbma22 <- cbind(matrix(1,dim(x.all)[1],1),x.all) %*% bicbma.model2$condpostmean

msebicbma <- mse(y.predict.bicbma2, y.all)
msebicbma2 <- mse(y.predict.bicbma22, y.all)

for (j in 1:100){
  folding.bma <- modelr::crossv_kfold(df[,c(2,seleccion+2)],k=fold)
  for(i in 1:fold) {
    idx.train<-folding.bma$train[[i]]$idx
    x.train<-df[idx.train,seleccion+2]
    x.test<-df[-idx.train,seleccion+2]
    
    y.train<-df[idx.train,2]
    y.test<-df[-idx.train,2]
    
    x.train.mean <- apply(x.train, 2, mean)
    x.train.variances <- apply(x.train, 2, sd)
    
    x.train<-sweep(x.train, 2, x.train.mean, FUN="-")
    x.train<-sweep(x.train, 2, x.train.variances, FUN="/")
    x.train<-data.matrix(x.train)
    
    x.test<-sweep(x.test, 2, x.train.mean, FUN="-")
    x.test<-sweep(x.test, 2, x.train.variances, FUN="/")
    x.test<-data.matrix(x.test)
    
    
    bicbma.model.train<-bic.glm(x.train, y.train, glm.family = gaussian(link = "identity"),maxCol=33,
                                strict = FALSE, OR = 20, OR.fix = 2, nbest = 10, occam.window = TRUE)
    
    y.predict.bicbma.test <- cbind(matrix(1,dim(x.test)[1],1),x.test) %*% bicbma.model.train$postmean
    y.predict.bicbma2.test <- cbind(matrix(1,dim(x.test)[1],1),x.test) %*% bicbma.model.train$condpostmean
    
    mse.test.bicbma[i] <- mse(y.predict.bicbma.test, y.test)
    mse.test.bicbma2[i] <- mse(y.predict.bicbma2.test, y.test)
    mse.test.total.bicbma2[i,j] <- mse(y.predict.bicbma2.test, y.test)
  }
}

colMeans(mse.test.total.bicbma2)

mean(colMeans(mse.test.total.bicbma2))

sd(colMeans(mse.test.total.bicbma2))

# resultados tabla final

modelfinal<-c(mselasso,mseridge,mselasticnet,msebicbma,msebicbma2)
modelscv<-cbind(mse.test.lasso,mse.test.ridge, mse.test.elasticnet, mse.test.bicbma, mse.test.bicbma2)
meanmodelscv<-apply(modelscv, 2, mean)
sdmodelscv<-apply(modelscv, 2, sd)
tablafinal<-rbind(round(modelfinal,2),round(modelscv,2),round(meanmodelscv,2),round(sdmodelscv,2))
rownames(tablafinal)<-c("Model_all","CV1","CV2","CV3","CV4","CV5","meanCV","sdCV")
colnames(tablafinal)<-c("MSEL","MSER","MSEE","MSEB","MSEB2")

#resultados finales
tablafinal
tablafinal<-t(tablafinal)

#modelos finales ------------------------------------------------------------
lasso.coef <- coef(lasso.model2)
ridge.coef <- coef(ridge.model2)
elasticnet.coef <- coef(elasticnet.model2)

modelsf<-matrix(0,6,length(seleccion)+1)
modelsf<-data.frame(modelsf)
modelsf[1,lasso.coef@i+1]<-lasso.coef@x
modelsf[2,ridge.coef@i+1]<-ridge.coef@x
modelsf[3,elasticnet.coef@i+1]<-elasticnet.coef@x
modelsf[4,]<-bicbma.model2$postmean
modelsf[5,]<-bicbma.model2$condpostmean
modelsf[6,]<-c(100,bicbma.model2$probne0)
colnames(modelsf)<-c("intercept",names(df[,seleccion+2]))
rownames(modelsf)<-c("Lasso","Ridge","Elasticnet","BMA_post","BMA_condpost","PIP_BMA")
modelsf

summary(bicbma.model2)

# medias x.all
x.all.mean <- apply(df[,seleccion+2], 2, mean)

# varianzas x.all
x.all.sd <- apply(df[,seleccion+2], 2, sd)


plot(y.all - y.predict.bicbma2, ylab="Residuals", xlab="observaciones",main="Grafico residuales") 
abline(0, 0) 

tablafinal

