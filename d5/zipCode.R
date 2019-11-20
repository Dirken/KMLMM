#
#Author:Ricard Meyerhofer Parra
#

plot_img <- function(train_data, i ,show_target=FALSE){
  CUSTOM_COLORS = colorRampPalette(colors = c("black", "white"))
  if(show_target==TRUE){
    print(train_data[i,1])
  }
  train_data <- train_data[,-1]
  z <- unname(unlist((train_data[i,])))
  k <- matrix(z,nrow = 16,ncol = 16)
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(t(k)), col = CUSTOM_COLORS(256))
}

################################################################################
#Step1:
################################################################################
#Read the "zip_train.dat" and "zip_test.dat" files provided. 
#Select a 5% random sample (without replacement) of the train data. 
#Use this sample as your training data, and the complete test data for testing.
################################################################################
set.seed(42) #so that results are replicable
readLines("data/zip_train.dat", n=10)
trainSet <- read.delim2("data/zip_train.dat", header = F, sep = " ")
ncol(trainSet) #258 columns
nrow(trainSet) #7291
trainSet$V258 <- NULL

#for(i in 1:12){
#  plot_img(trainSet, i)
#}


testSet <- read.delim2("data/zip_test.dat", header = F, sep = " ")
testSet$V258 <- NULL
ncol(testSet) #258
nrow(testSet) #2006

library("caret")
#now we are going to select a 5% random sample of the train data:
train5SetIndex <- createDataPartition(trainSet$V1, p =0.05, list = F)
train5Set <- trainSet[train5SetIndex,]
nrow(train5Set)
ncol(train5Set)


################################################################################
#Step2:
################################################################################
#Define the response matrix (Y) and the predictor matrix (X). Center the predictor matrix.
################################################################################
Y <- as.factor(train5Set[,1])
X <- train5Set[,-1]

#extra visualization
library(Rtsne)
tsne <- Rtsne(X, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=Y, col=colors(Y))

library(dummies)
#the predictor matrix we will do a one hot encoding so we are going to create a column per digit
Y <- as.matrix(dummy(Y))

#we are not scaling because if we do so we would just not give importance
#to the position of each pixel and this IS important.
X <- as.matrix(X)
mode(X) <- "double"
X <- scale(X, center = T, scale = F)
n <- nrow(X)
#same for test:
Ytest <- as.factor(testSet[,1])
Ytest <- as.matrix(dummy(Ytest))
Xtest <- testSet[,-1]
Xtest <- as.matrix(Xtest)
mode(Xtest) <- "double"
Xtest <- scale(Xtest, scale = F, center=attr(X, 'scaled:center'))

################################################################################
#Step3:
################################################################################
#Perform a multivariate regression with the training data. Compute the average R2.
################################################################################
multiReg <- lm(Y ~ X)
multiReg.summary <- summary(multiReg)
averageR2 <- mean(sapply(multiReg.summary, function(x) x$r.squared))

################################################################################
#Step4:
################################################################################
#Compute the average of the R2 by Leave One Out
################################################################################
PRESS  <- colSums((multiReg$residuals/(1-ls.diag(multiReg)$hat))^2)
R2cv <- 1-PRESS/(diag(var(Y))*(n-1))
mean(R2cv)

################################################################################
#Step5:
################################################################################
#Predict the responses in the test data, be aware of the appropriate centering. 
#You can compute the prediction by a direct scalar product without using the predict function. 
#Compute the average R2 in the test data.
################################################################################
coefficients <- multiReg$coefficients[-1,]
bias <- multiReg$coefficients[1,]
Yhat <- Xtest%*%coefficients + bias

################################################################################
#Step6:
################################################################################
#Assign every test individual to the maximum response and compute the error rate
################################################################################
Ypred <- apply(Yhat, 1, which.max)
suma = 0
for(i in 1:length(Ypred)){
  if(Ytest[i,][[Ypred[i]]] == 1){
    suma = suma +1
  }
}
accuracy <- suma/length(Ypred)
error <- Yhat-Ytest
R2test <- mean(1-abs(diag(crossprod(error)))/abs(diag(crossprod(Yhat))))
errorRate <- 1-accuracy
################################################################################
#Step7:
################################################################################
#Perform a PCR (using LOO). Decide how many components you retain for prediction.
################################################################################
library(pls)
pc <- pcr(Y ~ X, center = FALSE, validation="LOO")

R2.cv <- R2(pc)$val[1,,]
meanR2cv <- data.frame(apply(R2.cv, 2, mean))
meanR2cv$id <- 1:nrow(meanR2cv) 

library(ggplot2)
g1<-ggplot(data = meanR2cv, 
       mapping = aes(x =meanR2cv$id  , y = meanR2cv$apply.R2.cv..2..mean.)) +
  geom_point()
#validationplot(pc, val.type = "R2")

##too big, trying with less:
meanR250cv <- head(meanR2cv, 50)
g2<-ggplot(data = meanR250cv, 
       mapping = aes(x =meanR250cv$id, y = meanR250cv$apply.R2.cv..2..mean.)) +
  geom_point()


#we can cut at 20
meanR230cv <- head(meanR2cv, 30)
g3<-ggplot(data = meanR230cv, 
       mapping = aes(x =meanR230cv$id, y = meanR230cv$apply.R2.cv..2..mean.)) +
  geom_point()

library("ggpubr")
ggarrange(g1,g2,g3,ncol=3)
################################################################################
#Step8:
################################################################################
#Repeat steps 5 and 6 for the PCR model
################################################################################
nd <- 20
pc20 <- pcr(Y ~ X, validation="LOO", center = FALSE, ncomp=nd)
Yhat <- as.matrix(Xtest) %*% pc$coefficients[,,nd]
predicts <- colnames(Yhat)[apply(Yhat, 1, which.max)]
Ytests2 <- as.matrix(colnames(Ytest)[apply(Ytest, 1, which.max)])
accuracy <- sum(Ytests2 == predicts)/length(Ytests2)
error <- diag(crossprod(as.matrix(Ytest))) - diag(crossprod(Yhat))
R2test <- mean(1 - abs(error)/abs(diag(crossprod(as.matrix(Ytest)))))
errorRate <- 1-accuracy


