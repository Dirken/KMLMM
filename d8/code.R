#Zip practical work: Fourth delivery, Ricard Meyerhofer Parra

###############################################################################################
#Step 1:
#Read the “zip_train.dat” and “zip_test.dat” files provided. 
#Select the same 5% random sample (without replacement) of the train data used in exercise 1. 
#Use this sample as your training data, and the complete test data for testing
###############################################################################################
set.seed(42) 
readLines("data/zip_train.dat", n=10)
trainSet <- read.delim2("data/zip_train.dat", header = F, sep = " ")
trainSet$V258 <- NULL
testSet <- read.delim2("data/zip_test.dat", header = F, sep = " ")
testSet$V258 <- NULL

library("caret")
#now we are going to select a 5% random sample of the train data:
train5SetIndex <- createDataPartition(trainSet$V1, p =0.05, list = F)
train5Set <- trainSet[train5SetIndex,]
###############################################################################################
#Step 2:
#Define the response matrix (Y) and the predictor matrix (X). Center the predictor matrix
###############################################################################################
Y <- as.factor(train5Set[,1])
X <- train5Set[,-1]

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
Ytest3 <- as.matrix(testSet[,1])
mode(Xtest) <- "double"
Ytest <- as.matrix(dummy(Ytest))
Xtest <- testSet[,-1]
Xtest <- as.matrix(Xtest)
mode(Xtest) <- "double"
Xtest <- scale(Xtest, scale = F, center=attr(X, 'scaled:center'))
library(pls)

###############################################################################################
#Step3: 
#Perform a PLSR2 using “CV” or “LOO” for validation. Decide how many components you retain for prediction?
###############################################################################################
pls <- plsr(Y ~ X, validation="CV")

r2_cv <- R2(pls)$val[1,,]
r2_mean <- apply(r2_cv, MARGIN = 2, mean)
max_comp <- which.max(r2_mean)
plot(1:length(r2_mean),r2_mean, cex=.6, pch =19, col='red', ylab = "RSquare", xlab="Components")
abline(v=max_comp, col='blue', lty = 2)
max_comp

plot(1:25,r2_mean[1:25], cex=.6, pch =19, col='red', ylab = "RSquare", xlab="Components") #17coms
abline(v=max_comp, col='blue', lty = 2)
max_comp


###############################################################################################
#Step 4:
#Predict the responses in the test data, be aware of the appropriate centering. Compute the average R2 in the test data
###############################################################################################
Yhat <- as.matrix(X) %*% pls$projection[, 1:max_comp]
plscores <- data.frame(pls$scores[,1:max_comp])
model <- lm(Y~., data=plscores)
predictions <- predict(model, data.frame(Yhat) , type="response")
Yhat <- predict(model, data.frame(Yhat))
RSS <- colSums(Y-Yhat^2)
TSS <- apply(Y,2, function(x){sum((x-mean(x))^2)})
r2_train <- mean (1 - (RSS/TSS)) #train

#test
Yhat <- as.matrix(Xtest) %*% pls$projection[, 1:max_comp]
plscores <- data.frame(pls$scores[,1:max_comp])
model <- lm(Ytest~., data=plscores)
predictions <- predict(model, data.frame(Yhat) , type="response")
Yhat <- predict(model, data.frame(Yhat))
RSS <- colSums(c(Ytest)-c(Yhat)^2)
TSS <- apply(Y,2, function(x){sum((x-mean(x))^2)})
r2_test <- mean (1 - (RSS/TSS)) #test

plot((scores[,1:2]), col="white",xlab='Comp1', ylab='Comp2') +
  text(scores[,1:2], labels=train[,1], col=train[,1])

###############################################################################################
#Step5: 
#Assign every test individual to the maximum response and compute the error rate
###############################################################################################
labels <- apply(predictions, 1, function(x) which.max(x)) - 1
accuracy <- sum(labels == apply(Ytest, 1, which.max)-1) / length(labels)
E <- diag(crossprod(as.matrix(Ytest))) - diag(crossprod(predictions))
R2 <- mean(1 - abs(E)/abs(diag(crossprod(as.matrix(Ytest)))))


