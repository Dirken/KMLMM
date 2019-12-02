#Zip practical work: Second delivery, Ricard Meyerhofer Parra

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
Ytest <- as.matrix(dummy(Ytest))
Xtest <- testSet[,-1]
Xtest <- as.matrix(Xtest)
mode(Xtest) <- "double"
Xtest <- scale(Xtest, scale = F, center=attr(X, 'scaled:center'))

###############################################################################################
#Step 3:
#Perform the Inter Batteries Analysis following the formulae given in the slides. 
#Be aware that Y is not of full rank.  Decide how many components you retain for prediction?
###############################################################################################
Vxy <- var(X,Y)
rank <- qr(Vxy)$rank
aib <- eigen(t(Vxy)%*%Vxy)
A <- Vxy %*% aib$vectors %*% diag(aib$values^(-0.5))
TIB <- as.matrix(X) %*% A

lmb <- aib$values
p <- ncol(X)
a <- sum(lmb > 0.0000001)
com.xt <- matrix(NA,p,a)

for (i in 1:p) {for ( j in 1:a) {com.xt[i,j] <- summary(lm(X[,i]~TIB[,1:j]))$r.squared}}

# R squared by component
R_squared <- apply(com.xt,2,mean) 
Components <- data.frame(cbind("Components" = 1:rank, R_squared))

ggplot(Components,aes(x=Components,y=R_squared)) + 
  geom_point(shape=21) + geom_line() + theme_bw() + scale_x_continuous(breaks = c(1:9)) +
  labs(y = expression(R^{2})) + ggtitle(expression(paste("Number of Components against  ", R^{2})))


###############################################################################################
#Step 4:
#Predict the responses in the test data, be aware of the appropriate centering. 
#Compute the average R2 in the test data
#Step 5: 
#Assign every test individual to the maximum response and compute the error rate.
#Compare the results with the obtained in exercise 1
###############################################################################################


#we choose 9 components
model <- lm(as.matrix(Y) ~ TIB - 1)
summaryModel <- summary(model)

#train
pred <- TIB %*% model$coefficients
pred_values <- unname(apply(pred,1,function(x) which.max(x)-1))

t_cm <- table(pred_values,train5Set[,1])
rownames(t_cm) <- c(0:9)
colnames(t_cm) <- c(0:9)
t_accuracy <- sum(diag(t_cm))/sum(t_cm)
IndividualAccuracy <- diag(t_cm)/colSums(t_cm)

#        0         1         2         3         4         5         6         7         8         9 
#0.9000000 1.0000000 0.8378378 0.9696970 0.7272727 0.7857143 0.7941176 0.7878788 0.8571429 0.8787879 
#test
Vxy <- var(Xtest,Ytest)
aib <- eigen(t(Vxy)%*%Vxy)
A <- Vxy %*% aib$vectors %*% diag(aib$values^(-0.5))
A[,10] <- 0
TIB <- as.matrix(Xtest) %*% A
pred_test <- TIB %*% model$coefficients
pred_test_values <- unname(apply(pred_test,1,function(x) which.max(x)-1))

#confusion matrix
table(pred_test_values)
table(Ytest2)
t_cm <- table(pred_test_values,Ytest2)
rownames(t_cm) <- c(0:9)
colnames(t_cm) <- c(0:9)
t_cm
t_accuracy <- sum(diag(t_cm))/sum(t_cm)
IndividualAccuracy <- diag(t_cm)/colSums(t_cm)
Accuracy <- round(c(IndividualAccuracy,"Model" = t_accuracy)*100,0)
TSS = apply(Ytest,2,function(x){sum((x-mean(x))^2)})
RSS = colSums((Ytest- pred_test)^2)
R_sq_test = 100 - mean(RSS/TSS)



