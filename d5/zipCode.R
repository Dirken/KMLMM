#
#Author:Ricard Meyerhofer Parra
#
################################################################################
#Step1:
################################################################################
#Read the “zip_train.dat” and “zip_test.dat” files provided. 
#Select a 5% random sample (without replacement) of the train data. 
#Use this sample as your training data, and the complete test data for testing.
################################################################################
readLines("data/zip_train.dat", n=10)
trainSet <- read.delim2("data/zip_train.dat", header = F, sep = " ")
ncol(trainSet) #258 columns
nrow(trainSet) #7291
trainSet$V258 <- NULL

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

library(dummies)
#the predictor matrix we will do a one hot encoding so we are going to create a column per digit
Y <- as.matrix(dummy(Y))

#we are not scaling because if we do so we would just not give importance
#to the position of each pixel and this IS important.
X <- as.matrix(X)
mode(X) <- "double"
X <- scale(as.numeric(X), center = T, scale = F)

#same for test:
Ytest <- as.factor(testSet[,1])
Xtest <- testSet[,-1]

Ytest <- as.matrix(dummy(Ytest))
Xtest <- as.matrix(Xtest)
mode(Xtest) <- "double"
Xtest <- scale(as.numeric(Xtest), center = T, scale = F)

################################################################################
#Step3:
################################################################################
#Perform a multivariate regression with the training data. Compute the average R2.
################################################################################


################################################################################
#Step4:
################################################################################
#Compute the average of the R2 by Leave One Out
################################################################################

################################################################################
#Step5:
################################################################################
#Predict the responses in the test data, be aware of the appropriate centering. 
#You can compute the prediction by a direct scalar product without using the predict function. 
#Compute the average R2 in the test data.
################################################################################

################################################################################
#Step6:
################################################################################
#Assign every test individual to the maximum response and compute the error rate
################################################################################

################################################################################
#Step7:
################################################################################
#Perform a PCR (using LOO). Decide how many components you retain for prediction.
################################################################################

################################################################################
#Step8:
################################################################################
#Repeat steps 5 and 6 for the PCR model
################################################################################