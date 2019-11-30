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


###############################################################################################
#Step 4:
#Predict the responses in the test data, be aware of the appropriate centering. 
#Compute the average R2 in the test data
###############################################################################################


###############################################################################################
#Step 5: 
#Assign every test individual to the maximum response and compute the error rate.
#Compare the results with the obtained in exercise 1
###############################################################################################


