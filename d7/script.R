library("pls")

################################################################################
#Step 1 and 2:
################################################################################
#1.	Read the data files “data_set_ALL_AML_train.csv” and “data_set_ALL_AML_independent.csv”. 
#Enter the response manually into R from the “table_ALL_AML_predic.doc” document 
#(the response corresponds to the “actual” column of such word document).
#2.	Form the data matrices X and Xt, containing the gene expression for the 38 training samples and 34 test samples. 
#Be aware that data is presented in its transposed form, with instances as columns and they are not in order. 
#Only numeric information is pertinent to solve the problem.

source(utils.R)
train <- read.csv("data/data_set_ALL_AML_train.csv", sep=";", header=TRUE)
test  <- read.csv("data/data_set_ALL_AML_independent.csv", sep=";", header=TRUE)

train_t <- filterDataset(train)
test_t <- filterDataset(test)
train_response <- c(rep(0,27),rep(1,11))
test_response <- c(rep(0,11),rep(1,5),rep(0,2),rep(1,2),rep(0,1),rep(1,7),rep(0,6))

train_t <- cbind(train_t, train_response)
response <- which(colnames(train_t)=="train_response")
X_train <- train_t[,-response]
Y_train <- train_t[,response]
X_test <- test_t[,-response]

################################################################################
#Step3:
################################################################################
#3.	Perform the PLS1 regression of the training data. Select the number of PLS1 components

p1 <- plsr(train_response ~ ., center = TRUE, ncomp = 10, data = train_t, validation = "LOO")
plot(RMSEP(p1), legendpos = "topright")
R2(p1)
plot(R2(p1), legendpos = "bottomright")

nd <- 4

thresholds <- seq(0,1, by=0.1)
accuracyTrain <- rep(0,length(thresholds))
accuracyTest <- rep(0,length(thresholds))
for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  train_predicted <- leukemia(p1, X_train, threshold=t, ncomp=nd)
  accuracyTrain[i] <- sum(train_predicted == train_response)/length(train_response)*100
  test_predicted <- leukemia(p1, X_test, threshold=t, ncomp=nd)
  accuracyTest[i] <- sum(test_predicted == test_response)/length(test_predicted)*100
}

plot(rep(thresholds,2), c(accuracyTrain,accuracyTest), main="Accuracy with PSLR model", xlab="Thresholds", ylab="Accuracy")
lines(thresholds, accuracyTrain, col="blue", lwd=4)
lines(thresholds, accuracyTest, col="green", lwd=4)

################################################################################
#Step4:
################################################################################
#4.	Project the test data as supplementary individuals onto the selected PLS1 components 
#(be aware of centering the test data respect to the mean of the training data).

X_train_pls <- p1$scores[,1:nd]
train_pls <- as.data.frame(cbind(X_train_pls, train_response))
X_train_pls <- train_pls[,which(colnames(train_pls)!="train_response")]
X_test_centered <- scale(X_test, center = colMeans(X_train), scale = FALSE)
X_test_pls <- X_test_centered %*% p1$projection
X_test_pls <- as.data.frame(X_test_pls[,1:nd])

################################################################################
#Step 5:
################################################################################
# 5 Perform a joint plot of the train and test individuals in the plane of the two first PLS1 components, 
#differentiating those individuals with ALL from those with AML
plot(p1, plottype = "scores", comps = 1:2, type="n", main="X Scores")
text(X_train_pls, labels=rownames(X_train_pls), col=as.vector(factor(train_response,levels=c(0,1),labels=c("red","blue"))))
text(X_test_pls, labels=rownames(X_test_pls), col=as.vector(factor(test_response,levels=c(0,1),labels=c("darkred","darkblue"))))
legend(53100, 50500, legend=c("ALL train", "AML train", "ALL test", "AML test"),
       col=c("red", "blue", "darkred", "darkblue"), lty=c(1,1,1,1), cex=0.8)


################################################################################
#Step 6:
################################################################################
# 6 Obtain the logistic regression model (or any other model of your choice) to predict the response in the training data, 
#using as input the selected PLS1 components
logit.mod <- glm(train_response ~ ., data=train_pls, family=binomial(link="logit"))
################################################################################
#Step 7:
################################################################################
# 7.	Predict the probability of AML leukemia in the test sample

thresholds <- seq(0,1, by=0.1)
accuracyTrain <- rep(0,length(thresholds))
accuracyTest <- rep(0,length(thresholds))
for (i in 1:length(thresholds)) {
  t <- thresholds[i]
  train_predicted <- leukemia(logit.mod, X_train_pls, threshold=t)
  accuracyTrain[i] <- sum(train_predicted == train_response)/length(train_response)*100
  test_predicted <- leukemia(logit.mod, X_test_pls, threshold=t)
  accuracyTest[i] <- sum(test_predicted == test_response)/length(test_predicted)*100
}

plot(rep(thresholds,2), c(accuracyTrain,accuracyTest), main="Accuracy with Logistic model", xlab="Thresholds", ylab="Accuracy")
lines(thresholds, accuracyTrain, col="green", lwd=4)
lines(thresholds, accuracyTest, col="blue", lwd=4)

df <- data.frame(c=X_train_pls[,1])
df$resp <- predict(logit.mod, newdata=X_train_pls, threshold="0.5", type="response")+0.00
df2 <- data.frame(c=X_test_pls[,1])
df2$resp <- predict(logit.mod, newdata=X_test_pls, threshold="0.5", type="response")-0.00
plot(df$c,df$resp, ylab="Response", xlab="Component 1", ylim=c(-0.05, 1.05), pch=16, cex=0.8,
     col=as.vector(factor(train_response,levels=c(0,1),labels=c("red","blue"))))
axe.x = seq(range(df$c)[1],range(df$c)[2],length=1000)
f.x = exp(logit.mod$coef[1]+axe.x*logit.mod$coef[2])/(1+exp(logit.mod$coef[1]+axe.x*logit.mod$coef[2]))
lines(axe.x,f.x,col="green",lwd=2)
points(df$c,df$resp, ylab="Response", xlab="Component 1", pch=16, cex=0.8,
       col=as.vector(factor(train_response,levels=c(0,1),labels=c("red","blue"))))
points(df2$c,df2$resp, ylab="Response", xlab="Component 1", pch=16, cex=0.8,
       col=as.vector(factor(test_response,levels=c(0,1),labels=c("darkred","darkblue"))))
legend(43500, 0.9, legend=c("ALL train", "AML train", "ALL test", "AML test", "link function"),
       col=c("red", "blue", "darkred", "darkblue", "green"), pch=c(16,16,16,16,-1), lty=c(0,0,0,0,1), cex=0.8)

