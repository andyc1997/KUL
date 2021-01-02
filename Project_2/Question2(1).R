## Other classification methods
library(MASS) 
library(tree)
library(randomForest)
library(gbm)




#functions
err<-function(observed,prob,cutoff)
{tab<-table(observed,ifelse(prob>=0.5,1,0))
err<-1-sum(diag(tab))/sum(tab)
return(err)
}

err2<-function(observed,predicted)
{tab<-table(observed,predicted)
err2<-1-sum(diag(tab))/sum(tab)
return(err2)
}


# Working directory -------------------------------------------------------
setwd("~/multivariate")
# Load data ---------------------------------------------------------------
load("~/multivariate/spamdata.Rdata")
# Select data ---------------------------------------------
#create train and test set
set.seed(229)
trainsize<-2500
train = sample(nrow(spamdata), trainsize)
data.train<-spamdata[train,]
data.test<-spamdata[-train,]

n <- dim(data.train)[1]
p <- dim(data.train)[2]

# complex classification tree  ----------------------------------------

#grow complex tree using deviance as criterion
tree.mod = tree(as.factor(spam)~., data.train,control=
                  tree.control(nobs=2500, minsize=2, mincut=1), split="deviance")
summary(tree.mod)

#plot tree (before pruning, full size)
plot(tree.mod)
text(tree.mod, pretty=0, cex=0.7, col='blue')
title('Classification tree before pruning')

#use cross-validation to select tuning parameter for pruning the tree
set.seed(209)
cv.out = cv.tree(tree.mod, K=11)
par(cex=1.4)
plot(cv.out$size, cv.out$dev, type='b', col='red',
     xlab='Tree Complexity', ylab='Crosss-Validation Error',
     main='Cross-Validation Error vs Tree Complexity') # CV error becomes flatten when tree complexity is 11

#plot tree (prune the tree)
prune.mod = prune.tree(tree.mod, best=11)  
plot(prune.mod)
text(prune.mod, pretty=0, cex=0.7, col='blue')
title('Classification tree after pruning')

#make predictions on training and test set using the unpruned tree
pred.train <- predict(tree.mod, newdata=data.train)
err.full.train <- err(data.train$spam, pred.train[,2], cutoff=0.5)
#0.078

pred.test <- predict(tree.mod,newdata=data.test)
err.full.test <- err(data.test$spam, pred.test[,2], cutoff=0.5)
#0.09424084

#make predictions on training and test set using the pruned tree
pred.train <- predict(prune.mod, newdata=data.train)
err.prun.train <- err(data.train$spam, pred.train[,2], cutoff=0.5)
#0.0836 

pred.test <- predict(prune.mod, newdata=data.test)
err.prun.test <- err(data.test$spam, pred.test[,2], cutoff=0.5)
#0.09852451

# Summarize the above errors in the dataframe
data.frame(error.train=c(err.full.train, err.prun.train),
           error.test=c(err.full.test, err.prun.test),
           row.names=c('Full Tree', 'Pruned Tree'))

#linear discriminant analysis-------------------------------------------

amountofspam <- sum(data.train$spam)
amountofemail <- n - amountofspam

amountofemail_prior <- amountofemail / n
amountofspam_prior <- amountofspam / n

# Account for different prior probabilities and equal classification costs

lda.out<-lda(spam~.,prior=c(amountofemail_prior,amountofspam_prior),data=data.train)
pred.train<-predict(lda.out,prior=c(amountofemail_prior,amountofspam_prior),data.train)
pred.train$class[1:100]
pred.train$posterior[1:5,]
#observed versus predicted class labels
tab<-table(data.train$spam,pred.train$class)
print(tab)
#classification error
1-sum(diag(tab))/sum(tab)

#sensitivity 
sens1 <- 757 /(219 + 757)
spec1 <- 1462 / (1462 + 62)

# Account for different prior probabilities and unequal classification costs: 

classif<-ifelse(1*pred.train$posterior[,1]>=10*pred.train$posterior[,2],1,2)
tab<-table(data.train$spam,classif)
print(tab)
#classification error
1-sum(diag(tab))/sum(tab)

#sensitivity 
sens2 <- 935 /(41 + 935)
spec2 <- 1066 / (1066 + 458)

# classify test sample using model
# calibrated on training sample
pred.test<-predict(lda.out,data.test)

#classification test sample after accounting for prior probabilities
tab<-table(data.test$spam,pred.test$class)
print(tab)
1-sum(diag(tab))/sum(tab)

# classify test sample using model

# calibrated on training sample
pred.test<-predict(lda.out,books2[,5:6])

#classification test sample after accounting for prior probabilities
tab<-table(books2$buyer,pred.test$class)
print(tab)
1-sum(diag(tab))/sum(tab)

sens3 <- 651 /(651 + 186)
spec3 <- 1210 / (1210 + 54)

#classification of test sample when accounting for asymmetric classification cost
classif2<-ifelse((pred.test$posterior[,1]*1)>=(pred.test$posterior[,2]*10),1,2)
tab<-table(data.test$spam,classif2)
print(tab)
1-sum(diag(tab))/sum(tab)

sens4 <- 801 /(801 + 36)
spec4 <- 909 / (909 + 355)

#bagging-----------------------------------------------------------------

set.seed(2500)
bag.mod=randomForest(as.factor(spam)~.,data=data.train,mtry=57,ntree=5000,importance=TRUE)
bag.mod

pred.train<-predict(bag.mod,newdata=data.train)
err2(data.train$spam,pred.train)

pred.test<-predict(bag.mod,newdata=data.test)
err2(data.test$spam,pred.test)

sens5 <- 881 /(881 + 95)
spec5 <- 1465 / (1465 + 59)
#random Forests---------------------------------------------------------------

set.seed(2500)
rf.mod=randomForest(as.factor(spam)~.,data=data.train,mtry=20,ntree=5000,importance=TRUE)
rf.mod

pred.train<-predict(rf.mod,newdata=data.train)
err2(data.train$spam,pred.train)

pred.test<-predict(rf.mod,newdata=data.test)
err2(data.test$spam,pred.test)

sens6 <- 887 /(887 + 89)
spec6 <- 1471 / (1471 + 53)

#Boosting-----------------------------------------------------------------------

set.seed(2501)

boost.mod=gbm(spam~.,data=data.train,distribution="bernoulli",n.trees=10000,interaction.depth=4,shrinkage=0.001,cv.folds=5)
gbm.perf(boost.mod,method="cv")
legend("topright",c("train error","CV error"),col=c("green","black"),lty=c(1,1))

pred.train<-predict(boost.mod,n.trees=10000,newdata=data.train,type="response")
err(data.train$spam,pred.train,cutoff=0.5)
pred.test<-predict(boost.mod,n.trees=10000,newdata=data.test,type="response")
err(data.test$spam,pred.test,cutoff=0.5)





