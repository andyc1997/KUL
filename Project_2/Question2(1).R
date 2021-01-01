## Other classification methods
library(MASS) 
library(tree)



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
tree.mod=tree(as.factor(spam)~.,data.train,control=tree.control(nobs=2500,minsize=2,mincut=1),split="deviance")
summary(tree.mod)

#plot tree
plot(tree.mod)
text(tree.mod,pretty=0,cex=1.4)

#use cross-validation to select tuning parameter for pruning the tree
set.seed(209)
cv.out=cv.tree(tree.mod,K=3)
par(cex=1.4)
plot(cv.out$size,cv.out$dev,type='b')

#prune the tree
prune.mod=prune.tree(tree.mod,best=12)
plot(prune.mod)
text(prune.mod,pretty=0)

#make predictions on training and test set using the unpruned tree
pred.train<-predict(tree.mod,newdata=data.train)
err(data.train$spam,pred.train[,2],cutoff=0.5)
#0.078

pred.test<-predict(tree.mod,newdata=data.test)
err(data.test$spam,pred.test[,2],cutoff=0.5)
#0.094

#make predictions on training and test set using the pruned tree
pred.train<-predict(prune.mod,newdata=data.train)
err(data.train$spam,pred.train[,2],cutoff=0.5)
#0.078

pred.test<-predict(prune.mod,newdata=data.test)
err(data.test$spam,pred.test[,2],cutoff=0.5)
#0.094

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


#classification of test sample when accounting for asymmetric classification cost
classif2<-ifelse((pred.test$posterior[,1]*1)>=(pred.test$posterior[,2]*10),1,2)
tab<-table(data.test$spam,classif2)
print(tab)
1-sum(diag(tab))/sum(tab)






