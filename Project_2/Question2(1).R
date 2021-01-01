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
prune.mod=prune.tree(tree.mod,best=13)
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






