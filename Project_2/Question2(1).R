## Other classification methods
library(MASS) 
library(tree)
library(randomForest)
library(gbm)


#functions
err <- function(observed, prob, cutoff){
  tab <- table(observed, ifelse(prob>=0.5, 1, 0))
  err <- 1-sum(diag(tab))/sum(tab)
  return(err)
}

err2 <- function(observed, predicted){
  tab <- table(observed, predicted)
  err2 <- 1-sum(diag(tab))/sum(tab)
  return(err2)
}


# Working directory -------------------------------------------------------
setwd("C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-2")

# Load data ---------------------------------------------------------------
load("spamdata.Rdata")

# Select data ---------------------------------------------
#create train and test set
set.seed(5) # Seed for splitting data
trainsize <- 2500
train <-  sample(nrow(spamdata), trainsize)
data.train <- spamdata[train,]
data.test <- spamdata[-train,]

n <- dim(data.train)[1] # Number of observations
p <- dim(data.train)[2] # Number of features

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
cv.out = cv.tree(tree.mod, K=10, FUN=prune.tree)
par(cex=1.4)
plot(cv.out$size, cv.out$dev, type='b', col='red',
     xlab='Tree Complexity', ylab='Crosss-Validation Error',
     main='Cross-Validation Error vs Tree Complexity') # CV error becomes flatten when tree complexity is 11

#plot tree (prune the tree)
prune.mod = prune.tree(tree.mod, best=10)  
plot(prune.mod)
text(prune.mod, pretty=0, cex=0.7, col='blue')
title('Classification tree after pruning')

#make predictions on training and test set using the unpruned tree
pred.train <- predict(tree.mod, newdata=data.train)
err.full.train <- err(data.train$spam, pred.train[,2], cutoff=0.5)

pred.test <- predict(tree.mod,newdata=data.test)
err.full.test <- err(data.test$spam, pred.test[,2], cutoff=0.5)

#make predictions on training and test set using the pruned tree
pred.train <- predict(prune.mod, newdata=data.train)
err.prun.train <- err(data.train$spam, pred.train[,2], cutoff=0.5)

pred.test <- predict(prune.mod, newdata=data.test)
err.prun.test <- err(data.test$spam, pred.test[,2], cutoff=0.5)

# Summarize the above errors in the dataframe
data.frame(error.train=c(err.full.train, err.prun.train),
           error.test=c(err.full.test, err.prun.test),
           row.names=c('Full Tree', 'Pruned Tree'))


#Rewrite ---------------------------------------------------------------
# Empirical prior + other prior probabilities
em_prior <- as.vector(prop.table(table(data.train$spam)))
all_prior <- matrix(c(0.5, 0.5, 0.6, 0.4, 0.7, 0.3, 0.8, 0.2, 0.9, 0.1), ncol = 2, byrow = TRUE)
colnames(all_prior) <- c(0, 1)

# LDA: Shorter code for prior probabilities and classification costs
get_metrics <- function(tab){
  # Error, sensitivity, false postive rate
  # Order of factor = 0, 1
  # Order of table = row: predicted, column: true
  error <- 1 - sum(diag(tab))/sum(tab)
  sensitivity <- tab['1', '1']/sum(tab[, '1'])
  false_positive <- 1 - tab['0', '0']/sum(tab[, '0'])
  return(c(error, sensitivity, false_positive))
}

summary.lda <- function(prior, penalty){
  # LDA classifier
  lda.out <- lda(as.factor(spam)~., prior = prior, data = data.train)
  lda.pred <- predict(lda.out, prior = prior, newdata = data.train) # Training error
  # Bayes classifier
  class.lbl <- ifelse(penalty*lda.pred$posterior[,1] >= lda.pred$posterior[,2],
                      0, 1)
  confus.mat <- table(class.lbl, data.train$spam)
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.train <- get_metrics(confus.mat)
  print(confus.mat)
  
  lda.pred <- predict(lda.out, prior = prior, newdata = data.test) # Testing error
  # Bayes classifier
  class.lbl <- ifelse(penalty*lda.pred$posterior[,1] >= lda.pred$posterior[,2],
                      0, 1)
  confus.mat <- table(class.lbl, data.test$spam) # Order: Observed, True
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.test <- get_metrics(confus.mat)
  print(confus.mat)
  # Return results 
  return(data.frame(train = metrics.train, test = metrics.test,
                       row.names = c('error', 'sens', 'fp')))
}

# LDA: Different prior + Equal cost
(sum.lda.equal <- summary.lda(all_prior[5, ], 1)) # Change the first argument to see different results
# LDA: Different prior + Unequal cost
(sum.lda.unequal <- summary.lda(all_prior[5, ], 10)) # Change the first argument to see different results

# Bagging
summary.bagging <- function(penalty){
  set.seed(78)
  bag.out <- randomForest(as.factor(spam)~., data = data.train,
                          mtry = p - 1, ntree = 5000) 
  # Training error
  bag.pred <- predict(bag.out, newdata = data.train, type = 'prob')
  class.lbl <- ifelse(penalty*bag.pred[, 1] >= bag.pred[, 2],
                      0, 1)
  confus.mat <- table(class.lbl, data.train$spam)
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.train <- get_metrics(confus.mat)
  print(confus.mat)
  # Testing error
  bag.pred <- predict(bag.out, newdata = data.test, type = 'prob')
  class.lbl <- ifelse(penalty*bag.pred[, 1] >= bag.pred[, 2],
                      0, 1)
  confus.mat <- table(class.lbl, data.test$spam) # Order: Observed, True
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.test <- get_metrics(confus.mat)
  print(confus.mat)
  # Return results 
  return(data.frame(train = metrics.train, test = metrics.test,
                    row.names = c('error', 'sens', 'fp')))
}

# BAG: Different prior + Equal cost
(sum.bag.equal <- summary.bagging(1))
# BAG: Different prior + Unequal cost
(sum.bag.unequal <- summary.bagging(10))

# Random forest
summary.rf <- function(penalty){
  set.seed(78)
  bag.out <- randomForest(as.factor(spam)~., data = data.train,
                          mtry = floor((p - 1)/6), ntree = 5000) 
  # Training error
  bag.pred <- predict(bag.out, newdata = data.train, type = 'prob')
  class.lbl <- ifelse(penalty*bag.pred[, 1] >= bag.pred[, 2],
                      0, 1)
  confus.mat <- table(class.lbl, data.train$spam)
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.train <- get_metrics(confus.mat)
  print(confus.mat)
  # Testing error
  bag.pred <- predict(bag.out, newdata = data.test, type = 'prob')
  class.lbl <- ifelse(penalty*bag.pred[, 1] >= bag.pred[, 2],
                      0, 1)
  confus.mat <- table(class.lbl, data.test$spam) # Order: Observed, True
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.test <- get_metrics(confus.mat)
  print(confus.mat)
  # Return results 
  return(data.frame(train = metrics.train, test = metrics.test,
                    row.names = c('error', 'sens', 'fp')))
}

# RF: Different prior + Equal cost
(sum.rf.equal <- summary.rf(1))
# RF: Different prior + Unequal cost
(sum.rf.unequal <- summary.rf(10))

# Gradient Boosting Method
summary.gbm <- function(penalty){
  set.seed(78)
  gbm.out <- gbm(spam~., distribution = 'bernoulli',
                          data = data.train, n.tree = 5000, cv.folds = 5,
                          interaction.depth = 3, shrinkage = 0.05) 
  # Training error
  gbm.pred <- predict(gbm.out, newdata = data.train, type = 'response', n.trees = 1000)
  class.lbl <- ifelse(penalty*(1 - gbm.pred) >= gbm.pred,
                      0, 1)
  confus.mat <- table(class.lbl, data.train$spam)
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.train <- get_metrics(confus.mat)
  print(confus.mat)
  # Testing error
  gbm.pred <- predict(gbm.out, newdata = data.test, type = 'response', n.trees = 1000)
  class.lbl <- ifelse(penalty*(1 - gbm.pred) >= gbm.pred,
                      0, 1)
  confus.mat <- table(class.lbl, data.test$spam) # Order: Observed, True
  names(attributes(confus.mat)$dimnames) <- c('Predicted', 'True')
  metrics.test <- get_metrics(confus.mat)
  print(confus.mat)
  # Return results 
  return(data.frame(train = metrics.train, test = metrics.test,
                    row.names = c('error', 'sens', 'fp')))
}

# GBM Different prior + Equal cost
(sum.gbm.equal <- summary.gbm(1))
# GBM: Different prior + Unequal cost
(sum.gbm.unequal <- summary.gbm(10))
