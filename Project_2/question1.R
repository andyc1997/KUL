# Multivariate Statistics - Assignment 2
# Task 1

# Library ----------------------------------------------------------------------
library(candisc)
library(MASS)
library(class)
library(HDclassif)
library(dplyr)
library(nnet)

# Import data ------------------------------------------------------------------
setwd('C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-2')
load('dwvs.Rdata')

# 1. Canonical Discriminant Analysis -------------------------------------------
dwvs_candisc_lm <- lm(cbind(F_rights, F_steal, F_crime, F_religion, F_realizeself, 
                             F_dogood, F_violence) ~ country, data = dwvs)
dwvs_candisc <- candisc(dwvs_candisc_lm)

summary(dwvs_candisc)
print(dwvs_candisc) # Print test for canonical discriminant analysis
plot(dwvs_candisc, scale = 4.5, cex = 0.5, var.cex = 1.2, var.col = 'blue', ellipse = TRUE,
     main = 'Observations in Discriminant space') # Observations in discriminant space
legend('bottomleft', legend = levels(dwvs$country), pch = 1:3,
       col = c('red', 'green', 'blue'))

# 2. LDA with empirical prior --------------------------------------------------
dwvs_lda.train <- lda(country ~., data = dwvs, CV = FALSE) # LDA for training error
pred.train <- predict(dwvs_lda.train, dwvs)
(confusion_mat <- table(dwvs$country, pred.train$class)) # Confusion matrix
(error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error

dwvs_lda.loocv <- lda(country ~., data = dwvs, CV = TRUE) # LDA for loocv error
(confusion_mat <- table(dwvs$country, dwvs_lda.loocv$class)) # Confusion matrix
(error.loocv <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # LOOCV error

# Store results
error_lda_emprior <- c('error.train' = error.train, 'error.loocv' = error.loocv)

# 2. LDA with equal prior ----------------------------------------------------
prior <- c(1/3, 1/3, 1/3)
dwvs_lda.train <- lda(country ~., data = dwvs, CV = FALSE, prior = prior) # LDA for training error 
pred.train <- predict(dwvs_lda.train, dwvs, prior = prior)
(confusion_mat <- table(dwvs$country, pred.train$class)) # Confusion matrix
(error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error

dwvs_lda.loocv <- lda(country ~., data = dwvs, CV = TRUE, prior = prior) # LDA for LOOCV error
(confusion_mat <- table(dwvs$country, dwvs_lda.loocv$class)) # Confusion matrix
(error.loocv <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # LOOCV error

# Store results
error_lda_eqprior <- c('error.train' = error.train, 'error.loocv' = error.loocv)

# 2. QDA with empirical prior ------------------------------------------------
dwvs_qda.train <- qda(country ~., data = dwvs, CV = FALSE) # QDA for training error 
pred.train <- predict(dwvs_qda.train, dwvs)
(confusion_mat <- table(dwvs$country, pred.train$class)) # Confusion matrix
(error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error

dwvs_qda.loocv <- qda(country ~., data = dwvs, CV = TRUE) # QDA for loocv error 
(confusion_mat <- table(dwvs$country, dwvs_qda.loocv$class)) # Confusion matrix
(error.loocv <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # LOOCV error

error_qda_emprior <- c('error.train' = error.train, 'error.loocv' = error.loocv)


# 2. QDA with equal prior ----------------------------------------------------
dwvs_qda.train <- qda(country ~., data = dwvs, CV = FALSE, prior = prior) # QDA for training error 
pred.train <- predict(dwvs_qda.train, dwvs)
(confusion_mat <- table(dwvs$country, pred.train$class)) # Confusion matrix
(error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error

dwvs_qda.loocv <- qda(country ~., data = dwvs, CV = TRUE, prior = prior) # QDA for loocv error 
(confusion_mat <- table(dwvs$country, dwvs_qda.loocv$class)) # Confusion matrix
(error.loocv <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # LOOCV error

error_qda_eqprior <- c('error.train' = error.train, 'error.loocv' = error.loocv)


# 2. KNN ------------------------------------------------------------------
set.seed(979797) # Repeatable results
k_min <- 1; k_max <- 100 # k from 1 to 100
error.train <- rep(0, k_max); error.loocv <- rep(0, k_max) # Vectors to hold error

hitratknn <- function(target, pred){
  # function to calculate error for kNN classifier
  target <- as.character(target)
  pred <- as.character(pred)
  n <- length(target)
  
  error <- 1 - sum(target == pred)/n
  return(error)
}

for (i in k_min:k_max){
  # Get training error for kNN classifier
  dwvs_knn.train <- knn(dwvs[, -1], dwvs[, -1], dwvs$country, k = i)
  error.train[i] <- hitratknn(dwvs$country, dwvs_knn.train)
}

for (i in k_min:k_max){
  # Get training error for kNN classifier
  dwvs_knn.loocv <- knn.cv(dwvs[, -1], dwvs$country, k = i)
  error.loocv[i] <- hitratknn(dwvs$country, dwvs_knn.loocv)
}

error_df_knn <- data.frame(knn.train = error.train, knn.loocv = error.loocv)


# 2. HDclassif ------------------------------------------------------------
model.code <- c('AkjBkQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkjBQkDk', 'AkBQkDk', 'ABQkDk', 
                'AkjBkQkD', 'AkBkQkD', 'ABkQkD', 'AkjBQkD', 'AkBQkD', 'ABQkD', 'AjBQD',
                'ABQD')

error_df_hdda <- data.frame(error.train = NULL, error.loocv = NULL)

for (code in model.code){
  dwvs_hdda.train <- hdda(dwvs[, -1], dwvs$country, model = code, LOO = FALSE,
                          d_select = "Cattell", threshold = 0.05) # Hdda for training error
  summary(dwvs_hdda.train)
  pred.train <- predict(dwvs_hdda.train, dwvs[, -1], dwvs$country)
  (confusion_mat <- table(dwvs$country, pred.train$class)) # Confusion matrix
  (error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error
  
  dwvs_hdda.loocv <- hdda(dwvs[, -1], dwvs$country, model = code, LOO = TRUE,
                          d_select = "Cattell", threshold = 0.05) # Hdda for training error
  summary(dwvs_hdda.loocv)
  (confusion_mat <- table(dwvs$country, dwvs_hdda.loocv$class)) # Confusion matrix
  (error.loocv <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error
  
  error_hdda <- c('error.train' = error.train, 'error.loocv' = error.loocv)
  error_df_hdda <- error_df_hdda %>% rbind(error_hdda)
}

row.names(error_df_hdda) <- model.code
colnames(error_df_hdda) <- c('error.train', 'error.loocv')

# Summary plot ------------------------------------------------------------
# Comparing LDA and QDA with different prior probabilities
cex.size <- 2 # Double marker sizes
color.code <- c('red', 'blue') # Red for LDA; Blue for QDA
pch.code <- c(3, 2) # 3 for Empirical prior; 2 for Equal prior
plot(error_lda_emprior, col = color.code[1], ylim = c(0.145, 0.156), 
     pch = pch.code[1], xaxt = 'n', las = 1, cex = cex.size,
     ylab = 'Error', xlab = '', main = 'Comparing LDA and QDA with different \n prior probabilities')
points(error_lda_eqprior, col = color.code[1], pch = pch.code[2], cex = cex.size)
points(error_qda_emprior, col = color.code[2], pch = pch.code[1], cex = cex.size)
points(error_qda_eqprior, col = color.code[2], pch = pch.code[2], cex = cex.size)
axis(1, at = 1:2, labels = c('Training Error', 'LOOCV Error'))
legend('topleft', legend = c('LDA', 'QDA', 'Empirical Prior', 'Equal Prior'), 
       col = c('red', 'blue', 'black', 'black'), pch = c(19, 19, 3, 2))

data.frame(error_lda_emprior, error_lda_eqprior, error_qda_emprior, error_qda_eqprior)
# Best model for LDA: LDA with empirical prior
# Best model for QDA: QDA with equal prior

# Comparing different HDDA models
cex.size <-  1.2 # 1.2 marker sizes
pch.code <- c(18, 17) # 18 for training error, 17 for loocv error
color.code <- c('red', 'blue') # red for training error, blue for loocv error
plot(error_df_hdda[, 1], pch = pch.code[1], col = color.code[1], xaxt = 'n', las = 1, cex = cex.size,
     ylab = 'Error', xlab = '', main = 'Comparing different HDDA models')
points(error_df_hdda[, 2], pch = pch.code[2], col = color.code[2], cex = cex.size)
abline(v = 1:14, col = 'grey', lty = 'dashed')
legend('left', legend = c('Training Error', 'LOOCV Error'), pch = pch.code, col = color.code)
for (p in c(1, 4, 7, 10)){
  # Points labeling
  text(p, error_df_hdda[p, 2], labels = round(error_df_hdda[p, 2], 4), pos = 4, col = 'blue')
  text(p, error_df_hdda[p, 1], labels = round(error_df_hdda[p, 1], 4), pos = 4, col = 'red')
}
axis(1, at = 1:14, model.code, las = 2)
# Best model for HDDA: full model


# Comparing kNN with other methods
color.code <- c('blue', 'red')
plot(error_df_knn[, 1], type = 'l', col = color.code[1], ylim = c(0.06, 0.155), las = 1,
     xlab = 'k', ylab = 'Error', main = 'kNN performance compared to other methods')
points(error_df_knn[, 2], type = 'l', col = color.code[2])

abline(v = 9, col = 'black', lty = 'dashed') # Best k for kNN
abline(h = error_df_hdda[1, 2], col = 'grey', lty = 'dashed') # hdda
abline(h = error_qda_eqprior[2], col = 'brown', lty = 'dotted') # qda
abline(h = error_lda_emprior[1], col = 'brown', lty = 'dotted') # lda

text(9, 0.06, labels = 'k = 9', pos = 4, col = 'black')
text(80, error_df_hdda[1, 2], labels = 'HDDA', pos = 1, col = 'grey')
text(60, error_qda_eqprior[2], labels = 'QDA', pos = 1, col = 'brown')
text(60, error_lda_emprior[1], labels = 'LDA', pos = 3, col = 'brown')

legend('right', legend = c('Training Error', 'LOOCV Error'), col = color.code, lty = 1)

# Summary of Training and LOOCV errors for all chosen models
colnames(error_df_knn) <- c('error.train', 'error.loocv')
rbind('error_lda_emprior' = error_lda_emprior, 
      'error_qda_eqprior' = error_qda_eqprior, 
      error_df_hdda['AkjBkQkDk', ],
      'error_kNN=9' = error_df_knn[9, ])

# 3. Multinomial Logistic Regression -----------------------------------------------
dwvs_multinom <- multinom(country ~., family = 'multinomial', data = dwvs, 
                          maxit = 1000, hess = TRUE)
summary(dwvs_multinom) # Also get the standard error
devs_sum_multinom <- summary(dwvs_multinom)
exp(devs_sum_multinom$coefficients)

# 3. Multinomial as a classifier ------------------------------------------
pred.train <- predict(dwvs_multinom, dwvs) # Training error for multinomial logistic regression
(confusion_mat <- table(dwvs$country, pred.train)) # Confusion matrix
(error.train <- 1 - sum(diag(confusion_mat))/sum(confusion_mat)) # Training error

n <- nrow(dwvs)
error.loocv <- rep(0, n)
for (i in 1:n){
  dwvs_multinom.loocv <- multinom(country ~., family = 'multinomial', data = dwvs[-i, ],
                                  maxit = 1000, trace = FALSE)
  error.loocv[i] <- ifelse(predict(dwvs_multinom.loocv, dwvs[i, ]) == dwvs$country[i], 0, 1)
}
error.loocv <- mean(error.loocv)


