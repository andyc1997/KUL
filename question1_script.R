# Question 1: PCA + Biplot of value items 
rm(list=ls())
library(dplyr)
library(maptools)

# Working directory -------------------------------------------------------
course.path <- 'C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-1' # Modify if necessary
setwd(course.path)

# Load data ---------------------------------------------------------------
file.name <- 'wvs(1).Rdata'
load(file.name)

# Select and standardize data ---------------------------------------------
f.std <- function(x) (x - mean(x))/sd(x)
data_value <- wvs %>% select(starts_with('V_')) %>% 
  apply(MARGIN = 2, FUN = f.std) %>% as.data.frame

# Mean score by country ---------------------------------------------------------------------
data_value <- data_value %>% aggregate(list(country = wvs$country), mean)

# PCA ---------------------------------------------------------------------
fit_pca <- data_value %>% select(starts_with('V_')) %>% prcomp

# eigenvalues and cumulative proportion of variance explained
eigenvalue <- fit_pca$sdev^2
round(eigenvalue / sum(eigenvalue)*100, 2)

# scree plot
xtick <- 1:length(eigenvalue)
plot(xtick, eigenvalue, type = 'l', col = 'blue', las = 1, 
     main = 'Scree Plot of PCA', xlab = 'Number of Principal Components', 
     ylab = 'Eigenvalues', xaxt = 'n')
points(xtick, eigenvalue, pch = 19, col = 'black')
axis(side = 1, at = xtick, labels = TRUE)

# Horn's procedure
# Bootstrap from raw data
n_row <- dim(data_value)[1]
n_col <- dim(data_value)[2] - 1
for (j in 1:10){
  boot_mat <- matrix(data = rep(0, n_row*n_col), nrow = n_row)
  for (i in 1:n_col){
    samp_mat <- sample(1:n_row, size = n_row, replace = TRUE)
    boot_mat[, i] <- data_value[samp_mat, i + 1]
  }
  eigenvalue_boot <- prcomp(boot_mat)$sdev ^ 2
  lines(xtick, eigenvalue_boot, type = 'l', col = 'red', lty = 2)
}
legend('topright', legend = c('Real Data', 'Bootstrapped Data'), 
       col = c('blue', 'red'), lty = c(1, 2))

# Loading matrix
V <- data_value %>% select(starts_with('V_')) %>% apply(MARGIN = 2, FUN = var)
V.inv_root <- (1 / sqrt(V)) %>% diag
Z <- fit_pca$rotation
D.root <- fit_pca$sdev %>% diag

loading_mat <- V.inv_root %*% Z %*% D.root
loading_mat

plot(loading_mat[, 1:2], xlab = 'PC 1', ylab = 'PC 2', main = 'Plot of Loading Matrix')
pointLabel(loading_mat[, 1], loading_mat[, 2], colnames(data_value)[-1], col = 'red')

# Biplot -----------------------------------------------------------------
biplot(fit_pca, pc.biplot = TRUE, xlabs = data_value$country, col = c('blue', 'red'),
       xlab = 'PC 1', ylab = 'PC 2')
