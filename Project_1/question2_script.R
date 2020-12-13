# Exploratory factor analysis (EFA)
rm(list=ls())
library(dplyr)
library(psych)
library(GPArotation)

# Working directory -------------------------------------------------------
course.path <- 'C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-1' # Modify if necessary
setwd(course.path)

# Load data ---------------------------------------------------------------
file.name <- 'wvs(1).Rdata'
load(file.name)

# Select and standardize data ---------------------------------------------
f.std <- function(x) (x - mean(x))/sd(x)
data_just <- wvs %>% select(starts_with('J_')) %>% 
  apply(MARGIN = 2, FUN = f.std) %>% as.data.frame

# Number of factors by PCA ------------------------------------------------
fit_pca <- prcomp(data_just)
eigenvalue <- fit_pca$sdev^2
round(eigenvalue / sum(eigenvalue)*100, 2)

# scree plot
xtick <- 1:length(eigenvalue)
plot(xtick, eigenvalue, type = 'l', col = 'blue', las = 1, 
     main = 'Scree Plot of PCA', xlab = 'Number of Principal Components', 
     ylab = 'Eigenvalues', xaxt = 'n')
points(xtick, eigenvalue, pch = 19, col = 'black')
axis(side = 1, at = xtick, labels = TRUE)

# Kaiser's rule
abline(h = 1, col = 'red', lty = 2)

# Horn's procedure
# Bootstrap from raw data
n_row <- dim(data_just)[1]
n_col <- dim(data_just)[2]
for (j in 1:100){
  boot_mat <- matrix(data = rep(0, n_row*n_col), nrow = n_row)
  for (i in 1:n_col){
    samp_mat <- sample(1:n_row, size = n_row, replace = TRUE)
    boot_mat[, i] <- data_just[samp_mat, i]
  }
  eigenvalue_boot <- prcomp(boot_mat)$sdev ^ 2
  lines(xtick, eigenvalue_boot, type = 'l', col = 'green', lty = 2)
}
legend('topright', legend = c('Real Data', 'Kaiser\'s rule', 'Bootstrapped Data'), 
       col = c('blue', 'red', 'green'), lty = c(1, 2, 2))


# Factor analysis (No rotation) -------------------------------------------
fit_fa <- factanal(data_just, factors = 3, rotation = 'none')
print(fit_fa, cutoff = 0)

# Residual correlations
cor_mat <- cor(data_just)
res_cor_mat <- cor_mat - fit_fa$loadings %*% t(fit_fa$loadings) - diag(fit_fa$uniquenesses)
n <-  sum(ifelse(abs(res_cor_mat) > 0.05, 1, 0))/2
cat('Percentage of nonredundant residuals:', 2 * n/(nrow(cor_mat)^2 - nrow(cor_mat)))

# Factor analysis (Varimax rotation) --------------------------------------
fit_fa_varimax <- factanal(data_just, factors = 3,
                           rotation = 'varimax', scores = 'regression')
print(fit_fa_varimax, cutoff = 0)

# Factor analysis (Oblique rotation) --------------------------------------
fit_fa_obliq <- fa(data_just, nfactors = 3, rotate = 'oblimin', fm = 'mle',
                    scores = 'regression')
print(fit_fa_obliq, cutoff = 0)

# Residual correlations
res_cor_mat <- cor_mat - fit_fa_obliq$loadings %*% fit_fa_obliq$Phi %*% t(fit_fa_obliq$loadings) - diag(fit_fa_obliq$uniquenesses)
n <-  sum(ifelse(abs(res_cor_mat) > 0.05, 1, 0))/2
cat('Percentage of nonredundant residuals:', 2 * n/(nrow(cor_mat)^2 - nrow(cor_mat)))

# Factor scores and Boxplots -----------------------------------------------------------
fa_score <- fit_fa_varimax$scores %>% as.data.frame
fa_score$country <- wvs$country

par(mar = c(5, 9, 2, 2))
boxplot(Factor1 ~ country, data = fa_score, horizontal = TRUE, las = 1, cex.y = 0.2,
        col = 'lightblue1', xlab = 'Factor Score', ylab = '',
        main = 'Distribution of Factor1 Scores by Country')

boxplot(Factor2 ~ country, data = fa_score, horizontal = TRUE, las = 1, cex.y = 0.2,
        col = 'lightpink', xlab = 'Factor Score', ylab = '',
        main = 'Distribution of Factor2 Scores by Country')

boxplot(Factor3 ~ country, data = fa_score, horizontal = TRUE, las = 1, cex.y = 0.2,
        col = 'lightgreen', xlab = 'Factor Score', ylab = '',
        main = 'Distribution of Factor3 Scores by Country')
