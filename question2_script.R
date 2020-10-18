# Exploratory factor analysis (EFA)
rm(list=ls())
library(dplyr)

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
legend('topright', legend = c('Real Data', 'Kaiser\'s rule'), col = c('blue', 'red'),
       lty = c(1, 2))

# Factor analysis (No rotation) -------------------------------------------
cov_mat <- cov(data_just)
fit_fa <- factanal(covmat = , n.obs = nrow(data_just), )

