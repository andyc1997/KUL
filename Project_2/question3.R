# Multivariate Statistics - Assignment 2
# Task 3

# Library -----------------------------------------------------------------
library(dplyr)
library(dendextend)
library(HDclassif)
library(mclust)
library(ggplot2)

# Import data -------------------------------------------------------------
setwd('C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-2')
load('shopping.Rdata')


# Standardize Data --------------------------------------------------------
shopping_std <- shopping %>% scale() # Subtract mean, then divide each column by s.d.


# 1. HAC + Kmeans ---------------------------------------------------------
dist_mat <- dist(shopping_std, method = 'euclidean')^2 # Distance matrix with squared Euclidean distance
shopping_hclust <- hclust(dist_mat, method = 'ward.D2') # Ward's method

# Dendrogram - Selecting number of clusters
shopping_dend <- as.dendrogram(shopping_hclust) %>% 
  color_branches(k = 6)
plot(shopping_dend, leaflab = 'none', las = 1,
     main = 'Dendrogram of Hierarchical Clustering')

# Centroid from HAC
chosen.k <- 6; lbl <- cutree(shopping_hclust, k = chosen.k)
init_guess <- shopping_std %>% aggregate(by = list('group' = lbl), FUN = mean) %>%
  dplyr::select(-group)

# Kmeans
shopping_kmeans <- kmeans(shopping_std, init_guess)

# Random initialization method with different seeds for HDDC -----------------------
# Warning! This section of code can take a long time to run #
max_iter <- 100
set.seed(1)
seed_mat <- floor(runif(max_iter, 0, 1000)) # Randomly generate some seeds between 0 to 1000
bic_mat <- matrix(rep(0, 6*max_iter), nrow = 6) # Contain BIC for different numbers of cluster and seed

for (i in 1:max_iter){
  # The outer iteration loops through different seeds
  for (k in 1:6){
    # the inner iteration loops through different number of cluster
    set.seed(seed_mat[i])
    shopping_hddc <- hddc(shopping_std, K = k, model = 'ALL',
                          algo = 'SEM', init = 'random', d_select = 'BIC')
    bic_mat[k, i] <- shopping_hddc$BIC # Store BIC
  }
}
bic_df <- as.data.frame(bic_mat)

# 1. HDDC -----------------------------------------------------------------
idx_df <- bic_df %>% apply(MARGIN = 1, FUN = which.max)
seed_chosen <- seed_mat[idx_df]
df_measures <- data.frame()

for (i in 1:6){
  # Rerun the model with the best seed
  set.seed(seed_chosen[i]) # Best seed for each cluster number
  shopping_hddc <- hddc(shopping_std, K = i, model = 'ALL',
                        algo = 'SEM', init = 'random', d_select = 'BIC') 
  # Compare all possible models given the number of cluster
  df_measures <- df_measures %>% rbind(c(shopping_hddc$model, shopping_hddc$K, 
                                         shopping_hddc$BIC, shopping_hddc$ICL, 
                                         seed_chosen[i]))
}
colnames(df_measures) <- c('model', 'k', 'BIC', 'ICL', 'seed')
set.seed(794) # Best seed
shopping_hddc <- hddc(shopping_std, K = 6, model = 'ALL', algo = 'SEM',
                      init = 'random', d_select = 'BIC')


# 1. GMM ------------------------------------------------------------------
shopping_gmm <- Mclust(shopping_std)
summary(shopping_gmm)
plot(shopping_gmm, what = 'BIC')


# 2. Visualization with PC ------------------------------------------------
# Do a PCA
shopping_pca <- princomp(shopping_std)
shopping_main_pc <- as.data.frame(shopping_pca$scores[, 1:2]) 
colnames(shopping_main_pc) <- c('PC1', 'PC2')
summary(shopping_pca)
shopping_pca$loadings[, 1:6]

#
#
# HAC + Kmeans
df_kmeans <- data.frame('group'= shopping_kmeans$cluster, shopping_main_pc)
df_proj_kmeans_center <- 1:6 %>% 
  data.frame(predict(shopping_pca, shopping_kmeans$centers)[, 1:2]) # Projected Kmeans center
colnames(df_proj_kmeans_center) <- c('group', 'PC1', 'PC2')

# PC plot
ggplot(df_kmeans, aes(x = PC1, y = PC2)) + geom_point(aes(color = as.factor(group))) +
  theme_bw() + labs(color = 'Cluster group') + ggtitle('Visualization of HAC + Kmeans results') +
  geom_point(data = df_proj_kmeans_center, aes(colour = as.factor(group)), size = 5)

# Barplot
ggplot(df_kmeans, aes(x = group)) + geom_bar(aes(color = as.factor(group)), fill = NA) + 
  theme_bw() + ggtitle('Cluster sizes')

# 
#
# HDDC
df_hddc <- data.frame('group'= shopping_hddc$class, shopping_main_pc)
shopping_hddc_mu <- shopping_hddc$mu
colnames(shopping_hddc_mu) <- colnames(shopping)
df_proj_hddc_center <- 1:6 %>% 
  data.frame(predict(shopping_pca, shopping_hddc_mu)[, 1:2]) # Projected HDDC center
colnames(df_proj_hddc_center) <- c('group', 'PC1', 'PC2')

# PC plot
ggplot(df_hddc, aes(x = PC1, y = PC2)) + geom_point(aes(color = as.factor(group))) +
  theme_bw() + labs(color = 'Cluster group') + ggtitle('Visualization of HDDC results') +
  geom_point(data = df_proj_hddc_center, aes(colour = as.factor(group)), size = 5)

# Barplot
ggplot(df_hddc, aes(x = group)) + geom_bar(aes(color = as.factor(group)), fill = NA) + 
  theme_bw() + ggtitle('Cluster sizes')

#
#
# GMM
df_gmm <- data.frame('group'= shopping_gmm$classification, shopping_main_pc)
df_proj_gmm_center <- 1:4 %>% 
  data.frame(predict(shopping_pca, t(shopping_gmm$parameters$mean))[, 1:2]) # Projected Kmeans center
colnames(df_proj_gmm_center) <- c('group', 'PC1', 'PC2')

# PC plot
ggplot(df_gmm, aes(x = PC1, y = PC2)) + geom_point(aes(color = as.factor(group))) +
  theme_bw() + labs(color = 'Cluster group') + ggtitle('Visualization of GMM results') +
  geom_point(data = df_proj_gmm_center, aes(colour = as.factor(group)), size = 5)

# Barplot
ggplot(df_gmm, aes(x = group)) + geom_bar(aes(color = as.factor(group)), fill = NA) + 
  theme_bw() + ggtitle('Cluster sizes')

# 3. Stability ------------------------------------------------------------
# Split data
set.seed(989898)
n <- nrow(shopping_std)
idx <- sample(1:n, size = round(0.5*n, 0), replace = FALSE) # 50:50
shopping.train <- shopping_std[idx, ]
shopping.valid <- shopping_std[-idx, ]


# Validation analysis: HAC + Kmeans

# Helper function
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}

# Training data + Predicted labels
dist_mat <- dist(shopping.train, method = 'euclidean')^2 # Distance matrix with squared Euclidean distance
shopping_hclust <- hclust(dist_mat, method = 'ward.D2') # Ward's method
chosen.k <- 6; lbl <- cutree(shopping_hclust, k = chosen.k)
init_guess <- shopping.train %>% aggregate(by = list('group' = lbl), FUN = mean) %>%
  dplyr::select(-group)
shopping_kmeans <- kmeans(shopping.train, init_guess)
pred_kmeans <- clusters(shopping.valid, shopping_kmeans$centers)

# Validation data
dist_mat <- dist(shopping.valid, method = 'euclidean')^2 # Distance matrix with squared Euclidean distance
shopping_hclust <- hclust(dist_mat, method = 'ward.D2') # Ward's method
chosen.k <- 6; lbl <- cutree(shopping_hclust, k = chosen.k)
init_guess <- shopping.valid %>% aggregate(by = list('group' = lbl), FUN = mean) %>%
  dplyr::select(-group)
shopping_kmeans <- kmeans(shopping.valid, init_guess)
out_kmeans <- shopping_kmeans$cluster

# Adjusted Rand Index
table(pred_kmeans, out_kmeans)[c(5, 3, 4, 2, 1, 6),]
adjustedRandIndex(pred_kmeans, out_kmeans) # ARI = 0.7035

# Validation analysis: HDDC, k = 6
# Training data + Predicted labels
set.seed(1997)
seed_mat <- floor(runif(max_iter, 0, 1000)) # Randomly generate some seeds between 0 to 1000
bic_mat <- rep(0, max_iter) # Contain BIC for different seeds given 6 clusters

for (i in 1:max_iter){
  set.seed(seed_mat[i])
  shopping_hddc <- hddc(shopping.train, K = 6, model = 'AJBQD', algo = 'SEM',
                        init = 'random', d_select = 'BIC')
  bic_mat[i] <- shopping_hddc$BIC
}

idx <- which.max(bic_mat) # Get index
set.seed(seed_mat[idx]) # Best seed
shopping_hddc <- hddc(shopping.train, K = 6, model = 'AJBQD', algo = 'SEM',
                      init = 'random', d_select = 'BIC')
pred_hddc <- predict(shopping_hddc, shopping.valid)$class

# Validation data
set.seed(seed_mat[idx]) # Best seed
shopping_hddc <- hddc(shopping.valid, K = 6, model = 'AJBQD', algo = 'SEM',
                      init = 'random', d_select = 'BIC')
out_hddc <- shopping_hddc$class

# Adjusted Rand Index
# Compared to init = 'random', the default setting init = 'kmeans' yields a stabler result
table(pred_hddc, out_hddc)[c(4, 2, 3, 5, 1, 6),]
adjustedRandIndex(pred_hddc, out_hddc) # ARI = 0.1361

# Validation analysis: GMM

# Training data + Predicted labels
shopping_gmm <- Mclust(shopping.train, G = 4, model = 'VVE')
pred_gmm <- predict(shopping_gmm, shopping.valid)$classification

# Validation data
shopping_gmm <- Mclust(shopping.valid, G = 4, model = 'VVE')
out_gmm <- shopping_gmm$classification

# Adjusted Rand Index
table(pred_gmm, out_gmm)[c(2, 4, 3, 1),]
adjustedRandIndex(pred_gmm, out_gmm) # ARI = 0.3834


# Appendix. HDDC with kmeans initialization -------------------------------
df_measures <- data.frame()

for (i in 1:6){
  # Rerun the model with the best seed
  set.seed(780) 
  shopping_hddc <- hddc(shopping_std, K = i, model = 'ALL',
                        algo = 'SEM', init = 'kmeans') 
  # Compare all possible models given the number of cluster
  df_measures <- df_measures %>% rbind(c(shopping_hddc$model, shopping_hddc$K, 
                                         shopping_hddc$BIC, shopping_hddc$ICL))
}
colnames(df_measures) <- c('model', 'k', 'BIC', 'ICL')
set.seed(780)
shopping_hddc <- hddc(shopping_std, K = 6, model = 'ALL', algo = 'SEM',
                      init = 'kmeans')

# Training data + Predicted labels
set.seed(1997)
seed_mat <- floor(runif(max_iter, 0, 1000)) # Randomly generate some seeds between 0 to 1000
bic_mat <- rep(0, max_iter) # Contain BIC for different seeds given 6 clusters

for (i in 1:max_iter){
  set.seed(seed_mat[i])
  shopping_hddc <- hddc(shopping.train, K = 6, model = 'AJBQD', algo = 'SEM',
                        init = 'kmeans')
  bic_mat[i] <- shopping_hddc$BIC
}

idx <- which.max(bic_mat) # Get index
set.seed(seed_mat[idx]) # Best seed
shopping_hddc <- hddc(shopping.train, K = 6, model = 'AJBQD', algo = 'SEM',
                      init = 'kmeans')
pred_hddc <- predict(shopping_hddc, shopping.valid)$class

# Validation data
set.seed(seed_mat[idx]) # Best seed
shopping_hddc <- hddc(shopping.valid, K = 6, model = 'AJBQD', algo = 'SEM',
                      init = 'kmeans')
out_hddc <- shopping_hddc$class

# Adjusted Rand Index
# Compared to init = 'random', the default setting init = 'kmeans' yields a stabler result
table(pred_hddc, out_hddc)[c(1, 4, 2, 5, 6, 3),]
adjustedRandIndex(pred_hddc, out_hddc) # ARI = 0.5815

