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


# 1. HDDC -----------------------------------------------------------------
df_measures <- data.frame() 
for (i in 1:6){
  # Compare all number of cluster
  set.seed(989898)
  shopping_hddc <- hddc(shopping_std, K = i, model = 'ALL',
                        algo = 'SEM') # Compare all possible models given the number of cluster
  df_measures <- df_measures %>% rbind(c(shopping_hddc$model, shopping_hddc$K, 
                                         shopping_hddc$BIC, shopping_hddc$ICL))
}
colnames(df_measures) <- c('model', 'k', 'BIC', 'ICL')

shopping_hddc <- hddc(shopping_std, K = 6, model = 'AJBQD', algo = 'SEM')


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
adjustedRandIndex(pred_kmeans, out_kmeans)

# Validation analysis: HDDC

# Training data + Predicted labels
shopping_hddc <- hddc(shopping.train, K = 6, model = 'AJBQD', algo = 'SEM')
pred_hddc <- predict(shopping_hddc, shopping.valid)$class

# Validation data
shopping_hddc <- hddc(shopping.valid, K = 6, model = 'AJBQD', algo = 'SEM')
out_hddc <- shopping_hddc$class

# Adjusted Rand Index
table(pred_hddc, out_hddc)[c(5, 3, 1, 2, 4, 6),]
adjustedRandIndex(pred_hddc, out_hddc)

# Validation analysis: GMM

# Training data + Predicted labels
shopping_gmm <- Mclust(shopping.train, G = 4, model = 'VVE')
pred_gmm <- predict(shopping_gmm, shopping.valid)$classification

# Validation data
shopping_gmm <- Mclust(shopping.valid, G = 4, model = 'VVE')
out_gmm <- shopping_gmm$classification

# Adjusted Rand Index
table(pred_gmm, out_gmm)[c(2, 4, 3, 1),]
adjustedRandIndex(pred_gmm, out_gmm)
