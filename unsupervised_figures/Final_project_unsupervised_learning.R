load("MyData.RData")

# using pca
set.seed(123) 
sample_size <- floor(nrow(MyTrain_pca))
sample_indices <- sample(1:nrow(MyTrain_pca), sample_size)
train_sample <- MyTrain_pca[sample_indices, ]

wss <- sapply(1:10, function(k) {
  kmeans(train_sample[, -1], centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares")

hc <- hclust(dist(train_sample[, -1]))
plot(hc)
set.seed(123)
km_result <- kmeans(train_sample[, -1], centers = 8)
h_clusters <- cutree(hc, 8)

kmeans_scores <- aggregate(train_sample[, 1], by = list(Cluster = km_result$cluster), FUN = mean)

hclust_scores <- aggregate(train_sample[, 1], by = list(Cluster = h_clusters), FUN = mean)

#using random forest

set.seed(123) 
sample_size_rf <- floor(nrow(MyTrain_RF))
sample_indices_rf <- sample(1:nrow(MyTrain_RF), sample_size_rf)
train_sample_rf <- MyTrain_RF[sample_indices_rf, ]

wss <- sapply(1:10, function(k) {
  kmeans(train_sample_rf[, -1], centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares")

hc_rf <- hclust(dist(train_sample_rf[, -1]))
plot(hc_rf)
#plot(hc_rf, main="Cluster Dendrogram", xlab="", sub="", ylim=c(0, 10000))
set.seed(123)
km_result_rf <- kmeans(train_sample_rf[, -1], centers = 7)
h_clusters_rf <- cutree(hc_rf, 7)
kmeans_scores_rf <- aggregate(train_sample_rf[, 1], by = list(Cluster = km_result_rf$cluster), FUN = mean)

hclust_scores_rf <- aggregate(train_sample_rf[, 1], by = list(Cluster = h_clusters_rf), FUN = mean)

#using umap

set.seed(123) 
sample_size_u <- floor(nrow(MyTrain_umap))
sample_indices_u <- sample(1:nrow(MyTrain_umap), sample_size_u)
train_sample_u <- MyTrain_umap[sample_indices_u, ]

wss <- sapply(1:10, function(k) {
  kmeans(train_sample_u[, -1], centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within Sum of Squares")

hc_u <- hclust(dist(train_sample_u[, -1]))
plot(hc_u)
set.seed(123)
km_result_u <- kmeans(train_sample_u[, -1], centers = 5)
h_clusters_u <- cutree(hc_u, 5)

kmeans_scores_u <- aggregate(train_sample_u[, 1], by = list(Cluster = km_result_u$cluster), FUN = mean)

hclust_scores_u <- aggregate(train_sample_u[, 1], by = list(Cluster = h_clusters_u), FUN = mean)

# cluster stability

#pca
set.seed(123)
km_result1 <- kmeans(train_sample[, -1], centers = 7)

set.seed(456)
km_result2 <- kmeans(train_sample[, -1], centers = 7)

# Compare the clustering results
table(km_result1$cluster, km_result2$cluster)

#rf
set.seed(123)
km_result3 <- kmeans(train_sample_rf[, -1], centers = 6)

set.seed(456)
km_result4 <- kmeans(train_sample_rf[, -1], centers = 6)

# Compare the clustering results
table(km_result3$cluster, km_result4$cluster)

#umap
set.seed(123)
km_result4 <- kmeans(train_sample_u[, -1], centers = 5)

set.seed(456)
km_result5 <- kmeans(train_sample_u[, -1], centers = 5)

# Compare the clustering results
table(km_result4$cluster, km_result5$cluster)
