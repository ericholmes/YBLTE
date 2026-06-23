# -----------------------------
# Load packages
# -----------------------------
library(dtw)
library(pheatmap)
library(dtwclust)

# -----------------------------
# 1. Prepare trajectories
# -----------------------------
# Ensure data is ordered by time
pc_score <- pc_score[order(pc_score$Sitefac, pc_score$week), ]

# Extract unique sites
sites <- unique(pc_score$Sitefac)

# Build a list of PC1 trajectories per site
traj <- lapply(sites, function(s) {
  subset(pc_score, Sitefac == s)$PC1
})
names(traj) <- as.character(sites)

# -----------------------------
# 2. Compute DTW distance matrix
# -----------------------------
n <- length(traj)
dtw_mat <- matrix(0, n, n)
rownames(dtw_mat) <- colnames(dtw_mat) <- names(traj)

for(i in 1:n){
  for(j in 1:n){
    dtw_mat[i, j] <- dtw(traj[[i]], traj[[j]])$normalizedDistance
  }
}

# Build a list of PC1 trajectories per site
traj2 <- lapply(sites, function(s) {
  subset(pc_score, Sitefac == s)$PC2
})
names(traj2) <- as.character(sites)

# -----------------------------
# 2. Compute DTW distance matrix
# -----------------------------
n <- length(traj2)
dtw_mat2 <- matrix(0, n, n)
rownames(dtw_mat2) <- colnames(dtw_mat2) <- names(traj2)

for(i in 1:n){
  for(j in 1:n){
    dtw_mat2[i, j] <- dtw(traj2[[i]], traj2[[j]])$normalizedDistance
  }
}


# -----------------------------
# 3. Heatmap of DTW distances
# -----------------------------

png("Output/Figures/YBLTE_Point_wq_DTW_clust%02d.png",
    height = 6, width = 7, units = "in", res = 1000, family = "serif")

pheatmap(
  dtw_mat,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  main = "DTW Distance Between Site Trajectories (PC1)"
)

pheatmap(
  dtw_mat2,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  main = "DTW Distance Between Site Trajectories (PC2)"
)
dev.off()
# -----------------------------
# 4. Hierarchical clustering
# -----------------------------
hc <- hclust(as.dist(dtw_mat), method = "average")
plot(hc, main = "DTW-Based Clustering of Sites")

# -----------------------------
# 5. Pairwise DTW alignment plot
# -----------------------------
# Choose two sites to compare
siteA <- names(traj)[1]
siteB <- names(traj)[12]

alignment <- dtw(traj[[siteA]], traj[[siteB]], keep = TRUE)

plot(
  alignment,
  type = "twoway",
  main = paste("DTW Alignment:", siteA, "vs", siteB)
)

# -----------------------------
# 6. Optional: Plot trajectories colored by DTW clusters
# -----------------------------
clusters <- cutree(hc, k = 3)
cluster_df <- data.frame(Sitefac = names(clusters), cluster = clusters)

pc_score2 <- merge(pc_score, cluster_df, by = "Sitefac")
pc_score2$clustfac <- factor(pc_score2$cluster)
png("Output/Figures/YBLTE_Point_wq_DTW_clust_ts%02d.png",
    height = 6, width = 7, units = "in", res = 1000, family = "serif")
ggplot(pc_score2, aes(x = week, y = PC1, color = Sitefac, linetype = clustfac)) +
  geom_line(linewidth = 1) +
  theme_minimal() + animCol +
  labs(
    title = "PC1 Trajectories Colored by DTW Clusters",
    color = "Cluster",
    linetype = "Cluster"
  )


ggplot(pc_score2, aes(x = week, y = PC2, color = Sitefac, linetype = factor(cluster))) +
  geom_line(size = 1) +
  theme_minimal() + animCol +
  labs(
    title = "PC2 Trajectories Colored by DTW Clusters",
    color = "Cluster"
  )
dev.off()

# Multivariate DTW --------------------------------------------------------

sites <- unique(pc_score$Sitefac)

traj_multi <- lapply(sites, function(s) {
  df <- subset(pc_score, Sitefac == s)
  df <- df[order(df$week), ]
  as.matrix(df[, c("PC1", "PC2")])   # multivariate trajectory
})
names(traj_multi) <- as.character(sites)

n <- length(traj_multi)
dtw_multi <- matrix(0, n, n)
rownames(dtw_multi) <- colnames(dtw_multi) <- names(traj_multi)

for(i in 1:n){
  for(j in 1:n){
    dtw_multi[i, j] <- dtw_basic(
      x = traj_multi[[i]],
      y = traj_multi[[j]],
      dist_method = "Euclidean"   # distance between PC1+PC2 vectors
    )
  }
}

png("Output/Figures/YBLTE_Point_wq_DTW_clust_mv%02d.png",
    height = 6, width = 7, units = "in", res = 1000, family = "serif")
pheatmap(
  dtw_multi,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  main = "Multivariate DTW Distance (PC1 + PC2)"
)
dev.off()
par(mfrow = c(1, 3))

pheatmap(dtw_mat, main = "DTW PC1")
pheatmap(dtw_mat2, main = "DTW PC2")
pheatmap(dtw_multi, main = "Multivariate DTW (PC1+PC2)")

