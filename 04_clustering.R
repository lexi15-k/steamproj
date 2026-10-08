# =============================================================================
# COMP3020 Group Project - 04 Clustering
# RQ3: What TYPES of complaint appear in negative reviews, and do different
#      games fail in the same way?
# Method: (a) k-means on TF-IDF vectors of negative reviews (complaint types),
#             k chosen with the elbow method + average silhouette width;
#         (b) hierarchical clustering of the 24 games by their complaint profile.
# Input : data/dtm.rds (from 03_textmining.R)
# Output: data/review_clusters.rds, data/game_profiles.rds, figures/fig_cluster_*.png
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tm); library(cluster); library(SnowballC)
})
set.seed(3020)

d    <- readRDS("data/dtm.rds")
neg  <- d$meta$recommended == "Negative"
dtm  <- d$dtm[neg, ]
meta <- d$meta[neg, ]

# ---- 1. TF-IDF matrix of negative reviews ------------------------------------------
# Keep terms used in >= 1% of negative reviews, weight by TF-IDF, then scale each
# review vector to length 1 so that k-means (Euclidean) groups reviews by cosine
# similarity - long reviews do not dominate.
dtm <- removeSparseTerms(dtm, 0.99)
keep <- slam::row_sums(dtm) > 0
dtm <- dtm[keep, ]; meta <- meta[keep, ]
X <- as.matrix(weightTfIdf(dtm, normalize = TRUE))
X <- X / sqrt(rowSums(X^2))
X[is.na(X)] <- 0
cat("Negative reviews clustered:", nrow(X), " terms:", ncol(X), "\n")

# ---- 2. Choosing k ------------------------------------------------------------------
# (slow: ~14 min. Cached in data/k_selection.rds; delete that file to recompute)
ks <- 2:14
if (file.exists("data/k_selection.rds")) sel <- readRDS("data/k_selection.rds") else {
sil_idx <- sample(nrow(X), 3000)                          # silhouette on a random sample
D_sil   <- dist(X[sil_idx, ])
sel <- do.call(rbind, lapply(ks, function(k) {
  km <- kmeans(X, centers = k, nstart = 5, iter.max = 50)
  s  <- mean(silhouette(km$cluster[sil_idx], D_sil)[, 3])
  data.frame(k = k, within_ss = km$tot.withinss, silhouette = s)
}))
saveRDS(sel, "data/k_selection.rds")
}
print(sel, digits = 3)

p_sel <- ggplot(tidyr::pivot_longer(sel, -k), aes(k, value)) +
  geom_line() + geom_point() +
  facet_wrap(~ name, scales = "free_y",
             labeller = as_labeller(c(silhouette = "Average silhouette width (higher = better)",
                                      within_ss = "Total within-cluster SS (look for the elbow)"))) +
  scale_x_continuous(breaks = ks) +
  labs(title = "Choosing the number of complaint clusters", x = "k", y = NULL) +
  theme_minimal(base_size = 10)
ggsave("figures/fig_cluster_choose_k.png", p_sel, width = 9, height = 3.8, dpi = 200)

# ---- 3. Final k-means ---------------------------------------------------------------
# K is fixed after inspecting the elbow/silhouette plot AND the interpretability of
# the clusters (a cluster must have a clear, nameable theme). See the report.
# Stability (stability_check.R): with k = 8, seven themes appeared in every run but
# the 8th flipped between "always-online/servers" and "space/exploration" with
# near-identical within-SS (18132.5 vs 18133.5). k = 9 holds both. Two k = 9 runs
# agreed with ARI = 0.67; we keep the run with the lower within-SS (seed 9001).
K  <- 9
set.seed(9001)
km <- kmeans(X, centers = K, nstart = 10, iter.max = 100)

# Top terms per cluster = largest centroid weights
top_terms <- lapply(1:K, function(k) names(sort(km$centers[k, ], decreasing = TRUE))[1:12])
sizes <- as.vector(table(km$cluster))
for (k in 1:K) cat(sprintf("Cluster %d (n = %d): %s\n", k, sizes[k], paste(top_terms[[k]], collapse = ", ")))

saveRDS(list(km = km, meta = meta, top_terms = top_terms), "data/review_clusters_raw.rds")
cat("\nNow name the clusters in 04b (labels depend on the run) - see cluster_labels below.\n")
