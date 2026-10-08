# Stability check for k-means: several seeds for k = 8, 9, 10; compare total within-SS
# and how often each complaint theme appears. Output: data/kmeans_stability.rds
suppressPackageStartupMessages({ library(tm) })
d <- readRDS("data/dtm.rds"); neg <- d$meta$recommended == "Negative"
dtm <- removeSparseTerms(d$dtm[neg, ], 0.99); dtm <- dtm[slam::row_sums(dtm) > 0, ]
X <- as.matrix(weightTfIdf(dtm, normalize = TRUE)); X <- X / sqrt(rowSums(X^2)); X[is.na(X)] <- 0
runs <- list()
for (k in 9) for (s in 1:2) {
  set.seed(1000 * k + s)
  km <- kmeans(X, centers = k, nstart = 10, iter.max = 100)
  tops <- apply(km$centers, 1, function(v) paste(names(sort(v, TRUE))[1:5], collapse = " "))
  runs[[length(runs) + 1]] <- list(k = k, seed = 1000 * k + s, wss = km$tot.withinss, tops = tops, cluster = km$cluster)
  cat(sprintf("\nk=%d seed=%d WSS=%.1f\n", k, 1000 * k + s, km$tot.withinss)); cat(paste(" ", tops), sep = "\n")
}
saveRDS(runs, "data/kmeans_stability_k9.rds")
