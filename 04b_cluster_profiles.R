# =============================================================================
# COMP3020 Group Project - 04b Complaint-cluster labels and game profiles
# Uses the k-means result saved by 04_clustering.R.
# (a) names each cluster from its top terms (rule-based, so labels survive re-runs)
# (b) builds each game's complaint profile = share of its negative reviews per cluster
# (c) hierarchical clustering of games by profile: which games "fail the same way"?
# Output: data/review_clusters.rds, data/game_profiles.rds,
#         figures/fig_cluster_terms.png, fig_cluster_heatmap.png, fig_cluster_dendrogram.png
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(tidyr) })

r <- readRDS("data/review_clusters_raw.rds")
km <- r$km; meta <- r$meta; top_terms <- r$top_terms
K <- length(top_terms)

# ---- 1. Name clusters from their top 6 stemmed terms -------------------------------
rules <- list(                                   # first matching rule wins
  "Cheaters & bots"            = c("cheater", "bot", "cheat", "anticheat", "hacker"),
  "Crashes & stability"        = c("crash", "driver", "constant", "minut"),
  "PC performance"             = c("fps", "perform", "optim", "stutter", "frame"),
  "Always-online & servers"    = c("server", "onlin", "offlin", "matchmak", "connect"),
  "Monetisation & live-service" = c("skin", "pass", "battl", "hero", "pve", "money", "microtransact", "monet", "price", "promis"),
  "Bugs, patches & dev decisions" = c("bug", "dev", "updat", "nerf", "develop"),
  "Space & exploration"        = c("ship", "planet", "space", "explor", "travel"),
  "Story, content & gameplay"  = c("stori", "quest", "world", "charact", "combat"),
  "Not worth it (general)"     = c("buy", "bad", "refund", "worth", "bore")
)
label_cluster <- function(terms) {
  t6 <- terms[1:6]
  hits <- sapply(rules, function(r) sum(t6 %in% r))   # best-matching theme, >= 2 terms
  if (max(hits) >= 2) return(names(rules)[which.max(hits)])
  paste(t6[1:3], collapse = "/")                         # unnamed: show its top terms
}
labs_k <- sapply(top_terms, label_cluster)
labs_k <- make.unique(labs_k, sep = " ")
cat("Cluster labels:\n"); for (k in 1:K)
  cat(sprintf("  %d %-32s %s\n", k, labs_k[k], paste(top_terms[[k]][1:8], collapse = ", ")))

meta$cluster <- factor(labs_k[km$cluster], levels = labs_k[order(-tabulate(km$cluster))])
saveRDS(meta, "data/review_clusters.rds")

# ---- 2. Top-term plot per cluster ---------------------------------------------------
tt <- do.call(rbind, lapply(1:K, function(k) {
  w <- sort(km$centers[k, ], decreasing = TRUE)[1:10]
  data.frame(cluster = labs_k[k], term = names(w), weight = w,
             n = sum(km$cluster == k))
})) %>% mutate(cluster = sprintf("%s (n = %d)", cluster, n))
p_terms <- ggplot(tt, aes(reorder(paste(term, cluster, sep = "___"), weight), weight)) +
  geom_col(fill = "#d7301f") + coord_flip() +
  facet_wrap(~ cluster, scales = "free", ncol = 4) +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  labs(title = sprintf("Complaint clusters in negative reviews (k-means, k = %d, TF-IDF)", K),
       subtitle = "Top stemmed terms by centroid weight", x = NULL, y = "Centroid TF-IDF weight") +
  theme_minimal(base_size = 8) + theme(axis.text.x = element_blank())
ggsave("figures/fig_cluster_terms.png", p_terms, width = 11, height = 6, dpi = 200)

# ---- 3. Game x cluster profile (heatmap) --------------------------------------------
prof <- meta %>% count(game, cluster) %>%
  group_by(game) %>% mutate(share = n / sum(n)) %>% ungroup()
P <- prof %>% select(game, cluster, share) %>%
  pivot_wider(names_from = cluster, values_from = share, values_fill = 0)
Pm <- as.matrix(P[, -1]); rownames(Pm) <- P$game
saveRDS(Pm, "data/game_profiles.rds")

# ---- 4. Hierarchical clustering of games by complaint profile ----------------------
# Distance = Euclidean between profiles (shares sum to 1); Ward's linkage gives
# compact, similar-sized groups. The "general" cluster is excluded because it is a
# catch-all that every game has - it would hide the specific differences.
spec <- Pm[, !grepl("general", colnames(Pm)), drop = FALSE]
spec <- spec / rowSums(spec)
hc <- hclust(dist(spec), method = "ward.D2")
groups <- cutree(hc, k = 4)
cat("\nGame groups by complaint profile (Ward, 4 groups):\n")
for (g in 1:4) cat(sprintf("  %d: %s\n", g, paste(names(groups)[groups == g], collapse = ", ")))
saveRDS(groups, "data/game_complaint_groups.rds")

png("figures/fig_cluster_dendrogram.png", width = 1800, height = 1100, res = 200)
par(mar = c(2, 4, 3, 1))
plot(hc, main = "Games grouped by what their negative reviews complain about",
     sub = "", xlab = "", ylab = "Ward distance", cex = 0.75)
rect.hclust(hc, k = 4, border = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"))
dev.off()

prof$game <- factor(prof$game, levels = hc$labels[hc$order])
p_heat <- ggplot(prof, aes(cluster, game, fill = share)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(share >= 0.15, scales::percent(share, 1), "")), size = 2.3) +
  scale_fill_gradient(low = "white", high = "#b30000", labels = scales::percent) +
  labs(title = "What each game's negative reviews are about",
       subtitle = "Share of the game's negative reviews in each complaint cluster (rows ordered by dendrogram)",
       x = NULL, y = NULL, fill = "Share") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), panel.grid = element_blank())
ggsave("figures/fig_cluster_heatmap.png", p_heat, width = 9, height = 8, dpi = 200)
ggsave("figures/poster_cluster_heatmap.png",
       p_heat + labs(title = NULL, subtitle = NULL) + theme(axis.text = element_text(size = 11), legend.position = "none"),
       width = 7.2, height = 7.6, dpi = 250)

# Which game is most associated with each cluster?
cat("\nGame with the highest share in each cluster:\n")
print(prof %>% group_by(cluster) %>% slice_max(share, n = 2) %>%
        mutate(share = round(share, 2)) %>% as.data.frame())
cat("Saved data/review_clusters.rds, data/game_profiles.rds and cluster figures\n")
