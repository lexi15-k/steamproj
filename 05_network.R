# =============================================================================
# COMP3020 Group Project - 05 Network analysis
# RQ4: Do the same players review several of these games, and does the network
#      of shared reviewers split into communities (e.g. single-player RPG fans vs
#      live-service/multiplayer players)? Are games linked by shared *negative*
#      reviewers the same as games linked by shared *positive* reviewers?
# Method: bipartite user-game graph -> one-mode game-game projection
#         (edge weight = number of shared reviewers); degree, strength,
#         betweenness, PageRank; Louvain community detection; comparison with the
#         complaint-profile similarity from 04 (permutation / QAP test).
# Input : data/reviews_clean.rds  (+ data/game_profiles.rds from 04, optional)
# Output: figures/fig_network_*.png, data/network_centrality.csv
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(igraph); library(ggplot2) })
set.seed(3020)

rev <- readRDS("data/reviews_clean.rds")

# ---- 1. Bipartite user-game graph ------------------------------------------------
multi <- rev %>% group_by(user) %>% filter(n_distinct(game) >= 2) %>% ungroup()
cat("Users who reviewed >= 2 of the 24 games:", n_distinct(multi$user),
    " (", nrow(multi), "reviews )\n")
print(table(games_per_user = table(multi$user)))

edges_ug <- multi %>% select(user, game)
B <- graph_from_data_frame(edges_ug, directed = FALSE)
V(B)$type <- V(B)$name %in% unique(rev$game)              # TRUE = game, FALSE = user
cat(sprintf("Bipartite graph: %d users, %d games, %d edges, density %.4f\n",
            sum(!V(B)$type), sum(V(B)$type), ecount(B),
            ecount(B) / (sum(!V(B)$type) * sum(V(B)$type))))

# ---- 2. Game-game projection --------------------------------------------------------
# Built from the incidence matrix: W = t(A) %*% A, where A is users x games.
project_games <- function(df) {
  A <- table(df$user, df$game) > 0
  A <- A[rowSums(A) >= 2, , drop = FALSE]
  W <- crossprod(A * 1)
  diag(W) <- 0
  graph_from_adjacency_matrix(W, mode = "undirected", weighted = TRUE)
}
all_games <- sort(unique(rev$game))
G <- project_games(multi)
G <- add_vertices(G, sum(!all_games %in% V(G)$name), name = setdiff(all_games, V(G)$name))

cat(sprintf("\nGame-game graph: %d games, %d edges (of %d possible), density %.2f\n",
            vcount(G), ecount(G), choose(vcount(G), 2), edge_density(G)))
print(summary(E(G)$weight))

# ---- 3. Centrality -------------------------------------------------------------------
# With a near-complete graph, unweighted degree says little, so we use weighted
# versions: strength (total shared reviewers), weighted betweenness (distance =
# 1/weight, so strong ties are "short"), and weighted PageRank.
games_meta <- rev %>% group_by(game) %>%
  summarise(neg_share = mean(recommended == "Negative"), .groups = "drop")
cent <- data.frame(
  game        = V(G)$name,
  degree      = degree(G),
  strength    = strength(G),
  betweenness = betweenness(G, weights = 1 / E(G)$weight, normalized = TRUE),
  pagerank    = page_rank(G, weights = E(G)$weight)$vector
) %>% left_join(games_meta, by = "game") %>% arrange(desc(strength))
print(cent, digits = 3)
write.csv(cent, "data/network_centrality.csv", row.names = FALSE)

# ---- 4. Community detection on a popularity-corrected network --------------------
# Raw shared-reviewer counts are dominated by popular games (Cyberpunk, BG3 share
# reviewers with everything). We therefore weight each edge by its LIFT:
#   lift_ij = observed shared reviewers / expected if users picked games at random
#           = w_ij * N / (n_i * n_j)
# (N = multi-game users, n_i = multi-game users who reviewed game i).
# lift > 1 means the two games share MORE players than their popularity implies.
# All edges are kept for community detection (min_lift = 0). Keeping only lift > 1
# edges makes ANY graph - even a random one - sparse and highly "modular", so the
# null-model test below had no power on the thresholded graph (Q = 0.46 vs 0.46).
# Thresholds are used for the plots only.
lift_graph <- function(df, min_lift = 0) {
  A <- table(df$user, df$game) > 0
  A <- A[rowSums(A) >= 2, , drop = FALSE] * 1
  W <- crossprod(A); diag(W) <- 0
  n <- colSums(A); N <- nrow(A)
  L <- W * N / (n %o% n)
  L[L <= min_lift] <- 0
  graph_from_adjacency_matrix(L, mode = "undirected", weighted = TRUE)
}
GL <- lift_graph(multi)
cat(sprintf("
Lift network: %d edges, %d with lift > 1 (median lift %.2f, max %.2f)
",
            ecount(GL), sum(E(GL)$weight > 1), median(E(GL)$weight), max(E(GL)$weight)))

comm <- cluster_louvain(GL, weights = E(GL)$weight, resolution = 1)
V(GL)$community <- membership(comm)
V(G)$community  <- membership(comm)[match(V(G)$name, V(GL)$name)]
Q_obs <- modularity(comm)
cat(sprintf("Louvain on lift network: %d communities, modularity Q = %.3f
", length(comm), Q_obs))
for (c in sort(unique(membership(comm))))
  cat(sprintf("  Community %d: %s
", c, paste(V(GL)$name[membership(comm) == c], collapse = ", ")))

# Null model: shuffle which games the multi-game users reviewed, keeping how many
# games each user reviewed and how often each game was reviewed (bipartite
# configuration model), then repeat the whole lift + Louvain pipeline.
shuffle_bipartite <- function(df) {
  df$game <- sample(df$game)
  df[!duplicated(df[, c("user", "game")]), ]   # drop the rare repeated user-game pairs (~1%)
}
null_Q <- replicate(200, {
  g0 <- lift_graph(shuffle_bipartite(edges_ug))
  modularity(cluster_louvain(g0, weights = E(g0)$weight))
})
cat(sprintf("Null model (200 bipartite shuffles): mean Q %.3f, 95th pct %.3f; p = %.3f -> %s
",
            mean(null_Q), quantile(null_Q, .95), mean(null_Q >= Q_obs),
            ifelse(Q_obs > quantile(null_Q, .95), "community structure is real",
                   "NOT above chance")))

saveRDS(data.frame(game = V(GL)$name, community = membership(comm)),
        "data/network_communities.rds")

# ---- 5. Plot (edges thresholded for readability only) ----------------------------
plot_net <- function(g, file, title, min_w, lay_fun = layout_with_fr, lab_cex = 0.62, res = 220) {
  gp <- delete_edges(g, E(g)[weight < min_w])
  set.seed(3020)
  lay <- if (identical(lay_fun, layout_with_fr)) lay_fun(gp, weights = E(gp)$weight) else lay_fun(gp)
  pal <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")
  png(file, width = 2000, height = 1700, res = res)
  par(mar = c(1, 1, 3, 1))
  plot(gp, layout = lay,
       vertex.size = 6 + 14 * sqrt(strength(G)[match(V(g)$name, V(G)$name)] / max(strength(G))),
       vertex.color = pal[V(g)$community],
       vertex.frame.color = ifelse(games_meta$neg_share[match(V(g)$name, games_meta$game)] > 0.5,
                                   "#d7301f", "grey40"),
       vertex.label.cex = lab_cex, vertex.label.dist = 1.3, vertex.label.degree = -pi / 2, vertex.label.color = "black", vertex.label.family = "sans",
       edge.width = 0.4 + 5 * E(gp)$weight / max(E(gp)$weight),
       edge.color = adjustcolor("grey50", 0.5),
       main = title)
  legend("bottomleft", bty = "n", cex = 0.75,
         legend = c(paste("Community", sort(unique(V(g)$community))),
                    "Red border = >50% negative in sample", "Node size = shared reviewers (strength)"),
         pt.bg = c(pal[sort(unique(V(g)$community))], "white", NA),
         col = c(rep("grey40", length(unique(V(g)$community))), "#d7301f", NA),
         pch = 21, pt.cex = 1.5)
  dev.off()
}
plot_net(GL, "figures/fig_network_games.png",
         "Games that share more reviewers than popularity predicts (lift >= 1.3 shown)", 1.3)
plot_net(GL, "figures/poster_network_games.png", "", 1.3, lab_cex = 0.95, res = 250)

# ---- 6. Shared NEGATIVE vs shared POSITIVE reviewers ---------------------------------
G_neg <- lift_graph(multi %>% filter(recommended == "Negative"))
G_pos <- lift_graph(multi %>% filter(recommended == "Positive"))
for (nm in c("G_neg", "G_pos")) {
  g <- get(nm)
  cm <- cluster_louvain(g, weights = E(g)$weight)
  cat(sprintf("\n%s: %d games with edges, %d edges, Louvain Q = %.3f\n",
              nm, vcount(g), ecount(g), modularity(cm)))
  for (c in sort(unique(membership(cm))))
    cat(sprintf("  %d: %s\n", c, paste(V(g)$name[membership(cm) == c], collapse = ", ")))
  V(g)$community <- membership(cm)
  assign(nm, g)
}
plot_net(G_neg, "figures/fig_network_negative.png",
         "Games linked by reviewers who were NEGATIVE about both (lift >= 1.5 shown)", 1.5,
         lay_fun = function(g) layout_with_kk(g, weights = NA) * 1.3)

# User consistency: do multi-game reviewers like or dislike everything?
cons <- multi %>% group_by(user) %>%
  summarise(n = n(), share_neg = mean(recommended == "Negative"), .groups = "drop")
cat("\nMulti-game reviewers: all positive", round(mean(cons$share_neg == 0), 3),
    "| all negative", round(mean(cons$share_neg == 1), 3),
    "| mixed", round(mean(cons$share_neg > 0 & cons$share_neg < 1), 3), "\n")

# ---- 7. Link to clustering: do connected games complain about the same things? -----
# QAP-style permutation test: correlation between shared-reviewer weights and
# cosine similarity of the games' complaint-cluster profiles (from 04).
if (file.exists("data/game_profiles.rds")) {
  P <- readRDS("data/game_profiles.rds")                 # games x clusters (shares)
  gs <- intersect(rownames(P), V(GL)$name)
  P <- P[gs, ]
  S <- (P %*% t(P)) / (sqrt(rowSums(P^2)) %o% sqrt(rowSums(P^2)))
  W <- as_adjacency_matrix(GL, attr = "weight", sparse = FALSE)[gs, gs]
  ut <- upper.tri(W)
  obs <- cor(W[ut], S[ut], method = "spearman")
  perm <- replicate(2000, { p <- sample(length(gs)); cor(W[p, p][ut], S[ut], method = "spearman") })
  cat(sprintf("\nShared reviewers vs complaint similarity: Spearman rho = %.3f, permutation p = %.4f\n",
              obs, mean(abs(perm) >= abs(obs))))
}
cat("Saved figures/fig_network_games.png, fig_network_negative.png, data/network_centrality.csv\n")
