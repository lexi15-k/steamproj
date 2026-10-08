# =============================================================================
# COMP3020 Group Project - 06 Statistical tests
# RQ5: How do negative reviewers differ from positive ones, and are complaint
#      types linked to the player communities found in the network?
#
# Sampling note: 75% of our reviews were selected as "most helpful", which is not
# a random sample (Section 4 showed it over-represents negative reviews). Tests on
# reviewer behaviour therefore use the RECENT sample (newest 500 reviews per game),
# which is not selected on helpfulness; results on the full sample are shown as a
# robustness check.
# Input : data/reviews_clean.rds, data/review_clusters.rds, data/network_communities.rds
# Output: data/stats_results.csv, figures/fig_stats_playtime.png
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

rev  <- readRDS("data/reviews_clean.rds")
recent <- rev %>% filter(source == "recent")
results <- list()
add <- function(test, sample, stat, p, effect) {
  results[[length(results) + 1]] <<- data.frame(test, sample, statistic = stat, p_value = p, effect)
}

# ---- H1: Negative reviewers have played less when they write their review --------
# Playtime is extremely right-skewed (a few players have thousands of hours), so we
# use the Wilcoxon rank-sum test (no normality assumption) rather than a t-test.
for (nm in c("recent", "all")) {
  d <- if (nm == "recent") recent else rev
  w <- wilcox.test(playtime_h ~ recommended, data = d)
  med <- tapply(d$playtime_h, d$recommended, median, na.rm = TRUE)
  add("H1 playtime at review: Positive vs Negative (Wilcoxon)", nm, w$statistic, w$p.value,
      sprintf("median %.1f h vs %.1f h", med["Positive"], med["Negative"]))
}
# Per game, with Holm correction for 24 tests
per_game <- recent %>% group_by(game) %>%
  summarise(med_pos = median(playtime_h[recommended == "Positive"]),
            med_neg = median(playtime_h[recommended == "Negative"]),
            n_neg = sum(recommended == "Negative"),
            p = if (n_neg >= 10 && n_neg < n() - 10) wilcox.test(playtime_h ~ recommended)$p.value else NA,
            .groups = "drop") %>%
  mutate(p_holm = p.adjust(p, "holm"))
cat("H1 per game (recent sample): negative reviewers played less in",
    sum(per_game$med_neg < per_game$med_pos, na.rm = TRUE), "of", sum(!is.na(per_game$p)),
    "testable games; significant after Holm correction in",
    sum(per_game$p_holm < 0.05 & per_game$med_neg < per_game$med_pos, na.rm = TRUE), "\n")
print(as.data.frame(per_game), digits = 3)

p_play <- ggplot(recent, aes(recommended, playtime_h + 1, fill = recommended)) +
  geom_boxplot(outlier.alpha = 0.1) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c(Positive = "#2c7fb8", Negative = "#d7301f"), guide = "none") +
  labs(title = "Hours played when the review was written",
       subtitle = "Recent-review sample; log scale", x = NULL, y = "Hours played + 1 (log scale)") +
  theme_minimal(base_size = 10)
ggsave("figures/fig_stats_playtime.png", p_play, width = 5, height = 4.5, dpi = 200)

# ---- H2: Negative reviews are longer ------------------------------------------------
for (nm in c("recent", "all")) {
  d <- if (nm == "recent") recent else rev
  w <- wilcox.test(n_words ~ recommended, data = d)
  med <- tapply(d$n_words, d$recommended, median)
  add("H2 review length (words): Positive vs Negative (Wilcoxon)", nm, w$statistic, w$p.value,
      sprintf("median %d vs %d words", as.integer(med["Positive"]), as.integer(med["Negative"])))
}

# ---- H3: Negative reviews are more often voted helpful ----------------------------
# Recent sample only (the helpful sample is selected ON this variable).
tab3 <- table(recent$recommended, recent$votes_up >= 1)
c3 <- chisq.test(tab3)
pr <- prop.table(tab3, 1)[, "TRUE"]
add("H3 has >= 1 helpful vote x recommendation (chi-squared)", "recent", c3$statistic, c3$p.value,
    sprintf("%.1f%% of positive vs %.1f%% of negative", 100 * pr["Positive"], 100 * pr["Negative"]))

# ---- H4: Do players who got the game free review differently? ---------------------
# Free copies are concentrated in a few games (Fallout 76, Diablo IV, Redfall), so a
# plain 2x2 test would mix up "free" with "which game". The Cochran-Mantel-Haenszel
# test compares free vs paid WITHIN each game and pools the result.
tab4 <- table(rev$received_for_free, rev$recommended)
p4 <- prop.test(tab4[, "Positive"], rowSums(tab4))
add("H4a free copy vs paid: share positive (2-sample proportion test, unadjusted)", "all",
    p4$statistic, p4$p.value,
    sprintf("%.1f%% positive (free) vs %.1f%% (paid)", 100 * p4$estimate[2], 100 * p4$estimate[1]))
t4 <- xtabs(~ received_for_free + recommended + game, rev)
t4 <- t4[, , apply(t4, 3, function(m) all(rowSums(m) > 0))]   # games with both free and paid
m4 <- mantelhaen.test(t4)
add("H4b free vs paid, stratified by game (Cochran-Mantel-Haenszel)", "all",
    m4$statistic, m4$p.value,
    sprintf("common OR (paid:positive) = %.2f, 95%% CI %.2f-%.2f",
            m4$estimate, m4$conf.int[1], m4$conf.int[2]))

# ---- H5: Complaint type depends on the network community of the game --------------
# Links Section 6 (clusters) and Section 7 (communities).
cl   <- readRDS("data/review_clusters.rds")
comm <- readRDS("data/network_communities.rds")
cl <- cl %>% left_join(comm, by = "game")
tab5 <- table(cl$cluster, cl$community)
c5 <- chisq.test(tab5)
cramer_v <- sqrt(c5$statistic / (sum(tab5) * (min(dim(tab5)) - 1)))
add("H5 complaint cluster x network community (chi-squared)", "negative reviews",
    c5$statistic, c5$p.value, sprintf("Cramer's V = %.2f", cramer_v))
cat("\nH5: complaint cluster share within each network community (column %):\n")
print(round(100 * prop.table(tab5, 2), 1))
cat("Largest standardised residuals (which cluster is over-represented where):\n")
res <- as.data.frame(as.table(c5$stdres)); names(res) <- c("cluster", "community", "std_resid")
print(res %>% arrange(desc(std_resid)) %>% head(8), digits = 3)

# ---- Summary ------------------------------------------------------------------------
out <- do.call(rbind, results)
out$p_value <- format.pval(out$p_value, digits = 3, eps = 1e-16)
out$statistic <- signif(out$statistic, 4)
cat("\n===== Summary of tests =====\n")
print(out[, c("test", "sample", "p_value", "effect")], right = FALSE, row.names = FALSE)
write.csv(out, "data/stats_results.csv", row.names = FALSE)
