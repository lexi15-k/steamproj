# =============================================================================
# COMP3020 Group Project - 02 Time-trend analysis: detecting review bombs
# RQ1: When do negative reviews spike, and do the spikes line up with
#      identifiable controversies (platform policy, performance, monetisation)?
# Input : data/hist_monthly.rds (TRUE monthly up/down counts, all reviews in all languages)
#         data/reviews_clean.rds (our sample, for the sampling-bias check)
# Output: figures/fig_timetrend_all.png, figures/fig_timetrend_key.png,
#         figures/fig_sample_bias.png, data/spikes.csv
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(scales) })
dir.create("figures", showWarnings = FALSE)

hist  <- readRDS("data/hist_monthly.rds")
rev   <- readRDS("data/reviews_clean.rds")
games <- readRDS("data/games.rds")

# ---- 1. Spike detection -----------------------------------------------------
# For each game, the baseline is its median monthly negative share.
# A month is a "negative spike" (possible review bomb) when
#   (a) its negative share is at least 20 percentage points above the baseline, and
#   (b) it has at least the game's median monthly review volume and >= 200 reviews
#       (ignores tiny, noisy months, e.g. Redfall after launch had < 20 reviews/month).
# The 20-point threshold is a judgement call; sensitivity to it is checked below.
detect_spikes <- function(h, jump = 0.20) {
  h %>%
    group_by(game) %>%
    mutate(baseline = median(neg_share),
           median_total = median(total),
           excess = neg_share - baseline,
           spike = excess >= jump & total >= median_total & total >= 200) %>%
    ungroup()
}
hist <- detect_spikes(hist)

spikes <- hist %>% filter(spike) %>%
  arrange(game, month) %>%
  select(game, month, up, down, total, neg_share, baseline, excess)
cat("Negative spikes (neg share >= baseline + 20 pts, volume >= median):\n")
print(as.data.frame(spikes), digits = 2)
write.csv(spikes, "data/spikes.csv", row.names = FALSE)

cat("\nSensitivity: number of games with >= 1 spike, by threshold\n")
for (j in c(0.10, 0.15, 0.20, 0.25, 0.30)) {
  s <- detect_spikes(hist %>% select(appid, game, month, up, down, total, neg_share), j)
  cat(sprintf("  jump = %.2f: %2d spike-months across %2d games\n",
              j, sum(s$spike), n_distinct(s$game[s$spike])))
}

# ---- 2. Figure: all 24 games -----------------------------------------------
p_all <- ggplot(hist, aes(month, neg_share)) +
  geom_hline(aes(yintercept = baseline), colour = "grey60", linetype = "dashed", linewidth = 0.3) +
  geom_line(colour = "grey30", linewidth = 0.4) +
  geom_point(data = filter(hist, spike), colour = "#d7301f", size = 1.3) +
  facet_wrap(~ game, ncol = 4, scales = "free_x") +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_x_date(date_labels = "%Y", breaks = scales::breaks_pretty(3)) +
  labs(title = "Monthly share of negative Steam reviews (all review languages)",
       subtitle = "Dashed = game's median month; red = spike (>= 20 pts above median, volume >= median and >= 200)",
       x = "Year", y = "Negative share") +
  theme_minimal(base_size = 9)
ggsave("figures/fig_timetrend_all.png", p_all, width = 10, height = 11, dpi = 200)

# ---- 3. Figure: key controversies (poster figure) --------------------------
# Only events we can document are labelled; cite a news source for each in the report.
events <- data.frame(
  game  = c("Helldivers 2", "Overwatch 2", "Team Fortress 2", "Counter-Strike 2",
            "Counter-Strike 2", "No Man's Sky", "Payday 3"),
  month = as.Date(c("2024-05-01", "2023-08-01", "2024-06-01", "2018-12-01",
                    "2023-10-01", "2016-08-01", "2023-09-01")),
  label = c("PSN account requirement", "Steam launch", "#SaveTF2 (bot crisis)",
            "CS:GO goes free-to-play", "CS2 replaces CS:GO", "Launch: missing promised features",
            "Launch: always-online server outages")
)
key <- hist %>% filter(game %in% events$game)
ev  <- events %>% left_join(key %>% select(game, month, neg_share), by = c("game", "month"))
# labels near the right edge of a panel go to the left of the event line
rng <- key %>% group_by(game) %>% summarise(lo = min(month), hi = max(month))
ev  <- ev %>% left_join(rng, by = "game") %>%
  mutate(pos = as.numeric(month - lo) / as.numeric(hi - lo),
         hj = ifelse(pos > 0.6, 1, 0),
         x_lab = month + ifelse(pos > 0.6, -20, 20),
         y_lab = ifelse(duplicated(game), 0.80, 0.97))

p_key <- ggplot(key, aes(month)) +
  geom_line(aes(y = neg_share), colour = "grey25", linewidth = 0.5) +
  geom_point(data = filter(key, spike), aes(y = neg_share), colour = "#d7301f", size = 1.8) +
  geom_vline(data = ev, aes(xintercept = month), colour = "#d7301f", linetype = "dotted") +
  geom_label(data = ev, aes(x = x_lab, y = y_lab, label = label, hjust = hj), size = 2.6,
             linewidth = 0, fill = alpha("white", 0.8)) +
  facet_wrap(~ game, ncol = 2, scales = "free_x") +
  scale_x_date(date_labels = "%Y") +
  scale_y_continuous(labels = percent, limits = c(0, 1.02)) +
  labs(title = "Review bombs line up with platform and policy decisions",
       x = NULL, y = "Negative share of reviews (monthly)") +
  theme_minimal(base_size = 10)
ggsave("figures/fig_timetrend_key.png", p_key, width = 9, height = 7, dpi = 200)
ggsave("figures/poster_timetrend_key.png", p_key + theme_minimal(base_size = 12), width = 7.5, height = 6.4, dpi = 250)

# ---- 4. Sampling-bias check --------------------------------------------------
# Our text sample is 75% "most helpful" reviews. How far is its negative share
# from the true lifetime share reported by Steam?
bias <- rev %>%
  group_by(game) %>%
  summarise(sample_neg = mean(recommended == "Negative"), .groups = "drop") %>%
  left_join(games %>% select(game, true_neg_share), by = "game")
cat(sprintf("\nMean sample - true negative share: %+.3f  (correlation r = %.2f)\n",
            mean(bias$sample_neg - bias$true_neg_share),
            cor(bias$sample_neg, bias$true_neg_share)))

p_bias <- ggplot(bias, aes(true_neg_share, sample_neg, label = game)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_point(colour = "#2c7fb8") +
  geom_text(size = 2.4, vjust = -0.7, check_overlap = TRUE) +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Our sample over-represents negative reviews",
       subtitle = "'Most helpful' reviews skew negative; dashed line = no bias",
       x = "True negative share (all English reviews, Steam)", y = "Negative share in our sample") +
  theme_minimal(base_size = 10)
ggsave("figures/fig_sample_bias.png", p_bias, width = 7, height = 6, dpi = 200)

cat("Saved figures/fig_timetrend_all.png, fig_timetrend_key.png, fig_sample_bias.png\n")
