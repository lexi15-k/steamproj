##########################################################################################
# COMP3020 Social Web Analytics - Group Project: FULL ANALYSIS (all parts in one file)
# 'When Players Push Back' - Steam reviews of 24 controversial games
#
# How to run: open the project folder in RStudio (Session > Set Working Directory >
# To Project/Source File Location) and run this whole file, or in a terminal:
#     Rscript COMP3020_full_analysis.R
# Input : data/steam_reviews.csv, steam_histogram.csv, steam_games.csv
#         (collected on 29 Sep 2026 by collect_steam.py - do NOT re-collect, or every
#         number in the report changes)
# Output: data/*.rds / *.csv and figures/*.png used by COMP3020_Report.Rmd
# Packages: dplyr, ggplot2, tidyr, stringr, scales, tm, SnowballC, wordcloud,
#           cluster, igraph, slam
# Runtime: ~15 min (Part 4 k-means is the slow step). All random steps use fixed seeds,
# so the results are identical every run.
#
# This file is the same code as the separate scripts 01_clean.R ... 06_stats.R
# (run_all.R runs those in order). Edit ONE version only, or they will drift apart.
##########################################################################################


##########################################################################################
# PART 1 - Data cleaning (Report Section 3)
# (source file: 01_clean.R)
##########################################################################################

# =============================================================================
# COMP3020 Group Project - 01 Data cleaning
# Input : data/steam_reviews.csv, data/steam_histogram.csv, data/steam_games.csv
#         (collected by collect_steam.py on 29 Sep 2026)
# Output: data/reviews_clean.rds   one row per review, cleaned text + flags
#         data/hist_monthly.rds    one row per game x month (true up/down counts)
#         data/games.rds           game metadata
# Run from the project folder:  Rscript 01_clean.R
# =============================================================================
suppressPackageStartupMessages({ library(dplyr); library(stringr) })

# ---- 1. Load ---------------------------------------------------------------
raw <- read.csv("data/steam_reviews.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
games <- read.csv("data/steam_games.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
hist <- read.csv("data/steam_histogram.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
cat("Raw reviews:", nrow(raw), "\n")

# ---- 2. Clean review text --------------------------------------------------
clean_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- str_replace_all(x, "\\[/?[A-Za-z0-9*]+(=[^\\]]*)?\\]", " ")  # BBCode: [h1] [b] [spoiler] [url=..]
  x <- str_replace_all(x, "https?://\\S+", " ")                    # URLs
  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")
  str_squish(x)
}

# share of non-ASCII characters: high values = ASCII art / box drawing / other language
nonascii_share <- function(x) {
  n <- nchar(x)
  ifelse(n == 0, 0, nchar(gsub("[\x01-\x7F]", "", x, perl = TRUE)) / n)
}

rev <- raw %>%
  mutate(
    text        = clean_text(review),
    n_words     = str_count(text, "\\S+"),
    nonascii    = nonascii_share(text),
    date        = as.POSIXct(timestamp_created, origin = "1970-01-01", tz = "UTC"),
    month       = as.Date(format(date, "%Y-%m-01")),
    recommended = factor(ifelse(voted_up == 1, "Positive", "Negative"),
                         levels = c("Positive", "Negative")),
    playtime_h  = playtime_at_review_min / 60,       # hours played when the review was written
    playtime_total_h = playtime_forever_min / 60
  )

# ---- 3. Exclusions (logged so they can be reported) ------------------------
n0 <- nrow(rev)
rev <- rev %>% filter(text != "")
n_empty <- n0 - nrow(rev)
rev <- rev %>% filter(nonascii <= 0.30)
n_art <- n0 - n_empty - nrow(rev)

# text_ok: long enough to be useful for text mining / clustering.
# Short reviews ("GG", "NO") are kept for the recommendation statistics.
rev <- rev %>% mutate(text_ok = n_words >= 10)

cat(sprintf("Removed %d empty reviews and %d ASCII-art / non-English reviews\n", n_empty, n_art))
cat(sprintf("Remaining: %d reviews (%d with >= 10 words for text mining)\n",
            nrow(rev), sum(rev$text_ok)))

rev <- rev %>%
  select(appid, game, source, recommendationid, user, recommended, voted_up, text, n_words,
         text_ok, date, month, votes_up, votes_funny, weighted_vote_score, comment_count,
         steam_purchase, received_for_free, written_during_early_access, primarily_steam_deck,
         num_games_owned, num_reviews, playtime_h, playtime_total_h)

# ---- 4. Histogram: harmonise weekly + monthly rows to calendar months -------
# Steam returns monthly rollups for older games but weekly rollups for recent ones.
hist_monthly <- hist %>%
  mutate(month = as.Date(format(as.POSIXct(month_start_unix, origin = "1970-01-01", tz = "UTC"),
                                "%Y-%m-01"))) %>%
  group_by(appid, game, month) %>%
  summarise(up = sum(up), down = sum(down), .groups = "drop") %>%
  mutate(total = up + down, neg_share = down / total)

# ---- 5. Game metadata ------------------------------------------------------
games <- games %>%
  mutate(release_date = as.Date(release_date, format = "%d %b, %Y"),
         true_neg_share = total_negative / total_reviews)

# ---- 6. Summary table (Section 2 / 3 of the report) ------------------------
summary_tab <- rev %>%
  group_by(game) %>%
  summarise(reviews = n(),
            text_ok = sum(text_ok),
            sample_neg = mean(recommended == "Negative"),
            median_words = median(n_words),
            median_playtime_h = median(playtime_h, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(games %>% select(game, true_neg_share), by = "game") %>%
  arrange(desc(sample_neg))
print(as.data.frame(summary_tab), digits = 2)

saveRDS(rev, "data/reviews_clean.rds")
saveRDS(hist_monthly, "data/hist_monthly.rds")
saveRDS(games, "data/games.rds")
write.csv(summary_tab, "data/summary_by_game.csv", row.names = FALSE)
cat("Saved data/reviews_clean.rds, data/hist_monthly.rds, data/games.rds\n")


##########################################################################################
# PART 2 - Time trends / review bombs (Section 4, RQ1)
# (source file: 02_timetrend.R)
##########################################################################################

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


##########################################################################################
# PART 3 - Text mining (Section 5, RQ2)
# (source file: 03_textmining.R)
##########################################################################################

# =============================================================================
# COMP3020 Group Project - 03 Text mining
# RQ2: What words distinguish negative from positive reviews, and what is
#      distinctive about each game's reviews?
# Method: tm corpus -> clean -> stem -> document-term matrix -> term frequency,
#         TF-IDF (game-level documents) and log-odds ratio (negative vs positive).
# Input : data/reviews_clean.rds
# Output: data/dtm.rds (for 04_clustering.R), figures/fig_*words*.png, tables
# =============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(tm); library(SnowballC); library(wordcloud)
})
set.seed(3020)

rev <- readRDS("data/reviews_clean.rds") %>% filter(text_ok)   # >= 10 words
cat("Reviews used for text mining:", nrow(rev), "\n")

# ---- 1. Stop words ------------------------------------------------------------
# Standard English stop words + words that are common in every game review and
# carry no opinion. Game-title words are removed too, otherwise TF-IDF and the
# clusters in 04 would simply rediscover which game a review belongs to.
# Company names (sony, ubisoft, valve...) are KEPT: they are complaint targets.
game_words <- c("helldivers", "helldiver", "cities", "skylines", "starfield", "overwatch",
                "palworld", "payday", "diablo", "redfall", "skull", "bones", "suicide", "squad",
                "justice", "league", "ghost", "tsushima", "hogwarts", "legacy", "baldur", "baldurs",
                "gate", "elden", "ring", "counter", "strike", "csgo", "team", "fortress", "monster",
                "hunter", "wilds", "borderlands", "battlefield", "assassin", "assassins", "creed",
                "shadows", "kingdom", "deliverance", "kcd", "cyberpunk", "nms", "fallout", "tf",
                "bg", "cs", "ow", "mh", "mhw", "ac", "bf", "sky", "mans")
review_words <- c("game", "games", "play", "played", "playing", "player", "players", "just",
                  "like", "get", "got", "even", "really", "can", "will", "one", "also", "much",
                  "make", "made", "still", "way", "thing", "things", "lot", "dont", "doesnt",
                  "didnt", "cant", "ive", "im", "youre", "thats", "isnt", "wont", "go", "going",
                  "want", "know", "think", "say", "said", "time", "hours", "hour")
stop_all <- unique(c(stopwords("english"), gsub("'", "", stopwords("english")),
                     game_words, review_words))

# ---- 2. Corpus and cleaning -----------------------------------------------------
corpus <- VCorpus(VectorSource(rev$text))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, "UTF-8", "ASCII", sub = " ")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("'", "", x)))  # don't -> dont
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stop_all)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# Document-term matrix; keep terms that appear in >= 0.5% of reviews (~200 reviews)
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, 20)))
dtm <- removeSparseTerms(dtm, 0.995)
cat("DTM:", nrow(dtm), "reviews x", ncol(dtm), "terms\n")

# Most common original word for each stem, so plots show "performance" not "perform"
unstem <- function(stems) {
  words <- unlist(strsplit(tolower(gsub("[^A-Za-z' ]", " ", sample(rev$text, 8000))), "\\s+"))
  words <- gsub("'", "", words[nchar(words) > 2])
  tab <- sort(table(words), decreasing = TRUE)
  st <- wordStem(names(tab))
  sapply(stems, function(s) { w <- names(tab)[st == s]; if (length(w)) w[1] else s })
}

saveRDS(list(dtm = dtm, meta = rev %>% select(recommendationid, user, game, recommended,
                                              month, playtime_h, n_words, votes_up, source)),
        "data/dtm.rds")

# ---- 3. Term frequency: negative vs positive --------------------------------------
neg <- rev$recommended == "Negative"
freq_neg <- slam::col_sums(dtm[neg, ])
freq_pos <- slam::col_sums(dtm[!neg, ])

# Log-odds ratio (with +1 smoothing): how much more likely a term is in negative
# than in positive reviews. Positive = complaint word, negative = praise word.
lor <- log((freq_neg + 1) / (sum(freq_neg) + ncol(dtm))) -
       log((freq_pos + 1) / (sum(freq_pos) + ncol(dtm)))
terms <- data.frame(term = names(lor), freq_neg, freq_pos, lor) %>%
  filter(freq_neg + freq_pos >= 300)                       # ignore rare terms
top_lor <- bind_rows(terms %>% slice_max(lor, n = 20) %>% mutate(side = "More likely in NEGATIVE reviews"),
                     terms %>% slice_min(lor, n = 20) %>% mutate(side = "More likely in POSITIVE reviews"))
top_lor$word <- unstem(top_lor$term)
cat("\nTop complaint words (log-odds negative vs positive):\n")
print(head(top_lor %>% select(word, freq_neg, freq_pos, lor), 20), digits = 2)

p_lor <- ggplot(top_lor, aes(reorder(word, lor), lor, fill = side)) +
  geom_col() + coord_flip() +
  facet_wrap(~ side, scales = "free_y") +
  scale_fill_manual(values = c("#d7301f", "#2c7fb8"), guide = "none") +
  labs(title = "Words that separate negative from positive Steam reviews",
       subtitle = "Log-odds ratio of term frequency (stemmed; terms with >= 300 occurrences)",
       x = NULL, y = "Log-odds ratio (negative vs positive)") +
  theme_minimal(base_size = 10)
ggsave("figures/fig_logodds_words.png", p_lor, width = 10, height = 6, dpi = 200)
ggsave("figures/poster_logodds_words.png", p_lor + theme_minimal(base_size = 13) + labs(title = NULL, subtitle = NULL),
       width = 7.5, height = 5.4, dpi = 250)

# Comparison word cloud (poster-friendly)
m <- cbind(Negative = freq_neg, Positive = freq_pos)
rownames(m) <- unstem(rownames(m))
m <- rowsum(m, rownames(m))
png("figures/fig_wordcloud_compare.png", width = 1600, height = 1600, res = 200)
comparison.cloud(m, max.words = 150, colors = c("#d7301f", "#2c7fb8"), title.size = 1.4)
dev.off()

# ---- 4. TF-IDF with each GAME as one document ---------------------------------------
# Terms that are frequent in one game's reviews but rare in the others = what is
# distinctive about that game's reception.
game_dtm <- rowsum(as.matrix(dtm), rev$game)             # 24 games x terms
game_tf  <- game_dtm / rowSums(game_dtm)
idf      <- log(nrow(game_dtm) / colSums(game_dtm > 0))
game_tfidf <- sweep(game_tf, 2, idf, "*")

top_game <- do.call(rbind, lapply(rownames(game_tfidf), function(g) {
  x <- sort(game_tfidf[g, ], decreasing = TRUE)[1:8]
  data.frame(game = g, term = names(x), tfidf = x)
}))
top_game$word <- unstem(top_game$term)
cat("\nMost distinctive words per game (TF-IDF):\n")
print(aggregate(word ~ game, top_game, paste, collapse = ", "), right = FALSE)
write.csv(top_game, "data/tfidf_top_terms_by_game.csv", row.names = FALSE)

p_tfidf <- ggplot(top_game %>% group_by(game) %>% slice_max(tfidf, n = 6, with_ties = FALSE),
                  aes(tidytext_reorder <- reorder(paste(word, game, sep = "___"), tfidf), tfidf)) +
  geom_col(fill = "#636363") + coord_flip() +
  facet_wrap(~ game, scales = "free", ncol = 4) +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  labs(title = "Most distinctive words in each game's reviews (TF-IDF, game = document)",
       x = NULL, y = "TF-IDF") +
  theme_minimal(base_size = 8) + theme(axis.text.x = element_blank())
ggsave("figures/fig_tfidf_by_game.png", p_tfidf, width = 10, height = 12, dpi = 200)

cat("Saved data/dtm.rds and figures fig_logodds_words, fig_wordcloud_compare, fig_tfidf_by_game\n")


##########################################################################################
# PART 4 - k-means clustering of complaints (Section 6, RQ3)
# (source file: 04_clustering.R)
##########################################################################################

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


##########################################################################################
# PART 4b - Cluster labels, game profiles, dendrogram (Section 6)
# (source file: 04b_cluster_profiles.R)
##########################################################################################

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


##########################################################################################
# PART 5 - Network analysis (Section 7, RQ4)
# (source file: 05_network.R)
##########################################################################################

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


##########################################################################################
# PART 6 - Statistical tests (Section 8, RQ5)
# (source file: 06_stats.R)
##########################################################################################

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
