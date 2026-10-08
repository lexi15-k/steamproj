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
