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
