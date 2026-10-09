# COMP3020 Group Project – Steam reviews of controversial games

We looked at Steam reviews for 24 games that had a public controversy (plus a few well-liked
games as a comparison) to see when review bombs happen, what people complain about, and whether
the same players turn up across games. The report is `COMP3020_Report.pdf` and the poster is
`COMP3020_Poster.pdf`.

## Data

The data came from Steam's public store endpoints on 29 September 2026 using `collect_steam.py`
(no API key is needed). The raw files are in `data/`:

- `steam_reviews.csv` – 46,296 English reviews (text, thumbs up/down, playtime, votes, etc.)
- `steam_histogram.csv` – monthly counts of positive/negative reviews for each game
- `steam_games.csv` – basic info about each game

Steam IDs were hashed before saving, and we didn't keep usernames. The hashing uses a salt that
we've kept private, so `collect_steam.py` won't run unless you set `STEAM_ID_SALT` yourself.
Running it again would also give different reviews, since Steam changes every day, so please use
the CSV files here to check our results.

## Running the analysis

You need R (we used 4.6.1) with these packages:

```r
install.packages(c("dplyr", "tidyr", "stringr", "ggplot2", "scales", "tm", "SnowballC",
                   "wordcloud", "cluster", "igraph", "slam", "rmarkdown", "knitr"))
```

Set the working directory to this folder and run:

```
Rscript COMP3020_full_analysis.R
```

It takes about 15 minutes, mostly the k-means step. The seeds are fixed, so you should get the
same numbers as the report. Everything it produces goes into `data/` and `figures/`.

The same code is also split into separate scripts if that's easier to read. `run_all.R` runs them
in order:

| Script | Report section |
|---|---|
| `01_clean.R` | 3 – cleaning |
| `02_timetrend.R` | 4 – review bomb detection |
| `03_textmining.R` | 5 – text mining |
| `04_clustering.R`, `04b_cluster_profiles.R` | 6 – clustering |
| `05_network.R` | 7 – network analysis |
| `06_stats.R` | 8 – statistical tests |

`stability_check.R` is the extra k-means run we used to decide on k = 9 (Section 6.1). Its output
is in `stability_log_k8.txt` and `stability_log_k9.txt`.

Note: `data/k_selection.rds` stores the results of trying k = 2 to 14, because that part takes
about 14 minutes. Delete it if you want the script to redo it.

## Building the report

`COMP3020_Report_G5.Rmd` reads the saved results in `data/` and the images in `figures/`, so it can
be knitted without re-running the analysis. In RStudio, open it and click Knit (it needs a LaTeX
installation, e.g. `tinytex::install_tinytex()`).

The poster is COMP3020-Poster.pptx (PowerPoint).
