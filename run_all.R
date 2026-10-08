# Runs the full analysis in order. From the project folder:  Rscript run_all.R
# (collect_steam.py must have been run first to create data/steam_*.csv)
# 04_clustering.R takes ~15 min the first time (k selection); later runs use the cache.
for (f in c("01_clean.R", "02_timetrend.R", "03_textmining.R", "04_clustering.R",
            "04b_cluster_profiles.R", "05_network.R", "06_stats.R")) {
  cat("\n==========", f, "==========\n")
  source(f, echo = FALSE)
}
