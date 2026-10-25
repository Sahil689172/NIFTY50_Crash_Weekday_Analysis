# =============================================================================
# Script  : 00_run_analysis.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Optional master orchestration script.  Sources analysis scripts
#           04 through 09 in the correct order.  Does NOT re-run data
#           acquisition (01), cleaning (02), or feature engineering (03).
# Author  : [Your Name]
# Usage   :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/00_run_analysis.R")
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Guard: confirm working directory and prerequisite outputs exist
# ---------------------------------------------------------------------------

REQUIRED_INPUT <- file.path("data", "processed", "features",
                             "nifty50_index_features.csv")
if (!file.exists(REQUIRED_INPUT)) {
  stop(
    "Feature data not found at: ", REQUIRED_INPUT, "\n",
    "Run scripts 01, 02, and 03 first:\n",
    "  source('R/01_download_data.R')\n",
    "  source('R/02_data_cleaning.R')\n",
    "  source('R/03_feature_engineering.R')\n"
  )
}

cat(rep("=", 70), "\n", sep = "")
cat("  NIFTY 50 Analysis Pipeline — Scripts 04 to 09\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Run analysis scripts in order
# ---------------------------------------------------------------------------

SCRIPTS <- c(
  "R/04_eda.R",
  "R/05_hypothesis_testing.R",
  "R/06_regression_analysis.R",
  "R/07_crash_analysis.R",
  "R/08_robustness_checks.R",
  "R/09_generate_research_outputs.R"
)

for (script in SCRIPTS) {
  if (!file.exists(script)) {
    stop("Script not found: ", script,
         "\nEnsure all scripts 04-09 are present in R/.")
  }
  cat(sprintf("\n>>> Sourcing: %s  [%s]\n", script, Sys.time()))
  tryCatch(
    source(script, echo = FALSE),
    error = function(e) {
      stop(sprintf("FATAL ERROR in %s:\n  %s\n", script, e$message))
    }
  )
  cat(sprintf("<<< Completed: %s\n", script))
}

cat(rep("=", 70), "\n", sep = "")
cat(sprintf("  All scripts completed: %s\n", Sys.time()))
cat("  Results saved under: results/\n")
cat(rep("=", 70), "\n", sep = "")
