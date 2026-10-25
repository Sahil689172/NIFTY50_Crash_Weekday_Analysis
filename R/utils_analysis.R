# =============================================================================
# Script  : utils_analysis.R
# Purpose : Shared constants, helpers, and I/O utilities sourced by
#           scripts 04-09.  Not run directly.
# =============================================================================

# ---------------------------------------------------------------------------
# 1. Package guard
# ---------------------------------------------------------------------------

#' Check that a package is installed and load it.
#' Prints a clear install command if missing.
require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf(
      "Package '%s' is required but not installed.\n  Run: install.packages('%s')",
      pkg, pkg
    ))
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ---------------------------------------------------------------------------
# 2. Project-wide path constants
# ---------------------------------------------------------------------------

DIR_FEATURES      <- file.path("data", "processed", "features")
DIR_FEAT_STOCKS   <- file.path(DIR_FEATURES, "constituent_features")
DIR_QUALITY       <- file.path("data", "processed", "quality_reports")

PATH_INDEX_FEAT   <- file.path(DIR_FEATURES, "nifty50_index_features.csv")
PATH_PANEL_FEAT   <- file.path(DIR_FEATURES, "all_constituents_features.csv")

DIR_RESULTS       <- "results"
DIR_TABLES        <- file.path(DIR_RESULTS, "tables")
DIR_FIGURES       <- file.path(DIR_RESULTS, "figures")
DIR_REPORTS       <- file.path(DIR_RESULTS, "reports")

# ---------------------------------------------------------------------------
# 3. Confirmed column names (from 03_feature_engineering.R output)
# ---------------------------------------------------------------------------

COL_DATE          <- "Date"
COL_RETURN        <- "ReturnPrice"        # arithmetic daily return
COL_RETURN_SRC    <- "ReturnSource"       # "Adjusted" | "Close" | NA
COL_WEEKDAY       <- "Weekday"            # "Monday" … "Friday"
COL_WEEKDAY_NUM   <- "WeekdayNum"         # integer 1=Mon … 5=Fri
COL_ROLLVOL_5     <- "RollVol_5"
COL_ROLLVOL_20    <- "RollVol_20"
COL_ROLLVOL_30    <- "RollVol_30"
COL_PEAK          <- "RunningPeak"
COL_DRAWDOWN      <- "Drawdown"
COL_CRASH3        <- "CrashFlag_3pct"     # logical; TRUE = return <= -3%
COL_CRASH5        <- "CrashFlag_5pct"     # logical; TRUE = return <= -5%
COL_DD20          <- "DrawdownFlag_20pct" # logical; TRUE = drawdown <= -20%
COL_ADJUSTED      <- "Adjusted"
COL_CLOSE         <- "Close"
COL_NSE           <- "NSESymbol"
COL_OHLCFLAG      <- "OHLCFlag"

# Ordered weekday factor levels
WEEKDAY_LEVELS <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# Crash thresholds (must match 03_feature_engineering.R)
CRASH_THRESH_3 <- -0.03
CRASH_THRESH_5 <- -0.05
DD_THRESH_20   <- -0.20

# ---------------------------------------------------------------------------
# 4. Directory helpers
# ---------------------------------------------------------------------------

ensure_dir <- function(path) {
  if (!dir.exists(path))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

create_results_dirs <- function() {
  for (d in c(DIR_RESULTS, DIR_TABLES, DIR_FIGURES, DIR_REPORTS))
    ensure_dir(d)
}

# ---------------------------------------------------------------------------
# 5. Safe CSV read
# ---------------------------------------------------------------------------

NA_STRINGS <- c("", "NA", "N/A", "n/a", "NULL", "null", ".", "#N/A",
                 "#VALUE!", "#REF!", "NaN", "-", "--", "?")

safe_read <- function(path, label = path) {
  if (!file.exists(path))
    stop(sprintf("Required file not found: %s", path))
  df <- utils::read.csv(path, stringsAsFactors = FALSE,
                        na.strings = NA_STRINGS)
  message(sprintf("  Read %d x %d: %s", nrow(df), ncol(df), label))
  df
}

# ---------------------------------------------------------------------------
# 6. Data preparation helpers
# ---------------------------------------------------------------------------

#' Prepare a feature data frame for analysis.
#' - Parse Date to Date class.
#' - Convert logical columns (may be read as character from CSV).
#' - Coerce Weekday to ordered factor with Monday as reference.
#' - Keep only trading days (Weekday in Mon-Fri).
prepare_df <- function(df) {
  df[[COL_DATE]] <- as.Date(df[[COL_DATE]])

  # Coerce logical flag columns
  for (col in c(COL_CRASH3, COL_CRASH5, COL_DD20, COL_OHLCFLAG)) {
    if (col %in% colnames(df) && is.character(df[[col]])) {
      df[[col]] <- df[[col]] == "TRUE"
    }
  }

  # Ordered weekday factor with Monday as reference
  df[[COL_WEEKDAY]] <- factor(df[[COL_WEEKDAY]],
                               levels = WEEKDAY_LEVELS,
                               ordered = FALSE)

  # Drop rows with Weekday outside Mon-Fri (should not exist, but guard)
  df <- df[!is.na(df[[COL_WEEKDAY]]), ]

  df
}

#' Return the index feature data, prepared and validated.
load_index <- function() {
  df <- safe_read(PATH_INDEX_FEAT, "NIFTY 50 index features")
  df <- prepare_df(df)
  # Keep only rows with valid returns for return-based analysis
  # (caller decides whether to use ret_only subset)
  df
}

#' Return the stacked constituent panel, prepared.
load_panel <- function() {
  df <- safe_read(PATH_PANEL_FEAT, "constituent panel features")
  df <- prepare_df(df)
  df
}

#' Return a single constituent's feature data.
load_stock <- function(symbol) {
  safe_sym <- gsub("[^A-Za-z0-9._-]", "_", symbol)
  path <- file.path(DIR_FEAT_STOCKS, paste0(safe_sym, ".csv"))
  df   <- safe_read(path, symbol)
  prepare_df(df)
}

#' Extract valid (non-NA) return rows from a prepared data frame.
valid_returns <- function(df) {
  df[!is.na(df[[COL_RETURN]]), ]
}

# ---------------------------------------------------------------------------
# 7. Descriptive statistics helper
# ---------------------------------------------------------------------------

#' Compute a named summary vector for a numeric vector x.
desc_stats <- function(x, na.rm = TRUE) {
  require_pkg("e1071")      # for skewness / kurtosis
  x_valid <- x[!is.na(x)]
  n  <- length(x_valid)
  q  <- quantile(x_valid, probs = c(0.25, 0.50, 0.75))
  c(
    N        = n,
    Mean     = mean(x_valid),
    Median   = median(x_valid),
    SD       = sd(x_valid),
    Variance = var(x_valid),
    Min      = min(x_valid),
    Max      = max(x_valid),
    Q1       = unname(q[1]),
    Q3       = unname(q[3]),
    IQR      = unname(q[3] - q[1]),
    Skewness = e1071::skewness(x_valid, type = 2),
    Kurtosis = e1071::kurtosis(x_valid, type = 2)   # excess kurtosis
  )
}

# ---------------------------------------------------------------------------
# 8. Plot saving helpers
# ---------------------------------------------------------------------------

#' Save a base-R plot to PNG.
save_png <- function(file, expr, width = 1600, height = 1000, res = 150) {
  ensure_dir(dirname(file))
  png(file, width = width, height = height, res = res)
  tryCatch(force(expr), finally = dev.off())
  message(sprintf("  Figure saved: %s", file))
}

#' Save a ggplot object to PNG.
save_gg <- function(file, p, width = 10, height = 6, dpi = 150) {
  ensure_dir(dirname(file))
  require_pkg("ggplot2")
  ggplot2::ggsave(file, plot = p, width = width, height = height,
                  dpi = dpi, bg = "white")
  message(sprintf("  Figure saved: %s", file))
}

# ---------------------------------------------------------------------------
# 9. Table saving helper
# ---------------------------------------------------------------------------

#' Write a data frame to CSV under results/tables/, warning on overwrite.
save_table <- function(df, filename, subdir = DIR_TABLES) {
  ensure_dir(subdir)
  path <- file.path(subdir, filename)
  if (file.exists(path))
    message(sprintf("  [OVERWRITE] %s", path))
  utils::write.csv(df, path, row.names = FALSE)
  message(sprintf("  Table saved: %s", path))
}

# ---------------------------------------------------------------------------
# 10. Effect size helpers
# ---------------------------------------------------------------------------

#' Cohen's d (pooled SD) for two numeric vectors.
cohen_d <- function(x, y) {
  nx <- length(x[!is.na(x)]); ny <- length(y[!is.na(y)])
  if (nx < 2 || ny < 2) return(NA_real_)
  sp <- sqrt(((nx - 1) * var(x, na.rm = TRUE) +
              (ny - 1) * var(y, na.rm = TRUE)) / (nx + ny - 2))
  if (sp == 0) return(NA_real_)
  (mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)) / sp
}

#' Eta-squared from an aov object.
eta_squared <- function(aov_obj) {
  ss  <- summary(aov_obj)[[1]][["Sum Sq"]]
  ss[1] / sum(ss)   # SS_between / SS_total
}

#' Cramer's V from a chi-square test result and contingency table.
cramers_v <- function(chisq_result, n, k) {
  # k = min(nrow, ncol) of contingency table
  sqrt(chisq_result$statistic / (n * (k - 1)))
}
