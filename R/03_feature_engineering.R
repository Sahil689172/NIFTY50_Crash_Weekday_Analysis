# =============================================================================
# Script  : 03_feature_engineering.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Feature Engineering — compute daily returns, weekday variables,
#           rolling volatility, running peaks, drawdowns, and crash flags
#           for the NIFTY 50 index and every constituent.  No modelling,
#           backtesting, or statistical tests are performed here.
# Author  : [Your Name]
# Date    : 2026
# =============================================================================
#
# INPUT FILES (read-only — never modified)
# ----------------------------------------
# data/processed/nifty50_index_clean.csv
# data/processed/constituent_prices_clean/<SYMBOL>.csv   (50 files)
# data/processed/quality_reports/data_quality_report.csv (for validation)
#
# INPUT SCHEMA (from 02_data_cleaning.R output, confirmed 2026-10-21)
# -------------------------------------------------------------------
# Columns: Date, Open, High, Low, Close, Adjusted, Volume,
#          CompanyName, NSESymbol, YahooSymbol, Source, OHLCFlag, Weekday
# Notes:
#   • Weekday already present from 02_data_cleaning.R; re-derived here
#     from Date for verification and to ensure WeekdayNum is consistent.
#   • CompanyName is NA in ALL files (NSE data gap — use NSESymbol).
#   • ADANIENT: Adjusted is NA for early rows (47 rows also had negative
#     Adjusted replaced with NA by 02_data_cleaning.R).
#   • ^NSEI: 31 rows have NA across all OHLCAV simultaneously.
#   • Individual stocks: 6-11 rows per file have NA Close/Adjusted.
#   • BAJAJ-AUTO.csv: filename contains a hyphen (valid on all platforms).
#   • M_M.csv:  NSESymbol inside the file is "M&M" (not "M_M").
#
# OUTPUT FILES
# ------------
# data/processed/features/nifty50_index_features.csv
# data/processed/features/constituent_features/<SYMBOL>.csv
# data/processed/features/all_constituents_features.csv  (stacked panel)
# data/processed/quality_reports/feature_engineering_report.csv
#
# FEATURE DEFINITIONS
# -------------------
# ReturnPrice    : (PriceToday / PriceYesterday) - 1  (simple arithmetic)
#                  Using Adjusted as the primary price series.
#                  If Adjusted is NA for the current or prior row, fall back
#                  to Close.  If Close is also NA or the prior row has no
#                  usable price, Return = NA.  Never interpolated.
#
# ReturnSource   : "Adjusted" | "Close" | NA
#                  Documents which price was used to compute each return.
#
# Weekday        : Character.  Full English weekday name (e.g. "Monday").
#                  Re-derived from Date to verify 02_data_cleaning.R values.
#
# WeekdayNum     : Integer 1-5 (Monday=1, Friday=5).
#                  Used as an ordered categorical in regressions.
#
# RollVol_5      : Rolling standard deviation of ReturnPrice over the past
# RollVol_20       5 / 20 / 30 trading days respectively (trailing window,
# RollVol_30       NOT centred).  Computed on non-NA returns only.
#                  Result is NA when fewer than the full window of non-NA
#                  returns precede the current observation.
#                  These are HISTORICAL volatility measures: the window
#                  contains only past returns; no look-ahead bias.
#
# RunningPeak    : Cumulative maximum of Adjusted (or Close fallback) up to
#                  and including the current row.  Computed only on rows
#                  where the chosen price is non-NA.
#                  NA where no valid price has yet been observed.
#
# Drawdown       : (CurrentPrice / RunningPeak) - 1  (always <= 0).
#                  NA where RunningPeak or CurrentPrice is NA.
#
# CrashFlag_3pct : TRUE if ReturnPrice <= -0.03 (i.e. >= -3% daily loss).
# CrashFlag_5pct : TRUE if ReturnPrice <= -0.05 (i.e. >= -5% daily loss).
#                  FALSE on all other valid-return rows, NA where Return = NA.
#                  Genuine crash observations are NEVER removed.
#
# DrawdownFlag_20pct : TRUE if Drawdown <= -0.20 (>=20% from running peak).
#                      FALSE otherwise; NA where Drawdown = NA.
#
# LOOK-AHEAD BIAS NOTES
# ---------------------
# • Rolling volatility uses a trailing (left-aligned) window on past returns.
#   The window at position t spans returns[t-k .. t-1] for prediction use,
#   and returns[t-k+1 .. t] for contemporaneous use.  This script computes
#   the CONTEMPORANEOUS version (includes the return at t) which is the
#   correct choice for labelling/descriptive analysis.  If you use
#   RollVol_k as a predictor in a regression predicting future events,
#   shift it forward by one period in that later script.
# • RunningPeak at time t includes the price at t (expanding maximum),
#   which is standard for drawdown calculation and not a bias for
#   retrospective crash labelling.
# • CrashFlags are contemporaneous labels — they depend on today's return.
#
# MISSING-VALUE POLICY
# --------------------
# • Rows with NA Close AND NA Adjusted are excluded from return calculation
#   only.  All other columns on those rows are preserved.
# • NA returns propagate correctly into rolling volatility (treated as gaps).
# • No forward-filling, no interpolation, no artificial returns.
# • OHLCFlag rows (OHLC inconsistency) are retained; features are still
#   computed because Close or Adjusted may be valid even when Open/High/Low
#   are inconsistent.  OHLCFlag is preserved in the output for filtering.
#
# PACKAGES REQUIRED (base R only)
# ---------------------------------
#   utils, tools, stats — all part of base R; no installation needed.
# =============================================================================


# ---------------------------------------------------------------------------
# 0. Safety guard
# ---------------------------------------------------------------------------
if (!file.exists(file.path("data", "processed", "nifty50_index_clean.csv"))) {
  stop(
    "Cleaned data not found.  Run 02_data_cleaning.R first, then:\n",
    '  setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")\n',
    '  source("R/03_feature_engineering.R")'
  )
}


# ---------------------------------------------------------------------------
# 1. Directory and path constants
# ---------------------------------------------------------------------------

DIR_PROCESSED        <- file.path("data", "processed")
DIR_CLEAN_PRICES     <- file.path(DIR_PROCESSED, "constituent_prices_clean")
DIR_FEATURES         <- file.path(DIR_PROCESSED, "features")
DIR_FEAT_STOCKS      <- file.path(DIR_FEATURES,  "constituent_features")
DIR_QUALITY          <- file.path(DIR_PROCESSED, "quality_reports")

PATH_CLEAN_INDEX     <- file.path(DIR_PROCESSED, "nifty50_index_clean.csv")
PATH_FEAT_INDEX      <- file.path(DIR_FEATURES,  "nifty50_index_features.csv")
PATH_FEAT_PANEL      <- file.path(DIR_FEATURES,  "all_constituents_features.csv")
PATH_FE_REPORT       <- file.path(DIR_QUALITY,   "feature_engineering_report.csv")

# Rolling volatility window sizes (trading days)
VOL_WINDOWS    <- c(5L, 20L, 30L)

# Crash daily-return thresholds
CRASH_THRESH_3 <- -0.03   # -3 %
CRASH_THRESH_5 <- -0.05   # -5 %

# Drawdown threshold for drawdown flag
DD_THRESH_20   <- -0.20   # -20 %

# Strings treated as NA when reading CSVs
NA_STRINGS <- c("", "NA", "N/A", "n/a", "NULL", "null", ".", "#N/A",
                "#VALUE!", "#REF!", "NaN", "-", "--", "?")

# Weekday name -> number mapping (ISO: Mon=1, Fri=5)
WEEKDAY_MAP <- c(Monday    = 1L, Tuesday = 2L, Wednesday = 3L,
                 Thursday  = 4L, Friday  = 5L,
                 Saturday  = 6L, Sunday  = 7L)


# ---------------------------------------------------------------------------
# 2. Utility functions
# ---------------------------------------------------------------------------

#' Create directory if it does not already exist.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("  Created: %s", path))
  }
}


#' Read a CSV safely, returning NULL with a message on failure.
safe_read_csv <- function(path, label = path) {
  if (!file.exists(path)) {
    message(sprintf("  [MISSING] %s", label))
    return(NULL)
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, na.strings = NA_STRINGS),
    error = function(e) {
      message(sprintf("  [READ ERROR] %s: %s", label, e$message))
      NULL
    }
  )
}


#' Convert a column to Date; return NA for unparseable values.
to_date <- function(x) {
  suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
}


#' Append a row to a feature-engineering report data frame.
fe_report_row <- function(report_df, dataset, check, n_affected,
                          pct = NA_real_, detail = "", action = "") {
  rbind(report_df, data.frame(
    Dataset    = as.character(dataset),
    Check      = as.character(check),
    N_Affected = as.integer(n_affected),
    Pct        = round(as.numeric(pct), 4),
    Detail     = as.character(detail),
    Action     = as.character(action),
    stringsAsFactors = FALSE
  ))
}


# ---------------------------------------------------------------------------
# 3. Price selection: Adjusted with Close fallback
# ---------------------------------------------------------------------------

#' Choose the best available price for return / peak / drawdown calculations.
#'
#' Primary: Adjusted.  Fallback: Close.
#' NA is returned only when both are NA.
#' Also returns a character vector documenting which series was used
#' for each row.
#'
#' @param adj  Numeric vector.  Adjusted Close prices.
#' @param cls  Numeric vector.  Close prices.
#' @return List:
#'   $price  — numeric vector of selected prices
#'   $source — character vector: "Adjusted", "Close", or NA
select_price <- function(adj, cls) {
  n      <- length(adj)
  price  <- adj                         # start with Adjusted
  source <- rep("Adjusted", n)

  # Rows where Adjusted is NA but Close is valid
  use_close <- is.na(adj) & !is.na(cls)
  price[use_close]  <- cls[use_close]
  source[use_close] <- "Close"

  # Rows where both are NA
  both_na <- is.na(adj) & is.na(cls)
  source[both_na] <- NA_character_

  list(price = price, source = source)
}


# ---------------------------------------------------------------------------
# 4. Daily returns
# ---------------------------------------------------------------------------

#' Calculate simple arithmetic daily returns, NA-safe.
#'
#' Return at t = (Price_t / Price_{t-1}) - 1.
#' If either Price_t or Price_{t-1} is NA, Return_t = NA.
#' The first observation always produces NA (no prior price available).
#' No look-ahead: only lag-1 price is used.
#'
#' @param price  Numeric vector of prices (already sorted by Date).
#' @return Numeric vector of the same length; element 1 is always NA.
calc_returns <- function(price) {
  n <- length(price)
  if (n < 2L) return(rep(NA_real_, n))

  ret           <- rep(NA_real_, n)
  # lag: shift price by 1 position
  price_lag     <- c(NA_real_, price[-n])

  valid         <- !is.na(price) & !is.na(price_lag) & price_lag != 0
  ret[valid]    <- (price[valid] / price_lag[valid]) - 1

  ret
}


# ---------------------------------------------------------------------------
# 5. Weekday variables
# ---------------------------------------------------------------------------

#' Derive Weekday (character) and WeekdayNum (integer 1=Mon … 7=Sun) from Date.
#' Returns NA for NA dates.
#' Results are compared against the Weekday column already in the data;
#' mismatches are flagged in the FE report.
#'
#' @param dates  Date vector.
#' @return Data frame with columns Weekday_derived, WeekdayNum.
calc_weekday <- function(dates) {
  wd_str <- weekdays(dates)           # locale-independent in English
  wd_num <- WEEKDAY_MAP[wd_str]
  wd_num[is.na(wd_str)] <- NA_integer_

  data.frame(
    Weekday_derived = wd_str,
    WeekdayNum      = as.integer(wd_num),
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# 6. Rolling volatility
# ---------------------------------------------------------------------------

#' Compute trailing rolling standard deviation of returns.
#'
#' Uses an expanding-NA approach: for position t the window is
#' ret[t-k+1 .. t].  Result is NA whenever any of the k values in the
#' window is NA, OR when fewer than k observations have been seen.
#' This ensures no look-ahead: the window ends at t (current period).
#'
#' IMPORTANT: if used as a predictor of FUTURE events, lag the output
#' by one period in the analysis script.
#'
#' The minimum number of non-NA observations required equals the full
#' window width k.  No partial-window estimates are produced.
#'
#' @param ret  Numeric vector of returns (length n), possibly with NAs.
#' @param k    Integer.  Window width in trading days.
#' @return Numeric vector length n; NA wherever the full window is unavailable.
rolling_sd <- function(ret, k) {
  n      <- length(ret)
  result <- rep(NA_real_, n)
  if (k < 2L || n < k) return(result)

  for (i in seq(k, n)) {
    window <- ret[(i - k + 1L):i]
    # Require all k values to be non-NA (no partial estimation)
    if (sum(is.na(window)) == 0L) {
      result[i] <- stats::sd(window)
    }
    # If any NA in window, result stays NA — prevents look-ahead via
    # accidental non-NA run bridging over gaps
  }
  result
}


# ---------------------------------------------------------------------------
# 7. Running peak, drawdown, crash flags
# ---------------------------------------------------------------------------

#' Compute the running (expanding) maximum of a price series.
#' Only non-NA prices contribute to and inherit the peak.
#' Rows before the first valid price have NA peak.
#'
#' @param price  Numeric vector (sorted chronologically).
#' @return Numeric vector of running peaks.
running_peak <- function(price) {
  n      <- length(price)
  peak   <- rep(NA_real_, n)
  cur_pk <- NA_real_

  for (i in seq_len(n)) {
    if (!is.na(price[i])) {
      cur_pk <- if (is.na(cur_pk)) price[i] else max(cur_pk, price[i])
    }
    peak[i] <- cur_pk
  }
  peak
}


#' Compute drawdown = (price / running_peak) - 1.
#' Returns NA where either argument is NA.
#' Values are always <= 0 by construction.
#'
#' @param price  Numeric vector.
#' @param peak   Numeric vector (running peak, same length).
#' @return Numeric vector of drawdown values (negative or zero).
calc_drawdown <- function(price, peak) {
  dd              <- rep(NA_real_, length(price))
  valid           <- !is.na(price) & !is.na(peak) & peak > 0
  dd[valid]       <- (price[valid] / peak[valid]) - 1
  dd
}


#' Build crash flag vectors from a return vector.
#' NA returns produce NA flags — genuine crash rows are never removed.
#'
#' @param ret       Numeric vector of returns.
#' @param threshold Numeric scalar threshold (negative, e.g. -0.03).
#' @return Logical vector: TRUE = crash, FALSE = no crash, NA = no return.
crash_flag <- function(ret, threshold) {
  flag          <- rep(NA, length(ret))
  has_ret       <- !is.na(ret)
  flag[has_ret] <- ret[has_ret] <= threshold
  flag
}


#' Build drawdown flag: TRUE where drawdown <= threshold.
#'
#' @param dd        Numeric vector of drawdown values.
#' @param threshold Numeric scalar (negative, e.g. -0.20).
#' @return Logical vector.
drawdown_flag <- function(dd, threshold) {
  flag          <- rep(NA, length(dd))
  has_dd        <- !is.na(dd)
  flag[has_dd]  <- dd[has_dd] <= threshold
  flag
}


# ---------------------------------------------------------------------------
# 8. Validation of input data
# ---------------------------------------------------------------------------

#' Validate a cleaned price data frame before computing features.
#' Checks:
#'   (a) Expected columns present.
#'   (b) Date column parseable and sorted.
#'   (c) Duplicate dates.
#'   (d) NA counts for Close and Adjusted.
#'   (e) Negative Volume.
#'   (f) OHLC flag count inherited from 02_data_cleaning.R.
#'
#' Does NOT impute or remove anything.  Reports issues only.
#'
#' @param df     Data frame.
#' @param label  Short identifier for messages.
#' @return List: $report (data frame of issues), $n_issues (integer).
validate_input <- function(df, label) {

  report <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )

  n <- nrow(df)
  if (n == 0L) {
    report <- fe_report_row(report, label, "Empty data frame", 0L,
                            detail  = "No rows after cleaning",
                            action  = "Skip feature engineering for this dataset")
    return(list(report = report, n_issues = 1L))
  }

  # (a) Column presence
  required <- c("Date", "Close", "Adjusted", "NSESymbol", "YahooSymbol")
  missing  <- setdiff(required, colnames(df))
  if (length(missing) > 0) {
    report <- fe_report_row(report, label, "Missing required columns",
                            length(missing),
                            detail  = paste(missing, collapse = ", "),
                            action  = "Features that need missing columns will be NA")
  }

  # (b) Date sorted check
  if ("Date" %in% colnames(df) && inherits(df$Date, "Date")) {
    if (is.unsorted(df$Date, na.rm = TRUE)) {
      report <- fe_report_row(report, label, "Date column not sorted",
                              n,
                              detail  = "Will sort before feature computation",
                              action  = "Sorted in place; no data lost")
    }
  }

  # (c) Duplicate dates
  if ("Date" %in% colnames(df)) {
    n_dup <- sum(duplicated(df$Date, incomparables = NA))
    if (n_dup > 0) {
      report <- fe_report_row(report, label, "Duplicate dates remain",
                              n_dup,
                              pct    = 100 * n_dup / n,
                              detail = paste0(
                                n_dup, " duplicate date(s) not resolved by 02_data_cleaning.R; ",
                                "these are likely conflicting rows kept for manual review"
                              ),
                              action = "Features computed on all rows; interpret duplicates with caution")
    }
  }

  # (d) NA in Close and Adjusted
  for (col in intersect(c("Close", "Adjusted"), colnames(df))) {
    if (is.numeric(df[[col]])) {
      n_na <- sum(is.na(df[[col]]))
      if (n_na > 0) {
        report <- fe_report_row(report, label,
                                sprintf("NA %s values (input)", col),
                                n_na,
                                pct    = 100 * n_na / n,
                                detail = paste0(n_na, " rows with NA ", col,
                                                " inherited from cleaned data"),
                                action = paste0("Returns/peaks/drawdowns will be NA on ",
                                                "affected rows; no imputation"))
      }
    }
  }

  # (e) Negative Volume
  if ("Volume" %in% colnames(df) && is.numeric(df$Volume)) {
    n_neg_vol <- sum(!is.na(df$Volume) & df$Volume < 0)
    if (n_neg_vol > 0) {
      report <- fe_report_row(report, label, "Negative Volume (input)",
                              n_neg_vol,
                              pct    = 100 * n_neg_vol / n,
                              detail = "Should have been caught by 02_data_cleaning.R",
                              action = "Flagged; not used in feature computation")
    }
  }

  # (f) OHLCFlag
  if ("OHLCFlag" %in% colnames(df)) {
    ohlc_val <- df$OHLCFlag
    # OHLCFlag may be read as logical or character depending on CSV parsing
    if (is.character(ohlc_val)) ohlc_val <- ohlc_val == "TRUE"
    n_ohlc <- sum(ohlc_val, na.rm = TRUE)
    if (n_ohlc > 0) {
      report <- fe_report_row(report, label, "Rows with OHLCFlag=TRUE (input)",
                              n_ohlc,
                              pct    = 100 * n_ohlc / n,
                              detail = paste0(n_ohlc,
                                              " rows flagged for OHLC inconsistency by 02_data_cleaning.R"),
                              action = paste0("Retained; features computed using Close/Adjusted ",
                                              "which may still be valid"))
    }
  }

  list(report = report, n_issues = nrow(report))
}


# ---------------------------------------------------------------------------
# 9. Core feature engineering function
# ---------------------------------------------------------------------------

#' Compute all features for one price data frame (index or constituent).
#'
#' Steps:
#'  1. Validate input; collect issues.
#'  2. Sort by Date.
#'  3. Parse Date to Date class if character.
#'  4. Select best price (Adjusted > Close).
#'  5. Compute daily returns.
#'  6. Derive Weekday, WeekdayNum (verify against existing Weekday column).
#'  7. Compute rolling volatility (5, 20, 30 days).
#'  8. Compute running peak and drawdown.
#'  9. Compute crash flags and drawdown flag.
#' 10. Validate output feature columns.
#'
#' @param df        Data frame from a cleaned price CSV.
#' @param label     Short identifier (NSESymbol or "^NSEI").
#' @return List:
#'   $features  — original df plus all new feature columns
#'   $report    — data frame of issues found/documented
#'   $n_crashes — named integer: c(thresh3, thresh5, dd20)
engineer_features <- function(df, label) {

  report <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )

  # ---- Step 1: Validate input -------------------------------------------
  val <- validate_input(df, label)
  report <- rbind(report, val$report)

  if (nrow(df) == 0L) {
    return(list(features = df, report = report, n_crashes = c(0L, 0L, 0L)))
  }

  # ---- Step 2: Ensure Date is Date class --------------------------------
  if (!inherits(df$Date, "Date")) {
    df$Date <- to_date(df$Date)
  }

  # ---- Step 3: Sort by Date ---------------------------------------------
  df <- df[order(df$Date, na.last = TRUE), ]
  n  <- nrow(df)

  # ---- Step 4: Select best price ----------------------------------------
  adj_col <- if ("Adjusted" %in% colnames(df) && is.numeric(df$Adjusted)) {
    df$Adjusted
  } else rep(NA_real_, n)

  cls_col <- if ("Close" %in% colnames(df) && is.numeric(df$Close)) {
    df$Close
  } else rep(NA_real_, n)

  price_sel <- select_price(adj_col, cls_col)
  price_vec <- price_sel$price

  # Report how many rows use Close as fallback
  n_close_fallback <- sum(price_sel$source == "Close", na.rm = TRUE)
  n_price_na       <- sum(is.na(price_sel$source))

  if (n_close_fallback > 0) {
    report <- fe_report_row(
      report, label, "Return price: Close used as fallback for Adjusted",
      n_close_fallback,
      pct    = 100 * n_close_fallback / n,
      detail = paste0(n_close_fallback,
                      " row(s) used Close because Adjusted was NA. ",
                      "Adjusted-based returns and Close-based returns are ",
                      "not directly comparable; see ReturnSource column."),
      action = "ReturnSource = 'Close' on affected rows; documented"
    )
  }
  if (n_price_na > 0) {
    report <- fe_report_row(
      report, label, "Rows with no valid price (Close and Adjusted both NA)",
      n_price_na,
      pct    = 100 * n_price_na / n,
      detail = paste0(n_price_na,
                      " rows where both Adjusted and Close are NA; ",
                      "return, peak, drawdown, and flags will be NA"),
      action = "No imputation; rows retained with feature columns = NA"
    )
  }

  df$PriceForReturn <- price_vec
  df$ReturnSource   <- price_sel$source

  # ---- Step 5: Daily returns --------------------------------------------
  df$ReturnPrice <- calc_returns(df$PriceForReturn)

  n_na_ret <- sum(is.na(df$ReturnPrice))
  # First row is always NA; only report excess NAs beyond that
  excess_na_ret <- n_na_ret - 1L  # subtract the expected first-row NA
  if (excess_na_ret > 0) {
    report <- fe_report_row(
      report, label, "NA returns (beyond first row)",
      excess_na_ret,
      pct    = 100 * excess_na_ret / n,
      detail = paste0(excess_na_ret,
                      " returns are NA due to NA prices in current or prior row"),
      action = "NA returns excluded from rolling volatility windows and summaries"
    )
  }

  # ---- Step 6: Weekday and WeekdayNum -----------------------------------
  wd_df       <- calc_weekday(df$Date)
  df$WeekdayNum <- wd_df$WeekdayNum

  # Verify against existing Weekday column from 02_data_cleaning.R
  if ("Weekday" %in% colnames(df)) {
    existing_wd <- df$Weekday
    derived_wd  <- wd_df$Weekday_derived
    # Compare on rows where both are non-NA
    both_valid  <- !is.na(existing_wd) & !is.na(derived_wd)
    n_mismatch  <- sum(existing_wd[both_valid] != derived_wd[both_valid])
    if (n_mismatch > 0) {
      report <- fe_report_row(
        report, label, "Weekday mismatch: 02_data_cleaning.R vs re-derived",
        n_mismatch,
        pct    = 100 * n_mismatch / n,
        detail = paste0(n_mismatch, " rows where Weekday from cleaning differs ",
                        "from weekdays(Date).  Check locale settings."),
        action = paste0("Weekday column from 02_data_cleaning.R preserved; ",
                        "WeekdayNum derived from re-computation")
      )
    }
    # Weekday column already exists; do NOT overwrite it — keep original
  } else {
    # No Weekday column — add it from derivation
    df$Weekday <- wd_df$Weekday_derived
  }

  # ---- Step 7: Rolling volatility ---------------------------------------
  # All rolling SD calculations use the trailing window on ReturnPrice.
  # NA returns in the window cause the whole window's result to be NA.
  # This is intentional: it avoids silently bridging over missing-data gaps.
  for (k in VOL_WINDOWS) {
    col_name    <- paste0("RollVol_", k)
    df[[col_name]] <- rolling_sd(df$ReturnPrice, k)

    n_na_vol <- sum(is.na(df[[col_name]]))
    # Rolling vol is NA for first k-1 rows + any row whose window contains NA
    # This is expected; only report it for transparency
    report <- fe_report_row(
      report, label,
      sprintf("RollVol_%d: NA count", k),
      n_na_vol,
      pct    = 100 * n_na_vol / n,
      detail = sprintf(
        paste0("%d NA values (includes first %d rows where window is ",
               "not yet full, plus rows where any window return is NA)"),
        n_na_vol, k - 1L
      ),
      action = "Expected; NA rows excluded from any downstream volatility analysis"
    )
  }

  # ---- Step 8: Running peak and drawdown --------------------------------
  df$RunningPeak <- running_peak(df$PriceForReturn)
  df$Drawdown    <- calc_drawdown(df$PriceForReturn, df$RunningPeak)

  n_na_dd <- sum(is.na(df$Drawdown))
  if (n_na_dd > 0) {
    report <- fe_report_row(
      report, label, "NA Drawdown values",
      n_na_dd,
      pct    = 100 * n_na_dd / n,
      detail = paste0(n_na_dd, " drawdown values are NA (price or peak is NA)"),
      action = "Expected; no imputation"
    )
  }

  # ---- Step 9: Crash flags and drawdown flag ----------------------------
  df$CrashFlag_3pct      <- crash_flag(df$ReturnPrice, CRASH_THRESH_3)
  df$CrashFlag_5pct      <- crash_flag(df$ReturnPrice, CRASH_THRESH_5)
  df$DrawdownFlag_20pct  <- drawdown_flag(df$Drawdown, DD_THRESH_20)

  # Count crashes (never removed — genuine observations)
  n_crash3  <- sum(df$CrashFlag_3pct  == TRUE, na.rm = TRUE)
  n_crash5  <- sum(df$CrashFlag_5pct  == TRUE, na.rm = TRUE)
  n_dd20    <- sum(df$DrawdownFlag_20pct == TRUE, na.rm = TRUE)

  report <- fe_report_row(
    report, label, "CrashFlag_3pct: crash days identified",
    n_crash3,
    pct    = 100 * n_crash3 / max(sum(!is.na(df$ReturnPrice)), 1),
    detail = sprintf("Return <= -3%% on %d day(s)", n_crash3),
    action = "Retained as genuine crash observations; not removed"
  )
  report <- fe_report_row(
    report, label, "CrashFlag_5pct: crash days identified",
    n_crash5,
    pct    = 100 * n_crash5 / max(sum(!is.na(df$ReturnPrice)), 1),
    detail = sprintf("Return <= -5%% on %d day(s)", n_crash5),
    action = "Retained as genuine crash observations; not removed"
  )
  report <- fe_report_row(
    report, label, "DrawdownFlag_20pct: days in >=20%% drawdown",
    n_dd20,
    pct    = 100 * n_dd20 / max(sum(!is.na(df$Drawdown)), 1),
    detail = sprintf("Drawdown <= -20%% on %d day(s)", n_dd20),
    action = "Retained; flag used for context in crash analysis"
  )

  # ---- Step 10: Drop helper column (PriceForReturn) ---------------------
  # ReturnSource is kept; PriceForReturn was an intermediate step
  df$PriceForReturn <- NULL

  list(
    features  = df,
    report    = report,
    n_crashes = c(crash3 = n_crash3, crash5 = n_crash5, dd20 = n_dd20)
  )
}


# ---------------------------------------------------------------------------
# 10. Console summary printer
# ---------------------------------------------------------------------------

#' Print a formatted summary to the console after all processing is done.
print_fe_summary <- function(index_result, stock_results, n_ok, n_fail) {

  sep <- paste(rep("=", 72), collapse = "")
  message("\n", sep)
  message("  FEATURE ENGINEERING SUMMARY")
  message(sep)

  # Index summary
  if (!is.null(index_result)) {
    idx_feat <- index_result$features
    n_idx    <- nrow(idx_feat)
    cr3  <- index_result$n_crashes["crash3"]
    cr5  <- index_result$n_crashes["crash5"]
    dd20 <- index_result$n_crashes["dd20"]
    message(sprintf("\n  NIFTY 50 Index:"))
    message(sprintf("    Rows              : %d", n_idx))
    message(sprintf("    Date range        : %s  ->  %s",
                    min(idx_feat$Date, na.rm = TRUE),
                    max(idx_feat$Date, na.rm = TRUE)))
    message(sprintf("    NA returns        : %d",
                    sum(is.na(idx_feat$ReturnPrice))))
    message(sprintf("    Crashes (>=-3%%)   : %d  (%.2f%% of valid returns)",
                    cr3, 100 * cr3 / max(sum(!is.na(idx_feat$ReturnPrice)), 1)))
    message(sprintf("    Crashes (>=-5%%)   : %d  (%.2f%% of valid returns)",
                    cr5, 100 * cr5 / max(sum(!is.na(idx_feat$ReturnPrice)), 1)))
    message(sprintf("    Days in 20%% DD    : %d", dd20))
    message(sprintf("    RollVol_5  NAs    : %d",
                    sum(is.na(idx_feat$RollVol_5))))
    message(sprintf("    RollVol_20 NAs    : %d",
                    sum(is.na(idx_feat$RollVol_20))))
    message(sprintf("    RollVol_30 NAs    : %d",
                    sum(is.na(idx_feat$RollVol_30))))
    if ("ReturnSource" %in% colnames(idx_feat)) {
      n_cls <- sum(idx_feat$ReturnSource == "Close", na.rm = TRUE)
      if (n_cls > 0)
        message(sprintf("    Close fallback    : %d row(s)", n_cls))
    }
  }

  # Constituent summary
  message(sprintf("\n  Constituents:"))
  message(sprintf("    Files processed   : %d OK,  %d errors",
                  n_ok, n_fail))

  total_crash3 <- sum(vapply(stock_results, function(r)
    if (is.null(r)) 0L else as.integer(r$n_crashes["crash3"]), integer(1L)))
  total_crash5 <- sum(vapply(stock_results, function(r)
    if (is.null(r)) 0L else as.integer(r$n_crashes["crash5"]), integer(1L)))
  total_rows   <- sum(vapply(stock_results, function(r)
    if (is.null(r)) 0L else nrow(r$features), integer(1L)))

  message(sprintf("    Total rows (panel): %d", total_rows))
  message(sprintf("    Total -3%% crashes  : %d", total_crash3))
  message(sprintf("    Total -5%% crashes  : %d", total_crash5))

  # Per-stock crash table header
  message(sprintf("\n  %-20s %-8s %-8s %-8s %-12s %-12s",
                  "NSESymbol", "Rows", "Cr3", "Cr5", "FirstDate", "LastDate"))
  message(sprintf("  %s", paste(rep("-", 72), collapse = "")))

  for (sym in names(stock_results)) {
    r <- stock_results[[sym]]
    if (is.null(r) || is.null(r$features)) {
      message(sprintf("  %-20s  ERROR", sym))
      next
    }
    f <- r$features
    message(sprintf("  %-20s %-8d %-8d %-8d %-12s %-12s",
                    sym,
                    nrow(f),
                    r$n_crashes["crash3"],
                    r$n_crashes["crash5"],
                    as.character(min(f$Date, na.rm = TRUE)),
                    as.character(max(f$Date, na.rm = TRUE))))
  }

  message(sprintf("\n  Feature report saved to: %s", PATH_FE_REPORT))
  message(sprintf("  Feature files saved to : %s", DIR_FEATURES))
  message(sep, "\n")
}


# ---------------------------------------------------------------------------
# 11. Main pipeline
# ---------------------------------------------------------------------------

main <- function() {

  message(rep("=", 72))
  message("  NIFTY 50 Feature Engineering Pipeline")
  message(sprintf("  Run started: %s", Sys.time()))
  message(rep("=", 72))

  # ---- 11.1  Create output directories ----------------------------------
  message("\n[Step 1] Creating output directories ...")
  ensure_dir(DIR_FEATURES)
  ensure_dir(DIR_FEAT_STOCKS)
  ensure_dir(DIR_QUALITY)

  # Master accumulator for all FE report rows
  all_report_rows <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )

  # ---- 11.2  Process NIFTY 50 index -------------------------------------
  message("\n[Step 2] Engineering features for NIFTY 50 index ...")

  raw_idx <- safe_read_csv(PATH_CLEAN_INDEX, "nifty50_index_clean.csv")
  if (is.null(raw_idx)) {
    stop("Index clean file not found.  Run 02_data_cleaning.R first.")
  }
  raw_idx$Date <- to_date(raw_idx$Date)

  message(sprintf("  Rows: %d  |  Date range: %s -> %s",
                  nrow(raw_idx),
                  min(raw_idx$Date, na.rm = TRUE),
                  max(raw_idx$Date, na.rm = TRUE)))

  idx_result <- engineer_features(raw_idx, label = "^NSEI")
  all_report_rows <- rbind(all_report_rows, idx_result$report)

  utils::write.csv(idx_result$features, PATH_FEAT_INDEX, row.names = FALSE)
  message(sprintf("  Saved: %s  (%d rows, %d columns)",
                  PATH_FEAT_INDEX,
                  nrow(idx_result$features),
                  ncol(idx_result$features)))

  # ---- 11.3  Process each constituent -----------------------------------
  message("\n[Step 3] Engineering features for constituent stocks ...")

  clean_files   <- list.files(DIR_CLEAN_PRICES, pattern = "\\.csv$",
                               full.names = TRUE)
  stock_results <- list()
  panel_frames  <- list()
  n_ok          <- 0L
  n_fail        <- 0L

  for (fpath in clean_files) {

    fname   <- basename(fpath)
    # Derive NSESymbol from inside the file (handles M_M.csv -> "M&M")
    raw_stk <- safe_read_csv(fpath, fname)
    if (is.null(raw_stk)) {
      n_fail <- n_fail + 1L
      next
    }

    raw_stk$Date <- to_date(raw_stk$Date)

    # Use NSESymbol from the file as the canonical label
    label <- if ("NSESymbol" %in% colnames(raw_stk) &&
                  any(!is.na(raw_stk$NSESymbol))) {
      unique(raw_stk$NSESymbol[!is.na(raw_stk$NSESymbol)])[1L]
    } else {
      tools::file_path_sans_ext(fname)
    }

    result <- tryCatch(
      engineer_features(raw_stk, label = label),
      error = function(e) {
        message(sprintf("  [ERROR] %s: %s", label, e$message))
        NULL
      }
    )

    if (is.null(result)) {
      n_fail <- n_fail + 1L
      all_report_rows <- fe_report_row(
        all_report_rows, label,
        "Feature engineering failed",
        NA_integer_,
        detail = "Unhandled error; see console output",
        action = "Skip this symbol; investigate before analysis"
      )
      next
    }

    n_ok <- n_ok + 1L
    stock_results[[label]] <- result
    panel_frames[[label]]  <- result$features
    all_report_rows        <- rbind(all_report_rows, result$report)

    # Save individual constituent feature file
    # Use safe filename for disk (mirrors 01/02 behaviour)
    out_name <- gsub("[^A-Za-z0-9._-]", "_", label)
    out_path <- file.path(DIR_FEAT_STOCKS, paste0(out_name, ".csv"))
    utils::write.csv(result$features, out_path, row.names = FALSE)

    message(sprintf("  [%d/%d] %-20s rows: %5d  cr3: %3d  cr5: %3d",
                    n_ok + n_fail,
                    length(clean_files),
                    label,
                    nrow(result$features),
                    result$n_crashes["crash3"],
                    result$n_crashes["crash5"]))
  }

  # ---- 11.4  Build stacked panel ----------------------------------------
  message("\n[Step 4] Building stacked constituent panel ...")

  if (length(panel_frames) > 0) {
    # Align columns: take the union of all column names, fill missing with NA
    all_cols <- unique(unlist(lapply(panel_frames, colnames)))
    panel_frames_aligned <- lapply(panel_frames, function(df) {
      missing_cols <- setdiff(all_cols, colnames(df))
      for (mc in missing_cols) df[[mc]] <- NA
      df[, all_cols, drop = FALSE]
    })
    panel_df <- do.call(rbind, panel_frames_aligned)
    rownames(panel_df) <- NULL

    utils::write.csv(panel_df, PATH_FEAT_PANEL, row.names = FALSE)
    message(sprintf("  Panel: %d rows x %d columns  |  Saved: %s",
                    nrow(panel_df), ncol(panel_df), PATH_FEAT_PANEL))
  } else {
    message("  No constituent feature frames to stack.")
  }

  # ---- 11.5  Save feature engineering report ----------------------------
  message("\n[Step 5] Saving feature engineering report ...")

  utils::write.csv(all_report_rows, PATH_FE_REPORT, row.names = FALSE)
  message(sprintf("  Report rows: %d  |  Saved: %s",
                  nrow(all_report_rows), PATH_FE_REPORT))

  # ---- 11.6  Console summary --------------------------------------------
  print_fe_summary(idx_result, stock_results, n_ok, n_fail)

  invisible(list(
    index_features = idx_result$features,
    stock_results  = stock_results,
    panel          = if (exists("panel_df")) panel_df else NULL,
    report         = all_report_rows
  ))
}


# ---------------------------------------------------------------------------
# 12. Entry point
# ---------------------------------------------------------------------------
# Set working directory to the project root before sourcing:
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/03_feature_engineering.R")

main()
