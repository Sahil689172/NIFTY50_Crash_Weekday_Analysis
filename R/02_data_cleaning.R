# =============================================================================
# Script  : 02_data_cleaning.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Data Cleaning Pipeline — inspect, validate, clean, and document
#           all raw data produced by 01_download_data.R.  Outputs go to
#           data/processed/.  Raw files are NEVER modified.
# Author  : [Your Name]
# Date    : 2026
# =============================================================================
#
# KNOWN RAW-DATA FACTS (observed from actual files, 2026-10-21)
# -------------------------------------------------------------
# • Index file  : data/raw/nifty50_index_yahoo.csv
#     Columns   : Date, Open, High, Low, Close, Adjusted, Volume,
#                 CompanyName, NSESymbol, YahooSymbol, Source
#     Rows      : 4715   |  Start: 2007-09-17  |  End: 2026-10-21
#     Note      : Yahoo Finance only provides ^NSEI data from Sep 2007.
#                 31 rows have NA Close (partial-day / halted sessions).
#     Volume    : Always 0 for the NIFTY 50 index — expected; NSE does not
#                 publish index-level volume through Yahoo.
#
# • Constituent master : data/raw/nifty50_constituents.csv
#     Columns   : CompanyName, NSESymbol, YahooSymbol, IndexName,
#                 ListSource, DownloadDate
#     Rows      : 50
#     Note      : CompanyName is NA for ALL rows.  The NSE Archives endpoint
#                 returned only Symbol data during download (the live CSV
#                 warm-up succeeded but the "Company Name" column was absent
#                 or blank in the response).  CompanyName is populated here
#                 from the constituent price files where possible, but
#                 remains NA if the stock-level file also has NA.
#                 DO NOT invent company names.
#
# • Constituent price files : data/raw/constituent_prices/<SYMBOL>.csv
#     Columns   : Date, Open, High, Low, Close, Adjusted, Volume,
#                 CompanyName, NSESymbol, YahooSymbol, Source
#     Note      : CompanyName is NA in every constituent file (same root
#                 cause as above).  NSESymbol / YahooSymbol are present.
#                 M&M is saved as M_M.csv (safe_filename sanitisation);
#                 NSESymbol inside the file is "M&M".
#                 Start dates vary widely (2000-01-03 to 2023-08-21).
#
# • Download log : data/raw/download_log.csv
#     All 51 entries (1 index + 50 stocks) have Status = "SUCCESS".
#
# RESEARCH LIMITATIONS
# --------------------
# 1. Survivorship bias: constituent list is current as of 2026-10-21.
#    Historical members that were removed are absent from this dataset.
# 2. Yahoo Finance NIFTY 50 history starts 2007-09-17, not 2000-01-01.
#    This constrains any analysis that requires the full index series.
# 3. Stocks have heterogeneous listing/data-start dates.  Missing early
#    prices reflect non-listing, not data errors, and must NOT be imputed.
# 4. NA Close prices are NOT assumed to indicate trading halts.  They are
#    treated as genuinely missing observations and flagged accordingly.
# 5. No price imputation of any kind is performed (no mean, median, zero,
#    interpolation, or forward-fill).
#
# PACKAGES REQUIRED (base R only — no installation needed)
# ---------------------------------------------------------
#   utils, tools, stats — all part of base R
# =============================================================================


# ---------------------------------------------------------------------------
# 0. Safety guard — abort if working directory is wrong
# ---------------------------------------------------------------------------
# Run this script from the project root:
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/02_data_cleaning.R")

if (!file.exists(file.path("data", "raw", "nifty50_index_yahoo.csv"))) {
  stop(
    "Working directory does not appear to be the project root.\n",
    "Please run:\n",
    '  setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")\n',
    '  source("R/02_data_cleaning.R")'
  )
}


# ---------------------------------------------------------------------------
# 1. Directory layout (read-only raw; write only to processed)
# ---------------------------------------------------------------------------

DIR_RAW              <- file.path("data", "raw")
DIR_RAW_PRICES       <- file.path(DIR_RAW, "constituent_prices")
DIR_PROCESSED        <- file.path("data", "processed")
DIR_PROC_PRICES      <- file.path(DIR_PROCESSED, "constituent_prices_clean")
DIR_QUALITY          <- file.path(DIR_PROCESSED, "quality_reports")

PATH_RAW_INDEX       <- file.path(DIR_RAW, "nifty50_index_yahoo.csv")
PATH_RAW_CONST       <- file.path(DIR_RAW, "nifty50_constituents.csv")
PATH_RAW_LOG         <- file.path(DIR_RAW, "download_log.csv")

PATH_CLEAN_INDEX     <- file.path(DIR_PROCESSED, "nifty50_index_clean.csv")
PATH_CLEAN_CONST     <- file.path(DIR_PROCESSED, "nifty50_constituents_clean.csv")

PATH_RPT_MISSING     <- file.path(DIR_QUALITY, "missing_values_report.csv")
PATH_RPT_QUALITY     <- file.path(DIR_QUALITY, "data_quality_report.csv")
PATH_RPT_SUMMARY     <- file.path(DIR_QUALITY, "cleaning_summary.csv")

# Numeric price/volume columns present in every price file
PRICE_COLS  <- c("Open", "High", "Low", "Close", "Adjusted", "Volume")
# Metadata columns present in every price file
META_COLS   <- c("CompanyName", "NSESymbol", "YahooSymbol", "Source")
# All expected columns in a price file
PRICE_SCHEMA <- c("Date", PRICE_COLS, META_COLS)

# Numeric price columns (excluding Volume — treated separately)
OHLCA_COLS  <- c("Open", "High", "Low", "Close", "Adjusted")

# Strings that should be read as NA
NA_STRINGS  <- c("", "NA", "N/A", "n/a", "NULL", "null", ".", "#N/A",
                 "#VALUE!", "#REF!", "NaN", "-", "--", "?")


# ---------------------------------------------------------------------------
# 2. Utility functions
# ---------------------------------------------------------------------------

#' Create a directory if it does not already exist.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("  Created: %s", path))
  }
}


#' Read a CSV and return a data frame.  Returns NULL with a message on error.
#' @param path  File path to read.
#' @param label Short label for console messages.
safe_read_csv <- function(path, label = path) {
  if (!file.exists(path)) {
    message(sprintf("  [MISSING FILE] %s", label))
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


#' Coerce a column to numeric, turning non-numeric strings into NA.
#' Does not silently suppress warnings from as.numeric(); captures them
#' and counts how many values were coerced.
#' @param x     Atomic vector to coerce.
#' @param label Column name, for reporting.
#' @return List: $values (numeric), $n_coerced (int).
coerce_numeric <- function(x, label = "") {
  already_numeric <- is.numeric(x)
  if (already_numeric) {
    return(list(values = x, n_coerced = 0L))
  }
  n_was_na <- sum(is.na(x))
  suppressWarnings({
    num <- as.numeric(x)
  })
  n_coerced <- sum(is.na(num)) - n_was_na
  list(values = num, n_coerced = max(0L, n_coerced))
}


#' Trim leading/trailing whitespace and normalise internal runs of spaces.
#' Converts blank strings (after trim) to NA.
clean_character <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s{2,}", " ", x)            # collapse multiple spaces
  x[nchar(x) == 0 | x %in% NA_STRINGS] <- NA_character_
  x
}


#' Parse a character vector as Date (tries YYYY-MM-DD then YYYY/MM/DD).
#' @return List: $dates (Date), $n_failed (int).
parse_dates <- function(x) {
  if (inherits(x, "Date")) {
    return(list(dates = x, n_failed = 0L))
  }
  d <- suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))
  failed_idx <- which(is.na(d) & !is.na(x))
  if (length(failed_idx) > 0) {
    # Try alternative format
    d[failed_idx] <- suppressWarnings(
      as.Date(as.character(x[failed_idx]), format = "%Y/%m/%d")
    )
  }
  n_failed <- sum(is.na(d) & !is.na(x))
  list(dates = d, n_failed = n_failed)
}


#' Count NA values per column and return a summary data frame.
na_summary <- function(df, label) {
  cols   <- colnames(df)
  counts <- vapply(df, function(v) sum(is.na(v)), integer(1L))
  pcts   <- round(100 * counts / max(nrow(df), 1L), 2)
  data.frame(
    Dataset    = label,
    Column     = cols,
    NA_Count   = counts,
    NA_Pct     = pcts,
    Total_Rows = nrow(df),
    row.names  = NULL,
    stringsAsFactors = FALSE
  )
}


#' Detect OHLC inconsistency: rows where High < Low, or any price <= 0
#' (excluding Volume), or Adjusted < 0.
#' Returns a logical vector TRUE for rows with any inconsistency.
flag_ohlc_inconsistency <- function(df) {
  n  <- nrow(df)
  ok <- rep(FALSE, n)

  has <- function(col) col %in% colnames(df) && is.numeric(df[[col]])

  if (has("High") && has("Low")) {
    ok <- ok | (!is.na(df$High) & !is.na(df$Low) & df$High < df$Low)
  }
  for (col in c("Open", "High", "Low", "Close")) {
    if (has(col)) {
      ok <- ok | (!is.na(df[[col]]) & df[[col]] <= 0)
    }
  }
  if (has("Adjusted")) {
    ok <- ok | (!is.na(df$Adjusted) & df$Adjusted < 0)
  }
  ok
}


#' Detect Inf / -Inf values in numeric columns.
flag_infinite <- function(df) {
  n <- nrow(df)
  inf_flag <- rep(FALSE, n)
  for (col in intersect(PRICE_COLS, colnames(df))) {
    if (is.numeric(df[[col]])) {
      inf_flag <- inf_flag | is.infinite(df[[col]])
    }
  }
  inf_flag
}


#' Detect negative Volume (valid: 0 or NA for indices, positive for stocks).
flag_negative_volume <- function(df) {
  if (!"Volume" %in% colnames(df) || !is.numeric(df$Volume)) {
    return(rep(FALSE, nrow(df)))
  }
  !is.na(df$Volume) & df$Volume < 0
}


#' Find duplicate Date rows.  Returns list: $dup_flag (logical), $dup_dates.
find_duplicate_dates <- function(df) {
  if (!"Date" %in% colnames(df) || !inherits(df$Date, "Date")) {
    return(list(dup_flag = rep(FALSE, nrow(df)), dup_dates = character(0)))
  }
  dups       <- duplicated(df$Date) | duplicated(df$Date, fromLast = TRUE)
  dup_dates  <- as.character(sort(unique(df$Date[dups])))
  list(dup_flag = dups, dup_dates = dup_dates)
}


#' Append a row to a quality-issues data frame.
add_issue <- function(issues_df, dataset, check, n_affected,
                      pct = NA_real_, detail = "", action = "") {
  rbind(issues_df, data.frame(
    Dataset    = dataset,
    Check      = check,
    N_Affected = n_affected,
    Pct        = ifelse(is.na(pct), NA_real_,
                        round(as.numeric(pct), 3)),
    Detail     = detail,
    Action     = action,
    stringsAsFactors = FALSE
  ))
}


# ---------------------------------------------------------------------------
# 3. Core cleaning function for a single price data frame
# ---------------------------------------------------------------------------

#' Clean one price data frame (index or constituent stock).
#'
#' Steps performed, in order:
#'   1. Verify expected columns are present.
#'   2. Clean metadata columns (trim, NA-ify blanks).
#'   3. Parse Date column; flag invalid dates.
#'   4. Sort chronologically.
#'   5. Detect duplicate dates; investigate conflicting values.
#'   6. Coerce numeric columns; count coercion failures.
#'   7. Replace Inf/-Inf with NA; flag.
#'   8. Flag negative prices, zero prices (OHLCA only), negative Volume.
#'   9. Flag OHLC inconsistencies.
#'  10. Add Weekday column.
#'  11. Exclude rows with NA Date (undatable rows cannot be used in any
#'      time-series analysis).
#'  12. For exact duplicate dates with IDENTICAL values: keep first,
#'      document count.
#'  13. For conflicting duplicate dates (same date, different values):
#'      flag for manual review, keep all rows, do NOT silently drop.
#'  14. Never impute or remove rows solely because Close/Adjusted is NA.
#'
#' @param df          Data frame read from a raw price CSV.
#' @param label       Short identifier (e.g. "^NSEI", "RELIANCE").
#' @param is_index    Logical. TRUE for the NIFTY 50 index.
#' @return List:
#'   $clean        — cleaned data frame
#'   $issues       — data frame of issues found
#'   $excluded     — data frame of excluded rows with reason
#'   $na_summary   — data frame of NA counts per column
clean_price_df <- function(df, label, is_index = FALSE) {

  n_raw    <- nrow(df)
  issues   <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )
  excluded <- data.frame(
    Dataset = character(), Date = character(),
    NSESymbol = character(), Reason = character(),
    stringsAsFactors = FALSE
  )

  # ---- 3.1  Column presence check ----------------------------------------
  missing_cols <- setdiff(PRICE_SCHEMA, colnames(df))
  if (length(missing_cols) > 0) {
    issues <- add_issue(issues, label, "Missing expected columns",
                        length(missing_cols),
                        detail = paste(missing_cols, collapse = ", "),
                        action = "Column absent; NA added where possible")
    for (mc in missing_cols) df[[mc]] <- NA
  }

  # ---- 3.2  Clean metadata columns ---------------------------------------
  for (col in intersect(META_COLS, colnames(df))) {
    df[[col]] <- clean_character(as.character(df[[col]]))
  }

  # Count missing metadata
  for (col in META_COLS) {
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) {
      issues <- add_issue(
        issues, label, sprintf("Missing %s", col), n_na,
        pct  = 100 * n_na / n_raw,
        detail = sprintf("%d rows have NA %s", n_na, col),
        action = ifelse(
          col == "CompanyName",
          paste0("CompanyName NA in all constituent files due to ",
                 "NSE warm-up parse issue; not invented; remains NA"),
          "Flag only; not imputed"
        )
      )
    }
  }

  # Standardise Source capitalisation
  if ("Source" %in% colnames(df)) {
    df$Source <- sub("yahoo finance", "Yahoo Finance",
                     df$Source, ignore.case = TRUE)
  }

  # ---- 3.3  Date parsing -------------------------------------------------
  date_res  <- parse_dates(df$Date)
  df$Date   <- date_res$dates

  if (date_res$n_failed > 0) {
    issues <- add_issue(
      issues, label, "Unparseable Date values",
      date_res$n_failed,
      pct    = 100 * date_res$n_failed / n_raw,
      detail = "Could not convert to Date object",
      action = "Row excluded: date-less rows cannot be placed in time series"
    )
  }

  # ---- 3.4  Sort by Date -------------------------------------------------
  df <- df[order(df$Date, na.last = TRUE), ]

  # ---- 3.5  Duplicate dates ----------------------------------------------
  dup_res  <- find_duplicate_dates(df)
  n_dups   <- sum(dup_res$dup_flag)

  if (n_dups > 0) {
    dup_detail <- paste0(length(dup_res$dup_dates), " date(s): ",
                         paste(head(dup_res$dup_dates, 5), collapse = ", "),
                         if (length(dup_res$dup_dates) > 5) " ..." else "")

    dup_rows <- df[dup_res$dup_flag, , drop = FALSE]

    # Identify exact duplicates (all PRICE_COLS identical for same date)
    # vs conflicting duplicates (same date but different price values)
    dup_dates_vec  <- dup_res$dup_dates
    exact_drop_idx <- integer(0)
    conflict_dates <- character(0)

    for (dd in dup_dates_vec) {
      rows_idx <- which(df$Date == as.Date(dd))
      if (length(rows_idx) < 2) next

      sub <- df[rows_idx, PRICE_COLS, drop = FALSE]
      # Check if all duplicate rows are identical (ignoring row names)
      if (nrow(unique(sub)) == 1L) {
        # Exact duplicates: keep the first occurrence, mark rest for removal
        exact_drop_idx <- c(exact_drop_idx, rows_idx[-1])
      } else {
        conflict_dates <- c(conflict_dates, dd)
      }
    }

    n_exact    <- length(exact_drop_idx)
    n_conflict <- length(conflict_dates)

    if (n_exact > 0) {
      issues <- add_issue(
        issues, label, "Exact duplicate dates (all OHLCAV identical)",
        n_exact,
        detail = sprintf("%d rows removed (identical values)", n_exact),
        action = "Kept first occurrence; extras removed"
      )
      # Record in excluded before dropping
      ex_rows <- df[exact_drop_idx, , drop = FALSE]
      for (k in seq_len(nrow(ex_rows))) {
        excluded <- rbind(excluded, data.frame(
          Dataset   = label,
          Date      = as.character(ex_rows$Date[k]),
          NSESymbol = ifelse("NSESymbol" %in% colnames(ex_rows),
                             ex_rows$NSESymbol[k], NA_character_),
          Reason    = "Exact duplicate date — identical OHLCAV values; kept first",
          stringsAsFactors = FALSE
        ))
      }
      df <- df[-exact_drop_idx, ]
    }

    if (n_conflict > 0) {
      issues <- add_issue(
        issues, label, "CONFLICTING duplicate dates (different OHLCAV values)",
        n_conflict,
        detail = paste0(
          "Dates: ", paste(head(conflict_dates, 5), collapse = ", "),
          if (length(conflict_dates) > 5) " ..." else ""
        ),
        action = paste0(
          "MANUAL REVIEW REQUIRED — all conflicting rows retained; ",
          "investigate source of discrepancy before analysis"
        )
      )
    }

    # Rebuild dup flag after removals
    dup_res2 <- find_duplicate_dates(df)
    n_dups2  <- sum(dup_res2$dup_flag)
    if (n_dups2 == 0 && n_conflict == 0) {
      # Successfully resolved
    }
  }

  # ---- 3.6  Coerce numeric columns ---------------------------------------
  total_coerced <- 0L
  for (col in intersect(PRICE_COLS, colnames(df))) {
    res <- coerce_numeric(df[[col]], col)
    df[[col]] <- res$values
    total_coerced <- total_coerced + res$n_coerced
    if (res$n_coerced > 0) {
      issues <- add_issue(
        issues, label, sprintf("Non-numeric strings in %s", col),
        res$n_coerced,
        pct    = 100 * res$n_coerced / nrow(df),
        detail = sprintf("%d values coerced to NA", res$n_coerced),
        action = "Converted to NA; not imputed"
      )
    }
  }

  # ---- 3.7  Inf / -Inf ---------------------------------------------------
  inf_flag <- flag_infinite(df)
  n_inf    <- sum(inf_flag)
  if (n_inf > 0) {
    issues <- add_issue(
      issues, label, "Infinite values in numeric columns",
      n_inf,
      pct    = 100 * n_inf / nrow(df),
      detail = sprintf("%d Inf/-Inf values across OHLCAV/Volume", n_inf),
      action = "Replaced with NA; not imputed"
    )
    for (col in intersect(PRICE_COLS, colnames(df))) {
      if (is.numeric(df[[col]])) {
        df[[col]][is.infinite(df[[col]])] <- NA_real_
      }
    }
  }

  # ---- 3.8  Negative prices, zero OHLCA, negative Volume ----------------

  # Negative OHLCA prices
  for (col in intersect(OHLCA_COLS, colnames(df))) {
    if (is.numeric(df[[col]])) {
      n_neg <- sum(!is.na(df[[col]]) & df[[col]] < 0)
      if (n_neg > 0) {
        issues <- add_issue(
          issues, label, sprintf("Negative %s price", col),
          n_neg,
          pct    = 100 * n_neg / nrow(df),
          detail = "Price below zero is economically impossible",
          action = "Replaced with NA; not imputed"
        )
        df[[col]][!is.na(df[[col]]) & df[[col]] < 0] <- NA_real_
      }
    }
  }

  # Zero OHLCA prices (not Volume — zero volume is valid for the index)
  for (col in intersect(OHLCA_COLS, colnames(df))) {
    if (is.numeric(df[[col]])) {
      n_zero <- sum(!is.na(df[[col]]) & df[[col]] == 0)
      if (n_zero > 0) {
        issues <- add_issue(
          issues, label, sprintf("Zero %s price", col),
          n_zero,
          pct    = 100 * n_zero / nrow(df),
          detail = paste0(
            "Zero price in ", col, " is implausible for a traded security; ",
            "may indicate a data placeholder"
          ),
          action = "Replaced with NA; flagged for manual review"
        )
        df[[col]][!is.na(df[[col]]) & df[[col]] == 0] <- NA_real_
      }
    }
  }

  # Negative Volume
  neg_vol <- flag_negative_volume(df)
  if (sum(neg_vol) > 0) {
    issues <- add_issue(
      issues, label, "Negative Volume",
      sum(neg_vol),
      pct    = 100 * sum(neg_vol) / nrow(df),
      detail = "Volume cannot be negative",
      action = "Replaced with NA; not imputed"
    )
    df$Volume[neg_vol] <- NA_real_
  }

  # ---- 3.9  OHLC consistency ---------------------------------------------
  ohlc_bad <- flag_ohlc_inconsistency(df)
  n_ohlc   <- sum(ohlc_bad)
  if (n_ohlc > 0) {
    issues <- add_issue(
      issues, label, "OHLC inconsistency (High < Low or price <= 0)",
      n_ohlc,
      pct    = 100 * n_ohlc / nrow(df),
      detail = paste0(
        "Rows where High < Low, Open/High/Low/Close <= 0 ",
        "after previous cleaning steps"
      ),
      action = paste0(
        "Added flag column OHLCFlag = TRUE; rows retained; ",
        "manual review recommended before analysis"
      )
    )
  }
  df$OHLCFlag <- ohlc_bad

  # ---- 3.10  Add Weekday column ------------------------------------------
  # Weekday is derived from Date only; never manufactured for missing dates.
  df$Weekday <- NA_character_
  valid_date  <- !is.na(df$Date)
  if (any(valid_date)) {
    df$Weekday[valid_date] <- weekdays(df$Date[valid_date])
  }

  # ---- 3.11  Exclude rows with NA Date -----------------------------------
  na_date_idx <- which(is.na(df$Date))
  if (length(na_date_idx) > 0) {
    ex_rows <- df[na_date_idx, , drop = FALSE]
    for (k in seq_len(nrow(ex_rows))) {
      excluded <- rbind(excluded, data.frame(
        Dataset   = label,
        Date      = NA_character_,
        NSESymbol = ifelse("NSESymbol" %in% colnames(ex_rows),
                           ex_rows$NSESymbol[k], NA_character_),
        Reason    = "NA Date — row cannot be placed in time series; excluded",
        stringsAsFactors = FALSE
      ))
    }
    df <- df[!is.na(df$Date), ]
  }

  # ---- 3.12  Missing Close / Adjusted: flag, never impute ----------------
  if ("Close" %in% colnames(df) && is.numeric(df$Close)) {
    n_na_close <- sum(is.na(df$Close))
    if (n_na_close > 0) {
      issues <- add_issue(
        issues, label, "Missing Close price",
        n_na_close,
        pct    = 100 * n_na_close / nrow(df),
        detail = paste0(
          "NA Close on ", n_na_close, " row(s). ",
          if (is_index) {
            paste0("For the NIFTY 50 index, Yahoo Finance records some days ",
                   "with partial/zero-volume sessions as NA. ")
          } else {
            ""
          },
          "Cause is NOT assumed to be a trading halt."
        ),
        action = paste0(
          "Rows retained with NA Close. Return cannot be computed for ",
          "these rows and must be excluded in later analysis steps only, ",
          "not here."
        )
      )
    }
  }

  # ---- Final NA summary --------------------------------------------------
  na_smry <- na_summary(df, label)

  list(
    clean      = df,
    issues     = issues,
    excluded   = excluded,
    na_summary = na_smry,
    n_raw      = n_raw,
    n_clean    = nrow(df)
  )
}


# ---------------------------------------------------------------------------
# 4. Clean the constituent master list
# ---------------------------------------------------------------------------

#' Clean the constituent master CSV.
#' Checks: column presence, whitespace, blanks, NSESymbol vs YahooSymbol
#' consistency (.NS suffix), duplicate symbols, DownloadDate validity.
#' Does NOT invent CompanyName where it is NA.
#'
#' @param df  Data frame read from nifty50_constituents.csv.
#' @return List: $clean, $issues, $na_summary.
clean_constituents <- function(df) {

  label    <- "nifty50_constituents"
  n_raw    <- nrow(df)
  issues   <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )

  expected_cols <- c("CompanyName", "NSESymbol", "YahooSymbol",
                     "IndexName", "ListSource", "DownloadDate")
  missing_cols  <- setdiff(expected_cols, colnames(df))
  if (length(missing_cols) > 0) {
    issues <- add_issue(issues, label, "Missing expected columns",
                        length(missing_cols),
                        detail = paste(missing_cols, collapse = ", "),
                        action = "NA added for missing columns")
    for (mc in missing_cols) df[[mc]] <- NA_character_
  }

  # Trim and blank-to-NA for all character columns
  for (col in colnames(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- clean_character(df[[col]])
    }
  }

  # CompanyName NA
  n_na_name <- sum(is.na(df$CompanyName))
  if (n_na_name > 0) {
    issues <- add_issue(
      issues, label, "Missing CompanyName",
      n_na_name,
      pct    = 100 * n_na_name / n_raw,
      detail = paste0(
        "CompanyName is NA for all ", n_na_name, " rows. ",
        "Root cause: NSE Archives endpoint returned only Symbol data ",
        "during 01_download_data.R run; 'Company Name' column was blank."
      ),
      action = paste0(
        "CompanyName NOT invented. Remains NA. Re-run 01_download_data.R ",
        "at a different time or download the NSE CSV manually and re-process."
      )
    )
  }

  # NSESymbol missing or blank
  n_na_sym <- sum(is.na(df$NSESymbol))
  if (n_na_sym > 0) {
    issues <- add_issue(issues, label, "Missing NSESymbol",
                        n_na_sym,
                        pct    = 100 * n_na_sym / n_raw,
                        detail = "NSESymbol is the primary key; NA rows are unusable",
                        action = "Flag; manual review required")
  }

  # Duplicate NSESymbol
  dup_sym <- duplicated(df$NSESymbol) | duplicated(df$NSESymbol, fromLast = TRUE)
  n_dup   <- sum(dup_sym, na.rm = TRUE)
  if (n_dup > 0) {
    dup_vals <- unique(df$NSESymbol[dup_sym])
    issues <- add_issue(
      issues, label, "Duplicate NSESymbol",
      n_dup,
      detail = paste(dup_vals, collapse = ", "),
      action = "Flag; manual review before analysis"
    )
  }

  # YahooSymbol consistency: should be NSESymbol + ".NS"
  # (except symbols that already contain "^")
  valid_nse <- !is.na(df$NSESymbol)
  expected_yahoo <- paste0(df$NSESymbol, ".NS")
  mismatch <- valid_nse & !is.na(df$YahooSymbol) &
    df$YahooSymbol != expected_yahoo
  n_mismatch <- sum(mismatch, na.rm = TRUE)
  if (n_mismatch > 0) {
    issues <- add_issue(
      issues, label, "YahooSymbol does not match NSESymbol + '.NS'",
      n_mismatch,
      detail = paste(df$NSESymbol[mismatch], collapse = ", "),
      action = "Flag; verify ticker mapping before Yahoo Finance lookups"
    )
  }

  # IndexName standardisation
  if ("IndexName" %in% colnames(df)) {
    df$IndexName <- gsub("nifty 50", "NIFTY 50",
                         df$IndexName, ignore.case = TRUE)
  }

  # DownloadDate validity
  if ("DownloadDate" %in% colnames(df)) {
    dd_res <- parse_dates(df$DownloadDate)
    n_bad_dd <- dd_res$n_failed
    if (n_bad_dd > 0) {
      issues <- add_issue(
        issues, label, "Invalid DownloadDate",
        n_bad_dd,
        detail = "Could not parse as a date",
        action = "Flag; does not affect price data"
      )
    }
  }

  na_smry <- na_summary(df, label)

  list(clean = df, issues = issues, na_summary = na_smry)
}


# ---------------------------------------------------------------------------
# 5. Cross-validation: constituent files vs master list
# ---------------------------------------------------------------------------

#' Compare the set of constituent price files on disk against the master list.
#' Reports symbols in the master but missing files, files on disk not in master,
#' and NSESymbol mismatches inside each file vs the master.
#'
#' @param constituents  Clean constituent master data frame.
#' @param price_dir     Path to constituent_prices directory.
#' @return Data frame of cross-validation issues.
cross_validate_constituents <- function(constituents, price_dir) {

  issues <- data.frame(
    Dataset = character(), Check = character(),
    N_Affected = integer(), Pct = numeric(),
    Detail = character(), Action = character(),
    stringsAsFactors = FALSE
  )

  disk_files <- list.files(price_dir, pattern = "\\.csv$", full.names = FALSE)
  disk_syms  <- tools::file_path_sans_ext(disk_files)   # strip .csv

  master_syms <- constituents$NSESymbol[!is.na(constituents$NSESymbol)]

  # Map master symbol -> safe filename equivalent (mirrors safe_filename() in 01)
  master_safe <- gsub("[^A-Za-z0-9._-]", "_", master_syms)

  # Symbols in master but no file on disk
  no_file <- master_syms[!master_safe %in% disk_syms]
  if (length(no_file) > 0) {
    issues <- add_issue(
      issues, "cross_validation",
      "Master list symbol has no price file on disk",
      length(no_file),
      detail = paste(no_file, collapse = ", "),
      action = "Re-run 01_download_data.R or investigate download failure"
    )
  }

  # Files on disk not matched by any master symbol
  unmatched_files <- disk_syms[!disk_syms %in% master_safe]
  if (length(unmatched_files) > 0) {
    issues <- add_issue(
      issues, "cross_validation",
      "Price file on disk has no matching master list symbol",
      length(unmatched_files),
      detail = paste(unmatched_files, collapse = ", "),
      action = paste0(
        "Possible stale file from a previous download; ",
        "verify before including in analysis"
      )
    )
  }

  issues
}


# ---------------------------------------------------------------------------
# 6. Produce quality reports
# ---------------------------------------------------------------------------

#' Aggregate issues from all datasets into quality report data frames.
#' @param all_issues  List of issue data frames.
#' @param all_na      List of NA-summary data frames.
#' @param all_excluded List of excluded-row data frames.
#' @param log_df      Download log data frame.
#' @param constituents Clean constituent master data frame.
#' @param stock_results List of per-stock clean results.
build_reports <- function(all_issues, all_na, all_excluded,
                          log_df, constituents, stock_results) {

  # ---- Missing values report ----
  mv_report <- do.call(rbind, all_na)
  rownames(mv_report) <- NULL

  # ---- Data quality report ----
  dq_report <- do.call(rbind, all_issues)
  rownames(dq_report) <- NULL

  # ---- Cleaning summary ----
  n_const    <- nrow(constituents)
  n_success  <- if (!is.null(log_df)) sum(log_df$Status == "SUCCESS") else NA
  n_failed   <- if (!is.null(log_df)) sum(log_df$Status == "FAILED")  else NA

  stock_start_dates <- vapply(stock_results, function(r) {
    if (is.null(r) || is.null(r$clean) || nrow(r$clean) == 0) return(NA_character_)
    as.character(min(r$clean$Date, na.rm = TRUE))
  }, character(1L))

  stock_end_dates <- vapply(stock_results, function(r) {
    if (is.null(r) || is.null(r$clean) || nrow(r$clean) == 0) return(NA_character_)
    as.character(max(r$clean$Date, na.rm = TRUE))
  }, character(1L))

  stock_raw_rows  <- vapply(stock_results, function(r) {
    if (is.null(r)) return(NA_integer_); as.integer(r$n_raw)
  }, integer(1L))

  stock_clean_rows <- vapply(stock_results, function(r) {
    if (is.null(r)) return(NA_integer_); as.integer(r$n_clean)
  }, integer(1L))

  stock_names <- names(stock_results)

  summary_rows <- data.frame(
    Metric = c(
      "Constituents in master list",
      "Successful downloads (log)",
      "Failed downloads (log)",
      "Stock price files processed",
      "Stock files with data issues",
      "All CompanyName NA (known data gap)",
      "Index history start (Yahoo)",
      "Index history start caveat",
      "Cleaning run date"
    ),
    Value = c(
      as.character(n_const),
      as.character(n_success),
      as.character(n_failed),
      as.character(length(stock_results)),
      as.character(sum(vapply(stock_results, function(r) {
        if (is.null(r)) return(0L); as.integer(nrow(r$issues) > 0)
      }, integer(1L)))),
      "TRUE — see data_quality_report.csv for root cause",
      "2007-09-17",
      paste0("Yahoo Finance does not provide ^NSEI data before Sep 2007. ",
             "This is a known data limitation, not an error."),
      as.character(Sys.Date())
    ),
    stringsAsFactors = FALSE
  )

  # Per-stock history summary
  stock_summary <- data.frame(
    NSESymbol  = stock_names,
    RawRows    = stock_raw_rows,
    CleanRows  = stock_clean_rows,
    FirstDate  = stock_start_dates,
    LastDate   = stock_end_dates,
    stringsAsFactors = FALSE
  )
  rownames(stock_summary) <- NULL

  list(
    missing_values = mv_report,
    data_quality   = dq_report,
    summary        = summary_rows,
    stock_summary  = stock_summary
  )
}


# ---------------------------------------------------------------------------
# 7. Console summary printer
# ---------------------------------------------------------------------------

print_console_summary <- function(reports, n_stocks_ok, n_stocks_fail) {

  sep <- paste(rep("=", 72), collapse = "")
  message("\n", sep)
  message("  DATA CLEANING SUMMARY")
  message(sep)

  message("\n  Overall:")
  for (i in seq_len(nrow(reports$summary))) {
    message(sprintf("    %-45s %s",
                    reports$summary$Metric[i],
                    reports$summary$Value[i]))
  }

  total_issues <- nrow(reports$data_quality)
  message(sprintf("\n  Quality checks fired   : %d", total_issues))
  message(sprintf("  Stock files cleaned OK : %d", n_stocks_ok))
  message(sprintf("  Stock files errored    : %d", n_stocks_fail))

  # Highlight anything needing manual review
  manual <- reports$data_quality[
    grepl("MANUAL REVIEW|manual review", reports$data_quality$Action,
          ignore.case = TRUE), , drop = FALSE
  ]
  if (nrow(manual) > 0) {
    message(sprintf("\n  *** MANUAL REVIEW REQUIRED (%d item(s)) ***",
                    nrow(manual)))
    for (i in seq_len(nrow(manual))) {
      message(sprintf("    [%s] %s — %s",
                      manual$Dataset[i],
                      manual$Check[i],
                      manual$Detail[i]))
    }
  }

  message(sprintf("\n  Reports saved to: %s", DIR_QUALITY))
  message(sprintf("  Cleaned files saved to: %s", DIR_PROCESSED))
  message(sep, "\n")
}


# ---------------------------------------------------------------------------
# 8. Main pipeline
# ---------------------------------------------------------------------------

main <- function() {

  message(rep("=", 72))
  message("  NIFTY 50 Data Cleaning Pipeline")
  message(sprintf("  Run started: %s", Sys.time()))
  message(rep("=", 72))

  # ---- 8.1  Create output directories ------------------------------------
  message("\n[Step 1] Creating output directories ...")
  ensure_dir(DIR_PROCESSED)
  ensure_dir(DIR_PROC_PRICES)
  ensure_dir(DIR_QUALITY)

  # Accumulators
  all_issues   <- list()
  all_na       <- list()
  all_excluded <- list()

  # ---- 8.2  Clean NIFTY 50 index -----------------------------------------
  message("\n[Step 2] Cleaning NIFTY 50 index ...")

  raw_index <- safe_read_csv(PATH_RAW_INDEX, "nifty50_index_yahoo.csv")
  if (is.null(raw_index)) {
    stop("Cannot read index file. Ensure 01_download_data.R has been run.")
  }

  message(sprintf("  Raw rows: %d | Columns: %s",
                  nrow(raw_index),
                  paste(colnames(raw_index), collapse = ", ")))

  idx_result    <- clean_price_df(raw_index, label = "^NSEI", is_index = TRUE)
  all_issues[["index"]]   <- idx_result$issues
  all_na[["index"]]       <- idx_result$na_summary
  all_excluded[["index"]] <- idx_result$excluded

  utils::write.csv(idx_result$clean, PATH_CLEAN_INDEX, row.names = FALSE)
  message(sprintf("  Cleaned rows: %d  |  Saved: %s",
                  idx_result$n_clean, PATH_CLEAN_INDEX))

  # ---- 8.3  Clean constituent master -------------------------------------
  message("\n[Step 3] Cleaning constituent master list ...")

  raw_const <- safe_read_csv(PATH_RAW_CONST, "nifty50_constituents.csv")
  if (is.null(raw_const)) {
    message("  WARNING: Constituent master file not found. Skipping.")
    const_result <- list(clean = data.frame(), issues = data.frame(),
                         na_summary = data.frame())
  } else {
    message(sprintf("  Raw rows: %d | Columns: %s",
                    nrow(raw_const),
                    paste(colnames(raw_const), collapse = ", ")))
    const_result <- clean_constituents(raw_const)
  }

  all_issues[["constituents"]]   <- const_result$issues
  all_na[["constituents"]]       <- const_result$na_summary

  utils::write.csv(const_result$clean, PATH_CLEAN_CONST, row.names = FALSE)
  message(sprintf("  Cleaned rows: %d  |  Saved: %s",
                  nrow(const_result$clean), PATH_CLEAN_CONST))

  # ---- 8.4  Cross-validate files vs master --------------------------------
  message("\n[Step 4] Cross-validating constituent price files vs master ...")

  xval_issues <- cross_validate_constituents(const_result$clean, DIR_RAW_PRICES)
  all_issues[["cross_validation"]] <- xval_issues

  if (nrow(xval_issues) == 0) {
    message("  All price files match master list — OK")
  } else {
    for (i in seq_len(nrow(xval_issues))) {
      message(sprintf("  [%s] %s (%d affected)",
                      xval_issues$Check[i],
                      xval_issues$Detail[i],
                      xval_issues$N_Affected[i]))
    }
  }

  # ---- 8.5  Clean each constituent's price file ---------------------------
  message("\n[Step 5] Cleaning constituent price files ...")

  price_files  <- list.files(DIR_RAW_PRICES, pattern = "\\.csv$",
                              full.names = TRUE)
  stock_results <- list()
  n_ok   <- 0L
  n_fail <- 0L

  for (fpath in price_files) {

    fname   <- basename(fpath)
    sym_raw <- tools::file_path_sans_ext(fname)   # e.g. "M_M", "RELIANCE"

    raw_stock <- safe_read_csv(fpath, sym_raw)
    if (is.null(raw_stock)) {
      n_fail <- n_fail + 1L
      message(sprintf("  [SKIP] %s — could not read file", sym_raw))
      next
    }

    # Use NSESymbol from inside the file as the canonical label
    # (handles M_M.csv -> "M&M" correctly)
    label <- if ("NSESymbol" %in% colnames(raw_stock) &&
                 !all(is.na(raw_stock$NSESymbol))) {
      unique(raw_stock$NSESymbol[!is.na(raw_stock$NSESymbol)])[1]
    } else {
      sym_raw
    }

    result <- tryCatch(
      clean_price_df(raw_stock, label = label, is_index = FALSE),
      error = function(e) {
        message(sprintf("  [ERROR] %s: %s", label, e$message))
        NULL
      }
    )

    if (is.null(result)) {
      n_fail <- n_fail + 1L
      next
    }

    n_ok <- n_ok + 1L
    stock_results[[label]] <- result

    all_issues[[label]]   <- result$issues
    all_na[[label]]       <- result$na_summary
    all_excluded[[label]] <- result$excluded

    # Save cleaned file — use safe_filename so M&M -> M_M.csv on output too
    out_sym  <- gsub("[^A-Za-z0-9._-]", "_", label)
    out_path <- file.path(DIR_PROC_PRICES, paste0(out_sym, ".csv"))
    utils::write.csv(result$clean, out_path, row.names = FALSE)

    message(sprintf("  [%d/%d] %-20s raw: %5d  clean: %5d  issues: %d",
                    n_ok + n_fail, length(price_files),
                    label,
                    result$n_raw,
                    result$n_clean,
                    nrow(result$issues)))
  }

  # ---- 8.6  Build and save quality reports --------------------------------
  message("\n[Step 6] Building quality reports ...")

  # Remove NULL entries from excluded list
  all_excluded_clean <- Filter(Negate(is.null), all_excluded)
  excluded_combined  <- if (length(all_excluded_clean) > 0) {
    do.call(rbind, all_excluded_clean)
  } else {
    data.frame(Dataset = character(), Date = character(),
               NSESymbol = character(), Reason = character(),
               stringsAsFactors = FALSE)
  }

  reports <- build_reports(
    all_issues     = Filter(Negate(is.null), all_issues),
    all_na         = Filter(Negate(is.null), all_na),
    all_excluded   = all_excluded_clean,
    log_df         = safe_read_csv(PATH_RAW_LOG, "download_log.csv"),
    constituents   = const_result$clean,
    stock_results  = stock_results
  )

  # Missing values report
  utils::write.csv(reports$missing_values, PATH_RPT_MISSING, row.names = FALSE)
  message(sprintf("  Saved: %s", PATH_RPT_MISSING))

  # Data quality report — combine issues + excluded rows
  # Add excluded rows as additional quality entries
  if (nrow(excluded_combined) > 0) {
    excl_as_issues <- data.frame(
      Dataset    = excluded_combined$Dataset,
      Check      = "Row excluded",
      N_Affected = 1L,
      Pct        = NA_real_,
      Detail     = excluded_combined$Reason,
      Action     = "Row removed from cleaned dataset",
      stringsAsFactors = FALSE
    )
    dq_full <- rbind(reports$data_quality, excl_as_issues)
  } else {
    dq_full <- reports$data_quality
  }
  utils::write.csv(dq_full, PATH_RPT_QUALITY, row.names = FALSE)
  message(sprintf("  Saved: %s", PATH_RPT_QUALITY))

  # Cleaning summary — overall metrics + per-stock table
  summary_combined <- rbind(
    reports$summary,
    data.frame(Metric = "--- Per-stock history ---", Value = "",
               stringsAsFactors = FALSE),
    data.frame(Metric = paste0("[", reports$stock_summary$NSESymbol, "] rows"),
               Value  = paste0("raw=", reports$stock_summary$RawRows,
                               " clean=", reports$stock_summary$CleanRows,
                               " start=", reports$stock_summary$FirstDate,
                               " end=", reports$stock_summary$LastDate),
               stringsAsFactors = FALSE)
  )
  utils::write.csv(summary_combined, PATH_RPT_SUMMARY, row.names = FALSE)
  message(sprintf("  Saved: %s", PATH_RPT_SUMMARY))

  # ---- 8.7  Console summary -----------------------------------------------
  print_console_summary(reports, n_ok, n_fail)

  invisible(list(
    index_clean    = idx_result$clean,
    constituents   = const_result$clean,
    stock_results  = stock_results,
    reports        = reports
  ))
}


# ---------------------------------------------------------------------------
# 9. Entry point
# ---------------------------------------------------------------------------
# Set working directory to the project root before sourcing:
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/02_data_cleaning.R")

main()
