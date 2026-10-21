# =============================================================================
# Script  : 01_download_data.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Data Acquisition Pipeline — download and validate raw price data
#           for the NIFTY 50 index and its current constituents from Yahoo
#           Finance; save all raw files without modification.
# Author  : [Your Name]
# Date    : 2026
# =============================================================================
#
# IMPORTANT RESEARCH LIMITATION — SURVIVORSHIP BIAS WARNING
# ----------------------------------------------------------
# The constituent list downloaded here reflects the CURRENT (as of download
# date) composition of the NIFTY 50 index.  It is NOT a historical membership
# list.  Companies enter and leave the index through semi-annual reviews
# conducted by NSE Indices Limited.  Price data downloaded for current
# constituents going back to 2000 therefore suffers from survivorship bias:
# firms that were in the index historically but have since been removed (due
# to delisting, mergers, or downgrading) are excluded, and firms recently
# added are included for their full available history.
#
# This limitation MUST be acknowledged in every statistical analysis that
# uses this dataset.  Historical constituent snapshots would be required to
# construct a fully bias-free panel.  The `DownloadDate` column in the
# constituent master file records when this list was retrieved so the bias
# can be clearly documented in any resulting paper.
#
# DATA SCOPE
# ----------
# Index    : NIFTY 50  (Yahoo ticker ^NSEI)
# Stocks   : Current NIFTY 50 constituents (Yahoo tickers: <NSESymbol>.NS)
# Period   : 2000-01-01 to latest available date in 2026
#            Individual stocks may start later if listed after 2000.
# Source   : Yahoo Finance via quantmod; constituent list via NSE Archives.
#
# OUTPUT FILES
# ------------
# data/raw/nifty50_index_yahoo.csv          — NIFTY 50 index daily OHLCAV
# data/raw/nifty50_constituents.csv         — Constituent master list
# data/raw/constituent_prices/<SYMBOL>.csv  — Per-stock daily OHLCAV
# data/raw/download_log.csv                 — Status log for all downloads
#
# PACKAGES REQUIRED
# -----------------
# install.packages(c("quantmod", "httr", "jsonlite"))
#   • quantmod  — Yahoo Finance data retrieval via getSymbols()
#   • httr      — HTTP request with custom headers for NSE CSV download
#   • jsonlite  — (loaded by httr; no direct calls needed here)
# Base R packages used: utils, tools, stats — no installation needed.
# =============================================================================


# ---------------------------------------------------------------------------
# 0. Package loading
# ---------------------------------------------------------------------------

required_packages <- c("quantmod", "httr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      sprintf(
        "Package '%s' is not installed. Run: install.packages('%s')",
        pkg, pkg
      )
    )
  }
}

suppressPackageStartupMessages({
  library(quantmod)
  library(httr)
})


# ---------------------------------------------------------------------------
# 1. Global configuration
# ---------------------------------------------------------------------------

# Date range for all downloads
START_DATE <- "2000-01-01"
END_DATE   <- "2026-12-31"   # quantmod/Yahoo will return up to latest available

# Official NSE Archives URL for the current NIFTY 50 constituent CSV.
# NSE Indices Limited publishes and maintains this file; it reflects the
# composition as of the time of download.
# The primary and fallback URLs are defined in Section 4 alongside the
# fetcher that uses them.

# Project directory layout (relative to workspace root)
DIR_RAW         <- file.path("data", "raw")
DIR_CONSTITUENT <- file.path(DIR_RAW, "constituent_prices")

# Output file paths
PATH_INDEX      <- file.path(DIR_RAW, "nifty50_index_yahoo.csv")
PATH_CONST_MASTER <- file.path(DIR_RAW, "nifty50_constituents.csv")
PATH_LOG        <- file.path(DIR_RAW, "download_log.csv")

# Pause between Yahoo Finance requests (seconds) — be polite to the API
REQUEST_PAUSE <- 2L


# ---------------------------------------------------------------------------
# 2. Helper utilities
# ---------------------------------------------------------------------------

#' Ensure a directory exists, creating it (recursively) if necessary.
#' @param path Character. Directory path to create.
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    message(sprintf("  Created directory: %s", path))
  }
}


#' Convert one or more NSE stock symbols to Yahoo Finance tickers.
#' Appends ".NS" to each symbol.  Index tickers (starting with "^") are
#' returned unchanged.  Vectorised — safe to call on a whole column.
#' @param nse_symbol Character vector. NSE trading symbols, e.g. "RELIANCE".
#' @return Character vector. Yahoo Finance tickers, e.g. "RELIANCE.NS".
nse_to_yahoo <- function(nse_symbol) {
  # ifelse() is vectorised; if() is not — using if() on a vector throws
  # "the condition has length > 1".
  ifelse(startsWith(nse_symbol, "^"), nse_symbol, paste0(nse_symbol, ".NS"))
}


#' Sanitise a symbol string for use as a safe filename.
#' Replaces characters that are illegal or problematic in file names
#' (^, &, /, \, :, *, ?, ", <, >, |, space) with underscores.
#' @param symbol Character. Ticker or NSE symbol.
#' @return Character. Safe filename base (no extension).
safe_filename <- function(symbol) {
  gsub("[^A-Za-z0-9._-]", "_", symbol)
}


#' Append a row to the in-memory download log data frame.
#' @param log      Data frame. Current log.
#' @param symbol   Character. NSE symbol.
#' @param yahoo    Character. Yahoo Finance ticker.
#' @param status   Character. "SUCCESS" or "FAILED".
#' @param message  Character. Detail message or error text.
#' @param rows     Integer or NA. Number of rows downloaded.
#' @param date_first Character or NA. First date in downloaded data.
#' @param date_last  Character or NA. Last date in downloaded data.
#' @return Data frame with the new row appended.
log_entry <- function(log, symbol, yahoo, status, message,
                      rows = NA_integer_,
                      date_first = NA_character_,
                      date_last  = NA_character_) {
  new_row <- data.frame(
    Symbol     = symbol,
    YahooTicker = yahoo,
    Status     = status,
    Message    = message,
    Rows       = rows,
    FirstDate  = date_first,
    LastDate   = date_last,
    LogDate    = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
  rbind(log, new_row)
}


#' Convert an xts/OHLCAV object returned by quantmod into a tidy data frame
#' with standardised column names plus metadata columns.
#' @param xts_obj   xts object from getSymbols().
#' @param company_name Character. Human-readable company/index name.
#' @param nse_symbol   Character. NSE symbol (or "^NSEI" for the index).
#' @param yahoo_symbol Character. Yahoo Finance ticker.
#' @return Data frame with columns:
#'   Date, Open, High, Low, Close, Adjusted, Volume,
#'   CompanyName, NSESymbol, YahooSymbol, Source
xts_to_dataframe <- function(xts_obj, company_name, nse_symbol, yahoo_symbol) {

  # zoo is a dependency of quantmod; index() is re-exported by xts/zoo.
  dates <- as.character(index(xts_obj))
  df <- as.data.frame(xts_obj)
  df$Date <- dates

  # quantmod names columns as "TICKER.Open", "TICKER.High", etc.
  # Extract the ticker prefix actually used (may differ from yahoo_symbol
  # due to quantmod internal naming, e.g. "^" is stripped).
  col_names <- colnames(df)

  # Build a mapping from the suffix pattern to our standard name
  suffix_map <- c(
    Open     = "Open",
    High     = "High",
    Low      = "Low",
    Close    = "Close",
    Volume   = "Volume",
    Adjusted = "Adjusted"
  )

  result <- data.frame(Date = df$Date, stringsAsFactors = FALSE)

  for (std_name in names(suffix_map)) {
    # Match column whose name ends with ".Open", ".High", etc.
    matched <- col_names[grepl(paste0("\\.", std_name, "$"), col_names,
                                ignore.case = TRUE)]
    if (length(matched) >= 1) {
      result[[std_name]] <- df[[matched[1]]]
    } else {
      # Column not present (e.g. Volume for some indices)
      result[[std_name]] <- NA_real_
    }
  }

  # Ensure standard column order
  result <- result[, c("Date", "Open", "High", "Low",
                       "Close", "Adjusted", "Volume")]

  # Add metadata
  result$CompanyName <- company_name
  result$NSESymbol   <- nse_symbol
  result$YahooSymbol <- yahoo_symbol
  result$Source      <- "Yahoo Finance"

  result
}


# ---------------------------------------------------------------------------
# 3. Validation helpers
# ---------------------------------------------------------------------------

#' Run a suite of validation checks on a price data frame.
#' Checks performed:
#'   (a) No duplicate dates
#'   (b) No missing closing prices
#'   (c) Date range coverage (warns if outside expected window)
#'   (d) Row count sanity (warns if fewer than 50 rows)
#'   (e) OHLC consistency: High >= Low, High >= Open, High >= Close,
#'       Low <= Open, Low <= Close, and no negative prices
#' Prints a warning for each issue found.  Does NOT modify the data.
#' @param df   Data frame produced by xts_to_dataframe().
#' @param label Character. Label used in warning messages (e.g. ticker).
#' @return Named logical vector of check results (TRUE = passed).
validate_price_data <- function(df, label) {

  checks <- c(
    no_duplicate_dates      = TRUE,
    no_missing_close        = TRUE,
    date_range_ok           = TRUE,
    sufficient_rows         = TRUE,
    ohlc_consistent         = TRUE
  )

  # (a) Duplicate dates
  dup_count <- sum(duplicated(df$Date))
  if (dup_count > 0) {
    warning(sprintf(
      "[VALIDATION] %s: %d duplicate date(s) found.", label, dup_count
    ))
    checks["no_duplicate_dates"] <- FALSE
  }

  # (b) Missing Close prices
  na_close <- sum(is.na(df$Close))
  if (na_close > 0) {
    warning(sprintf(
      "[VALIDATION] %s: %d row(s) with missing Close price.", label, na_close
    ))
    checks["no_missing_close"] <- FALSE
  }

  # (c) Date range
  if (nrow(df) > 0) {
    first_date <- min(df$Date, na.rm = TRUE)
    last_date  <- max(df$Date, na.rm = TRUE)
    if (last_date < "2024-01-01") {
      warning(sprintf(
        "[VALIDATION] %s: Last date is %s — data may be stale or truncated.",
        label, last_date
      ))
      checks["date_range_ok"] <- FALSE
    }
  }

  # (d) Row count
  if (nrow(df) < 50) {
    warning(sprintf(
      "[VALIDATION] %s: Only %d row(s) — suspiciously low row count.",
      label, nrow(df)
    ))
    checks["sufficient_rows"] <- FALSE
  }

  # (e) OHLC consistency (only on non-NA numeric rows)
  ohlc_cols <- c("Open", "High", "Low", "Close")
  complete_rows <- df[complete.cases(df[, ohlc_cols]), ]
  if (nrow(complete_rows) > 0) {
    violations <- with(complete_rows,
      High < Low   |
      High < Open  | High < Close |
      Low  > Open  | Low  > Close |
      Open < 0     | High < 0     | Low < 0 | Close < 0
    )
    n_viol <- sum(violations, na.rm = TRUE)
    if (n_viol > 0) {
      warning(sprintf(
        "[VALIDATION] %s: %d row(s) with OHLC inconsistency (e.g. High < Low).",
        label, n_viol
      ))
      checks["ohlc_consistent"] <- FALSE
    }
  }

  checks
}


# ---------------------------------------------------------------------------
# 4. Constituent list acquisition
# ---------------------------------------------------------------------------

# NSE Archives URL (primary) and an older mirror (fallback).
# Both serve the same file; the archives URL is preferred because it is
# more stable across NSE website redesigns.
NSE_CONSTITUENT_URLS <- c(
  primary  = "https://nsearchives.nseindia.com/content/indices/ind_nifty50list.csv",
  fallback = "https://www1.nseindia.com/content/indices/ind_nifty50list.csv"
)

#' Attempt a single HTTP GET for the NSE constituent CSV.
#' NSE requires browser-like headers; a warm-up request to the main site
#' establishes the session cookies that the archive endpoint checks.
#'
#' @param url Character. Full URL of the CSV to download.
#' @return Character string of raw CSV text, or NULL on failure.
try_fetch_nse_csv <- function(url) {

  # Warm-up: visit the NSE homepage to get session cookies
  warm <- tryCatch(
    httr::GET(
      "https://www.nseindia.com",
      httr::user_agent(
        paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) ",
               "Chrome/124.0.0.0 Safari/537.36")
      ),
      httr::add_headers(
        "Accept-Language" = "en-US,en;q=0.9",
        "Accept"          = paste0("text/html,application/xhtml+xml,",
                                   "application/xml;q=0.9,*/*;q=0.8")
      ),
      httr::timeout(20)
    ),
    error = function(e) NULL
  )

  Sys.sleep(2)

  # Build cookie header from warm-up response (may be NULL if warm-up failed)
  cookie_config <- if (!is.null(warm)) {
    httr::set_cookies(.cookies = unlist(httr::cookies(warm)))
  } else {
    NULL
  }

  resp <- tryCatch(
    httr::GET(
      url,
      httr::user_agent(
        paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
               "AppleWebKit/537.36 (KHTML, like Gecko) ",
               "Chrome/124.0.0.0 Safari/537.36")
      ),
      httr::add_headers(
        "Accept-Language" = "en-US,en;q=0.9",
        "Referer"         = "https://www.nseindia.com/market-data/live-equity-market",
        "Accept"          = "text/csv,text/plain,*/*"
      ),
      cookie_config,
      httr::timeout(30)
    ),
    error = function(e) {
      message(sprintf("    HTTP request failed for %s: %s", url, e$message))
      NULL
    }
  )

  if (is.null(resp))                  return(NULL)
  if (httr::http_error(resp))         return(NULL)

  raw <- httr::content(resp, as = "text", encoding = "UTF-8")

  # Reject empty responses or HTML error pages
  if (is.null(raw) || nchar(trimws(raw)) < 20)  return(NULL)
  if (grepl("^\\s*<!DOCTYPE|^\\s*<html",
            raw, ignore.case = TRUE, perl = TRUE)) return(NULL)

  raw
}


#' Parse raw CSV text from the NSE constituent file into a clean data frame.
#' NSE CSV columns: Company Name, Industry, Symbol, Series, ISIN Code
#'
#' @param raw_text Character. Raw CSV string.
#' @param source_url Character. URL used, recorded in ListSource column.
#' @return Data frame with research columns, or NULL if parsing fails.
parse_nse_csv <- function(raw_text, source_url) {

  raw_df <- tryCatch(
    utils::read.csv(
      text             = raw_text,
      header           = TRUE,
      stringsAsFactors = FALSE,
      strip.white      = TRUE
    ),
    error = function(e) {
      message(sprintf("    CSV parse error: %s", e$message))
      NULL
    }
  )

  if (is.null(raw_df) || nrow(raw_df) == 0) return(NULL)

  # Normalise column names (NSE may change capitalisation or spacing)
  colnames(raw_df) <- trimws(colnames(raw_df))

  if (!"Symbol" %in% colnames(raw_df)) {
    message(sprintf(
      "    'Symbol' column not found. Actual columns: %s",
      paste(colnames(raw_df), collapse = ", ")
    ))
    return(NULL)
  }

  company_col <- if ("Company Name" %in% colnames(raw_df)) "Company Name" else
                 if ("CompanyName"  %in% colnames(raw_df)) "CompanyName"  else
                 NA_character_

  constituents <- data.frame(
    CompanyName  = if (!is.na(company_col)) trimws(raw_df[[company_col]])
                   else rep(NA_character_, nrow(raw_df)),
    NSESymbol    = trimws(raw_df[["Symbol"]]),
    YahooSymbol  = nse_to_yahoo(trimws(raw_df[["Symbol"]])),
    IndexName    = "NIFTY 50",
    ListSource   = source_url,
    DownloadDate = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )

  # Drop blank / NA symbol rows (header artefacts, trailing empty lines)
  constituents <- constituents[
    !is.na(constituents$NSESymbol) &
      nchar(trimws(constituents$NSESymbol)) > 0, ]

  if (nrow(constituents) < 10) {
    message(sprintf(
      "    Parsed only %d rows — response may be malformed.", nrow(constituents)
    ))
    return(NULL)
  }

  constituents
}


#' Download the current NIFTY 50 constituent list from NSE.
#' Tries the primary NSE Archives URL first, then a fallback mirror.
#' If both live fetches fail, looks for a previously cached copy at
#' PATH_CONST_MASTER and uses it with a warning.
#' Stops with a clear manual-download message only if nothing works.
#'
#' @return Data frame with columns:
#'   CompanyName, NSESymbol, YahooSymbol, IndexName, ListSource, DownloadDate
fetch_nse_constituent_list <- function() {

  message("\n--- Fetching NIFTY 50 constituent list from NSE ---")

  for (attempt_name in names(NSE_CONSTITUENT_URLS)) {

    url <- NSE_CONSTITUENT_URLS[[attempt_name]]
    message(sprintf("  Trying %s URL: %s", attempt_name, url))

    raw_text <- try_fetch_nse_csv(url)

    if (!is.null(raw_text)) {
      result <- parse_nse_csv(raw_text, source_url = url)
      if (!is.null(result)) {
        message(sprintf("  Retrieved %d constituents from %s URL.",
                        nrow(result), attempt_name))
        return(result)
      }
      message(sprintf("  %s URL returned data but parsing failed.", attempt_name))
    } else {
      message(sprintf("  %s URL did not return usable CSV.", attempt_name))
    }

    Sys.sleep(2)
  }

  # ---- Both live fetches failed ----
  # Check for a manually placed or previously cached copy
  if (file.exists(PATH_CONST_MASTER)) {
    message(sprintf(
      "\n  WARNING: Live download failed. Using cached file: %s",
      PATH_CONST_MASTER
    ))
    message("  NOTE: This cached list may be outdated. Re-run when network access is available.")
    cached <- tryCatch(
      utils::read.csv(PATH_CONST_MASTER, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (!is.null(cached) && nrow(cached) > 0) return(cached)
  }

  # ---- Nothing worked ----
  stop(paste0(
    "Could not obtain the NIFTY 50 constituent list from NSE.\n\n",
    "MANUAL DOWNLOAD INSTRUCTIONS:\n",
    "  1. Open in your browser:\n",
    "     https://nsearchives.nseindia.com/content/indices/ind_nifty50list.csv\n",
    "  2. Save the file to: ", PATH_CONST_MASTER, "\n",
    "  3. Re-run this script.\n\n",
    "The NSE website sometimes blocks automated requests. ",
    "Waiting a few minutes and retrying often resolves this."
  ))
}


# ---------------------------------------------------------------------------
# 5. Index download
# ---------------------------------------------------------------------------

#' Download NIFTY 50 index OHLCAV from Yahoo Finance and save to CSV.
#' @param log  Data frame. Current download log (will be updated).
#' @return Updated log data frame.
download_index <- function(log) {

  ticker <- "^NSEI"
  message("\n--- Downloading NIFTY 50 index (^NSEI) ---")

  tryCatch({

    quantmod::getSymbols(
      ticker,
      src        = "yahoo",
      from       = START_DATE,
      to         = END_DATE,
      auto.assign = TRUE,
      env        = globalenv()   # symbol stored as NSEI in global env
    )

    # quantmod strips "^" and stores as "NSEI"
    xts_obj <- get("NSEI", envir = globalenv())

    df <- xts_to_dataframe(
      xts_obj,
      company_name = "NIFTY 50",
      nse_symbol   = "^NSEI",
      yahoo_symbol = "^NSEI"
    )

    # Validate
    validate_price_data(df, label = ticker)

    # Save — never overwrite without warning the user
    if (file.exists(PATH_INDEX)) {
      message(sprintf(
        "  NOTE: %s already exists and will be overwritten with fresh data.",
        PATH_INDEX
      ))
    }
    utils::write.csv(df, file = PATH_INDEX, row.names = FALSE)

    message(sprintf(
      "  Saved %d rows  [%s -> %s]  to: %s",
      nrow(df),
      min(df$Date, na.rm = TRUE),
      max(df$Date, na.rm = TRUE),
      PATH_INDEX
    ))

    log <- log_entry(
      log,
      symbol     = "^NSEI",
      yahoo      = "^NSEI",
      status     = "SUCCESS",
      message    = "Index download OK",
      rows       = nrow(df),
      date_first = min(df$Date, na.rm = TRUE),
      date_last  = max(df$Date, na.rm = TRUE)
    )

  }, error = function(e) {
    message(sprintf("  ERROR downloading index %s: %s", ticker, e$message))
    log <<- log_entry(
      log,
      symbol  = "^NSEI",
      yahoo   = "^NSEI",
      status  = "FAILED",
      message = conditionMessage(e)
    )
  })

  log
}


# ---------------------------------------------------------------------------
# 6. Constituent stock download
# ---------------------------------------------------------------------------

#' Download daily OHLCAV for a single constituent and save to CSV.
#' @param nse_symbol   Character. NSE symbol, e.g. "RELIANCE".
#' @param yahoo_symbol Character. Yahoo ticker, e.g. "RELIANCE.NS".
#' @param company_name Character. Human-readable company name.
#' @param log          Data frame. Current download log.
#' @return Updated log data frame.
download_one_stock <- function(nse_symbol, yahoo_symbol, company_name, log) {

  out_file <- file.path(
    DIR_CONSTITUENT,
    paste0(safe_filename(nse_symbol), ".csv")
  )

  tryCatch({

    # quantmod stores the result under a mangled env name; use a fresh
    # temporary environment to avoid polluting the global namespace.
    tmp_env <- new.env(parent = emptyenv())

    quantmod::getSymbols(
      yahoo_symbol,
      src         = "yahoo",
      from        = START_DATE,
      to          = END_DATE,
      auto.assign = TRUE,
      env         = tmp_env
    )

    # The env key may differ from yahoo_symbol (quantmod sanitises names)
    env_keys <- ls(tmp_env)
    if (length(env_keys) == 0) {
      stop("getSymbols returned no data object in the environment.")
    }
    xts_obj <- get(env_keys[1], envir = tmp_env)

    df <- xts_to_dataframe(
      xts_obj,
      company_name = company_name,
      nse_symbol   = nse_symbol,
      yahoo_symbol = yahoo_symbol
    )

    validate_price_data(df, label = yahoo_symbol)

    if (file.exists(out_file)) {
      message(sprintf(
        "  NOTE: %s already exists; overwriting with fresh data.", out_file
      ))
    }
    utils::write.csv(df, file = out_file, row.names = FALSE)

    log <- log_entry(
      log,
      symbol     = nse_symbol,
      yahoo      = yahoo_symbol,
      status     = "SUCCESS",
      message    = "OK",
      rows       = nrow(df),
      date_first = min(df$Date, na.rm = TRUE),
      date_last  = max(df$Date, na.rm = TRUE)
    )

  }, error = function(e) {
    message(sprintf(
      "  [FAILED] %s (%s): %s", nse_symbol, yahoo_symbol, e$message
    ))
    log <<- log_entry(
      log,
      symbol  = nse_symbol,
      yahoo   = yahoo_symbol,
      status  = "FAILED",
      message = conditionMessage(e)
    )
  })

  log
}


#' Download daily OHLCAV for all constituents in the master list.
#' Iterates sequentially with a pause between requests.
#' Failed downloads are logged but do not halt the pipeline.
#'
#' @param constituents Data frame. Output of fetch_nse_constituent_list().
#' @param log          Data frame. Current download log.
#' @return Updated log data frame.
download_all_constituents <- function(constituents, log) {

  n_total <- nrow(constituents)
  message(sprintf(
    "\n--- Downloading price data for %d constituents ---", n_total
  ))

  for (i in seq_len(n_total)) {

    nse_sym  <- constituents$NSESymbol[i]
    yah_sym  <- constituents$YahooSymbol[i]
    co_name  <- constituents$CompanyName[i]

    message(sprintf(
      "  [%d/%d] %s (%s) ...",
      i, n_total, nse_sym, yah_sym
    ))

    log <- download_one_stock(nse_sym, yah_sym, co_name, log)

    # Pause between requests to avoid rate-limiting
    if (i < n_total) Sys.sleep(REQUEST_PAUSE)
  }

  log
}


# ---------------------------------------------------------------------------
# 7. Summary report
# ---------------------------------------------------------------------------

#' Print a formatted summary of the download run to the console.
#' @param log          Data frame. Final download log.
#' @param constituents Data frame. Constituent master list.
print_summary <- function(log, constituents) {

  sep <- paste(rep("=", 70), collapse = "")
  message("\n", sep)
  message("  DOWNLOAD SUMMARY")
  message(sep)

  n_const     <- nrow(constituents)
  n_success   <- sum(log$Status == "SUCCESS", na.rm = TRUE)
  n_failed    <- sum(log$Status == "FAILED",  na.rm = TRUE)

  # Index row
  idx_row <- log[log$Symbol == "^NSEI" & log$Status == "SUCCESS", ]

  message(sprintf("  Total constituents    : %d", n_const))
  message(sprintf("  Successful downloads  : %d", n_success))
  message(sprintf("  Failed downloads      : %d", n_failed))

  if (nrow(idx_row) > 0) {
    message(sprintf(
      "  NIFTY 50 index range  : %s  ->  %s  (%d rows)",
      idx_row$FirstDate[1], idx_row$LastDate[1], idx_row$Rows[1]
    ))
  }

  message("")
  message("  Per-constituent summary:")
  message(sprintf("  %-20s %-14s %-12s %-12s %s",
                  "NSESymbol", "Status", "FirstDate", "LastDate", "Rows"))
  message(sprintf("  %s", paste(rep("-", 68), collapse = "")))

  stock_log <- log[log$Symbol != "^NSEI", ]
  for (i in seq_len(nrow(stock_log))) {
    r <- stock_log[i, ]
    message(sprintf("  %-20s %-14s %-12s %-12s %s",
                    r$Symbol,
                    r$Status,
                    ifelse(is.na(r$FirstDate), "N/A", r$FirstDate),
                    ifelse(is.na(r$LastDate),  "N/A", r$LastDate),
                    ifelse(is.na(r$Rows),      "N/A", as.character(r$Rows))
    ))
  }

  message(sep)

  if (n_failed > 0) {
    failed_syms <- log$Symbol[log$Status == "FAILED"]
    message(sprintf(
      "\n  WARNING: %d download(s) failed: %s",
      n_failed,
      paste(failed_syms, collapse = ", ")
    ))
    message(sprintf(
      "  See full error details in: %s", PATH_LOG
    ))
  }

  total_rows <- sum(log$Rows, na.rm = TRUE)
  message(sprintf("\n  Total observations saved: %d", total_rows))
  message(sep, "\n")
}


# ---------------------------------------------------------------------------
# 8. Main pipeline
# ---------------------------------------------------------------------------

main <- function() {

  message(rep("=", 70))
  message("  NIFTY 50 Data Acquisition Pipeline")
  message(sprintf("  Run started: %s", Sys.time()))
  message(rep("=", 70))

  # 8.1  Ensure all required directories exist
  message("\n[Step 1] Creating project directories ...")
  ensure_dir(DIR_RAW)
  ensure_dir(DIR_CONSTITUENT)

  # Initialise an empty log data frame
  log <- data.frame(
    Symbol      = character(),
    YahooTicker = character(),
    Status      = character(),
    Message     = character(),
    Rows        = integer(),
    FirstDate   = character(),
    LastDate    = character(),
    LogDate     = character(),
    stringsAsFactors = FALSE
  )

  # 8.2  Download NIFTY 50 index
  message("\n[Step 2] Downloading NIFTY 50 index ...")
  log <- download_index(log)

  # 8.3  Fetch current constituent list from NSE
  message("\n[Step 3] Fetching NIFTY 50 constituent list from NSE ...")
  constituents <- tryCatch(
    fetch_nse_constituent_list(),
    error = function(e) {
      message(sprintf(
        "  CRITICAL: Could not fetch constituent list: %s", e$message
      ))
      message(
        "  The script will stop here.  Please check your internet connection\n",
        "  or download the file manually from:\n",
        "  https://nsearchives.nseindia.com/content/indices/ind_nifty50list.csv\n",
        "  and place it at: ", PATH_CONST_MASTER
      )
      stop(e)
    }
  )

  # Save constituent master list
  utils::write.csv(constituents, file = PATH_CONST_MASTER, row.names = FALSE)
  message(sprintf("  Saved constituent master list to: %s", PATH_CONST_MASTER))

  # 8.4  Download each constituent's price history
  message("\n[Step 4] Downloading constituent price data ...")
  log <- download_all_constituents(constituents, log)

  # 8.5  Save download log
  utils::write.csv(log, file = PATH_LOG, row.names = FALSE)
  message(sprintf("\n[Step 5] Download log saved to: %s", PATH_LOG))

  # 8.6  Print summary
  print_summary(log, constituents)

  invisible(list(log = log, constituents = constituents))
}


# ---------------------------------------------------------------------------
# 9. Entry point
# ---------------------------------------------------------------------------
# Run the pipeline when the script is sourced or Rscript-ed directly.
# Set the working directory to the project root before running:
#   setwd("path/to/NIFTY50_Crash_Weekday_Analysis")
#   source("R/01_download_data.R")

main()
