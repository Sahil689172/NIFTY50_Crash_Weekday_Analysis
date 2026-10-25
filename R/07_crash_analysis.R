# =============================================================================
# Script  : 07_crash_analysis.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Detailed crash and drawdown analysis — event identification,
#           severity, recovery context, constituent comparisons, and
#           time-period breakdown.  No statistical modelling.
# Inputs  : data/processed/features/nifty50_index_features.csv
#           data/processed/features/all_constituents_features.csv
# Outputs : results/tables/crash_*.csv
#           results/figures/crash_*.png
# Packages: ggplot2
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/07_crash_analysis.R")
# =============================================================================
#
# KEY DEFINITIONS (matching 03_feature_engineering.R)
# ----------------------------------------------------
# Daily crash: ReturnPrice <= -0.03 (-3%) or <= -0.05 (-5%)
# Drawdown   : (Price / RunningPeak) - 1  (always <= 0)
# Drawdown period: consecutive days where Drawdown < 0; a "bear period"
#   is when drawdown <= -20%.
# Recovery: drawdown returns to 0 (new running peak).
# NOTE: Missing prices are NOT interpreted as trading halts.
#       Incomplete sessions are left as NA and excluded from event detection.
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
require_pkg("ggplot2")

create_results_dirs()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  07_crash_analysis.R\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
cat("[Step 1] Loading data ...\n")
idx     <- load_index()
idx_r   <- valid_returns(idx)
panel   <- load_panel()
panel_r <- valid_returns(panel)

cat(sprintf("  Index: %d valid-return rows (%s to %s)\n",
            nrow(idx_r), min(idx_r[[COL_DATE]]), max(idx_r[[COL_DATE]])))

# ---------------------------------------------------------------------------
# Helper: identify drawdown episodes
# ---------------------------------------------------------------------------
#' Find contiguous drawdown periods (drawdown continuously < 0).
#' @param df  Data frame with Date and Drawdown columns, sorted by Date.
#' @param threshold  Numeric (negative). Only episodes reaching this depth.
#' @return Data frame of episodes: start, end, trough date, max depth,
#'         duration in trading days.
find_drawdown_episodes <- function(df, threshold = 0) {
  dd   <- df[[COL_DRAWDOWN]]
  dt   <- df[[COL_DATE]]
  n    <- length(dd)
  in_dd <- FALSE
  ep_start <- NA
  episodes <- list()

  for (i in seq_len(n)) {
    if (is.na(dd[i])) { in_dd <- FALSE; next }
    if (!in_dd && dd[i] < 0) {
      in_dd    <- TRUE
      ep_start <- i
    }
    if (in_dd && (dd[i] == 0 || i == n)) {
      ep_end <- ifelse(dd[i] == 0, i - 1, i)
      ep_dd  <- dd[ep_start:ep_end]
      max_dd <- min(ep_dd, na.rm = TRUE)
      if (max_dd <= threshold) {
        trough_idx <- ep_start + which.min(ep_dd) - 1
        episodes[[length(episodes) + 1]] <- data.frame(
          EpisodeStart = as.character(dt[ep_start]),
          EpisodeTrough= as.character(dt[trough_idx]),
          EpisodeEnd   = as.character(dt[ep_end]),
          MaxDrawdown  = max_dd,
          DurationDays = ep_end - ep_start + 1,
          stringsAsFactors = FALSE
        )
      }
      in_dd <- FALSE
    }
  }
  if (length(episodes) == 0)
    return(data.frame(EpisodeStart = character(), EpisodeTrough = character(),
                      EpisodeEnd = character(), MaxDrawdown = numeric(),
                      DurationDays = integer(), stringsAsFactors = FALSE))
  do.call(rbind, episodes)
}

# ---------------------------------------------------------------------------
# 2. Index crash event catalogue
# ---------------------------------------------------------------------------
cat("\n[Step 2] Index crash event catalogue ...\n")

for (thr_col in c(COL_CRASH3, COL_CRASH5)) {
  thr_label <- ifelse(thr_col == COL_CRASH3, "-3%", "-5%")
  crash_days <- idx_r[idx_r[[thr_col]] == TRUE, , drop = FALSE]

  event_df <- data.frame(
    Date        = as.character(crash_days[[COL_DATE]]),
    Weekday     = as.character(crash_days[[COL_WEEKDAY]]),
    Return      = crash_days[[COL_RETURN]],
    Drawdown    = crash_days[[COL_DRAWDOWN]],
    RollVol_20  = crash_days[[COL_ROLLVOL_20]],
    ReturnSource= crash_days[[COL_RETURN_SRC]],
    stringsAsFactors = FALSE
  )
  event_df <- event_df[order(event_df$Return), ]   # worst first
  fname <- sprintf("crash_%s_index_crash_events.csv",
                   sub("-", "", thr_label))
  save_table(event_df, fname)
  cat(sprintf("  %s crashes: %d events\n", thr_label, nrow(event_df)))
  cat("  Top 5 worst:\n")
  for (i in seq_len(min(5, nrow(event_df)))) {
    r <- event_df[i, ]
    cat(sprintf("    %s  %s  ret=%.4f  dd=%.4f\n",
                r$Date, r$Weekday, r$Return, r$Drawdown))
  }
}

# ---------------------------------------------------------------------------
# 3. Crash frequency by weekday — index (detailed)
# ---------------------------------------------------------------------------
cat("\n[Step 3] Crash frequency by weekday — index ...\n")

crash_wd_detail <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  sub <- idx_r[idx_r[[COL_WEEKDAY]] == wd, ]
  n   <- nrow(sub)
  r3  <- sum(sub[[COL_CRASH3]] == TRUE, na.rm = TRUE)
  r5  <- sum(sub[[COL_CRASH5]] == TRUE, na.rm = TRUE)
  mean_ret_on_crash <- if (r3 > 0) {
    mean(sub[[COL_RETURN]][sub[[COL_CRASH3]] == TRUE], na.rm = TRUE)
  } else NA_real_
  data.frame(
    Weekday           = wd,
    N_TradingDays     = n,
    N_Crash3          = r3,
    N_Crash5          = r5,
    Rate_Crash3       = r3 / n,
    Rate_Crash5       = r5 / n,
    Mean_Return_All   = mean(sub[[COL_RETURN]], na.rm = TRUE),
    Mean_Return_Crash3= mean_ret_on_crash,
    SD_Return         = sd(sub[[COL_RETURN]], na.rm = TRUE),
    stringsAsFactors  = FALSE
  )
}))
save_table(crash_wd_detail, "crash_index_weekday_frequency_detail.csv")

# ---------------------------------------------------------------------------
# 4. Drawdown episodes — index (>= 20%)
# ---------------------------------------------------------------------------
cat("\n[Step 4] Drawdown episodes >= 20% ...\n")

dd_episodes <- find_drawdown_episodes(idx, threshold = DD_THRESH_20)
dd_episodes$MaxDrawdown_Pct <- round(dd_episodes$MaxDrawdown * 100, 2)
dd_episodes <- dd_episodes[order(dd_episodes$MaxDrawdown), ]
save_table(dd_episodes, "crash_index_drawdown_episodes_20pct.csv")

cat(sprintf("  Drawdown episodes (>= 20%%): %d\n", nrow(dd_episodes)))
if (nrow(dd_episodes) > 0) {
  cat("  Top 5 deepest:\n")
  for (i in seq_len(min(5, nrow(dd_episodes)))) {
    r <- dd_episodes[i, ]
    cat(sprintf("    Start: %s  Trough: %s  End: %s  MaxDD: %.2f%%  Days: %d\n",
                r$EpisodeStart, r$EpisodeTrough,
                r$EpisodeEnd, r$MaxDrawdown_Pct, r$DurationDays))
  }
}

# Overall drawdown stats
dd_overall <- data.frame(
  Dataset            = "NIFTY50_Index",
  MaxDrawdown        = min(idx[[COL_DRAWDOWN]], na.rm = TRUE),
  MaxDrawdown_Pct    = min(idx[[COL_DRAWDOWN]], na.rm = TRUE) * 100,
  N_episodes_20pct   = nrow(dd_episodes),
  Max_episode_duration = if (nrow(dd_episodes) > 0)
    max(dd_episodes$DurationDays) else NA_integer_,
  stringsAsFactors = FALSE
)
save_table(dd_overall, "crash_index_drawdown_overall.csv")

# ---------------------------------------------------------------------------
# 5. Volatility around crash days (window ± 5 days where available)
# ---------------------------------------------------------------------------
cat("\n[Step 5] Volatility context around crash days ...\n")

crash3_dates <- idx_r[[COL_DATE]][idx_r[[COL_CRASH3]] == TRUE]
idx_r_sorted  <- idx_r[order(idx_r[[COL_DATE]]), ]
all_dates     <- idx_r_sorted[[COL_DATE]]

window_stats <- do.call(rbind, lapply(crash3_dates, function(cd) {
  pos <- which(all_dates == cd)
  if (length(pos) == 0) return(NULL)
  pre5  <- max(1, pos - 5):(pos - 1)
  post5 <- (pos + 1):min(length(all_dates), pos + 5)
  data.frame(
    CrashDate     = as.character(cd),
    CrashReturn   = idx_r_sorted[[COL_RETURN]][pos],
    RollVol_pre   = mean(idx_r_sorted[[COL_ROLLVOL_20]][pre5],  na.rm = TRUE),
    RollVol_crash = idx_r_sorted[[COL_ROLLVOL_20]][pos],
    RollVol_post  = mean(idx_r_sorted[[COL_ROLLVOL_20]][post5], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
save_table(window_stats, "crash_index_volatility_around_crashes.csv")
cat(sprintf("  Volatility context computed for %d crash days\n",
            nrow(window_stats)))

# ---------------------------------------------------------------------------
# 6. Constituent crash analysis
# ---------------------------------------------------------------------------
cat("\n[Step 6] Constituent crash analysis ...\n")

# Per-symbol crash summary
syms <- sort(unique(panel_r[[COL_NSE]]))
sym_crash_rows <- do.call(rbind, lapply(syms, function(sym) {
  sub <- panel_r[panel_r[[COL_NSE]] == sym, ]
  n   <- nrow(sub)
  if (n < 10) return(NULL)
  data.frame(
    NSESymbol     = sym,
    N_Returns     = n,
    FirstDate     = as.character(min(sub[[COL_DATE]])),
    LastDate      = as.character(max(sub[[COL_DATE]])),
    N_Crash3      = sum(sub[[COL_CRASH3]] == TRUE, na.rm = TRUE),
    N_Crash5      = sum(sub[[COL_CRASH5]] == TRUE, na.rm = TRUE),
    Rate_Crash3   = sum(sub[[COL_CRASH3]] == TRUE, na.rm = TRUE) / n,
    Rate_Crash5   = sum(sub[[COL_CRASH5]] == TRUE, na.rm = TRUE) / n,
    Worst_Return  = min(sub[[COL_RETURN]], na.rm = TRUE),
    Max_Drawdown  = min(sub[[COL_DRAWDOWN]], na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))
sym_crash_rows <- sym_crash_rows[order(sym_crash_rows$Rate_Crash3,
                                       decreasing = TRUE), ]
save_table(sym_crash_rows, "crash_constituent_crash_summary.csv")

cat(sprintf("  Summarised %d constituents\n", nrow(sym_crash_rows)))
cat("  Top 5 highest -3% crash rates:\n")
for (i in seq_len(min(5, nrow(sym_crash_rows)))) {
  r <- sym_crash_rows[i, ]
  cat(sprintf("    %-15s  N=%5d  Crash3=%.2f%%  Worst=%.4f\n",
              r$NSESymbol, r$N_Returns,
              100 * r$Rate_Crash3, r$Worst_Return))
}

# Crash rates by weekday across all constituents
sym_wd_crash <- do.call(rbind, lapply(syms, function(sym) {
  sub <- panel_r[panel_r[[COL_NSE]] == sym, ]
  lapply(WEEKDAY_LEVELS, function(wd) {
    w <- sub[sub[[COL_WEEKDAY]] == wd, ]
    n <- nrow(w)
    data.frame(
      NSESymbol = sym,
      Weekday   = wd,
      N         = n,
      Rate_Crash3 = if (n > 0) mean(w[[COL_CRASH3]] == TRUE, na.rm = TRUE)
                    else NA_real_,
      Rate_Crash5 = if (n > 0) mean(w[[COL_CRASH5]] == TRUE, na.rm = TRUE)
                    else NA_real_,
      stringsAsFactors = FALSE
    )
  }) |> do.call(what = rbind)
}))
save_table(sym_wd_crash, "crash_constituent_weekday_crash_rates.csv")

# ---------------------------------------------------------------------------
# 7. Time-period breakdown
# ---------------------------------------------------------------------------
cat("\n[Step 7] Time-period crash breakdown ...\n")
#
# Period boundaries are based on actual data coverage, not arbitrary claims:
#   P1: 2007-09-17 to 2012-12-31  (GFC and European debt crisis era)
#   P2: 2013-01-01 to 2019-12-31  (recovery and expansion)
#   P3: 2020-01-01 to 2026-10-21  (COVID shock and post-pandemic)
#
# NOTE: Index data begins 2007-09-17. Claims of coverage from 2000 are
#       invalid for the index; constituent data extends earlier but with
#       survivorship bias.

PERIOD_BOUNDS <- list(
  list(label = "P1_2007to2012", start = as.Date("2007-09-17"),
       end   = as.Date("2012-12-31")),
  list(label = "P2_2013to2019", start = as.Date("2013-01-01"),
       end   = as.Date("2019-12-31")),
  list(label = "P3_2020to2026", start = as.Date("2020-01-01"),
       end   = as.Date("2026-12-31"))
)

period_crash <- do.call(rbind, lapply(PERIOD_BOUNDS, function(p) {
  sub <- idx_r[idx_r[[COL_DATE]] >= p$start &
               idx_r[[COL_DATE]] <= p$end, ]
  n   <- nrow(sub)
  if (n < 5) return(NULL)
  lapply(WEEKDAY_LEVELS, function(wd) {
    w  <- sub[sub[[COL_WEEKDAY]] == wd, ]
    nw <- nrow(w)
    data.frame(
      Period     = p$label,
      PeriodStart= as.character(p$start),
      PeriodEnd  = as.character(p$end),
      Weekday    = wd,
      N_Days     = nw,
      N_Crash3   = sum(w[[COL_CRASH3]] == TRUE, na.rm = TRUE),
      Rate_Crash3= if (nw > 0) mean(w[[COL_CRASH3]] == TRUE, na.rm = TRUE)
                   else NA_real_,
      N_Crash5   = sum(w[[COL_CRASH5]] == TRUE, na.rm = TRUE),
      Rate_Crash5= if (nw > 0) mean(w[[COL_CRASH5]] == TRUE, na.rm = TRUE)
                   else NA_real_,
      Mean_Return= mean(w[[COL_RETURN]], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }) |> do.call(what = rbind)
}))
save_table(period_crash, "crash_index_period_weekday_breakdown.csv")
cat(sprintf("  Period-weekday breakdown: %d rows\n", nrow(period_crash)))

# ---------------------------------------------------------------------------
# 8. Figures
# ---------------------------------------------------------------------------
cat("\n[Step 8] Crash analysis figures ...\n")

# ---- 8a. Top-20 worst return days — heatmap style -------------------------
top20 <- head(idx_r[order(idx_r[[COL_RETURN]]), ], 20)
top20$Label <- paste0(top20[[COL_DATE]], " (", top20[[COL_WEEKDAY]], ")")
top20$Return_Pct <- top20[[COL_RETURN]] * 100
top20$Label <- factor(top20$Label,
                       levels = rev(top20$Label))

p_top20 <- ggplot2::ggplot(top20,
    ggplot2::aes(x = Return_Pct, y = Label,
                 fill = Return_Pct)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_gradient(low = "firebrick", high = "orange",
                                name = "Return (%)") +
  ggplot2::geom_vline(xintercept = c(CRASH_THRESH_3, CRASH_THRESH_5) * 100,
                       linetype = c("dashed", "solid"),
                       colour = "grey30") +
  ggplot2::labs(
    title    = "Top 20 Worst Daily Returns — NIFTY 50 Index",
    subtitle = "Vertical lines: -3% and -5% thresholds",
    x = "Daily Return (%)", y = NULL
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "crash_01_top20_worst_returns.png"), p_top20,
        width = 10, height = 8)

# ---- 8b. Drawdown time-series with episode shading -----------------------
idx_dd <- idx[!is.na(idx[[COL_DRAWDOWN]]), ]
idx_dd$DD_Pct <- idx_dd[[COL_DRAWDOWN]] * 100

p_dd_ep <- ggplot2::ggplot(idx_dd,
    ggplot2::aes(x = .data[[COL_DATE]], y = DD_Pct)) +
  ggplot2::geom_line(colour = "firebrick", linewidth = 0.35) +
  ggplot2::geom_hline(yintercept = DD_THRESH_20 * 100,
                       linetype = "dashed", colour = "grey30") +
  ggplot2::annotate("text", x = min(idx_dd[[COL_DATE]]),
                    y = DD_THRESH_20 * 100 - 0.8,
                    label = "-20% threshold", hjust = 0,
                    colour = "grey30", size = 3)
# Shade each 20%+ episode
if (nrow(dd_episodes) > 0) {
  for (i in seq_len(nrow(dd_episodes))) {
    ep <- dd_episodes[i, ]
    p_dd_ep <- p_dd_ep +
      ggplot2::annotate("rect",
        xmin = as.Date(ep$EpisodeStart), xmax = as.Date(ep$EpisodeEnd),
        ymin = -Inf, ymax = 0,
        fill = "firebrick", alpha = 0.08)
  }
}
p_dd_ep <- p_dd_ep +
  ggplot2::labs(
    title    = "NIFTY 50 Drawdown from Running Peak",
    subtitle = sprintf("Shaded regions: %d episodes reaching >= 20%% drawdown",
                       nrow(dd_episodes)),
    x = "Date", y = "Drawdown (%)"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "crash_02_drawdown_with_episodes.png"), p_dd_ep,
        width = 14, height = 5)

# ---- 8c. Crash rate by weekday and period (stacked bar) ------------------
period_crash_wd <- period_crash[!is.na(period_crash$Rate_Crash3) &
                                 period_crash$N_Days >= 10, ]
period_crash_wd$Weekday <- factor(period_crash_wd$Weekday,
                                   levels = WEEKDAY_LEVELS)

p_period_wd <- ggplot2::ggplot(period_crash_wd,
    ggplot2::aes(x = Weekday, y = Rate_Crash3 * 100,
                 fill = Period)) +
  ggplot2::geom_col(position = "dodge", alpha = 0.85) +
  ggplot2::scale_fill_brewer(palette = "Set1",
    labels = c("P1_2007to2012" = "2007-2012 (GFC era)",
               "P2_2013to2019" = "2013-2019 (expansion)",
               "P3_2020to2026" = "2020-2026 (COVID+)")) +
  ggplot2::labs(
    title    = "NIFTY 50 Crash Rate (-3%) by Weekday and Period",
    subtitle = paste0("Period boundaries based on data coverage; ",
                      "index data starts 2007-09-17"),
    x = "Day of Week", y = "Crash Rate (%)",
    fill = "Period"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5),
                 legend.position = "bottom")
save_gg(file.path(DIR_FIGURES, "crash_03_crash_rate_by_period_weekday.png"),
        p_period_wd, width = 10, height = 6)

# ---- 8d. Constituent crash-rate bar chart ---------------------------------
sym_crash_plot <- sym_crash_rows[order(-sym_crash_rows$Rate_Crash3), ]
sym_crash_plot$NSESymbol <- factor(sym_crash_plot$NSESymbol,
                                    levels = sym_crash_plot$NSESymbol)

p_sym_crash <- ggplot2::ggplot(sym_crash_plot,
    ggplot2::aes(x = NSESymbol, y = Rate_Crash3 * 100)) +
  ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
  ggplot2::geom_hline(
    yintercept = mean(sym_crash_rows$Rate_Crash3, na.rm = TRUE) * 100,
    linetype = "dashed", colour = "firebrick") +
  ggplot2::annotate("text",
    x = 1, y = mean(sym_crash_rows$Rate_Crash3, na.rm = TRUE) * 100 + 0.05,
    label = "Panel mean", hjust = 0, colour = "firebrick", size = 3) +
  ggplot2::labs(
    title    = "Crash Rate (-3% threshold) by Constituent",
    subtitle = "Current NIFTY 50 constituents; survivorship bias present",
    x = NULL, y = "Crash Rate (%)"
  ) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.title  = ggplot2::element_text(face = "bold", hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "crash_04_constituent_crash_rates.png"),
        p_sym_crash, width = 14, height = 6)

# ---- 8e. Volatility context around crashes -------------------------------
if (!is.null(window_stats) && nrow(window_stats) > 0) {
  # Summary bar: mean vol in pre/crash/post windows
  vol_context <- data.frame(
    Window   = c("Pre-crash (5d avg)", "Crash day", "Post-crash (5d avg)"),
    Mean_Vol = c(
      mean(window_stats$RollVol_pre,   na.rm = TRUE) * 100,
      mean(window_stats$RollVol_crash, na.rm = TRUE) * 100,
      mean(window_stats$RollVol_post,  na.rm = TRUE) * 100
    )
  )
  vol_context$Window <- factor(vol_context$Window,
    levels = c("Pre-crash (5d avg)", "Crash day", "Post-crash (5d avg)"))

  p_vol_ctx <- ggplot2::ggplot(vol_context,
      ggplot2::aes(x = Window, y = Mean_Vol, fill = Window)) +
    ggplot2::geom_col(alpha = 0.85, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c(
      "Pre-crash (5d avg)"  = "steelblue",
      "Crash day"           = "firebrick",
      "Post-crash (5d avg)" = "darkorange")) +
    ggplot2::labs(
      title    = "Average Rolling Volatility Around -3% Crash Days",
      subtitle = sprintf("Based on RollVol_20; averaged across %d crash events",
                         nrow(window_stats)),
      x = NULL, y = "Mean 20-Day Rolling Volatility (%)"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5))
  save_gg(file.path(DIR_FIGURES, "crash_05_volatility_around_crashes.png"),
          p_vol_ctx, width = 7, height = 5)
}

cat(sprintf("\n[Crash Analysis Complete]\n"))
cat(sprintf("  Tables saved to  : %s\n", DIR_TABLES))
cat(sprintf("  Figures saved to : %s\n", DIR_FIGURES))
cat(sprintf("  Finished: %s\n\n", Sys.time()))
