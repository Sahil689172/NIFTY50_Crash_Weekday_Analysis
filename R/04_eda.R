# =============================================================================
# Script  : 04_eda.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Exploratory Data Analysis — descriptive statistics, return
#           distributions, weekday patterns, crash counts, drawdown
#           analysis, and rolling volatility visualisations.
#           Index and constituents are analysed separately.
#           NO statistical testing or modelling is performed here.
# Inputs  : data/processed/features/nifty50_index_features.csv
#           data/processed/features/all_constituents_features.csv
# Outputs : results/tables/eda_*.csv
#           results/figures/eda_*.png
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/04_eda.R")
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
require_pkg("ggplot2")
require_pkg("e1071")

create_results_dirs()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  04_eda.R — Exploratory Data Analysis\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
cat("[Step 1] Loading feature data ...\n")

idx   <- load_index()
panel <- load_panel()

idx_r   <- valid_returns(idx)     # rows with non-NA ReturnPrice
panel_r <- valid_returns(panel)

cat(sprintf("  Index   : %d total rows | %d with valid returns\n",
            nrow(idx), nrow(idx_r)))
cat(sprintf("  Panel   : %d total rows | %d with valid returns\n",
            nrow(panel), nrow(panel_r)))
cat(sprintf("  Stocks  : %d unique constituents in panel\n",
            length(unique(panel[[COL_NSE]]))))

# Verify weekday coverage
cat("\n  Index weekday distribution (valid-return rows):\n")
print(table(idx_r[[COL_WEEKDAY]]))

# ---------------------------------------------------------------------------
# 2. Overall descriptive statistics — Index
# ---------------------------------------------------------------------------
cat("\n[Step 2] Descriptive statistics — NIFTY 50 index ...\n")

## 2a. Full-sample
idx_stats <- as.data.frame(t(desc_stats(idx_r[[COL_RETURN]])))
idx_stats$Dataset  <- "NIFTY50_Index"
idx_stats$Weekday  <- "All"
idx_stats$Period   <- paste0(min(idx_r[[COL_DATE]]), " to ",
                              max(idx_r[[COL_DATE]]))
idx_stats$N_total_rows    <- nrow(idx)
idx_stats$N_NA_returns    <- sum(is.na(idx[[COL_RETURN]]))
idx_stats$N_valid_returns <- nrow(idx_r)

## 2b. By weekday
wd_stats_idx <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  sub <- idx_r[idx_r[[COL_WEEKDAY]] == wd, ]
  if (nrow(sub) < 5) return(NULL)
  s <- as.data.frame(t(desc_stats(sub[[COL_RETURN]])))
  s$Dataset <- "NIFTY50_Index"
  s$Weekday <- wd
  s$Period  <- paste0(min(sub[[COL_DATE]]), " to ", max(sub[[COL_DATE]]))
  s$N_total_rows    <- nrow(sub)
  s$N_NA_returns    <- 0L   # valid_returns already filtered
  s$N_valid_returns <- nrow(sub)
  s
}))

idx_all_stats <- rbind(idx_stats, wd_stats_idx)
save_table(idx_all_stats, "eda_index_descriptive_stats.csv")

## Print to console
cat(sprintf(
  "  Full sample — N=%d  Mean=%.5f  SD=%.5f  Skew=%.3f  Kurt=%.3f\n",
  as.integer(idx_stats$N), idx_stats$Mean, idx_stats$SD,
  idx_stats$Skewness, idx_stats$Kurtosis
))
cat("  By weekday:\n")
for (i in seq_len(nrow(wd_stats_idx))) {
  r <- wd_stats_idx[i, ]
  cat(sprintf("    %-10s N=%4d  Mean=%+.5f  SD=%.5f\n",
              r$Weekday, as.integer(r$N), r$Mean, r$SD))
}

# ---------------------------------------------------------------------------
# 3. Overall descriptive statistics — Constituents
# ---------------------------------------------------------------------------
cat("\n[Step 3] Descriptive statistics — constituents ...\n")

## Per-symbol summary
syms <- sort(unique(panel_r[[COL_NSE]]))
sym_stats <- do.call(rbind, lapply(syms, function(sym) {
  sub <- panel_r[panel_r[[COL_NSE]] == sym, ]
  if (nrow(sub) < 10) return(NULL)
  s <- as.data.frame(t(desc_stats(sub[[COL_RETURN]])))
  s$NSESymbol  <- sym
  s$FirstDate  <- as.character(min(sub[[COL_DATE]]))
  s$LastDate   <- as.character(max(sub[[COL_DATE]]))
  s$N_Close_fallback <- sum(sub[[COL_RETURN_SRC]] == "Close", na.rm = TRUE)
  s
}))
save_table(sym_stats, "eda_constituent_descriptive_stats.csv")

## Panel-wide aggregate
panel_overall <- as.data.frame(t(desc_stats(panel_r[[COL_RETURN]])))
panel_overall$Dataset <- "All_Constituents"
panel_overall$N_symbols <- length(syms)
save_table(panel_overall, "eda_constituent_panel_overall_stats.csv")

cat(sprintf("  Constituents summarised: %d symbols\n", nrow(sym_stats)))
cat(sprintf("  Panel-wide  N=%d  Mean=%.5f  SD=%.5f\n",
            as.integer(panel_overall$N), panel_overall$Mean, panel_overall$SD))

## Weekday stats for constituents (panel pooled)
wd_stats_panel <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  sub <- panel_r[panel_r[[COL_WEEKDAY]] == wd, ]
  if (nrow(sub) < 5) return(NULL)
  s <- as.data.frame(t(desc_stats(sub[[COL_RETURN]])))
  s$Dataset <- "All_Constituents"
  s$Weekday <- wd
  s
}))
save_table(wd_stats_panel, "eda_constituent_weekday_stats.csv")

# ---------------------------------------------------------------------------
# 4. Crash counts and proportions
# ---------------------------------------------------------------------------
cat("\n[Step 4] Crash counts and proportions ...\n")

crash_summary <- function(df_r, label) {
  total <- nrow(df_r)
  rows <- lapply(WEEKDAY_LEVELS, function(wd) {
    sub   <- df_r[df_r[[COL_WEEKDAY]] == wd, ]
    n_wd  <- nrow(sub)
    n_c3  <- sum(sub[[COL_CRASH3]] == TRUE, na.rm = TRUE)
    n_c5  <- sum(sub[[COL_CRASH5]] == TRUE, na.rm = TRUE)
    data.frame(
      Dataset        = label,
      Weekday        = wd,
      N_Returns      = n_wd,
      N_Crash3       = n_c3,
      N_Crash5       = n_c5,
      Rate_Crash3    = if (n_wd > 0) n_c3 / n_wd else NA_real_,
      Rate_Crash5    = if (n_wd > 0) n_c5 / n_wd else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  totals <- data.frame(
    Dataset     = label,
    Weekday     = "All",
    N_Returns   = total,
    N_Crash3    = sum(df_r[[COL_CRASH3]] == TRUE, na.rm = TRUE),
    N_Crash5    = sum(df_r[[COL_CRASH5]] == TRUE, na.rm = TRUE),
    Rate_Crash3 = mean(df_r[[COL_CRASH3]] == TRUE, na.rm = TRUE),
    Rate_Crash5 = mean(df_r[[COL_CRASH5]] == TRUE, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  rbind(do.call(rbind, rows), totals)
}

crash_idx   <- crash_summary(idx_r,   "NIFTY50_Index")
crash_panel <- crash_summary(panel_r, "All_Constituents")
crash_all   <- rbind(crash_idx, crash_panel)
save_table(crash_all, "eda_crash_counts_by_weekday.csv")

cat("  Index crash rates by weekday (-3% threshold):\n")
for (i in seq_len(nrow(crash_idx))) {
  r <- crash_idx[i, ]
  cat(sprintf("    %-12s N=%4d  Crash3=%3d (%.2f%%)  Crash5=%3d (%.2f%%)\n",
              r$Weekday, r$N_Returns,
              r$N_Crash3, 100 * r$Rate_Crash3,
              r$N_Crash5, 100 * r$Rate_Crash5))
}

# ---------------------------------------------------------------------------
# 5. Drawdown summary
# ---------------------------------------------------------------------------
cat("\n[Step 5] Drawdown summary ...\n")

dd_summary <- function(df, label) {
  dd <- df[[COL_DRAWDOWN]]
  dd <- dd[!is.na(dd)]
  data.frame(
    Dataset       = label,
    N_dd_obs      = length(dd),
    Max_Drawdown  = min(dd),
    Mean_Drawdown = mean(dd),
    Median_Drawdown = median(dd),
    N_dd_over20pct = sum(dd <= DD_THRESH_20),
    Pct_dd_over20pct = 100 * mean(dd <= DD_THRESH_20),
    stringsAsFactors = FALSE
  )
}

dd_idx_sum   <- dd_summary(idx,   "NIFTY50_Index")
dd_panel_sum <- dd_summary(panel, "All_Constituents")
save_table(rbind(dd_idx_sum, dd_panel_sum), "eda_drawdown_summary.csv")

cat(sprintf("  Index max drawdown: %.2f%%  |  Days in >=20%% DD: %d (%.1f%%)\n",
            100 * dd_idx_sum$Max_Drawdown,
            dd_idx_sum$N_dd_over20pct,
            dd_idx_sum$Pct_dd_over20pct))

# ---------------------------------------------------------------------------
# 6. Figures
# ---------------------------------------------------------------------------
cat("\n[Step 6] Generating figures ...\n")

# --- Helper: common ggplot theme -------------------------------------------
theme_research <- function() {
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, colour = "grey40"),
      axis.title    = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ---- 6a. Return distribution — histogram + density (Index) ----------------
p_hist_idx <- ggplot2::ggplot(idx_r,
    ggplot2::aes(x = .data[[COL_RETURN]])) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                           bins = 80, fill = "steelblue", colour = "white",
                           alpha = 0.8) +
  ggplot2::geom_density(colour = "firebrick", linewidth = 0.9) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  ggplot2::labs(
    title    = "NIFTY 50 Daily Return Distribution",
    subtitle = sprintf("N = %d  |  Mean = %.4f  |  SD = %.4f  |  Period: %s – %s",
                       nrow(idx_r),
                       mean(idx_r[[COL_RETURN]]),
                       sd(idx_r[[COL_RETURN]]),
                       min(idx_r[[COL_DATE]]),
                       max(idx_r[[COL_DATE]])),
    x = "Daily Return (arithmetic)",
    y = "Density"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_01_index_return_histogram.png"), p_hist_idx)

# ---- 6b. Q-Q plot — Index returns -----------------------------------------
save_png(file.path(DIR_FIGURES, "eda_02_index_return_qqplot.png"), {
  qqnorm(idx_r[[COL_RETURN]],
         main = "Q-Q Plot: NIFTY 50 Daily Returns vs Normal",
         pch  = 16, cex = 0.4, col = "steelblue")
  qqline(idx_r[[COL_RETURN]], col = "firebrick", lwd = 2)
  mtext(sprintf("N = %d  (2007-09-17 to %s)", nrow(idx_r),
                max(idx_r[[COL_DATE]])),
        side = 3, line = 0.2, cex = 0.8, col = "grey40")
})

# ---- 6c. Boxplot by weekday — Index ----------------------------------------
p_box_idx <- ggplot2::ggplot(idx_r,
    ggplot2::aes(x = .data[[COL_WEEKDAY]],
                 y = .data[[COL_RETURN]],
                 fill = .data[[COL_WEEKDAY]])) +
  ggplot2::geom_boxplot(outlier.size = 0.6, outlier.alpha = 0.4,
                         alpha = 0.7, show.legend = FALSE) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    title    = "NIFTY 50 Daily Returns by Weekday",
    subtitle = "Crash observations retained (not removed as outliers)",
    x = "Day of Week",
    y = "Daily Return (arithmetic)"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_03_index_weekday_boxplot.png"), p_box_idx)

# ---- 6d. Mean return by weekday with 95% CI — Index -----------------------
wd_mean_idx <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  r <- idx_r[[COL_RETURN]][idx_r[[COL_WEEKDAY]] == wd]
  r <- r[!is.na(r)]
  n <- length(r)
  se <- sd(r) / sqrt(n)
  data.frame(Weekday = wd, Mean = mean(r), SE = se,
             CI_lo = mean(r) - 1.96 * se, CI_hi = mean(r) + 1.96 * se,
             N = n, stringsAsFactors = FALSE)
}))
wd_mean_idx$Weekday <- factor(wd_mean_idx$Weekday,
                               levels = WEEKDAY_LEVELS, ordered = FALSE)

p_mean_idx <- ggplot2::ggplot(wd_mean_idx,
    ggplot2::aes(x = Weekday, y = Mean, colour = Weekday)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(size = 4, show.legend = FALSE) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lo, ymax = CI_hi),
                          width = 0.25, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = paste0("n=", N)),
                     vjust = -1.5, size = 3, show.legend = FALSE) +
  ggplot2::scale_colour_brewer(palette = "Set1") +
  ggplot2::labs(
    title    = "Mean Daily Return by Weekday — NIFTY 50 Index",
    subtitle = "Error bars: 95% confidence intervals (±1.96 SE)",
    x = "Day of Week", y = "Mean Daily Return"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_04_index_weekday_mean_return.png"), p_mean_idx)
save_table(wd_mean_idx, "eda_index_weekday_mean_ci.csv")

# ---- 6e. Weekday volatility (SD) with SE bars — Index ---------------------
wd_vol_idx <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  r <- idx_r[[COL_RETURN]][idx_r[[COL_WEEKDAY]] == wd]
  r <- r[!is.na(r)]
  n <- length(r)
  # Bootstrap SE of SD using the asymptotic approximation
  se_sd <- sd(r) / sqrt(2 * (n - 1))
  data.frame(Weekday = wd, SD = sd(r), SE = se_sd,
             CI_lo = max(0, sd(r) - 1.96 * se_sd),
             CI_hi = sd(r) + 1.96 * se_sd,
             N = n, stringsAsFactors = FALSE)
}))
wd_vol_idx$Weekday <- factor(wd_vol_idx$Weekday,
                              levels = WEEKDAY_LEVELS, ordered = FALSE)

p_vol_idx <- ggplot2::ggplot(wd_vol_idx,
    ggplot2::aes(x = Weekday, y = SD, fill = Weekday)) +
  ggplot2::geom_col(alpha = 0.8, show.legend = FALSE) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lo, ymax = CI_hi),
                          width = 0.3) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    title    = "Daily Return Volatility (SD) by Weekday — NIFTY 50 Index",
    subtitle = "Error bars: approximate 95% CI",
    x = "Day of Week", y = "Standard Deviation of Returns"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_05_index_weekday_volatility.png"), p_vol_idx)
save_table(wd_vol_idx, "eda_index_weekday_volatility.csv")

# ---- 6f. Crash rates by weekday — Index -----------------------------------
crash_idx_wd <- crash_idx[crash_idx$Weekday != "All", ]
crash_idx_wd$Weekday <- factor(crash_idx_wd$Weekday,
                                levels = WEEKDAY_LEVELS, ordered = FALSE)

# Reshape for grouped bar
cr_long <- rbind(
  data.frame(Weekday = crash_idx_wd$Weekday,
             Rate = crash_idx_wd$Rate_Crash3,
             Threshold = "-3% crash", stringsAsFactors = FALSE),
  data.frame(Weekday = crash_idx_wd$Weekday,
             Rate = crash_idx_wd$Rate_Crash5,
             Threshold = "-5% crash", stringsAsFactors = FALSE)
)
cr_long$Weekday <- factor(cr_long$Weekday,
                           levels = WEEKDAY_LEVELS, ordered = FALSE)

p_crash_wd <- ggplot2::ggplot(cr_long,
    ggplot2::aes(x = Weekday, y = Rate * 100,
                 fill = Threshold)) +
  ggplot2::geom_col(position = "dodge", alpha = 0.85) +
  ggplot2::scale_fill_manual(values = c("-3% crash" = "steelblue",
                                         "-5% crash" = "firebrick")) +
  ggplot2::labs(
    title    = "Crash Frequency by Weekday — NIFTY 50 Index",
    subtitle = "Proportion of trading days with return at or below threshold",
    x = "Day of Week", y = "Crash Rate (%)",
    fill = "Threshold"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_06_index_crash_rate_by_weekday.png"),
        p_crash_wd)

# ---- 6g. Time-series of Index returns -------------------------------------
p_ts_ret <- ggplot2::ggplot(idx_r,
    ggplot2::aes(x = .data[[COL_DATE]], y = .data[[COL_RETURN]])) +
  ggplot2::geom_line(colour = "steelblue", linewidth = 0.25, alpha = 0.7) +
  ggplot2::geom_hline(yintercept = c(CRASH_THRESH_3, CRASH_THRESH_5),
                       linetype = c("dashed", "solid"),
                       colour = c("orange", "firebrick"), linewidth = 0.5) +
  ggplot2::annotate("text", x = min(idx_r[[COL_DATE]]),
                    y = CRASH_THRESH_3 - 0.003,
                    label = "-3% threshold", hjust = 0,
                    colour = "orange", size = 3) +
  ggplot2::annotate("text", x = min(idx_r[[COL_DATE]]),
                    y = CRASH_THRESH_5 - 0.003,
                    label = "-5% threshold", hjust = 0,
                    colour = "firebrick", size = 3) +
  ggplot2::labs(
    title    = "NIFTY 50 Daily Returns Over Time",
    subtitle = "Horizontal lines mark -3% and -5% crash thresholds",
    x = "Date", y = "Daily Return (arithmetic)"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_07_index_return_timeseries.png"),
        p_ts_ret, width = 14, height = 5)

# ---- 6h. Drawdown over time — Index ---------------------------------------
idx_dd <- idx[!is.na(idx[[COL_DRAWDOWN]]), ]
p_dd_ts <- ggplot2::ggplot(idx_dd,
    ggplot2::aes(x = .data[[COL_DATE]], y = .data[[COL_DRAWDOWN]] * 100)) +
  ggplot2::geom_line(colour = "firebrick", linewidth = 0.4) +
  ggplot2::geom_hline(yintercept = DD_THRESH_20 * 100,
                       linetype = "dashed", colour = "grey30") +
  ggplot2::annotate("text", x = min(idx_dd[[COL_DATE]]),
                    y = DD_THRESH_20 * 100 - 1,
                    label = "-20% threshold", hjust = 0,
                    colour = "grey30", size = 3) +
  ggplot2::labs(
    title    = "NIFTY 50 Drawdown from Running Peak",
    subtitle = "Drawdown = (Price / Running Peak) - 1",
    x = "Date", y = "Drawdown (%)"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_08_index_drawdown_timeseries.png"),
        p_dd_ts, width = 14, height = 5)

# ---- 6i. Rolling volatility over time — Index (all 3 windows) -------------
# Reshape to long format manually (no tidyr dependency)
rv_cols <- c(COL_ROLLVOL_5, COL_ROLLVOL_20, COL_ROLLVOL_30)
rv_rows <- lapply(rv_cols, function(col) {
  sub <- idx[!is.na(idx[[col]]), ]
  data.frame(Date    = sub[[COL_DATE]],
             RollVol = sub[[col]] * 100,   # express as %
             Window  = gsub("RollVol_", "", col),
             stringsAsFactors = FALSE)
})
rv_long <- do.call(rbind, rv_rows)
rv_long$Window <- factor(rv_long$Window,
                          levels = c("5", "20", "30"),
                          labels = c("5-day", "20-day", "30-day"))

p_rv_ts <- ggplot2::ggplot(rv_long,
    ggplot2::aes(x = Date, y = RollVol, colour = Window, group = Window)) +
  ggplot2::geom_line(linewidth = 0.4, alpha = 0.85) +
  ggplot2::scale_colour_manual(
    values = c("5-day" = "steelblue", "20-day" = "darkorange",
               "30-day" = "firebrick")) +
  ggplot2::labs(
    title    = "NIFTY 50 Rolling Return Volatility Over Time",
    subtitle = "Standard deviation of daily returns; trailing windows of 5, 20, 30 trading days",
    x = "Date", y = "Rolling Volatility (% per day)",
    colour   = "Window"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_09_index_rolling_volatility.png"),
        p_rv_ts, width = 14, height = 5)

# ---- 6j. Return distribution — constituent panel (density overlay sample) --
# Plot top-5 by row count to avoid overplotting
top5 <- head(names(sort(table(panel_r[[COL_NSE]]),
                         decreasing = TRUE)), 5)
panel_top5 <- panel_r[panel_r[[COL_NSE]] %in% top5, ]

p_dens_panel <- ggplot2::ggplot(panel_top5,
    ggplot2::aes(x = .data[[COL_RETURN]],
                 colour = .data[[COL_NSE]])) +
  ggplot2::geom_density(linewidth = 0.7, key_glyph = "path") +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                       colour = "grey40") +
  ggplot2::scale_colour_brewer(palette = "Set1") +
  ggplot2::labs(
    title    = "Return Density — 5 Longest-History Constituents",
    subtitle = "Based on Adjusted Close (Close where Adjusted unavailable)",
    x = "Daily Return (arithmetic)", y = "Density",
    colour = "Symbol"
  ) +
  ggplot2::coord_cartesian(xlim = c(-0.20, 0.20)) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_10_constituent_return_density.png"),
        p_dens_panel)

# ---- 6k. Mean return by weekday — panel pooled ----------------------------
wd_mean_panel <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  r <- panel_r[[COL_RETURN]][panel_r[[COL_WEEKDAY]] == wd]
  r <- r[!is.na(r)]
  n <- length(r)
  se <- sd(r) / sqrt(n)
  data.frame(Weekday = wd, Mean = mean(r), SE = se,
             CI_lo = mean(r) - 1.96 * se, CI_hi = mean(r) + 1.96 * se,
             N = n, stringsAsFactors = FALSE)
}))
wd_mean_panel$Weekday <- factor(wd_mean_panel$Weekday,
                                 levels = WEEKDAY_LEVELS, ordered = FALSE)

p_mean_panel <- ggplot2::ggplot(wd_mean_panel,
    ggplot2::aes(x = Weekday, y = Mean, colour = Weekday)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                       colour = "grey60") +
  ggplot2::geom_point(size = 4, show.legend = FALSE) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_lo, ymax = CI_hi),
                          width = 0.25, show.legend = FALSE) +
  ggplot2::scale_colour_brewer(palette = "Set1") +
  ggplot2::labs(
    title    = "Mean Daily Return by Weekday — All Constituents (Pooled)",
    subtitle = "Error bars: 95% confidence intervals (±1.96 SE); survivorship bias present",
    x = "Day of Week", y = "Mean Daily Return"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_11_constituent_weekday_mean_return.png"),
        p_mean_panel)
save_table(wd_mean_panel, "eda_constituent_weekday_mean_ci.csv")

# ---- 6l. Crash rate by weekday — panel pooled -----------------------------
crash_panel_wd <- crash_panel[crash_panel$Weekday != "All", ]
crash_panel_wd$Weekday <- factor(crash_panel_wd$Weekday,
                                  levels = WEEKDAY_LEVELS, ordered = FALSE)

crp_long <- rbind(
  data.frame(Weekday = crash_panel_wd$Weekday,
             Rate = crash_panel_wd$Rate_Crash3 * 100,
             Threshold = "-3% crash", stringsAsFactors = FALSE),
  data.frame(Weekday = crash_panel_wd$Weekday,
             Rate = crash_panel_wd$Rate_Crash5 * 100,
             Threshold = "-5% crash", stringsAsFactors = FALSE)
)
crp_long$Weekday <- factor(crp_long$Weekday,
                            levels = WEEKDAY_LEVELS, ordered = FALSE)

p_crash_panel <- ggplot2::ggplot(crp_long,
    ggplot2::aes(x = Weekday, y = Rate, fill = Threshold)) +
  ggplot2::geom_col(position = "dodge", alpha = 0.85) +
  ggplot2::scale_fill_manual(
    values = c("-3% crash" = "steelblue", "-5% crash" = "firebrick")) +
  ggplot2::labs(
    title    = "Crash Frequency by Weekday — All Constituents (Pooled)",
    subtitle = "Survivorship bias present; current NIFTY 50 constituents only",
    x = "Day of Week", y = "Crash Rate (%)",
    fill = "Threshold"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_12_constituent_crash_rate_by_weekday.png"),
        p_crash_panel)

# ---- 6m. Drawdown distribution — histogram --------------------------------
dd_valid <- idx[[COL_DRAWDOWN]]
dd_valid <- dd_valid[!is.na(dd_valid) & dd_valid < 0]   # exclude 0 (at peak)

p_dd_hist <- ggplot2::ggplot(data.frame(DD = dd_valid * 100),
    ggplot2::aes(x = DD)) +
  ggplot2::geom_histogram(bins = 60, fill = "firebrick",
                           colour = "white", alpha = 0.8) +
  ggplot2::geom_vline(xintercept = DD_THRESH_20 * 100,
                       linetype = "dashed", colour = "grey30", linewidth = 0.8) +
  ggplot2::annotate("text", x = DD_THRESH_20 * 100 + 0.5,
                    y = Inf, vjust = 1.5, hjust = 0,
                    label = "-20% threshold", colour = "grey30", size = 3.5) +
  ggplot2::labs(
    title    = "Distribution of NIFTY 50 Drawdown Values (Below-Peak Days Only)",
    subtitle = "Drawdown = (Price / Running Peak) - 1",
    x = "Drawdown (%)", y = "Count"
  ) +
  theme_research()
save_gg(file.path(DIR_FIGURES, "eda_13_index_drawdown_histogram.png"),
        p_dd_hist)

# ---------------------------------------------------------------------------
# 7. Console completion
# ---------------------------------------------------------------------------
cat("\n[EDA Complete]\n")
cat(sprintf("  Tables saved to  : %s\n", DIR_TABLES))
cat(sprintf("  Figures saved to : %s\n", DIR_FIGURES))
cat(sprintf("  Finished: %s\n\n", Sys.time()))
