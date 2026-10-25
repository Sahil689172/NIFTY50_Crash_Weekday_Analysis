# =============================================================================
# Script  : 08_robustness_checks.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Robustness and sensitivity analysis — subperiod stability,
#           alternative crash thresholds, multiple-testing correction,
#           sensitivity to NA observations, robust inference comparison,
#           and explicit documentation of limitations.
# Inputs  : data/processed/features/nifty50_index_features.csv
#           data/processed/features/all_constituents_features.csv
# Outputs : results/tables/rob_*.csv
#           results/figures/rob_*.png
# Packages: ggplot2, sandwich, lmtest
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/08_robustness_checks.R")
# =============================================================================
#
# CHECKS PERFORMED
# ----------------
# R1.  Subperiod ANOVA: weekday effects within each of 3 sub-periods.
# R2.  Alternative crash thresholds: -2%, -4%, -7%.
# R3.  Sensitivity to ReturnSource: Adjusted-only subsample vs full sample.
# R4.  OLS weekday regression on each sub-period.
# R5.  Multiple-testing correction across all hypothesis tests.
# R6.  Robust inference comparison: OLS vs HAC for key regressions.
# R7.  Constituent-level weekday tests (individual Welch ANOVAs).
# R8.  Explicit limitations documentation.
#
# LIMITATIONS EXPLICITLY DOCUMENTED
# ----------------------------------
# • Index history begins 2007-09-17 (Yahoo Finance). Any claim of 2000
#   coverage for the index is incorrect.
# • Current NIFTY 50 constituents introduce survivorship bias.
# • Financial return series violate i.i.d. assumptions (ARCH, fat tails).
# • Sample size for -5% crash days is small (n=22 for index); logistic
#   regression results at that threshold should be treated as exploratory.
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
require_pkg("ggplot2")
for (pkg in c("sandwich", "lmtest")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf("Package '%s' required. Run: install.packages('%s')", pkg, pkg))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

create_results_dirs()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  08_robustness_checks.R\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
cat("[Step 1] Loading data ...\n")
idx     <- load_index()
idx_r   <- valid_returns(idx)
idx_r[[COL_WEEKDAY]] <- relevel(idx_r[[COL_WEEKDAY]], ref = "Monday")

panel   <- load_panel()
panel_r <- valid_returns(panel)
cat(sprintf("  Index valid returns: %d\n", nrow(idx_r)))
cat(sprintf("  Panel valid returns: %d\n", nrow(panel_r)))

ALPHA <- 0.05

# Sub-period definitions — based on actual data coverage, not arbitrary
SUBPERIODS <- list(
  list(label = "P1_2007to2012", start = as.Date("2007-09-17"),
       end   = as.Date("2012-12-31"),
       note  = "GFC and European debt crisis era"),
  list(label = "P2_2013to2019", start = as.Date("2013-01-01"),
       end   = as.Date("2019-12-31"),
       note  = "Recovery and expansion phase"),
  list(label = "P3_2020to2026", start = as.Date("2020-01-01"),
       end   = as.Date("2026-12-31"),
       note  = "COVID-19 shock and post-pandemic")
)

# Alternative crash thresholds (in addition to -3% and -5%)
ALT_THRESHOLDS <- c(-0.02, -0.04, -0.07)

# ---------------------------------------------------------------------------
# Helper: run Welch ANOVA and return a tidy row
# ---------------------------------------------------------------------------
welch_anova_row <- function(df, dataset_label) {
  if (nrow(df) < 20) return(NULL)
  res <- tryCatch(
    oneway.test(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
                data = df, var.equal = FALSE),
    error = function(e) NULL
  )
  if (is.null(res)) return(NULL)
  data.frame(
    Dataset     = dataset_label,
    N           = nrow(df),
    F_stat      = round(res$statistic, 4),
    DF1         = round(res$parameter[1], 2),
    DF2         = round(res$parameter[2], 2),
    P_value     = signif(res$p.value, 4),
    Significant = res$p.value < ALPHA,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# R1. Subperiod ANOVA
# ---------------------------------------------------------------------------
cat("\n[R1] Subperiod ANOVA — weekday returns within each period ...\n")

sp_anova_rows <- do.call(rbind, lapply(SUBPERIODS, function(p) {
  sub <- idx_r[idx_r[[COL_DATE]] >= p$start &
               idx_r[[COL_DATE]] <= p$end, ]
  cat(sprintf("  %s (%s): N=%d\n", p$label, p$note, nrow(sub)))
  row <- welch_anova_row(sub, p$label)
  if (!is.null(row)) row$PeriodNote <- p$note
  row
}))
save_table(sp_anova_rows, "rob_subperiod_anova.csv")

cat("  Results:\n")
for (i in seq_len(nrow(sp_anova_rows))) {
  r <- sp_anova_rows[i, ]
  cat(sprintf("    %-20s  F(%.1f,%.1f)=%.4f  p=%s  Sig=%s\n",
              r$Dataset, r$DF1, r$DF2, r$F_stat,
              format(r$P_value, digits = 3), r$Significant))
}

# ---------------------------------------------------------------------------
# R2. Alternative crash thresholds
# ---------------------------------------------------------------------------
cat("\n[R2] Alternative crash thresholds ...\n")

alt_thresh_rows <- do.call(rbind, lapply(ALT_THRESHOLDS, function(thr) {
  crash_flag <- idx_r[[COL_RETURN]] <= thr
  n_crash    <- sum(crash_flag, na.rm = TRUE)
  n_total    <- sum(!is.na(idx_r[[COL_RETURN]]))

  # Chi-square: weekday vs crash (if expected counts allow)
  tab <- table(Weekday = idx_r[[COL_WEEKDAY]],
               Crash   = crash_flag)
  cs  <- tryCatch(chisq.test(tab), error = function(e) NULL)
  min_exp <- if (!is.null(cs)) min(cs$expected) else NA_real_

  cs_stat <- if (!is.null(cs)) cs$statistic else NA_real_
  cs_p    <- if (!is.null(cs)) cs$p.value    else NA_real_

  # Crash counts per weekday
  wd_counts <- vapply(WEEKDAY_LEVELS, function(wd) {
    sum(crash_flag & idx_r[[COL_WEEKDAY]] == wd, na.rm = TRUE)
  }, integer(1L))
  wd_rates <- wd_counts / vapply(WEEKDAY_LEVELS, function(wd)
    sum(idx_r[[COL_WEEKDAY]] == wd), integer(1L))

  row <- data.frame(
    Threshold   = sprintf("%.0f%%", thr * 100),
    Thr_decimal = thr,
    N_total     = n_total,
    N_Crash     = n_crash,
    Rate_Crash  = n_crash / n_total,
    Chi_sq      = round(cs_stat, 4),
    Chi_p       = signif(cs_p, 4),
    Chi_sig     = if (!is.na(cs_p)) cs_p < ALPHA else NA,
    Min_expected= round(min_exp, 2),
    stringsAsFactors = FALSE
  )
  for (wd in WEEKDAY_LEVELS) {
    row[[paste0("N_", wd)]]    <- wd_counts[wd]
    row[[paste0("Rate_", wd)]] <- round(wd_rates[wd], 5)
  }
  row
}))
save_table(alt_thresh_rows, "rob_alternative_crash_thresholds.csv")

# Combined with -3% and -5% for reference
ref_rows <- data.frame(
  Threshold = c("-3%", "-5%"),
  Thr_decimal = c(-0.03, -0.05),
  N_total   = nrow(idx_r),
  N_Crash   = c(sum(idx_r[[COL_CRASH3]] == TRUE, na.rm = TRUE),
                sum(idx_r[[COL_CRASH5]] == TRUE, na.rm = TRUE)),
  stringsAsFactors = FALSE
)
cat("  Alternative thresholds vs -3%/-5% baselines:\n")
all_thr_summary <- rbind(ref_rows, alt_thresh_rows[, names(ref_rows)])
for (i in seq_len(nrow(all_thr_summary))) {
  r <- all_thr_summary[i, ]
  cat(sprintf("    %-6s  N_crash=%4d  Rate=%.3f%%\n",
              r$Threshold, r$N_Crash,
              100 * r$N_Crash / r$N_total))
}

# Plot crash rates across thresholds
thr_plot_df <- data.frame(
  Threshold = c("-2%", "-3%", "-4%", "-5%", "-7%"),
  Thr_val   = c(-0.02, -0.03, -0.04, -0.05, -0.07),
  N_Crash   = c(
    sum(idx_r[[COL_RETURN]] <= -0.02, na.rm = TRUE),
    sum(idx_r[[COL_CRASH3]] == TRUE, na.rm = TRUE),
    sum(idx_r[[COL_RETURN]] <= -0.04, na.rm = TRUE),
    sum(idx_r[[COL_CRASH5]] == TRUE, na.rm = TRUE),
    sum(idx_r[[COL_RETURN]] <= -0.07, na.rm = TRUE)
  ),
  Rate = NA_real_
)
thr_plot_df$Rate <- 100 * thr_plot_df$N_Crash / nrow(idx_r)

p_thr <- ggplot2::ggplot(thr_plot_df,
    ggplot2::aes(x = Threshold, y = Rate, fill = Threshold)) +
  ggplot2::geom_col(alpha = 0.85, show.legend = FALSE) +
  ggplot2::scale_fill_brewer(palette = "Reds", direction = -1) +
  ggplot2::geom_text(ggplot2::aes(label = N_Crash), vjust = -0.4, size = 4) +
  ggplot2::labs(
    title    = "Crash Event Counts by Threshold — NIFTY 50 Index",
    subtitle = "Numbers above bars = event count; bars = crash rate (%)",
    x = "Return Threshold", y = "Crash Rate (%)"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "rob_01_crash_counts_by_threshold.png"), p_thr)

# ---------------------------------------------------------------------------
# R3. Sensitivity to ReturnSource (Adjusted-only subsample)
# ---------------------------------------------------------------------------
cat("\n[R3] Sensitivity to ReturnSource — Adjusted-only subsample ...\n")

adj_only <- idx_r[idx_r[[COL_RETURN_SRC]] == "Adjusted", ]
cat(sprintf("  Full sample: %d  |  Adjusted-only: %d  |  Close-fallback: %d\n",
            nrow(idx_r),
            nrow(adj_only),
            sum(idx_r[[COL_RETURN_SRC]] == "Close", na.rm = TRUE)))

anova_adj <- welch_anova_row(adj_only, "NIFTY50_Adjusted_only")
anova_all <- welch_anova_row(idx_r,    "NIFTY50_Full_sample")

src_comparison <- rbind(anova_all, anova_adj)
src_comparison$Note <- c("All returns (Adjusted + Close fallback)",
                          "Adjusted Close returns only")
save_table(src_comparison, "rob_return_source_sensitivity.csv")

cat(sprintf("  Full sample  : F(%.1f,%.1f)=%.4f  p=%s\n",
            anova_all$DF1, anova_all$DF2, anova_all$F_stat,
            format(anova_all$P_value, digits = 3)))
cat(sprintf("  Adjusted only: F(%.1f,%.1f)=%.4f  p=%s\n",
            anova_adj$DF1, anova_adj$DF2, anova_adj$F_stat,
            format(anova_adj$P_value, digits = 3)))

# ---------------------------------------------------------------------------
# R4. OLS weekday regression by subperiod
# ---------------------------------------------------------------------------
cat("\n[R4] OLS weekday regression by subperiod ...\n")

sp_reg_rows <- do.call(rbind, lapply(SUBPERIODS, function(p) {
  sub <- idx_r[idx_r[[COL_DATE]] >= p$start &
               idx_r[[COL_DATE]] <= p$end, ]
  if (nrow(sub) < 30) return(NULL)

  m   <- lm(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)), data = sub)
  hac <- tryCatch(sandwich::NeweyWest(m), error = function(e) vcov(m))
  ct  <- lmtest::coeftest(m, vcov = hac)

  # Extract as matrix to avoid dispatch issues with as.data.frame.coeftest
  ct_mat <- unclass(ct)                  # plain numeric matrix
  rows <- data.frame(
    Term        = rownames(ct_mat),
    Estimate    = ct_mat[, 1],
    Std_Error   = ct_mat[, 2],
    t_value     = ct_mat[, 3],
    P_value     = ct_mat[, 4],
    Period      = p$label,
    N           = nrow(sub),
    SE_type     = "HAC",
    stringsAsFactors = FALSE
  )
  rows$Significant <- rows$P_value < ALPHA
  rows
}))
save_table(sp_reg_rows, "rob_subperiod_regression.csv")

# Coefficient stability plot
sp_wd_coef <- sp_reg_rows[!grepl("Intercept", sp_reg_rows$Term), ]
sp_wd_coef$Weekday <- gsub(paste0("^", COL_WEEKDAY), "", sp_wd_coef$Term)
sp_wd_coef$Weekday <- factor(sp_wd_coef$Weekday, levels = WEEKDAY_LEVELS[-1])
sp_wd_coef$Period  <- factor(sp_wd_coef$Period)

p_sp_coef <- ggplot2::ggplot(sp_wd_coef,
    ggplot2::aes(x = Period, y = Estimate,
                 colour = Weekday, group = Weekday)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                       colour = "grey50") +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_colour_brewer(palette = "Set1") +
  ggplot2::labs(
    title    = "Weekday Return Coefficients Across Sub-Periods",
    subtitle = "HAC-robust estimates vs Monday; dashed line = zero",
    x = "Sub-period", y = "Coefficient (vs Monday)",
    colour = "Weekday"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5),
                 legend.position = "right")
save_gg(file.path(DIR_FIGURES, "rob_02_subperiod_coefficient_stability.png"),
        p_sp_coef, width = 9, height = 6)

# ---------------------------------------------------------------------------
# R5. Multiple-testing correction across all hypothesis tests
# ---------------------------------------------------------------------------
cat("\n[R5] Multiple-testing correction across all tests ...\n")

# Load test results from 05_hypothesis_testing.R if they exist
hyp_path <- file.path(DIR_TABLES, "hyp_all_test_results.csv")
if (file.exists(hyp_path)) {
  hyp_results <- utils::read.csv(hyp_path, stringsAsFactors = FALSE)
  p_vals  <- as.numeric(hyp_results$P_value)
  valid_p <- !is.na(p_vals)

  hyp_results$P_bonf_global <- NA_real_
  hyp_results$P_BH_global   <- NA_real_
  hyp_results$P_bonf_global[valid_p] <- p.adjust(p_vals[valid_p],
                                                   method = "bonferroni")
  hyp_results$P_BH_global[valid_p]   <- p.adjust(p_vals[valid_p],
                                                   method = "BH")
  hyp_results$Sig_bonf_global <- hyp_results$P_bonf_global < ALPHA
  hyp_results$Sig_BH_global   <- hyp_results$P_BH_global   < ALPHA
  save_table(hyp_results, "rob_multiple_testing_correction.csv")
  cat(sprintf("  %d tests corrected (Bonferroni + BH)\n", sum(valid_p)))
  cat(sprintf("  Significant after Bonferroni: %d/%d\n",
              sum(hyp_results$Sig_bonf_global, na.rm = TRUE), sum(valid_p)))
  cat(sprintf("  Significant after BH:         %d/%d\n",
              sum(hyp_results$Sig_BH_global, na.rm = TRUE), sum(valid_p)))
} else {
  message("  hyp_all_test_results.csv not found; ",
          "run 05_hypothesis_testing.R first")
}

# ---------------------------------------------------------------------------
# R6. Robust inference comparison: OLS vs HAC SEs for M1
# ---------------------------------------------------------------------------
cat("\n[R6] Robust inference comparison — OLS vs HAC ...\n")

m1_r6 <- lm(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)), data = idx_r)
ct_ols <- lmtest::coeftest(m1_r6)
ct_hac <- lmtest::coeftest(m1_r6, vcov = sandwich::NeweyWest(m1_r6))
ct_hc3 <- lmtest::coeftest(m1_r6,
                             vcov = sandwich::vcovHC(m1_r6, type = "HC3"))

make_se_row <- function(ct, se_type) {
  ct_mat <- unclass(ct)                  # plain numeric matrix
  df <- data.frame(
    Term      = rownames(ct_mat),
    Estimate  = ct_mat[, 1],
    Std_Error = ct_mat[, 2],
    t_stat    = ct_mat[, 3],
    P_value   = ct_mat[, 4],
    SE_type   = se_type,
    stringsAsFactors = FALSE
  )
  df$Sig <- df$P_value < ALPHA
  df
}
se_comparison <- rbind(
  make_se_row(ct_ols, "OLS"),
  make_se_row(ct_hac, "HAC_NeweyWest"),
  make_se_row(ct_hc3, "HC3")
)
save_table(se_comparison, "rob_se_comparison_ols_hac_hc3.csv")

# Pivot to show p-value changes by term
se_piv <- do.call(rbind, lapply(
  unique(se_comparison$Term[se_comparison$Term != "(Intercept)"]),
  function(trm) {
    sub <- se_comparison[se_comparison$Term == trm, ]
    data.frame(
      Term     = trm,
      P_OLS    = sub$P_value[sub$SE_type == "OLS"],
      P_HAC    = sub$P_value[sub$SE_type == "HAC_NeweyWest"],
      P_HC3    = sub$P_value[sub$SE_type == "HC3"],
      Sig_OLS  = sub$Sig[sub$SE_type == "OLS"],
      Sig_HAC  = sub$Sig[sub$SE_type == "HAC_NeweyWest"],
      Sig_HC3  = sub$Sig[sub$SE_type == "HC3"],
      stringsAsFactors = FALSE
    )
  }))
save_table(se_piv, "rob_pvalue_sensitivity_to_se_type.csv")

cat("  P-value sensitivity to SE type:\n")
for (i in seq_len(nrow(se_piv))) {
  r <- se_piv[i, ]
  cat(sprintf("    %-25s  OLS=%s  HAC=%s  HC3=%s\n",
              gsub(paste0("^", COL_WEEKDAY), "", r$Term),
              format(r$P_OLS, digits = 3, scientific = TRUE),
              format(r$P_HAC, digits = 3, scientific = TRUE),
              format(r$P_HC3, digits = 3, scientific = TRUE)))
}

# Plot SE comparison
se_plot_df <- se_comparison[se_comparison$Term != "(Intercept)", ]
se_plot_df$Weekday <- gsub(paste0("^", COL_WEEKDAY), "", se_plot_df$Term)
se_plot_df$CI_lo   <- se_plot_df$Estimate - 1.96 * se_plot_df$Std_Error
se_plot_df$CI_hi   <- se_plot_df$Estimate + 1.96 * se_plot_df$Std_Error
se_plot_df$Weekday <- factor(se_plot_df$Weekday, levels = WEEKDAY_LEVELS[-1])
se_plot_df$SE_type <- factor(se_plot_df$SE_type,
                              levels = c("OLS", "HC3", "HAC_NeweyWest"))

p_se_comp <- ggplot2::ggplot(se_plot_df,
    ggplot2::aes(x = Estimate, y = SE_type, colour = SE_type)) +
  ggplot2::facet_wrap(~ Weekday, ncol = 2) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                       colour = "grey40") +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_lo, xmax = CI_hi), height = 0.25) +
  ggplot2::scale_colour_manual(
    values = c("OLS" = "steelblue", "HC3" = "darkorange",
               "HAC_NeweyWest" = "firebrick")) +
  ggplot2::labs(
    title    = "Weekday Return Estimates: OLS vs HC3 vs HAC Standard Errors",
    subtitle = "Points: coefficient estimate; bars: 95% CI",
    x = "Estimate (vs Monday)", y = "SE Method",
    colour   = "SE type"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5),
                 legend.position = "bottom")
save_gg(file.path(DIR_FIGURES, "rob_03_se_comparison.png"), p_se_comp,
        width = 10, height = 8)

# ---------------------------------------------------------------------------
# R7. Constituent-level individual Welch ANOVAs
# ---------------------------------------------------------------------------
cat("\n[R7] Constituent-level weekday Welch ANOVAs ...\n")

syms <- sort(unique(panel_r[[COL_NSE]]))
sym_anova_rows <- do.call(rbind, lapply(syms, function(sym) {
  sub <- panel_r[panel_r[[COL_NSE]] == sym, ]
  sub[[COL_WEEKDAY]] <- relevel(sub[[COL_WEEKDAY]], ref = "Monday")
  row <- welch_anova_row(sub, sym)
  if (!is.null(row)) {
    row$FirstDate <- as.character(min(sub[[COL_DATE]]))
    row$LastDate  <- as.character(max(sub[[COL_DATE]]))
  }
  row
}))

# Multiple-testing correction across all constituent tests
sym_anova_rows$P_bonf <- p.adjust(sym_anova_rows$P_value, method = "bonferroni")
sym_anova_rows$P_BH   <- p.adjust(sym_anova_rows$P_value, method = "BH")
sym_anova_rows$Sig_bonf <- sym_anova_rows$P_bonf < ALPHA
sym_anova_rows$Sig_BH   <- sym_anova_rows$P_BH   < ALPHA
save_table(sym_anova_rows, "rob_constituent_individual_anova.csv")

cat(sprintf("  %d constituents tested\n", nrow(sym_anova_rows)))
cat(sprintf("  Significant at raw p < 0.05: %d\n",
            sum(sym_anova_rows$Significant, na.rm = TRUE)))
cat(sprintf("  Significant after Bonferroni: %d\n",
            sum(sym_anova_rows$Sig_bonf, na.rm = TRUE)))
cat(sprintf("  Significant after BH:         %d\n",
            sum(sym_anova_rows$Sig_BH, na.rm = TRUE)))

# ---------------------------------------------------------------------------
# R8. Limitations documentation
# ---------------------------------------------------------------------------
cat("\n[R8] Writing limitations report ...\n")

limitations <- data.frame(
  ID = paste0("L", sprintf("%02d", 1:10)),
  Category = c(
    "Data coverage",
    "Data coverage",
    "Survivorship bias",
    "Statistical assumption",
    "Statistical assumption",
    "Statistical assumption",
    "Sample size",
    "Return computation",
    "Multiple testing",
    "Interpretation"
  ),
  Limitation = c(
    paste0("NIFTY 50 index data (Yahoo Finance, ^NSEI) begins 2007-09-17, ",
           "NOT 2000-01-01. Any claim of 2000-2026 index coverage is incorrect. ",
           "The 2000-2026 claim applies only to some individual constituents."),
    paste0("Individual constituent data start dates vary from 2000-01-03 to ",
           "2023-08-21. Panel analyses pool heterogeneous history lengths."),
    paste0("Constituent list reflects CURRENT (2026-10-21) NIFTY 50 composition. ",
           "Firms removed from the index since 2000 are absent. Results are ",
           "biased toward surviving firms and cannot be generalised to historical ",
           "NIFTY 50 membership."),
    paste0("Daily stock returns violate ANOVA normality and i.i.d. assumptions. ",
           "ARCH effects, fat tails, and serial dependence are present. ",
           "HAC standard errors partially address this for OLS; ANOVA p-values ",
           "remain approximate."),
    paste0("Chi-square tests assume independent observations. Financial returns ",
           "exhibit serial dependence. Test p-values are approximate."),
    paste0("Pooled constituent panel pools observations across 50 firms and ",
           "varying time periods. Within-firm autocorrelation is not modelled. ",
           "Panel regressions without firm fixed effects are biased."),
    paste0("Only 22 index trading days have returns <= -5%. Logistic regression ",
           "on this threshold is exploratory; coefficient stability is low."),
    paste0("ReturnPrice uses Adjusted Close (primary) and falls back to Close ",
           "where Adjusted is NA (documented in ReturnSource column). Adjusted ",
           "and unadjusted returns are not directly comparable in mixed series."),
    paste0("Multiple hypothesis tests are conducted across weekdays, thresholds, ",
           "and sub-periods. Bonferroni and BH corrections reduce false discovery ",
           "rate but conclusions should still be interpreted cautiously."),
    paste0("Statistical significance does NOT imply economic significance or ",
           "trading opportunity. Effect sizes (Cohen's d, eta-squared, Cramer's V) ",
           "are small throughout. The Day-of-Week effect, if present, may reflect ",
           "data artefacts, calendar effects in macro data releases, or ",
           "microstructure differences rather than exploitable return patterns.")
  ),
  Implication = c(
    "Do not report pre-2007 index statistics. Report actual coverage dates.",
    "Report per-symbol date ranges; do not average across heterogeneous panels without caution.",
    "Label all constituent results as 'current composition' and note survivorship bias.",
    "Report HAC results as primary; ANOVA as preliminary. Do not over-interpret ANOVA p-values.",
    "Report chi-square results with this caveat; consider permutation tests as robustness.",
    "Treat pooled panel ANOVA as exploratory; individual firm results are more credible.",
    "Do not publish -5% logistic regression as a primary result; flag as sample-size-limited.",
    "Limit mixed-source comparisons to directional conclusions; do not interpret magnitudes directly.",
    "Apply corrections before declaring significance; report both corrected and uncorrected p-values.",
    "Emphasise effect sizes and practical significance over p-values in the discussion."
  ),
  stringsAsFactors = FALSE
)
save_table(limitations, "rob_limitations.csv")

# Also write as a readable text file
lim_path <- file.path(DIR_REPORTS, "limitations_and_caveats.txt")
ensure_dir(DIR_REPORTS)
con <- file(lim_path, open = "w")
writeLines("RESEARCH LIMITATIONS AND CAVEATS", con)
writeLines(paste(rep("=", 60), collapse = ""), con)
writeLines(sprintf("Project: NIFTY 50 Crash and Weekday Analysis"), con)
writeLines(sprintf("Generated: %s\n", Sys.time()), con)
for (i in seq_len(nrow(limitations))) {
  r <- limitations[i, ]
  writeLines(sprintf("[%s] %s", r$ID, r$Category), con)
  writeLines(paste0("  Limitation : ", r$Limitation), con)
  writeLines(paste0("  Implication: ", r$Implication), con)
  writeLines("", con)
}
close(con)
message(sprintf("  Limitations report saved: %s", lim_path))

cat(sprintf("\n[Robustness Checks Complete]\n"))
cat(sprintf("  Tables saved to  : %s\n", DIR_TABLES))
cat(sprintf("  Figures saved to : %s\n", DIR_FIGURES))
cat(sprintf("  Finished: %s\n\n", Sys.time()))
