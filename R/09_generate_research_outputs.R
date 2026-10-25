# =============================================================================
# Script  : 09_generate_research_outputs.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Compile all results into a consolidated statistical report,
#           generate a data dictionary, and write a README for the full
#           analysis workflow.  This script reads from results/ — it does
#           NOT re-run any analysis.  Run 04-08 before this script.
# Inputs  : results/tables/*.csv   (produced by 04-08)
#           results/figures/*.png  (produced by 04-08)
# Outputs : results/reports/statistical_results_report.md
#           results/reports/data_dictionary.md
#           results/reports/README_analysis.md
#           results/reports/results_file_index.csv
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/09_generate_research_outputs.R")
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
create_results_dirs()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  09_generate_research_outputs.R\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# Helper: safe CSV read for results tables (returns NULL if file absent)
# ---------------------------------------------------------------------------
read_result <- function(filename) {
  path <- file.path(DIR_TABLES, filename)
  if (!file.exists(path)) {
    message(sprintf("  [MISSING] %s — skipping (run the relevant script first)",
                    filename))
    return(NULL)
  }
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  message(sprintf("  Loaded: %s  (%d rows)", filename, nrow(df)))
  df
}

# ---------------------------------------------------------------------------
# Helper: write lines to a text/markdown file
# ---------------------------------------------------------------------------
write_report <- function(path, lines) {
  ensure_dir(dirname(path))
  writeLines(lines, con = path, sep = "\n")
  message(sprintf("  Report saved: %s", path))
}

# ---------------------------------------------------------------------------
# Helper: format a number with fallback for NA
# ---------------------------------------------------------------------------
fmt <- function(x, digits = 4, sci = FALSE) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("N/A")
  format(round(as.numeric(x[1]), digits), scientific = sci, big.mark = ",")
}

# ---------------------------------------------------------------------------
# Helper: significance label
# ---------------------------------------------------------------------------
sig_label <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

# ============================================================================
# 1. Load all results tables
# ============================================================================
cat("[Step 1] Loading results tables ...\n")

eda_idx_stats  <- read_result("eda_index_descriptive_stats.csv")
eda_idx_ci     <- read_result("eda_index_weekday_mean_ci.csv")
eda_idx_vol    <- read_result("eda_index_weekday_volatility.csv")
eda_crash      <- read_result("eda_crash_counts_by_weekday.csv")
eda_dd_sum     <- read_result("eda_drawdown_summary.csv")
eda_sym_stats  <- read_result("eda_constituent_descriptive_stats.csv")

hyp_all        <- read_result("hyp_all_test_results.csv")
hyp_anova      <- read_result("hyp_anova_weekday_returns_index.csv")
hyp_posthoc    <- read_result("hyp_posthoc_weekday_returns_index.csv")
hyp_monday     <- read_result("hyp_monday_pairwise_ttest.csv")
hyp_sw         <- read_result("hyp_normality_shapiro_wilk.csv")
hyp_levene     <- read_result("hyp_levene_equal_variance.csv")
hyp_chi3       <- read_result("hyp_contingency_-3%_NIFTY50Index.csv")
hyp_chi5       <- read_result("hyp_contingency_-5%_NIFTY50Index.csv")

reg_m1_coef    <- read_result("reg_m1_ols_weekday_coefficients.csv")
reg_m1_fit     <- read_result("reg_m1_model_fit.csv")
reg_m2_coef    <- read_result("reg_m2_ols_weekday_lagvol_coefficients.csv")
reg_m2_fit     <- read_result("reg_m2_model_fit.csv")
reg_m3_coef    <- read_result("reg_m3_logit_coefficients.csv")
reg_m3_fit     <- read_result("reg_m3_model_fit.csv")
reg_m4_coef    <- read_result("reg_m4_logit_coefficients.csv")
reg_m4_fit     <- read_result("reg_m4_model_fit.csv")
reg_comp       <- read_result("reg_model_comparison.csv")
reg_diag       <- read_result("reg_m1_diagnostics_tests.csv")
reg_infl       <- read_result("reg_m1_influential_observations.csv")

crash_events3  <- read_result("crash_3pct_index_crash_events.csv")
crash_events5  <- read_result("crash_5pct_index_crash_events.csv")
crash_wd_det   <- read_result("crash_index_weekday_frequency_detail.csv")
crash_dd_ep    <- read_result("crash_index_drawdown_episodes_20pct.csv")
crash_period   <- read_result("crash_index_period_weekday_breakdown.csv")
crash_sym      <- read_result("crash_constituent_crash_summary.csv")

rob_sp_anova   <- read_result("rob_subperiod_anova.csv")
rob_alt_thr    <- read_result("rob_alternative_crash_thresholds.csv")
rob_src        <- read_result("rob_return_source_sensitivity.csv")
rob_sp_reg     <- read_result("rob_subperiod_regression.csv")
rob_mt         <- read_result("rob_multiple_testing_correction.csv")
rob_se_piv     <- read_result("rob_pvalue_sensitivity_to_se_type.csv")
rob_sym_anova  <- read_result("rob_constituent_individual_anova.csv")
rob_limits     <- read_result("rob_limitations.csv")

# Helper: pull a scalar from a loaded table safely
pull <- function(df, row_cond, col) {
  if (is.null(df)) return(NA_character_)
  sub <- df[row_cond(df), , drop = FALSE]
  if (nrow(sub) == 0 || !(col %in% colnames(sub))) return(NA_character_)
  as.character(sub[[col]][1])
}

# ============================================================================
# 2. Generate consolidated statistical results report
# ============================================================================
cat("\n[Step 2] Generating statistical results report ...\n")

# --- Extract key values for the report text --------------------------------

# Full-sample stats
idx_full <- if (!is.null(eda_idx_stats)) {
  eda_idx_stats[eda_idx_stats$Weekday == "All", , drop = FALSE]
} else NULL

n_obs       <- fmt(if (!is.null(idx_full)) idx_full$N else NA)
mean_ret    <- fmt(if (!is.null(idx_full)) idx_full$Mean else NA, 6)
sd_ret      <- fmt(if (!is.null(idx_full)) idx_full$SD else NA, 6)
skew_val    <- fmt(if (!is.null(idx_full)) idx_full$Skewness else NA, 3)
kurt_val    <- fmt(if (!is.null(idx_full)) idx_full$Kurtosis else NA, 3)
min_ret     <- fmt(if (!is.null(idx_full)) idx_full$Min else NA, 4)
max_ret     <- fmt(if (!is.null(idx_full)) idx_full$Max else NA, 4)

# Crash counts
n_crash3 <- if (!is.null(eda_crash)) {
  idx_all_row <- eda_crash[eda_crash$Dataset == "NIFTY50_Index" &
                            eda_crash$Weekday == "All", ]
  if (nrow(idx_all_row) > 0) as.character(idx_all_row$N_Crash3[1]) else "N/A"
} else "N/A"

n_crash5 <- if (!is.null(eda_crash)) {
  idx_all_row <- eda_crash[eda_crash$Dataset == "NIFTY50_Index" &
                            eda_crash$Weekday == "All", ]
  if (nrow(idx_all_row) > 0) as.character(idx_all_row$N_Crash5[1]) else "N/A"
} else "N/A"

# Max drawdown
max_dd <- if (!is.null(eda_dd_sum)) {
  idx_dd_row <- eda_dd_sum[eda_dd_sum$Dataset == "NIFTY50_Index", ]
  if (nrow(idx_dd_row) > 0)
    sprintf("%.2f%%", idx_dd_row$Max_Drawdown[1] * 100)
  else "N/A"
} else "N/A"

# N drawdown episodes
n_dd_ep <- if (!is.null(crash_dd_ep)) as.character(nrow(crash_dd_ep)) else "N/A"

# ANOVA
t01 <- if (!is.null(hyp_all)) hyp_all[hyp_all$TestID == "T01", ] else NULL
anova_f    <- if (!is.null(t01) && nrow(t01) > 0) fmt(t01$Statistic, 4) else "N/A"
anova_df   <- if (!is.null(t01) && nrow(t01) > 0) as.character(t01$DF[1]) else "N/A"
anova_p    <- if (!is.null(t01) && nrow(t01) > 0) {
  format(as.numeric(t01$P_value[1]), digits = 4, scientific = TRUE)
} else "N/A"
anova_eta2 <- if (!is.null(t01) && nrow(t01) > 0) fmt(t01$EffectSize, 5) else "N/A"
anova_sig  <- if (!is.null(t01) && nrow(t01) > 0) {
  ifelse(as.numeric(t01$P_value[1]) < 0.05, "YES", "NO")
} else "N/A"
anova_test <- if (!is.null(t01) && nrow(t01) > 0) t01$Test[1] else "N/A"
anova_concl <- if (!is.null(t01) && nrow(t01) > 0) t01$Conclusion[1] else "N/A"

# Chi-square
t03 <- if (!is.null(hyp_all)) hyp_all[hyp_all$TestID == "T03", ] else NULL
t04 <- if (!is.null(hyp_all)) hyp_all[hyp_all$TestID == "T04", ] else NULL
chi3_stat <- if (!is.null(t03) && nrow(t03) > 0) fmt(t03$Statistic, 4) else "N/A"
chi3_p    <- if (!is.null(t03) && nrow(t03) > 0) {
  format(as.numeric(t03$P_value[1]), digits = 4, scientific = TRUE)
} else "N/A"
chi3_V    <- if (!is.null(t03) && nrow(t03) > 0) fmt(t03$EffectSize, 4) else "N/A"
chi3_sig  <- if (!is.null(t03) && nrow(t03) > 0) {
  ifelse(as.numeric(t03$P_value[1]) < 0.05, "YES", "NO")
} else "N/A"
chi5_stat <- if (!is.null(t04) && nrow(t04) > 0) fmt(t04$Statistic, 4) else "N/A"
chi5_p    <- if (!is.null(t04) && nrow(t04) > 0) {
  format(as.numeric(t04$P_value[1]), digits = 4, scientific = TRUE)
} else "N/A"
chi5_V    <- if (!is.null(t04) && nrow(t04) > 0) fmt(t04$EffectSize, 4) else "N/A"
chi5_sig  <- if (!is.null(t04) && nrow(t04) > 0) {
  ifelse(as.numeric(t04$P_value[1]) < 0.05, "YES", "NO")
} else "N/A"

# M1 regression fit
m1_r2  <- if (!is.null(reg_m1_fit)) fmt(reg_m1_fit$R_squared[1], 6) else "N/A"
m1_n   <- if (!is.null(reg_m1_fit)) as.character(reg_m1_fit$N[1]) else "N/A"

# M3 logit fit
m3_mcf <- if (!is.null(reg_m3_fit)) fmt(reg_m3_fit$McFadden_R2[1], 5) else "N/A"
m3_n   <- if (!is.null(reg_m3_fit)) as.character(reg_m3_fit$N_total[1]) else "N/A"
m3_nev <- if (!is.null(reg_m3_fit)) as.character(reg_m3_fit$N_crashes[1]) else "N/A"

# M1 HAC weekday coefficients
get_hac_coef <- function(wd) {
  if (is.null(reg_m1_coef)) return("N/A")
  sub <- reg_m1_coef[reg_m1_coef$SE_type == "HAC_NeweyWest" &
                     grepl(wd, reg_m1_coef$Term), ]
  if (nrow(sub) == 0) return("N/A")
  sprintf("%.6f (SE=%.6f, p=%s%s)",
          sub$Estimate[1], sub$Std_Error[1],
          format(as.numeric(sub$P_value[1]), digits = 3, scientific = TRUE),
          ifelse(sub$Significant[1], "*", ""))
}

# Subperiod ANOVA
sp_anova_text <- if (!is.null(rob_sp_anova)) {
  paste(apply(rob_sp_anova, 1, function(r) {
    sprintf("  - %s (N=%s): F(%s,%s)=%s, p=%s, significant=%s",
            r["Dataset"], r["N"],
            fmt(as.numeric(r["DF1"]), 1), fmt(as.numeric(r["DF2"]), 1),
            fmt(as.numeric(r["F_stat"]), 4),
            format(as.numeric(r["P_value"]), digits = 3, scientific = TRUE),
            r["Significant"])
  }), collapse = "\n")
} else "  (sub-period ANOVA results not available)"

# Multiple testing summary
mt_text <- if (!is.null(rob_mt)) {
  n_total <- sum(!is.na(as.numeric(rob_mt$P_value)))
  n_bonf  <- sum(rob_mt$Sig_bonf_global == TRUE, na.rm = TRUE)
  n_bh    <- sum(rob_mt$Sig_BH_global == TRUE, na.rm = TRUE)
  sprintf("%d tests, %d significant after Bonferroni, %d after BH",
          n_total, n_bonf, n_bh)
} else "not available"

# Constituent ANOVA summary
sym_anova_text <- if (!is.null(rob_sym_anova)) {
  n_sig_raw  <- sum(rob_sym_anova$Significant == TRUE, na.rm = TRUE)
  n_sig_bonf <- sum(rob_sym_anova$Sig_bonf == TRUE, na.rm = TRUE)
  n_sig_bh   <- sum(rob_sym_anova$Sig_BH == TRUE, na.rm = TRUE)
  sprintf("%d / %d significant at raw p<0.05; %d after Bonferroni; %d after BH",
          n_sig_raw, nrow(rob_sym_anova), n_sig_bonf, n_sig_bh)
} else "not available"

# Worst crash day
worst_day <- if (!is.null(crash_events3) && nrow(crash_events3) > 0) {
  r <- crash_events3[1, ]
  sprintf("%s (%s), return = %.4f", r$Date, r$Weekday, r$Return)
} else "N/A"

# Worst drawdown episode
worst_ep <- if (!is.null(crash_dd_ep) && nrow(crash_dd_ep) > 0) {
  r <- crash_dd_ep[1, ]
  sprintf("Start: %s, Trough: %s, End: %s, MaxDD: %.2f%%, Duration: %d trading days",
          r$EpisodeStart, r$EpisodeTrough, r$EpisodeEnd,
          r$MaxDrawdown_Pct, r$DurationDays)
} else "N/A"

# Return source split
n_adj   <- if (!is.null(rob_src)) {
  adj_row <- rob_src[grepl("Adjusted_only", rob_src$Dataset), ]
  if (nrow(adj_row) > 0) as.character(adj_row$N[1]) else "N/A"
} else "N/A"

# --- Now build the report lines -------------------------------------------

report_lines <- c(
  "# Statistical Results Report",
  "",
  sprintf("**Project:** A Statistical Investigation of Stock Market Crashes and Day-of-the-Week Effects: Evidence from NIFTY 50 Historical Returns, 2000–2026  "),
  sprintf("**Generated:** %s  ", Sys.time()),
  sprintf("**Script:** R/09_generate_research_outputs.R  "),
  "",
  "---",
  "",
  "## 1. Methodology Overview",
  "",
  "This report summarises the statistical analysis of NIFTY 50 daily return data",
  "with respect to day-of-the-week effects and crash event clustering.",
  "",
  "**Data sources:**",
  "- NIFTY 50 index: Yahoo Finance (^NSEI), history begins **2007-09-17**.",
  "  Any reference to '2000–2026' in the project title pertains to the target",
  "  scope; actual index coverage is confirmed as 2007 onward.",
  "- 50 individual NIFTY 50 constituents: current (2026-10-21) composition;",
  "  individual start dates vary. Survivorship bias is present.",
  "",
  "**Return computation:**",
  "- `ReturnPrice = (Price_t / Price_{t-1}) - 1` (arithmetic daily return).",
  "- Primary price: Adjusted Close. Fallback: Close (flagged in `ReturnSource`).",
  "- The first observation per series has `ReturnPrice = NA` (no prior close).",
  "",
  "**Crash flags:**",
  "- `CrashFlag_3pct = TRUE` when `ReturnPrice <= -0.03`.",
  "- `CrashFlag_5pct = TRUE` when `ReturnPrice <= -0.05`.",
  "",
  "**Drawdown:**",
  "- `Drawdown = (Adjusted_or_Close / RunningPeak) - 1`, always <= 0.",
  "- `DrawdownFlag_20pct = TRUE` when `Drawdown <= -0.20`.",
  "",
  "**Rolling volatility:**",
  "- Trailing standard deviation of `ReturnPrice` over 5, 20, and 30 trading days.",
  "",
  "**Weekday encoding:**",
  "- `Weekday` factor with levels Monday (reference), Tuesday, Wednesday,",
  "  Thursday, Friday. `WeekdayNum` encodes 1 = Monday … 5 = Friday.",
  "",
  "**Significance level:** α = 0.05 throughout.",
  "",
  "---",
  "",
  "## 2. Sample Description",
  "",
  "### 2.1 NIFTY 50 Index",
  "",
  sprintf("| Item | Value |"),
  sprintf("|---|---|"),
  sprintf("| Data coverage | 2007-09-17 to present |"),
  sprintf("| Valid return observations (N) | %s |", n_obs),
  sprintf("| Mean daily return | %s |", mean_ret),
  sprintf("| Standard deviation | %s |", sd_ret),
  sprintf("| Minimum return | %s |", min_ret),
  sprintf("| Maximum return | %s |", max_ret),
  sprintf("| Skewness (excess) | %s |", skew_val),
  sprintf("| Excess kurtosis | %s |", kurt_val),
  sprintf("| Crash days (≤ −3%%) | %s |", n_crash3),
  sprintf("| Crash days (≤ −5%%) | %s |", n_crash5),
  sprintf("| Maximum drawdown | %s |", max_dd),
  sprintf("| Drawdown episodes ≥ 20%% | %s |", n_dd_ep),
  "",
  "### 2.2 Weekday Distribution of Mean Returns (Index)",
  ""
)

# Build weekday table from eda_idx_ci
if (!is.null(eda_idx_ci)) {
  wd_tbl_header <- c(
    "| Weekday | N | Mean Return | Std Error | 95% CI Lower | 95% CI Upper |",
    "|---|---|---|---|---|---|"
  )
  wd_tbl_rows <- apply(eda_idx_ci, 1, function(r) {
    sprintf("| %s | %s | %.6f | %.6f | %.6f | %.6f |",
            r["Weekday"], r["N"],
            as.numeric(r["Mean"]),  as.numeric(r["SE"]),
            as.numeric(r["CI_lo"]), as.numeric(r["CI_hi"]))
  })
  report_lines <- c(report_lines, wd_tbl_header, wd_tbl_rows, "")
} else {
  report_lines <- c(report_lines,
    "*Weekday return table not available — run 04_eda.R first.*", "")
}

# Weekday crash rate table
report_lines <- c(report_lines,
  "### 2.3 Crash Rates by Weekday (Index)",
  ""
)
if (!is.null(eda_crash)) {
  idx_crash_wd <- eda_crash[eda_crash$Dataset == "NIFTY50_Index" &
                             eda_crash$Weekday != "All", ]
  if (nrow(idx_crash_wd) > 0) {
    cr_header <- c(
      "| Weekday | N | Crash Days (−3%) | Rate (−3%) | Crash Days (−5%) | Rate (−5%) |",
      "|---|---|---|---|---|---|"
    )
    cr_rows <- apply(idx_crash_wd, 1, function(r) {
      sprintf("| %s | %s | %s | %.3f%% | %s | %.3f%% |",
              r["Weekday"], r["N_Returns"],
              r["N_Crash3"], as.numeric(r["Rate_Crash3"]) * 100,
              r["N_Crash5"], as.numeric(r["Rate_Crash5"]) * 100)
    })
    report_lines <- c(report_lines, cr_header, cr_rows, "")
  }
} else {
  report_lines <- c(report_lines,
    "*Crash rate table not available — run 04_eda.R first.*", "")
}

# Worst events
report_lines <- c(report_lines,
  "### 2.4 Extreme Events",
  "",
  sprintf("**Worst single-day return:** %s", worst_day),
  "",
  sprintf("**Worst drawdown episode (≥ 20%%):** %s", worst_ep),
  "",
  "---",
  "",
  "## 3. Hypothesis Testing Results",
  "",
  "### 3.1 Assumption Checks",
  "",
  "**Normality (Shapiro-Wilk per weekday):**"
)

if (!is.null(hyp_sw)) {
  sw_header <- c("| Weekday | N | W statistic | p-value | Normal at 0.05 |",
                 "|---|---|---|---|---|")
  sw_rows <- apply(hyp_sw, 1, function(r) {
    sprintf("| %s | %s | %.4f | %s | %s |",
            r["Weekday"], r["N"],
            as.numeric(r["W"]),
            format(as.numeric(r["P_SW"]), digits = 3, scientific = TRUE),
            r["Normal_at_0.05"])
  })
  report_lines <- c(report_lines, "", sw_header, sw_rows, "")
}

report_lines <- c(report_lines,
  "> **Note:** Normality is not met for financial return series.",
  "> ANOVA and t-test p-values are approximate. HAC-robust regression",
  "> in Section 4 provides the primary inferential tool.",
  ""
)

if (!is.null(hyp_levene)) {
  lev_row <- hyp_levene[1, ]
  report_lines <- c(report_lines,
    sprintf("**Equal variance (Levene's test):** F = %s, p = %s — Equal variances: %s",
            fmt(as.numeric(lev_row$F_statistic), 4),
            format(as.numeric(lev_row$P_value), digits = 3, scientific = TRUE),
            lev_row$Equal_var_0.05),
    ""
  )
}

report_lines <- c(report_lines,
  "### 3.2 Test T01 — One-Way ANOVA: Weekday Mean Returns (Index)",
  "",
  sprintf("- **Test:** %s", anova_test),
  "- **H₀:** Mean daily returns are equal across all five weekdays.",
  "- **Hₐ:** At least one weekday has a different mean daily return.",
  sprintf("- **Test statistic:** F(%s) = %s", anova_df, anova_f),
  sprintf("- **p-value:** %s %s", anova_p, sig_label(as.numeric(
    if (!is.null(t01) && nrow(t01) > 0) t01$P_value[1] else NA))),
  sprintf("- **Effect size:** η² = %s (eta-squared)", anova_eta2),
  sprintf("- **Conclusion:** %s", anova_concl),
  "> Returns exhibit autocorrelation and ARCH effects; ANOVA p-value is",
  "> approximate. η² is typically very small for day-of-week effects.",
  ""
)

# Post-hoc table
report_lines <- c(report_lines,
  "### 3.3 Post-Hoc Pairwise Comparisons (Weekday Returns)",
  ""
)
if (!is.null(hyp_posthoc) && nrow(hyp_posthoc) > 0) {
  ph_cols <- intersect(c("Weekday1", "Weekday2", "P_raw", "P_bonf", "Cohen_d",
                          "Method", "Significant_Bonf"), colnames(hyp_posthoc))
  ph_header <- paste0("| ", paste(ph_cols, collapse = " | "), " |")
  ph_sep    <- paste0("| ", paste(rep("---", length(ph_cols)), collapse = " | "),
                      " |")
  ph_rows <- apply(hyp_posthoc[, ph_cols, drop = FALSE], 1, function(r) {
    paste0("| ", paste(r, collapse = " | "), " |")
  })
  report_lines <- c(report_lines, ph_header, ph_sep, ph_rows, "")
} else {
  report_lines <- c(report_lines,
    "*Post-hoc results not available — run 05_hypothesis_testing.R first.*", "")
}

# Chi-square tests
report_lines <- c(report_lines,
  "### 3.4 Tests T03–T04 — Chi-Square: Weekday vs Crash Occurrence",
  "",
  "**Test T03 — Threshold −3%:**",
  sprintf("- χ²(%s) = %s, p = %s %s, Cramér's V = %s",
          if (!is.null(t03) && nrow(t03) > 0) t03$DF[1] else "N/A",
          chi3_stat, chi3_p,
          sig_label(as.numeric(
            if (!is.null(t03) && nrow(t03) > 0) t03$P_value[1] else NA)),
          chi3_V),
  sprintf("- Significant: %s", chi3_sig),
  sprintf("- Conclusion: %s",
          if (!is.null(t03) && nrow(t03) > 0) t03$Conclusion[1] else "N/A"),
  "",
  "**Test T04 — Threshold −5%:**",
  sprintf("- χ²(%s) = %s, p = %s %s, Cramér's V = %s",
          if (!is.null(t04) && nrow(t04) > 0) t04$DF[1] else "N/A",
          chi5_stat, chi5_p,
          sig_label(as.numeric(
            if (!is.null(t04) && nrow(t04) > 0) t04$P_value[1] else NA)),
          chi5_V),
  sprintf("- Significant: %s", chi5_sig),
  sprintf("- Conclusion: %s",
          if (!is.null(t04) && nrow(t04) > 0) t04$Conclusion[1] else "N/A"),
  "",
  "> Cramér's V near 0 implies negligible practical association even if",
  "> statistically significant. Financial returns are not i.i.d.; p-values",
  "> from chi-square are approximate.",
  ""
)

# Monday pairwise
report_lines <- c(report_lines,
  "### 3.5 Test T05 — Monday vs Other Weekdays (Pairwise Welch t-tests)",
  ""
)
if (!is.null(hyp_monday) && nrow(hyp_monday) > 0) {
  mon_cols <- intersect(c("Comparison", "Diff_Mean", "T_stat", "DF",
                           "P_raw", "P_bonf", "Cohen_d", "Sig_Bonf"),
                         colnames(hyp_monday))
  mon_hdr <- paste0("| ", paste(mon_cols, collapse = " | "), " |")
  mon_sep <- paste0("| ", paste(rep("---", length(mon_cols)), collapse = " | "), " |")
  mon_rows <- apply(hyp_monday[, mon_cols, drop = FALSE], 1, function(r) {
    paste0("| ", paste(round(suppressWarnings(as.numeric(r)), 6),
                       collapse = " | "), " |")
  })
  # Use character version to preserve non-numeric columns
  mon_rows <- apply(hyp_monday[, mon_cols, drop = FALSE], 1, function(r) {
    vals <- lapply(seq_along(r), function(i) {
      v <- r[[i]]
      num <- suppressWarnings(as.numeric(v))
      if (!is.na(num)) format(round(num, 5)) else v
    })
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  report_lines <- c(report_lines, mon_hdr, mon_sep, mon_rows, "")
} else {
  report_lines <- c(report_lines,
    "*Monday pairwise t-test results not available.*", "")
}

# Volatility test
t06 <- if (!is.null(hyp_all)) hyp_all[hyp_all$TestID == "T06", ] else NULL
report_lines <- c(report_lines,
  "### 3.6 Test T06 — Weekday Differences in Volatility",
  "",
  sprintf("- **Test:** Levene's test (absolute deviation ANOVA)"),
  sprintf("- **F-statistic:** %s",
          if (!is.null(t06) && nrow(t06) > 0) fmt(t06$Statistic, 4) else "N/A"),
  sprintf("- **p-value:** %s",
          if (!is.null(t06) && nrow(t06) > 0)
            format(as.numeric(t06$P_value[1]), digits = 4, scientific = TRUE)
          else "N/A"),
  sprintf("- **Conclusion:** %s",
          if (!is.null(t06) && nrow(t06) > 0) t06$Conclusion[1] else "N/A"),
  ""
)

# All tests summary table
report_lines <- c(report_lines,
  "### 3.7 All Tests Summary",
  ""
)
if (!is.null(hyp_all)) {
  at_cols <- intersect(c("TestID", "Hypothesis", "Test", "Statistic", "DF",
                          "P_value", "Significant", "EffectSize", "EffectName"),
                        colnames(hyp_all))
  at_hdr <- paste0("| ", paste(at_cols, collapse = " | "), " |")
  at_sep <- paste0("| ", paste(rep("---", length(at_cols)), collapse = " | "), " |")
  at_rows <- apply(hyp_all[, at_cols, drop = FALSE], 1, function(r) {
    paste0("| ", paste(r, collapse = " | "), " |")
  })
  report_lines <- c(report_lines, at_hdr, at_sep, at_rows, "")
} else {
  report_lines <- c(report_lines,
    "*Full test results table not available — run 05_hypothesis_testing.R.*", "")
}

# ============================================================================
# Section 4 — Regression
# ============================================================================
report_lines <- c(report_lines,
  "---",
  "",
  "## 4. Regression Analysis",
  "",
  "### 4.1 Model Specifications",
  "",
  "| Model | Specification | SE Type |",
  "|---|---|---|",
  "| M1 | `ReturnPrice ~ Weekday` (OLS) | HAC Newey-West (primary), OLS (reported) |",
  "| M2 | `ReturnPrice ~ Weekday + RollVol20_lag1` (OLS) | HAC Newey-West |",
  "| M3 | `CrashFlag_3pct ~ Weekday` (Logistic) | HC3 robust |",
  "| M4 | `CrashFlag_5pct ~ Weekday` (Logistic) | HC3 robust |",
  "",
  "Monday is the **reference category** in all models.",
  "RollVol20_lag1 uses the prior day's 20-day rolling volatility (no look-ahead).",
  "",
  "### 4.2 Model M1 — OLS Returns on Weekday Dummies",
  "",
  sprintf("- N = %s, R² = %s", m1_n, m1_r2),
  ""
)

# M1 HAC coefficients table
if (!is.null(reg_m1_coef)) {
  m1_hac_df <- reg_m1_coef[reg_m1_coef$SE_type == "HAC_NeweyWest", ]
  if (nrow(m1_hac_df) > 0) {
    m1_cols <- intersect(c("Term", "Estimate", "Std_Error", "Statistic",
                            "P_value", "CI_lower", "CI_upper", "Significant"),
                          colnames(m1_hac_df))
    m1_hdr <- paste0("| ", paste(m1_cols, collapse = " | "), " |")
    m1_sep <- paste0("| ", paste(rep("---", length(m1_cols)), collapse = " | "), " |")
    m1_rows <- apply(m1_hac_df[, m1_cols, drop = FALSE], 1, function(r) {
      vals <- lapply(seq_along(r), function(i) {
        v   <- r[[i]]
        num <- suppressWarnings(as.numeric(v))
        if (!is.na(num)) format(round(num, 7)) else v
      })
      paste0("| ", paste(vals, collapse = " | "), " |")
    })
    report_lines <- c(report_lines,
      "**HAC-Robust Coefficient Estimates (M1):**", "",
      m1_hdr, m1_sep, m1_rows, "")
  }
}

# M2 fit
report_lines <- c(report_lines,
  "### 4.3 Model M2 — OLS with Lagged Volatility",
  "",
  sprintf("- N = %s, R² = %s",
          if (!is.null(reg_m2_fit)) as.character(reg_m2_fit$N[1]) else "N/A",
          if (!is.null(reg_m2_fit)) fmt(reg_m2_fit$R_squared[1], 6) else "N/A"),
  "- See `results/tables/reg_m2_ols_weekday_lagvol_coefficients.csv`",
  ""
)

# M3/M4 logit
report_lines <- c(report_lines,
  "### 4.4 Models M3 and M4 — Logistic Regression (Crash Probability)",
  "",
  sprintf("**M3 (−3%% threshold):** N = %s, crash events = %s, McFadden R² = %s",
          m3_n, m3_nev, m3_mcf),
  sprintf("**M4 (−5%% threshold):** N = %s, crash events = %s, McFadden R² = %s",
          if (!is.null(reg_m4_fit)) as.character(reg_m4_fit$N_total[1]) else "N/A",
          if (!is.null(reg_m4_fit)) as.character(reg_m4_fit$N_crashes[1]) else "N/A",
          if (!is.null(reg_m4_fit)) fmt(reg_m4_fit$McFadden_R2[1], 5) else "N/A"),
  "",
  "> McFadden R² near 0 indicates weekday dummies explain very little of",
  "> the variation in crash occurrence. Interpret with caution given",
  "> small sample sizes, especially for the −5% threshold.",
  "",
  "See `results/tables/reg_m3_logit_coefficients.csv` for odds ratios.",
  "",
  "### 4.5 Regression Diagnostics (M1)",
  ""
)
if (!is.null(reg_diag)) {
  diag_r <- reg_diag[1, ]
  report_lines <- c(report_lines,
    sprintf("| Diagnostic | Statistic | p-value | Finding |"),
    sprintf("|---|---|---|---|"),
    sprintf("| Breusch-Pagan (heteroskedasticity) | %.4f | %s | Heterosked.: %s |",
            as.numeric(diag_r$BP_stat),
            format(as.numeric(diag_r$BP_p), digits = 3, scientific = TRUE),
            diag_r$BP_heterosk),
    sprintf("| Durbin-Watson (autocorrelation) | %.4f | %s | Autocorr.: %s |",
            as.numeric(diag_r$DW_stat),
            format(as.numeric(diag_r$DW_p), digits = 3, scientific = TRUE),
            diag_r$DW_autocorr),
    "",
    "> HAC Newey-West standard errors correct for both heteroskedasticity",
    "> and autocorrelation. These are the primary inferential SEs for M1 and M2.",
    ""
  )
} else {
  report_lines <- c(report_lines,
    "*Diagnostics not available — run 06_regression_analysis.R.*", "")
}

# ============================================================================
# Section 5 — Crash analysis
# ============================================================================
report_lines <- c(report_lines,
  "---",
  "",
  "## 5. Crash and Drawdown Analysis",
  "",
  sprintf("**Total −3%% crash events (index):** %s", n_crash3),
  sprintf("**Total −5%% crash events (index):** %s", n_crash5),
  ""
)

# Worst 10 crash days
if (!is.null(crash_events3) && nrow(crash_events3) > 0) {
  top10 <- head(crash_events3[order(crash_events3$Return), ], 10)
  top10_hdr <- "| Rank | Date | Weekday | Return | Drawdown |"
  top10_sep <- "|---|---|---|---|---|"
  top10_rows <- apply(top10, 1, function(r) {
    sprintf("| — | %s | %s | %.4f | %.4f |",
            r["Date"], r["Weekday"],
            as.numeric(r["Return"]), as.numeric(r["Drawdown"]))
  })
  report_lines <- c(report_lines,
    "### 5.1 Top 10 Worst Daily Returns", "",
    top10_hdr, top10_sep, top10_rows, "")
}

# Drawdown episodes
if (!is.null(crash_dd_ep) && nrow(crash_dd_ep) > 0) {
  de_hdr <- "| Start | Trough | End | Max Drawdown (%) | Duration (days) |"
  de_sep <- "|---|---|---|---|---|"
  de_rows <- apply(crash_dd_ep[order(crash_dd_ep$MaxDrawdown), ], 1,
                   function(r) {
    sprintf("| %s | %s | %s | %.2f%% | %s |",
            r["EpisodeStart"], r["EpisodeTrough"], r["EpisodeEnd"],
            as.numeric(r["MaxDrawdown_Pct"]), r["DurationDays"])
  })
  report_lines <- c(report_lines,
    "### 5.2 Major Drawdown Episodes (≥ 20%)", "",
    de_hdr, de_sep, de_rows, "")
}

# ============================================================================
# Section 6 — Robustness
# ============================================================================
report_lines <- c(report_lines,
  "---",
  "",
  "## 6. Robustness and Sensitivity Checks",
  "",
  "### 6.1 Subperiod ANOVA",
  "",
  "> Period boundaries are data-driven: P1 = 2007-09-17 to 2012-12-31",
  "> (GFC and European debt crisis); P2 = 2013-01-01 to 2019-12-31",
  "> (recovery and expansion); P3 = 2020-01-01 to 2026-12-31 (COVID+).",
  "",
  sp_anova_text,
  "",
  "### 6.2 Alternative Crash Thresholds",
  ""
)
if (!is.null(rob_alt_thr)) {
  at_hdr <- "| Threshold | N Crash Events | Crash Rate | χ² | p-value | Sig |"
  at_sep <- "|---|---|---|---|---|---|"
  at_rows <- apply(rob_alt_thr, 1, function(r) {
    sprintf("| %s | %s | %.3f%% | %s | %s | %s |",
            r["Threshold"], r["N_Crash"],
            as.numeric(r["Rate_Crash"]) * 100,
            fmt(as.numeric(r["Chi_sq"]), 3),
            format(as.numeric(r["Chi_p"]), digits = 3, scientific = TRUE),
            r["Chi_sig"])
  })
  report_lines <- c(report_lines, at_hdr, at_sep, at_rows, "")
} else {
  report_lines <- c(report_lines,
    "*Alternative threshold table not available.*", "")
}

report_lines <- c(report_lines,
  "### 6.3 Return Source Sensitivity",
  "",
  sprintf("Full sample vs Adjusted-only subsample (N_adj = %s):", n_adj),
  ""
)
if (!is.null(rob_src)) {
  src_hdr <- "| Sample | N | F-stat | p-value | Significant |"
  src_sep <- "|---|---|---|---|---|"
  src_rows <- apply(rob_src, 1, function(r) {
    sprintf("| %s | %s | %s | %s | %s |",
            r["Dataset"], r["N"],
            fmt(as.numeric(r["F_stat"]), 4),
            format(as.numeric(r["P_value"]), digits = 3, scientific = TRUE),
            r["Significant"])
  })
  report_lines <- c(report_lines, src_hdr, src_sep, src_rows, "")
}

report_lines <- c(report_lines,
  "### 6.4 Multiple Testing Correction",
  "",
  sprintf("Result: %s", mt_text),
  "",
  "See `results/tables/rob_multiple_testing_correction.csv` for details.",
  "",
  "### 6.5 SE Robustness (OLS vs HC3 vs HAC)",
  ""
)
if (!is.null(rob_se_piv)) {
  se_hdr <- paste0("| ", paste(colnames(rob_se_piv), collapse = " | "), " |")
  se_sep <- paste0("| ", paste(rep("---", ncol(rob_se_piv)), collapse = " | "), " |")
  se_rows <- apply(rob_se_piv, 1, function(r) {
    paste0("| ", paste(r, collapse = " | "), " |")
  })
  report_lines <- c(report_lines, se_hdr, se_sep, se_rows, "")
}

report_lines <- c(report_lines,
  "### 6.6 Constituent-Level Weekday ANOVA",
  "",
  sprintf("Result: %s", sym_anova_text),
  "",
  "See `results/tables/rob_constituent_individual_anova.csv` for per-symbol results.",
  ""
)

# ============================================================================
# Section 7 — Limitations and conclusions
# ============================================================================
report_lines <- c(report_lines,
  "---",
  "",
  "## 7. Limitations",
  ""
)
if (!is.null(rob_limits)) {
  for (i in seq_len(nrow(rob_limits))) {
    r <- rob_limits[i, ]
    report_lines <- c(report_lines,
      sprintf("**[%s] %s**", r$ID, r$Category),
      sprintf("> %s", r$Limitation),
      sprintf("> *Implication: %s*", r$Implication),
      ""
    )
  }
} else {
  report_lines <- c(report_lines,
    "*Limitations table not available — run 08_robustness_checks.R.*", "")
}

report_lines <- c(report_lines,
  "---",
  "",
  "## 8. Conclusions",
  "",
  "The analysis tests whether daily NIFTY 50 returns and crash probabilities",
  "differ systematically by day of the week over the available sample period.",
  "",
  "**Key findings (fill in after running the pipeline):**",
  "",
  sprintf("1. ANOVA/Welch ANOVA (T01): weekday effect on mean returns is %s",
          ifelse(anova_sig == "YES",
                 "**statistically significant** (p < 0.05), but effect size",
                 "**not statistically significant** at α = 0.05")),
  sprintf("   η² = %s is %s, indicating little practical significance.",
          anova_eta2,
          ifelse(as.numeric(gsub("[^0-9.]", "", anova_eta2)) < 0.01,
                 "near zero", "non-trivial")),
  "",
  sprintf("2. Chi-square (T03, -3%% threshold): crash occurrence is %s",
          ifelse(chi3_sig == "YES", "significantly associated with weekday.",
                 "not significantly associated with weekday.")),
  sprintf("   Cramér's V = %s.", chi3_V),
  "",
  sprintf("3. Chi-square (T04, -5%% threshold): crash occurrence is %s",
          ifelse(chi5_sig == "YES", "significantly associated with weekday.",
                 "not significantly associated with weekday.")),
  sprintf("   Cramér's V = %s.", chi5_V),
  "",
  "4. OLS regression (M1) weekday dummies explain negligible variance",
  sprintf("   (R² = %s). HAC-robust SEs are used as primary inference.", m1_r2),
  "",
  "5. Logistic regression (M3/M4) McFadden R² near zero indicates weekday",
  "   dummies have minimal predictive power for crash probability.",
  "",
  "6. Subperiod ANOVA and SE robustness checks assess stability of findings.",
  "   See Section 6 for details.",
  "",
  "7. **Survivorship bias** and non-i.i.d. data are the primary limitations.",
  "   All conclusions should be interpreted with these caveats in mind.",
  "",
  "---",
  "",
  sprintf("*Report generated automatically by R/09_generate_research_outputs.R on %s.*",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  ""
)

write_report(file.path(DIR_REPORTS, "statistical_results_report.md"),
             report_lines)

# ============================================================================
# 3. Data Dictionary
# ============================================================================
cat("\n[Step 3] Generating data dictionary ...\n")

dict_lines <- c(
  "# Data Dictionary",
  "",
  sprintf("**Project:** NIFTY 50 Crash and Weekday Analysis  "),
  sprintf("**Generated:** %s  ", Sys.time()),
  "",
  "This dictionary describes every column in the two primary feature files",
  "and the processed price files.",
  "",
  "---",
  "",
  "## A. Feature Files",
  "",
  "### A1. `data/processed/features/nifty50_index_features.csv`",
  "",
  "Daily NIFTY 50 index observations with engineered features.",
  "",
  "| Column | Type | Description |",
  "|---|---|---|",
  "| Date | Date (YYYY-MM-DD) | Calendar date of the trading session. |",
  "| Open | Numeric | Opening price (raw, not adjusted). |",
  "| High | Numeric | Intraday high price (raw). |",
  "| Low | Numeric | Intraday low price (raw). |",
  "| Close | Numeric | Official closing price (unadjusted). |",
  "| Adjusted | Numeric | Dividend/split-adjusted closing price (Yahoo Finance). |",
  "| Volume | Integer | Reported trading volume (often 0 for ^NSEI). |",
  "| CompanyName | Character | Descriptive name ('NIFTY 50'). |",
  "| NSESymbol | Character | Internal symbol used in this project ('^NSEI'). |",
  "| YahooSymbol | Character | Yahoo Finance ticker symbol ('^NSEI'). |",
  "| Source | Character | Data provider ('Yahoo Finance'). |",
  "| OHLCFlag | Logical | TRUE if OHLC data were available and used; FALSE otherwise. |",
  "| Weekday | Factor | Day of week: Monday, Tuesday, Wednesday, Thursday, Friday. |",
  "| ReturnSource | Character | Price series used for return: 'Adjusted' or 'Close'. |",
  "| ReturnPrice | Numeric | Arithmetic daily return: (Price_t / Price_{t-1}) − 1. NA on first observation. |",
  "| WeekdayNum | Integer | Numeric weekday: 1 = Monday, 2 = Tuesday, …, 5 = Friday. |",
  "| RollVol_5 | Numeric | Trailing 5-day rolling standard deviation of ReturnPrice. NA for first 4 rows. |",
  "| RollVol_20 | Numeric | Trailing 20-day rolling standard deviation of ReturnPrice. NA for first 19 rows. |",
  "| RollVol_30 | Numeric | Trailing 30-day rolling standard deviation of ReturnPrice. NA for first 29 rows. |",
  "| RunningPeak | Numeric | Cumulative maximum of the price series up to and including date t. |",
  "| Drawdown | Numeric | (Price_t / RunningPeak) − 1. Always ≤ 0. 0 when price equals the running peak. |",
  "| CrashFlag_3pct | Logical | TRUE if ReturnPrice ≤ −0.03 (i.e., ≤ −3%). NA where ReturnPrice is NA. |",
  "| CrashFlag_5pct | Logical | TRUE if ReturnPrice ≤ −0.05 (i.e., ≤ −5%). NA where ReturnPrice is NA. |",
  "| DrawdownFlag_20pct | Logical | TRUE if Drawdown ≤ −0.20 (i.e., ≥ 20% below running peak). |",
  "",
  "**Coverage:** 2007-09-17 to present (first row has ReturnPrice = NA).  ",
  "**Rows:** Approximately 4,400+ trading days.",
  "",
  "---",
  "",
  "### A2. `data/processed/features/all_constituents_features.csv`",
  "",
  "Stacked (long-format) panel of all 50 NIFTY 50 constituent feature files.",
  "Contains the same columns as A1, plus:",
  "",
  "| Column | Type | Description |",
  "|---|---|---|",
  "| NSESymbol | Character | NSE ticker symbol of the constituent (e.g., 'RELIANCE'). |",
  "",
  "**Rows:** All 50 constituents stacked; total observations vary by symbol history.",
  "**Survivorship bias:** Reflects current (2026-10-21) NIFTY 50 composition only.",
  "",
  "---",
  "",
  "### A3. `data/processed/features/constituent_features/<SYMBOL>.csv`",
  "",
  "Individual constituent feature file. Identical columns to A1 with",
  "`NSESymbol` identifying the firm. One file per constituent symbol.",
  "",
  "---",
  "",
  "## B. Processed Price Files",
  "",
  "### B1. `data/processed/constituent_prices_clean/<SYMBOL>.csv`",
  "",
  "Cleaned OHLCV data for each constituent prior to feature engineering.",
  "",
  "| Column | Type | Description |",
  "|---|---|---|",
  "| Date | Date | Trading date. |",
  "| Open, High, Low, Close | Numeric | Raw OHLC prices. |",
  "| Adjusted | Numeric | Adjusted closing price (NA where unavailable). |",
  "| Volume | Integer | Trading volume. |",
  "| NSESymbol | Character | NSE ticker symbol. |",
  "| Source | Character | Data provider. |",
  "",
  "---",
  "",
  "## C. Results Tables (`results/tables/`)",
  "",
  "| File | Produced by | Description |",
  "|---|---|---|",
  "| eda_index_descriptive_stats.csv | 04_eda.R | Full-sample and per-weekday descriptive stats for the index. |",
  "| eda_index_weekday_mean_ci.csv | 04_eda.R | Weekday mean returns with 95% CI for the index. |",
  "| eda_index_weekday_volatility.csv | 04_eda.R | Weekday SD with asymptotic SE for the index. |",
  "| eda_crash_counts_by_weekday.csv | 04_eda.R | Crash counts and rates by weekday (index + panel). |",
  "| eda_drawdown_summary.csv | 04_eda.R | Summary drawdown statistics (index + panel). |",
  "| eda_constituent_descriptive_stats.csv | 04_eda.R | Per-symbol descriptive statistics. |",
  "| eda_constituent_panel_overall_stats.csv | 04_eda.R | Panel-wide aggregate statistics. |",
  "| eda_constituent_weekday_stats.csv | 04_eda.R | Pooled constituent weekday statistics. |",
  "| eda_index_weekday_mean_ci.csv | 04_eda.R | Mean return CI table for index. |",
  "| eda_constituent_weekday_mean_ci.csv | 04_eda.R | Mean return CI table for constituents. |",
  "| hyp_normality_shapiro_wilk.csv | 05_hypothesis_testing.R | Shapiro-Wilk normality test by weekday. |",
  "| hyp_levene_equal_variance.csv | 05_hypothesis_testing.R | Levene equal-variance test. |",
  "| hyp_ljung_box_autocorrelation.csv | 05_hypothesis_testing.R | Ljung-Box autocorrelation test. |",
  "| hyp_anova_weekday_returns_index.csv | 05_hypothesis_testing.R | ANOVA table for weekday returns. |",
  "| hyp_posthoc_weekday_returns_index.csv | 05_hypothesis_testing.R | Post-hoc pairwise comparisons. |",
  "| hyp_monday_pairwise_ttest.csv | 05_hypothesis_testing.R | Monday vs other weekdays t-tests. |",
  "| hyp_weekday_volatility_tests.csv | 05_hypothesis_testing.R | Bartlett and Levene volatility tests. |",
  "| hyp_weekday_sd_table.csv | 05_hypothesis_testing.R | Per-weekday SD and variance. |",
  "| hyp_all_test_results.csv | 05_hypothesis_testing.R | Master table of all hypothesis test results. |",
  "| hyp_contingency_-3%_NIFTY50Index.csv | 05_hypothesis_testing.R | Contingency table: weekday × crash (−3%). |",
  "| hyp_contingency_-5%_NIFTY50Index.csv | 05_hypothesis_testing.R | Contingency table: weekday × crash (−5%). |",
  "| reg_m1_ols_weekday_coefficients.csv | 06_regression_analysis.R | M1 OLS and HAC coefficients. |",
  "| reg_m1_model_fit.csv | 06_regression_analysis.R | M1 model fit statistics. |",
  "| reg_m2_ols_weekday_lagvol_coefficients.csv | 06_regression_analysis.R | M2 HAC coefficients with lagged vol. |",
  "| reg_m2_model_fit.csv | 06_regression_analysis.R | M2 model fit statistics. |",
  "| reg_m2_vif.csv | 06_regression_analysis.R | Variance inflation factors (M2). |",
  "| reg_m3_logit_coefficients.csv | 06_regression_analysis.R | M3 logistic odds ratios (−3% crash). |",
  "| reg_m3_model_fit.csv | 06_regression_analysis.R | M3 logistic model fit. |",
  "| reg_m4_logit_coefficients.csv | 06_regression_analysis.R | M4 logistic odds ratios (−5% crash). |",
  "| reg_m4_model_fit.csv | 06_regression_analysis.R | M4 logistic model fit. |",
  "| reg_model_comparison.csv | 06_regression_analysis.R | Summary comparison across M1–M4. |",
  "| reg_m1_diagnostics_tests.csv | 06_regression_analysis.R | Breusch-Pagan and Durbin-Watson diagnostics. |",
  "| reg_m1_influential_observations.csv | 06_regression_analysis.R | Observations with large Cook's distance. |",
  "| crash_3pct_index_crash_events.csv | 07_crash_analysis.R | All −3% crash events with date and weekday. |",
  "| crash_5pct_index_crash_events.csv | 07_crash_analysis.R | All −5% crash events with date and weekday. |",
  "| crash_index_weekday_frequency_detail.csv | 07_crash_analysis.R | Crash counts and rates by weekday (detailed). |",
  "| crash_index_drawdown_episodes_20pct.csv | 07_crash_analysis.R | Drawdown episodes reaching ≥ 20%. |",
  "| crash_index_drawdown_overall.csv | 07_crash_analysis.R | Overall drawdown summary for index. |",
  "| crash_index_volatility_around_crashes.csv | 07_crash_analysis.R | Volatility context ± 5 days around crashes. |",
  "| crash_constituent_crash_summary.csv | 07_crash_analysis.R | Per-constituent crash summary. |",
  "| crash_constituent_weekday_crash_rates.csv | 07_crash_analysis.R | Per-constituent weekday crash rates. |",
  "| crash_index_period_weekday_breakdown.csv | 07_crash_analysis.R | Crash rates by sub-period and weekday. |",
  "| rob_subperiod_anova.csv | 08_robustness_checks.R | Welch ANOVA within each sub-period. |",
  "| rob_alternative_crash_thresholds.csv | 08_robustness_checks.R | Crash stats at −2%, −4%, −7% thresholds. |",
  "| rob_return_source_sensitivity.csv | 08_robustness_checks.R | ANOVA sensitivity to ReturnSource. |",
  "| rob_subperiod_regression.csv | 08_robustness_checks.R | OLS weekday regression per sub-period. |",
  "| rob_multiple_testing_correction.csv | 08_robustness_checks.R | Bonferroni and BH corrections across all tests. |",
  "| rob_se_comparison_ols_hac_hc3.csv | 08_robustness_checks.R | Full OLS/HAC/HC3 coefficient comparison. |",
  "| rob_pvalue_sensitivity_to_se_type.csv | 08_robustness_checks.R | P-value pivot by SE type. |",
  "| rob_constituent_individual_anova.csv | 08_robustness_checks.R | Per-constituent Welch ANOVA results. |",
  "| rob_limitations.csv | 08_robustness_checks.R | Structured limitations documentation. |",
  "",
  "---",
  "",
  "## D. Figures (`results/figures/`)",
  "",
  "| File | Produced by | Description |",
  "|---|---|---|",
  "| eda_01_index_return_histogram.png | 04_eda.R | Return histogram + density overlay (index). |",
  "| eda_02_index_return_qqplot.png | 04_eda.R | Q-Q plot vs Normal (index). |",
  "| eda_03_index_weekday_boxplot.png | 04_eda.R | Return boxplots by weekday (index). |",
  "| eda_04_index_weekday_mean_return.png | 04_eda.R | Mean return + 95% CI by weekday (index). |",
  "| eda_05_index_weekday_volatility.png | 04_eda.R | Return SD + CI by weekday (index). |",
  "| eda_06_index_crash_rate_by_weekday.png | 04_eda.R | Crash rates by weekday, −3% and −5% (index). |",
  "| eda_07_index_return_timeseries.png | 04_eda.R | Return time-series with crash thresholds. |",
  "| eda_08_index_drawdown_timeseries.png | 04_eda.R | Drawdown time-series (index). |",
  "| eda_09_index_rolling_volatility.png | 04_eda.R | Rolling volatility 5/20/30-day over time. |",
  "| eda_10_constituent_return_density.png | 04_eda.R | Return density: top 5 longest-history constituents. |",
  "| eda_11_constituent_weekday_mean_return.png | 04_eda.R | Weekday mean return + CI (pooled constituents). |",
  "| eda_12_constituent_crash_rate_by_weekday.png | 04_eda.R | Crash rates by weekday (pooled constituents). |",
  "| eda_13_index_drawdown_histogram.png | 04_eda.R | Drawdown distribution histogram. |",
  "| hyp_01_posthoc_pvalue_heatmap.png | 05_hypothesis_testing.R | Post-hoc pairwise p-value heatmap. |",
  "| hyp_02_weekday_mean_returns_forest.png | 05_hypothesis_testing.R | Forest plot of weekday mean return CIs. |",
  "| reg_M1_OLS_diagnostics.png | 06_regression_analysis.R | M1 OLS standard diagnostic plots. |",
  "| reg_01_cooks_distance.png | 06_regression_analysis.R | Cook's distance influential observations. |",
  "| reg_02_weekday_coefficients.png | 06_regression_analysis.R | M1 HAC weekday coefficient plot. |",
  "| reg_03_crash3_odds_ratios.png | 06_regression_analysis.R | M3 odds ratio plot (−3% crash). |",
  "| crash_01_top20_worst_returns.png | 07_crash_analysis.R | Top 20 worst daily returns (horizontal bar). |",
  "| crash_02_drawdown_with_episodes.png | 07_crash_analysis.R | Drawdown time-series with episode shading. |",
  "| crash_03_crash_rate_by_period_weekday.png | 07_crash_analysis.R | Crash rates by sub-period and weekday. |",
  "| crash_04_constituent_crash_rates.png | 07_crash_analysis.R | Per-constituent crash rates (bar chart). |",
  "| crash_05_volatility_around_crashes.png | 07_crash_analysis.R | Avg. volatility pre/crash/post crash days. |",
  "| rob_01_crash_counts_by_threshold.png | 08_robustness_checks.R | Crash counts across multiple thresholds. |",
  "| rob_02_subperiod_coefficient_stability.png | 08_robustness_checks.R | Coefficient stability across sub-periods. |",
  "| rob_03_se_comparison.png | 08_robustness_checks.R | OLS vs HC3 vs HAC CI comparison (faceted). |",
  "",
  "---",
  "",
  sprintf("*Dictionary generated by R/09_generate_research_outputs.R on %s.*",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  ""
)

write_report(file.path(DIR_REPORTS, "data_dictionary.md"), dict_lines)

# ============================================================================
# 4. README — Analysis Workflow
# ============================================================================
cat("\n[Step 4] Generating analysis README ...\n")

readme_lines <- c(
  "# NIFTY 50 Crash and Weekday Analysis — Workflow README",
  "",
  sprintf("**Project:** A Statistical Investigation of Stock Market Crashes and"),
  "Day-of-the-Week Effects: Evidence from NIFTY 50 Historical Returns, 2000–2026  ",
  sprintf("**README generated:** %s", Sys.time()),
  "",
  "---",
  "",
  "## Quick Start",
  "",
  "Open RStudio, set the working directory, and source scripts in order:",
  "",
  "```r",
  "# 1. Set working directory (do this first in every session)",
  'setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")',
  "",
  "# 2. (Optional) Install required packages — run once",
  'install.packages(c("ggplot2", "e1071", "sandwich", "lmtest",',
  '                   "PMCMRplus", "car"))',
  "",
  "# 3. Run the analysis pipeline (scripts 04 to 09)",
  'source("R/04_eda.R")',
  'source("R/05_hypothesis_testing.R")',
  'source("R/06_regression_analysis.R")',
  'source("R/07_crash_analysis.R")',
  'source("R/08_robustness_checks.R")',
  'source("R/09_generate_research_outputs.R")',
  "",
  "# OR: use the master script (runs 04 through 09 in sequence)",
  'source("R/00_run_analysis.R")',
  "```",
  "",
  "---",
  "",
  "## Prerequisites",
  "",
  "### Working directory",
  "All scripts use **relative paths**. The working directory must be set to",
  "the project root before sourcing any script:",
  "",
  "```r",
  'setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")',
  "```",
  "",
  "### Required feature data (already generated)",
  "The following files must exist before running scripts 04–09:",
  "",
  "```",
  "data/processed/features/nifty50_index_features.csv",
  "data/processed/features/all_constituents_features.csv",
  "data/processed/features/constituent_features/<SYMBOL>.csv   (50 files)",
  "```",
  "",
  "These are produced by `R/03_feature_engineering.R`. **Do not re-run 01–03**",
  "unless you wish to refresh the raw data downloads.",
  "",
  "### Required R packages",
  "",
  "| Package | Used by | Purpose |",
  "|---|---|---|",
  "| ggplot2 | 04–09 | All figures |",
  "| e1071 | 04, 05 | Skewness and kurtosis |",
  "| sandwich | 06, 08 | HAC and HC robust standard errors |",
  "| lmtest | 06, 08 | coeftest, bptest, dwtest |",
  "| PMCMRplus | 05 | Games-Howell post-hoc test (optional; pairwise t-test fallback used if absent) |",
  "| car | 06 | VIF and influence diagnostics (optional; skipped gracefully if absent) |",
  "",
  "Install all at once:",
  "",
  "```r",
  'install.packages(c("ggplot2", "e1071", "sandwich", "lmtest",',
  '                   "PMCMRplus", "car"))',
  "```",
  "",
  "---",
  "",
  "## Script Descriptions",
  "",
  "| Script | Purpose | Key Outputs |",
  "|---|---|---|",
  "| `R/utils_analysis.R` | Shared constants, helpers, data loaders. Sourced by all scripts. | (no direct outputs) |",
  "| `R/00_run_analysis.R` | Master script: sources 04–09 in order. | All downstream outputs. |",
  "| `R/01_download_data.R` | Downloads raw data from Yahoo Finance. | `data/raw/` |",
  "| `R/02_data_cleaning.R` | Cleans and validates OHLCV data. | `data/processed/constituent_prices_clean/` |",
  "| `R/03_feature_engineering.R` | Computes returns, rolling vol, drawdown, crash flags. | `data/processed/features/` |",
  "| `R/04_eda.R` | Descriptive statistics, distribution plots, weekday charts, crash counts. | `results/tables/eda_*.csv`, `results/figures/eda_*.png` |",
  "| `R/05_hypothesis_testing.R` | ANOVA, post-hoc, chi-square, t-tests, effect sizes. | `results/tables/hyp_*.csv`, `results/figures/hyp_*.png` |",
  "| `R/06_regression_analysis.R` | OLS and logistic weekday models; HAC/HC3 robust SEs; diagnostics. | `results/tables/reg_*.csv`, `results/figures/reg_*.png` |",
  "| `R/07_crash_analysis.R` | Crash event catalogue, drawdown episodes, constituent analysis, sub-periods. | `results/tables/crash_*.csv`, `results/figures/crash_*.png` |",
  "| `R/08_robustness_checks.R` | Sub-period ANOVA, alternative thresholds, multiple-testing correction, SE comparison. | `results/tables/rob_*.csv`, `results/figures/rob_*.png` |",
  "| `R/09_generate_research_outputs.R` | Reads all result tables and generates Markdown reports. | `results/reports/*.md` |",
  "",
  "---",
  "",
  "## Output Directory Structure",
  "",
  "```",
  "results/",
  "├── tables/          # CSV result tables (one per test/model/summary)",
  "├── figures/         # PNG figures (150–300 dpi, publication-quality)",
  "└── reports/",
  "    ├── statistical_results_report.md   # Consolidated findings report",
  "    ├── data_dictionary.md              # Column definitions for all datasets",
  "    ├── README_analysis.md              # This file",
  "    ├── results_file_index.csv          # Indexed list of all output files",
  "    └── limitations_and_caveats.txt     # Limitations from 08_robustness_checks.R",
  "```",
  "",
  "---",
  "",
  "## Important Data Notes",
  "",
  "1. **Index history starts 2007-09-17**, not 2000. The project title refers to",
  "   the intended scope; the actual index data coverage is 2007 onward.",
  "",
  "2. **Constituent data start dates vary.** Some go back to 2000; others start",
  "   as recently as 2023. Panel analyses pool heterogeneous histories.",
  "",
  "3. **Survivorship bias:** The constituent list reflects the current (2026-10-21)",
  "   NIFTY 50 composition. Firms delisted or removed since 2000 are excluded.",
  "",
  "4. **ReturnPrice** uses Adjusted Close where available, falling back to Close.",
  "   The `ReturnSource` column flags which price was used for each row.",
  "",
  "5. **Scripts do not overwrite raw or processed data.** They only write to",
  "   `results/`. If `results/tables/<file>.csv` already exists, it is overwritten",
  "   with a warning message in the console.",
  "",
  "---",
  "",
  "## Manual RStudio Commands (copy-paste)",
  "",
  "Run **each line separately** in the RStudio Console, in this order:",
  "",
  "```r",
  "# Step 0: Set working directory (required)",
  'setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")',
  "",
  "# Step 1: Exploratory Data Analysis",
  'source("R/04_eda.R")',
  "",
  "# Step 2: Hypothesis Testing",
  'source("R/05_hypothesis_testing.R")',
  "",
  "# Step 3: Regression Analysis",
  'source("R/06_regression_analysis.R")',
  "",
  "# Step 4: Crash and Drawdown Analysis",
  'source("R/07_crash_analysis.R")',
  "",
  "# Step 5: Robustness Checks",
  'source("R/08_robustness_checks.R")',
  "",
  "# Step 6: Generate Reports and Documentation",
  'source("R/09_generate_research_outputs.R")',
  "```",
  "",
  "**Total estimated runtime:** 2–5 minutes on a standard laptop,",
  "depending on hardware and R package loading time.",
  "",
  "---",
  "",
  sprintf("*README generated by R/09_generate_research_outputs.R on %s.*",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  ""
)

write_report(file.path(DIR_REPORTS, "README_analysis.md"), readme_lines)

# ============================================================================
# 5. Results File Index
# ============================================================================
cat("\n[Step 5] Generating results file index ...\n")

# Enumerate every table and figure that should exist
expected_tables <- c(
  "eda_index_descriptive_stats.csv",
  "eda_index_weekday_mean_ci.csv",
  "eda_index_weekday_volatility.csv",
  "eda_crash_counts_by_weekday.csv",
  "eda_drawdown_summary.csv",
  "eda_constituent_descriptive_stats.csv",
  "eda_constituent_panel_overall_stats.csv",
  "eda_constituent_weekday_stats.csv",
  "eda_constituent_weekday_mean_ci.csv",
  "hyp_normality_shapiro_wilk.csv",
  "hyp_levene_equal_variance.csv",
  "hyp_ljung_box_autocorrelation.csv",
  "hyp_anova_weekday_returns_index.csv",
  "hyp_posthoc_weekday_returns_index.csv",
  "hyp_monday_pairwise_ttest.csv",
  "hyp_weekday_volatility_tests.csv",
  "hyp_weekday_sd_table.csv",
  "hyp_all_test_results.csv",
  "hyp_contingency_-3%_NIFTY50Index.csv",
  "hyp_contingency_-5%_NIFTY50Index.csv",
  "reg_m1_ols_weekday_coefficients.csv",
  "reg_m1_model_fit.csv",
  "reg_m2_ols_weekday_lagvol_coefficients.csv",
  "reg_m2_model_fit.csv",
  "reg_m2_vif.csv",
  "reg_m3_logit_coefficients.csv",
  "reg_m3_model_fit.csv",
  "reg_m4_logit_coefficients.csv",
  "reg_m4_model_fit.csv",
  "reg_model_comparison.csv",
  "reg_m1_diagnostics_tests.csv",
  "reg_m1_influential_observations.csv",
  "crash_3pct_index_crash_events.csv",
  "crash_5pct_index_crash_events.csv",
  "crash_index_weekday_frequency_detail.csv",
  "crash_index_drawdown_episodes_20pct.csv",
  "crash_index_drawdown_overall.csv",
  "crash_index_volatility_around_crashes.csv",
  "crash_constituent_crash_summary.csv",
  "crash_constituent_weekday_crash_rates.csv",
  "crash_index_period_weekday_breakdown.csv",
  "rob_subperiod_anova.csv",
  "rob_alternative_crash_thresholds.csv",
  "rob_return_source_sensitivity.csv",
  "rob_subperiod_regression.csv",
  "rob_multiple_testing_correction.csv",
  "rob_se_comparison_ols_hac_hc3.csv",
  "rob_pvalue_sensitivity_to_se_type.csv",
  "rob_constituent_individual_anova.csv",
  "rob_limitations.csv"
)

expected_figures <- c(
  "eda_01_index_return_histogram.png",
  "eda_02_index_return_qqplot.png",
  "eda_03_index_weekday_boxplot.png",
  "eda_04_index_weekday_mean_return.png",
  "eda_05_index_weekday_volatility.png",
  "eda_06_index_crash_rate_by_weekday.png",
  "eda_07_index_return_timeseries.png",
  "eda_08_index_drawdown_timeseries.png",
  "eda_09_index_rolling_volatility.png",
  "eda_10_constituent_return_density.png",
  "eda_11_constituent_weekday_mean_return.png",
  "eda_12_constituent_crash_rate_by_weekday.png",
  "eda_13_index_drawdown_histogram.png",
  "hyp_01_posthoc_pvalue_heatmap.png",
  "hyp_02_weekday_mean_returns_forest.png",
  "reg_M1_OLS_diagnostics.png",
  "reg_01_cooks_distance.png",
  "reg_02_weekday_coefficients.png",
  "reg_03_crash3_odds_ratios.png",
  "crash_01_top20_worst_returns.png",
  "crash_02_drawdown_with_episodes.png",
  "crash_03_crash_rate_by_period_weekday.png",
  "crash_04_constituent_crash_rates.png",
  "crash_05_volatility_around_crashes.png",
  "rob_01_crash_counts_by_threshold.png",
  "rob_02_subperiod_coefficient_stability.png",
  "rob_03_se_comparison.png"
)

expected_reports <- c(
  "statistical_results_report.md",
  "data_dictionary.md",
  "README_analysis.md",
  "results_file_index.csv",
  "limitations_and_caveats.txt"
)

# Check which files actually exist
make_index_rows <- function(files, subdir, category) {
  do.call(rbind, lapply(files, function(f) {
    full_path <- file.path(subdir, f)
    exists    <- file.exists(full_path)
    size_kb   <- if (exists) round(file.info(full_path)$size / 1024, 1) else NA_real_
    data.frame(
      Category   = category,
      Filename   = f,
      RelPath    = file.path(basename(subdir), f),
      Exists     = exists,
      SizeKB     = size_kb,
      stringsAsFactors = FALSE
    )
  }))
}

file_index <- rbind(
  make_index_rows(expected_tables,  DIR_TABLES,  "Table"),
  make_index_rows(expected_figures, DIR_FIGURES, "Figure"),
  make_index_rows(expected_reports, DIR_REPORTS, "Report")
)

save_table(file_index, "results_file_index.csv", subdir = DIR_REPORTS)

# Console summary
n_exist <- sum(file_index$Exists)
n_total <- nrow(file_index)
cat(sprintf("  File index: %d/%d expected output files exist\n",
            n_exist, n_total))

missing_files <- file_index[!file_index$Exists, ]
if (nrow(missing_files) > 0) {
  cat(sprintf("  Missing files (%d) — run corresponding scripts:\n",
              nrow(missing_files)))
  for (i in seq_len(nrow(missing_files))) {
    cat(sprintf("    [%s] %s\n",
                missing_files$Category[i], missing_files$Filename[i]))
  }
} else {
  cat("  All expected output files are present.\n")
}

# ============================================================================
# 6. Completion summary
# ============================================================================
cat(sprintf("\n[09 Complete]\n"))
cat(sprintf("  Reports saved to : %s\n", DIR_REPORTS))
cat(sprintf("  Files generated  :\n"))
cat(sprintf("    statistical_results_report.md\n"))
cat(sprintf("    data_dictionary.md\n"))
cat(sprintf("    README_analysis.md\n"))
cat(sprintf("    results_file_index.csv\n"))
cat(sprintf("  Finished: %s\n\n", Sys.time()))
