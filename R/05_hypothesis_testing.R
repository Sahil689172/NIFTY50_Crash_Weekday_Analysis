# =============================================================================
# Script  : 05_hypothesis_testing.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : Formal hypothesis testing — weekday effects on mean returns,
#           crash occurrence, and volatility.  Full specification of
#           hypotheses, assumptions, test statistics, p-values, confidence
#           intervals, effect sizes, and interpretations for every test.
# Inputs  : data/processed/features/nifty50_index_features.csv
#           data/processed/features/all_constituents_features.csv
# Outputs : results/tables/hyp_*.csv
#           results/figures/hyp_*.png
# Packages: e1071 (skewness/kurtosis), ggplot2, PMCMRplus (Games-Howell)
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/05_hypothesis_testing.R")
# =============================================================================
#
# STATISTICAL NOTES
# -----------------
# 1. Financial return series exhibit autocorrelation and conditional
#    heteroskedasticity (ARCH effects). Standard ANOVA and t-tests assume
#    i.i.d. errors. Tests here are labelled accordingly and robust inference
#    is noted where relevant. The regression script (06) uses HAC standard
#    errors as the primary inferential approach.
# 2. Statistical significance does NOT imply economic significance or
#    causation. Effect sizes are reported alongside p-values.
# 3. Multiple testing: Bonferroni correction is applied to pairwise
#    comparisons. BH (Benjamini-Hochberg) correction is also reported as a
#    less conservative alternative.
# 4. Survivorship bias in the constituent panel is explicitly documented.
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
require_pkg("ggplot2")
require_pkg("e1071")

# PMCMRplus provides Games-Howell post-hoc test (Welch-based pairwise)
if (!requireNamespace("PMCMRplus", quietly = TRUE)) {
  message(
    "  NOTE: Package 'PMCMRplus' not installed.\n",
    "  Games-Howell post-hoc test requires it.\n",
    "  Run: install.packages('PMCMRplus')\n",
    "  If unavailable, pairwise Welch t-tests with BH correction are used."
  )
  HAS_PMCMR <- FALSE
} else {
  suppressPackageStartupMessages(library(PMCMRplus))
  HAS_PMCMR <- TRUE
}

create_results_dirs()

# Accumulator for all test results — written to a single summary at the end
all_tests <- list()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  05_hypothesis_testing.R\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Load and prepare data
# ---------------------------------------------------------------------------
cat("[Step 1] Loading data ...\n")
idx   <- load_index()
panel <- load_panel()
idx_r <- valid_returns(idx)
panel_r <- valid_returns(panel)

# Create list of return vectors by weekday for index
wd_list_idx <- lapply(WEEKDAY_LEVELS, function(wd)
  idx_r[[COL_RETURN]][idx_r[[COL_WEEKDAY]] == wd])
names(wd_list_idx) <- WEEKDAY_LEVELS

# Significance level
ALPHA <- 0.05

# ---------------------------------------------------------------------------
# Helper: format a test result into a standard data frame row
# ---------------------------------------------------------------------------
make_result <- function(test_id, dataset, hypothesis, h0, ha,
                        test_name, statistic, stat_name, df_val,
                        p_value, alpha, ci_lo, ci_hi,
                        effect_size, effect_name,
                        conclusion, notes, assumption_ok) {
  data.frame(
    TestID          = test_id,
    Dataset         = dataset,
    Hypothesis      = hypothesis,
    H0              = h0,
    Ha              = ha,
    Test            = test_name,
    Statistic       = round(statistic, 5),
    StatName        = stat_name,
    DF              = if (is.null(df_val) || (length(df_val) == 1 && is.na(df_val))) {
                       NA_character_
                     } else if (is.character(df_val)) {
                       df_val                          # already formatted string
                     } else {
                       as.character(round(df_val, 2))  # numeric scalar
                     },
    P_value         = signif(p_value, 4),
    Alpha           = alpha,
    Significant     = p_value < alpha,
    CI_lower        = ifelse(is.na(ci_lo), NA_real_, round(ci_lo, 6)),
    CI_upper        = ifelse(is.na(ci_hi), NA_real_, round(ci_hi, 6)),
    EffectSize      = ifelse(is.na(effect_size), NA_real_,
                             round(effect_size, 5)),
    EffectName      = effect_name,
    AssumptionCheck = assumption_ok,
    Conclusion      = conclusion,
    Notes           = notes,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# 2. Assumption diagnostics
# ---------------------------------------------------------------------------
cat("[Step 2] Assumption diagnostics ...\n")

## 2a. Normality — Shapiro-Wilk on each weekday's index returns
##     (Shapiro-Wilk is limited to n <= 5000; use first 5000 obs if larger)
sw_results <- lapply(WEEKDAY_LEVELS, function(wd) {
  r   <- wd_list_idx[[wd]]
  n   <- length(r)
  r2  <- if (n > 5000) r[1:5000] else r
  sw  <- shapiro.test(r2)
  data.frame(Weekday = wd, N = n, W = sw$statistic, P_SW = sw$p.value,
             Normal_at_0.05 = sw$p.value >= 0.05,
             stringsAsFactors = FALSE)
})
sw_df <- do.call(rbind, sw_results)
save_table(sw_df, "hyp_normality_shapiro_wilk.csv")

cat("  Shapiro-Wilk normality (index returns by weekday):\n")
for (i in seq_len(nrow(sw_df))) {
  r <- sw_df[i, ]
  cat(sprintf("    %-10s W=%.4f  p=%s  Normal=%s\n",
              r$Weekday, r$W,
              format(r$P_SW, digits = 3, scientific = TRUE),
              r$Normal_at_0.05))
}
any_normal <- all(sw_df$Normal_at_0.05)
cat(sprintf("  All weekdays normal at alpha=0.05: %s\n", any_normal))

## 2b. Levene's / Bartlett's test for equal variances across weekdays
##     Bartlett is sensitive to non-normality; use Levene (via manual impl)
##     Levene's test: ANOVA on absolute deviations from group medians
levene_manual <- function(y, g) {
  groups   <- unique(g)
  med_devs <- unlist(lapply(groups, function(grp) {
    vals <- y[g == grp]
    abs(vals - median(vals, na.rm = TRUE))
  }))
  group_vec <- rep(groups, times = sapply(groups, function(grp)
    sum(g == grp)))
  summary(aov(med_devs ~ factor(group_vec)))[[1]]
}

ret_vec   <- idx_r[[COL_RETURN]]
wd_vec    <- as.character(idx_r[[COL_WEEKDAY]])
lev_aov   <- levene_manual(ret_vec, wd_vec)
lev_F     <- lev_aov[1, "F value"]
lev_p     <- lev_aov[1, "Pr(>F)"]

levene_df <- data.frame(
  Test            = "Levene (absolute deviation ANOVA)",
  Dataset         = "NIFTY50_Index",
  F_statistic     = round(lev_F, 4),
  P_value         = signif(lev_p, 4),
  Equal_var_0.05  = lev_p >= 0.05,
  Note            = paste0("Returns are financial time series; Levene test ",
                            "is preliminary; HAC inference used in regression"),
  stringsAsFactors = FALSE
)
save_table(levene_df, "hyp_levene_equal_variance.csv")
cat(sprintf("  Levene test: F=%.4f  p=%s  Equal variances: %s\n",
            lev_F, format(lev_p, digits = 3), lev_p >= 0.05))
USE_WELCH_ANOVA <- lev_p < ALPHA  # flag for test selection

## 2c. Autocorrelation check (Ljung-Box on returns — informational)
lb_test <- Box.test(idx_r[[COL_RETURN]], lag = 10, type = "Ljung-Box")
lb_df <- data.frame(
  Test        = "Ljung-Box (lag 10)",
  Dataset     = "NIFTY50_Index",
  Chi_sq      = round(lb_test$statistic, 4),
  DF          = lb_test$parameter,
  P_value     = signif(lb_test$p.value, 4),
  Autocorr_present = lb_test$p.value < ALPHA,
  Note        = paste0("Significant autocorrelation implies ANOVA/t-test ",
                       "p-values are approximate; HAC SEs in 06_regression"),
  stringsAsFactors = FALSE
)
save_table(lb_df, "hyp_ljung_box_autocorrelation.csv")
cat(sprintf("  Ljung-Box (lag 10): chi2=%.3f  p=%s  Autocorrelation: %s\n",
            lb_test$statistic, format(lb_test$p.value, digits = 3),
            lb_test$p.value < ALPHA))

# ---------------------------------------------------------------------------
# 3. Test 1 — One-way ANOVA / Welch ANOVA: weekday mean returns (Index)
# ---------------------------------------------------------------------------
cat(sprintf("\n[Step 3] Test 1 — %s: weekday mean returns (index) ...\n",
            ifelse(USE_WELCH_ANOVA, "Welch ANOVA", "Standard ANOVA")))

cat("  H0: Mean daily returns are equal across all five weekdays\n")
cat("  Ha: At least one weekday has a different mean daily return\n")
cat(sprintf("  Significance level: alpha = %.2f\n", ALPHA))

if (USE_WELCH_ANOVA) {
  # Welch's one-way ANOVA (does not assume equal variances)
  # oneway.test() implements Welch-Satterthwaite ANOVA
  anova_result <- oneway.test(
    as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
    data     = idx_r,
    var.equal = FALSE
  )
  F_stat   <- anova_result$statistic
  df1      <- anova_result$parameter[1]
  df2      <- anova_result$parameter[2]
  p_anova  <- anova_result$p.value
  anova_label <- "Welch one-way ANOVA (unequal variances)"
  # Eta-squared: approximate via SS from standard aov (for effect size only)
  aov_std  <- aov(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
                  data = idx_r)
  eta2     <- eta_squared(aov_std)
} else {
  aov_std  <- aov(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
                  data = idx_r)
  aov_sum  <- summary(aov_std)[[1]]
  F_stat   <- aov_sum[["F value"]][1]
  df1      <- aov_sum[["Df"]][1]
  df2      <- aov_sum[["Df"]][2]
  p_anova  <- aov_sum[["Pr(>F)"]][1]
  eta2     <- eta_squared(aov_std)
  anova_label <- "Standard one-way ANOVA (equal variances assumed)"
}

anova_row <- make_result(
  test_id     = "T01",
  dataset     = "NIFTY50_Index",
  hypothesis  = "Weekday effect on mean returns",
  h0          = "mu_Mon = mu_Tue = mu_Wed = mu_Thu = mu_Fri",
  ha          = "At least one weekday mean return differs",
  test_name   = anova_label,
  statistic   = F_stat,
  stat_name   = "F",
  df_val      = paste0(round(df1, 1), ", ", round(df2, 1)),
  p_value     = p_anova,
  alpha       = ALPHA,
  ci_lo       = NA, ci_hi = NA,
  effect_size = eta2,
  effect_name = "eta-squared",
  conclusion  = ifelse(p_anova < ALPHA,
    "Reject H0: significant weekday difference in mean returns",
    "Fail to reject H0: no significant weekday difference in mean returns"),
  notes = paste0(
    "Financial returns show autocorrelation and ARCH effects; ",
    "ANOVA p-value is approximate. ",
    "HAC-robust test in 06_regression_analysis.R is the primary inference. ",
    "Eta-squared computed from standard ANOVA SS (effect size approximation)."),
  assumption_ok = paste0("Normality: NOT met (Shapiro-Wilk). ",
                          "Equal variances: ",
                          ifelse(USE_WELCH_ANOVA, "NOT met — Welch used.",
                                 "Met — standard ANOVA used."))
)
all_tests[["T01"]] <- anova_row

cat(sprintf("  %s\n", anova_label))
cat(sprintf("  F(%s) = %.4f   p = %s   eta2 = %.5f\n",
            anova_row$DF, F_stat,
            format(p_anova, digits = 4, scientific = TRUE), eta2))
cat(sprintf("  Conclusion: %s\n", anova_row$Conclusion))

# Save ANOVA table
aov_std_sum <- summary(aov_std)[[1]]
aov_table_df <- data.frame(
  Term    = rownames(aov_std_sum),
  Df      = aov_std_sum$Df,
  SumSq   = aov_std_sum[["Sum Sq"]],
  MeanSq  = aov_std_sum[["Mean Sq"]],
  F_value = aov_std_sum[["F value"]],
  P_value = aov_std_sum[["Pr(>F)"]],
  stringsAsFactors = FALSE
)
save_table(aov_table_df, "hyp_anova_weekday_returns_index.csv")

# ---------------------------------------------------------------------------
# 4. Test 2 — Post-hoc pairwise comparisons (Index returns)
# ---------------------------------------------------------------------------
cat("\n[Step 4] Post-hoc pairwise weekday comparisons ...\n")

if (USE_WELCH_ANOVA && HAS_PMCMR) {
  cat("  Using Games-Howell post-hoc test (appropriate after Welch ANOVA)\n")
  gh_result  <- PMCMRplus::gamesHowellTest(
    as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
    data = idx_r
  )
  gh_sum     <- summary(gh_result)
  # Extract p-value matrix
  p_mat      <- gh_sum$p.value
  posthoc_method <- "Games-Howell"

  # Build pairwise table
  pairs <- which(upper.tri(p_mat), arr.ind = TRUE)
  ph_rows <- lapply(seq_len(nrow(pairs)), function(k) {
    i <- pairs[k, 1]; j <- pairs[k, 2]
    wdi <- rownames(p_mat)[i]; wdj <- colnames(p_mat)[j]
    ri  <- wd_list_idx[[wdi]]; rj <- wd_list_idx[[wdj]]
    d   <- cohen_d(ri, rj)
    data.frame(
      Weekday1   = wdi, Weekday2 = wdj,
      P_raw      = p_mat[i, j],
      P_bonf     = min(1, p_mat[i, j] * nrow(pairs)),
      Cohen_d    = round(d, 5),
      Method     = "Games-Howell",
      stringsAsFactors = FALSE
    )
  })
  posthoc_df <- do.call(rbind, ph_rows)

} else {
  cat("  Using pairwise Welch t-tests with Bonferroni and BH correction\n")
  posthoc_method <- "Pairwise Welch t-test"
  wd_pairs   <- combn(WEEKDAY_LEVELS, 2, simplify = FALSE)
  ph_rows    <- lapply(wd_pairs, function(pair) {
    wdi <- pair[1]; wdj <- pair[2]
    ri  <- wd_list_idx[[wdi]]; rj <- wd_list_idx[[wdj]]
    tt  <- t.test(ri, rj, var.equal = FALSE)
    d   <- cohen_d(ri, rj)
    data.frame(
      Weekday1   = wdi, Weekday2 = wdj,
      P_raw      = tt$p.value,
      P_bonf     = NA_real_,   # filled below
      Cohen_d    = round(d, 5),
      T_stat     = round(tt$statistic, 4),
      DF         = round(tt$parameter, 2),
      CI_lo      = tt$conf.int[1],
      CI_hi      = tt$conf.int[2],
      Method     = "Welch t-test",
      stringsAsFactors = FALSE
    )
  })
  posthoc_df <- do.call(rbind, ph_rows)
  # Apply multiple-testing corrections
  posthoc_df$P_bonf <- p.adjust(posthoc_df$P_raw, method = "bonferroni")
  posthoc_df$P_BH   <- p.adjust(posthoc_df$P_raw, method = "BH")
}

posthoc_df$Significant_Bonf <- posthoc_df$P_bonf < ALPHA
save_table(posthoc_df, "hyp_posthoc_weekday_returns_index.csv")

cat(sprintf("  Post-hoc results (%s):\n", posthoc_method))
for (i in seq_len(nrow(posthoc_df))) {
  r <- posthoc_df[i, ]
  cat(sprintf("    %s vs %-10s  p_raw=%s  p_bonf=%s  d=%.4f  Sig=%s\n",
              r$Weekday1, r$Weekday2,
              format(r$P_raw, digits = 3, scientific = TRUE),
              format(r$P_bonf, digits = 3, scientific = TRUE),
              r$Cohen_d, r$Significant_Bonf))
}

# Heatmap of pairwise p-values
p_mat_full <- matrix(NA, 5, 5,
                     dimnames = list(WEEKDAY_LEVELS, WEEKDAY_LEVELS))
for (i in seq_len(nrow(posthoc_df))) {
  r <- posthoc_df[i, ]
  p_mat_full[r$Weekday1, r$Weekday2] <- r$P_bonf
  p_mat_full[r$Weekday2, r$Weekday1] <- r$P_bonf
}
# Flatten for ggplot
ph_long <- expand.grid(Wd1 = WEEKDAY_LEVELS, Wd2 = WEEKDAY_LEVELS,
                        stringsAsFactors = FALSE)
ph_long$P_bonf <- as.vector(p_mat_full)
ph_long$Wd1 <- factor(ph_long$Wd1, levels = WEEKDAY_LEVELS)
ph_long$Wd2 <- factor(ph_long$Wd2, levels = rev(WEEKDAY_LEVELS))

p_pheat <- ggplot2::ggplot(ph_long[!is.na(ph_long$P_bonf), ],
    ggplot2::aes(x = Wd1, y = Wd2, fill = P_bonf)) +
  ggplot2::geom_tile(colour = "white") +
  ggplot2::geom_text(ggplot2::aes(
    label = ifelse(P_bonf < ALPHA,
                   sprintf("%.3f*", P_bonf),
                   sprintf("%.3f",  P_bonf))),
    size = 3.2) +
  ggplot2::scale_fill_gradient2(
    low  = "firebrick", mid = "white", high = "steelblue",
    midpoint = 0.5, limits = c(0, 1), name = "p (Bonf.)") +
  ggplot2::labs(
    title    = "Post-hoc Pairwise p-values (Bonferroni corrected)",
    subtitle = sprintf("* = significant at alpha = %.2f  |  Method: %s",
                       ALPHA, posthoc_method),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "hyp_01_posthoc_pvalue_heatmap.png"), p_pheat,
        width = 7, height = 6)

# ---------------------------------------------------------------------------
# 5. Test 3 — Chi-square: weekday vs crash occurrence (Index, -3%)
# ---------------------------------------------------------------------------
cat("\n[Step 5] Test 3 — Chi-square: weekday vs crash occurrence (-3%) ...\n")

run_chisq_crash <- function(df_r, threshold_col, threshold_label, dataset,
                             test_id) {
  # Contingency table: Weekday x Crash(TRUE/FALSE)
  crash_bin <- df_r[[threshold_col]] == TRUE
  tab       <- table(Weekday = df_r[[COL_WEEKDAY]],
                     Crash   = crash_bin)

  # Check expected cell counts >= 5
  exp_counts <- chisq.test(tab)$expected
  min_exp    <- min(exp_counts)
  cat(sprintf("  Contingency table (%s):\n", threshold_label))
  print(tab)
  cat(sprintf("  Min expected cell count: %.2f\n", min_exp))

  if (min_exp < 5) {
    cat("  WARNING: Expected cell count < 5; chi-square approximation ",
        "may be unreliable. Fisher's exact test used if 2x2, else noted.\n")
    assumption_note <- "VIOLATED: min expected count < 5; result approximate"
  } else {
    assumption_note <- "Met: all expected counts >= 5"
  }

  cs   <- chisq.test(tab)
  n    <- nrow(df_r)
  k    <- min(nrow(tab), ncol(tab))
  V    <- unname(cramers_v(cs, n, k))

  conclusion <- ifelse(cs$p.value < ALPHA,
    sprintf("Reject H0: crash distribution differs across weekdays (%s)",
            threshold_label),
    sprintf("Fail to reject H0: no significant weekday pattern in crashes (%s)",
            threshold_label))

  row <- make_result(
    test_id     = test_id,
    dataset     = dataset,
    hypothesis  = sprintf("Weekday independence from %s crash events",
                          threshold_label),
    h0          = "Crash occurrence is independent of weekday",
    ha          = "Crash occurrence depends on weekday",
    test_name   = "Pearson chi-square test of independence",
    statistic   = cs$statistic,
    stat_name   = "chi-sq",
    df_val      = cs$parameter,
    p_value     = cs$p.value,
    alpha       = ALPHA,
    ci_lo       = NA, ci_hi = NA,
    effect_size = V,
    effect_name = "Cramer's V",
    conclusion  = conclusion,
    notes       = paste0(
      "Crash defined as ReturnPrice <= ", threshold_label, ". ",
      "Financial returns are not i.i.d.; chi-square p-value is approximate. ",
      "Contingency table saved separately."),
    assumption_ok = assumption_note
  )

  # Save contingency table
  tab_df <- as.data.frame.matrix(tab)
  tab_df$Weekday <- rownames(tab_df)
  tab_df <- tab_df[, c("Weekday", setdiff(colnames(tab_df), "Weekday"))]
  save_table(tab_df, sprintf("hyp_contingency_%s_%s.csv",
                              gsub(" ", "_", threshold_label),
                              gsub("[^A-Za-z0-9]", "", dataset)))
  list(row = row, table = tab, chisq = cs)
}

chisq3_idx <- run_chisq_crash(idx_r, COL_CRASH3, "-3%", "NIFTY50_Index", "T03")
all_tests[["T03"]] <- chisq3_idx$row
cat(sprintf("  chi2(%.0f)=%.4f  p=%s  V=%.4f  Conclusion: %s\n",
            chisq3_idx$chisq$parameter, chisq3_idx$chisq$statistic,
            format(chisq3_idx$chisq$p.value, digits = 3),
            chisq3_idx$row$EffectSize,
            chisq3_idx$row$Conclusion))

# ---------------------------------------------------------------------------
# 6. Test 4 — Chi-square: weekday vs crash occurrence (Index, -5%)
# ---------------------------------------------------------------------------
cat("\n[Step 6] Test 4 — Chi-square: weekday vs crash occurrence (-5%) ...\n")
chisq5_idx <- run_chisq_crash(idx_r, COL_CRASH5, "-5%", "NIFTY50_Index", "T04")
all_tests[["T04"]] <- chisq5_idx$row
cat(sprintf("  chi2(%.0f)=%.4f  p=%s  V=%.4f  Conclusion: %s\n",
            chisq5_idx$chisq$parameter, chisq5_idx$chisq$statistic,
            format(chisq5_idx$chisq$p.value, digits = 3),
            chisq5_idx$row$EffectSize,
            chisq5_idx$row$Conclusion))

# ---------------------------------------------------------------------------
# 7. Test 5 — Targeted pairwise t-tests: Monday vs each other weekday
#    (Monday effect hypothesis — most cited in literature)
# ---------------------------------------------------------------------------
cat("\n[Step 7] Test 5 — Monday vs other weekdays (pairwise Welch t-tests) ...\n")
cat("  H0 for each: mu_Monday = mu_Weekday_j\n")
cat("  Ha for each: mu_Monday ≠ mu_Weekday_j\n")
cat("  Note: Post-hoc already covers these; reported separately for\n")
cat("  direct Monday-effect interpretation\n")

mon_returns <- wd_list_idx[["Monday"]]
monday_pairs <- lapply(setdiff(WEEKDAY_LEVELS, "Monday"), function(wd) {
  other_r <- wd_list_idx[[wd]]
  tt      <- t.test(mon_returns, other_r, var.equal = FALSE)
  d       <- cohen_d(mon_returns, other_r)
  data.frame(
    Comparison    = sprintf("Monday vs %s", wd),
    N_Mon         = length(mon_returns),
    N_Other       = length(other_r),
    Mean_Mon      = mean(mon_returns),
    Mean_Other    = mean(other_r),
    Diff_Mean     = mean(mon_returns) - mean(other_r),
    T_stat        = tt$statistic,
    DF            = tt$parameter,
    P_raw         = tt$p.value,
    CI_lo         = tt$conf.int[1],
    CI_hi         = tt$conf.int[2],
    Cohen_d       = d,
    stringsAsFactors = FALSE
  )
})
monday_df <- do.call(rbind, monday_pairs)
# Bonferroni and BH corrections
monday_df$P_bonf <- p.adjust(monday_df$P_raw, method = "bonferroni")
monday_df$P_BH   <- p.adjust(monday_df$P_raw, method = "BH")
monday_df$Sig_Bonf <- monday_df$P_bonf < ALPHA
monday_df$Sig_BH   <- monday_df$P_BH   < ALPHA
save_table(monday_df, "hyp_monday_pairwise_ttest.csv")

cat("  Results:\n")
for (i in seq_len(nrow(monday_df))) {
  r <- monday_df[i, ]
  cat(sprintf("    %-22s  diff=%+.5f  p_raw=%s  p_bonf=%s  d=%.4f\n",
              r$Comparison, r$Diff_Mean,
              format(r$P_raw, digits = 3, scientific = TRUE),
              format(r$P_bonf, digits = 3, scientific = TRUE),
              r$Cohen_d))
}

# Collect as formal test rows
for (i in seq_len(nrow(monday_df))) {
  r <- monday_df[i, ]
  key <- sprintf("T05_%s", gsub(" ", "_", r$Comparison))
  all_tests[[key]] <- make_result(
    test_id     = key,
    dataset     = "NIFTY50_Index",
    hypothesis  = r$Comparison,
    h0          = sprintf("mu_Monday = mu_%s",
                          sub("Monday vs ", "", r$Comparison)),
    ha          = sprintf("mu_Monday != mu_%s",
                          sub("Monday vs ", "", r$Comparison)),
    test_name   = "Welch two-sample t-test (two-tailed)",
    statistic   = r$T_stat,
    stat_name   = "t",
    df_val      = r$DF,
    p_value     = r$P_raw,
    alpha       = ALPHA,
    ci_lo       = r$CI_lo,
    ci_hi       = r$CI_hi,
    effect_size = r$Cohen_d,
    effect_name = "Cohen's d",
    conclusion  = ifelse(r$Sig_Bonf,
      sprintf("Significant after Bonferroni: Monday differs from %s",
              sub("Monday vs ", "", r$Comparison)),
      sprintf("Not significant after Bonferroni: Monday vs %s",
              sub("Monday vs ", "", r$Comparison))),
    notes = "Multiple-testing corrections: Bonferroni and BH reported",
    assumption_ok = "Normality not met; t-test is approximate for large n"
  )
}

# ---------------------------------------------------------------------------
# 8. Test 6 — Weekday differences in volatility (Levene-based F-test)
# ---------------------------------------------------------------------------
cat("\n[Step 8] Test 6 — Weekday differences in volatility ...\n")
cat("  H0: Variance of daily returns is equal across all weekdays\n")
cat("  Ha: At least one weekday has different return variance\n")

# Bartlett's test (sensitive to non-normality — note this)
bart <- bartlett.test(
  as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
  data = idx_r
)
# Levene's test (more robust)
lev_vol  <- levene_manual(idx_r[[COL_RETURN]],
                          as.character(idx_r[[COL_WEEKDAY]]))
lev_F_v  <- lev_vol[1, "F value"]
lev_p_v  <- lev_vol[1, "Pr(>F)"]

vol_tests <- data.frame(
  Test         = c("Bartlett", "Levene (absolute deviation)"),
  Dataset      = "NIFTY50_Index",
  Statistic    = round(c(bart$statistic, lev_F_v), 4),
  Stat_name    = c("K2", "F"),
  DF           = c(bart$parameter, lev_vol[1, "Df"]),
  P_value      = signif(c(bart$p.value, lev_p_v), 4),
  Significant  = c(bart$p.value < ALPHA, lev_p_v < ALPHA),
  Note         = c(
    "Bartlett sensitive to non-normality; treat as supplementary",
    "Levene more robust to non-normality; preferred result"
  ),
  stringsAsFactors = FALSE
)
save_table(vol_tests, "hyp_weekday_volatility_tests.csv")

all_tests[["T06"]] <- make_result(
  test_id     = "T06",
  dataset     = "NIFTY50_Index",
  hypothesis  = "Weekday difference in return volatility",
  h0          = "Var(Return) equal across all weekdays",
  ha          = "At least one weekday has different return variance",
  test_name   = "Levene's test (absolute deviation ANOVA)",
  statistic   = lev_F_v,
  stat_name   = "F",
  df_val      = paste0(lev_vol[1, "Df"], ", ", lev_vol[2, "Df"]),
  p_value     = lev_p_v,
  alpha       = ALPHA,
  ci_lo       = NA, ci_hi = NA,
  effect_size = NA,
  effect_name = "none",
  conclusion  = ifelse(lev_p_v < ALPHA,
    "Reject H0: weekday volatility differs significantly",
    "Fail to reject H0: no significant weekday volatility difference"),
  notes = paste0("Bartlett result also reported in hyp_weekday_volatility_tests.csv. ",
                 "Bartlett is sensitive to non-normality; Levene preferred."),
  assumption_ok = "Levene does not require normality"
)
cat(sprintf("  Levene F=%.4f  p=%s  Significant: %s\n",
            lev_F_v, format(lev_p_v, digits = 3),
            lev_p_v < ALPHA))

# Per-weekday SD table
wd_sd_df <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  r <- wd_list_idx[[wd]]
  data.frame(Weekday = wd, N = length(r),
             SD = sd(r), Var = var(r),
             stringsAsFactors = FALSE)
}))
save_table(wd_sd_df, "hyp_weekday_sd_table.csv")

# ---------------------------------------------------------------------------
# 9. Test 7 — ANOVA on constituent panel (pooled, with caveat)
# ---------------------------------------------------------------------------
cat("\n[Step 9] Test 7 — Welch ANOVA on constituent panel (pooled) ...\n")
cat("  NOTE: Panel pools heterogeneous firms and time periods; results\n")
cat("  subject to survivorship bias and correlated observations.\n")

panel_anova <- oneway.test(
  as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)),
  data      = panel_r,
  var.equal = FALSE
)

all_tests[["T07"]] <- make_result(
  test_id     = "T07",
  dataset     = "All_Constituents_Pooled",
  hypothesis  = "Weekday effect on mean returns (pooled constituents)",
  h0          = "Equal weekday mean returns across pooled constituent panel",
  ha          = "At least one weekday mean return differs",
  test_name   = "Welch one-way ANOVA",
  statistic   = panel_anova$statistic,
  stat_name   = "F",
  df_val      = paste0(round(panel_anova$parameter[1], 1), ", ",
                       round(panel_anova$parameter[2], 1)),
  p_value     = panel_anova$p.value,
  alpha       = ALPHA,
  ci_lo       = NA, ci_hi = NA,
  effect_size = NA,
  effect_name = "not computed (pooled panel)",
  conclusion  = ifelse(panel_anova$p.value < ALPHA,
    "Reject H0 (pooled panel): weekday effect present",
    "Fail to reject H0 (pooled panel)"),
  notes = paste0(
    "Panel pools 50 constituents with heterogeneous listing dates. ",
    "Survivorship bias: current NIFTY 50 composition, not historical. ",
    "Observations within firms are serially correlated. ",
    "Treat as exploratory; firm-level tests are more rigorous."),
  assumption_ok = paste0("Independence VIOLATED in panel context; ",
                          "result is descriptive only")
)
cat(sprintf("  F(%s) = %.4f   p = %s\n",
            all_tests[["T07"]]$DF,
            panel_anova$statistic,
            format(panel_anova$p.value, digits = 4, scientific = TRUE)))

# ---------------------------------------------------------------------------
# 10. Consolidate and save all test results
# ---------------------------------------------------------------------------
cat("\n[Step 10] Saving consolidated results ...\n")

results_df <- do.call(rbind, all_tests)
rownames(results_df) <- NULL
save_table(results_df, "hyp_all_test_results.csv")

# Print summary
cat("\n  === TEST RESULTS SUMMARY ===\n")
cat(sprintf("  %-6s %-30s %-10s %-10s %s\n",
            "TestID", "Hypothesis", "Statistic", "P-value", "Conclusion"))
cat(sprintf("  %s\n", paste(rep("-", 72), collapse = "")))
for (i in seq_len(nrow(results_df))) {
  r <- results_df[i, ]
  cat(sprintf("  %-6s %-30s %-10s %-10s %s\n",
              r$TestID,
              substr(r$Hypothesis, 1, 30),
              sprintf("%.4f", r$Statistic),
              format(r$P_value, digits = 3, scientific = TRUE),
              ifelse(r$Significant, "SIGNIFICANT", "ns")))
}

# ---------------------------------------------------------------------------
# 11. Figure — forest plot of weekday mean returns with CIs
# ---------------------------------------------------------------------------
cat("\n[Step 11] Forest plot of weekday mean returns ...\n")

wd_means <- do.call(rbind, lapply(WEEKDAY_LEVELS, function(wd) {
  r  <- wd_list_idx[[wd]]
  n  <- length(r)
  se <- sd(r) / sqrt(n)
  # Flag significance from Monday pairwise (Bonferroni)
  is_mon <- wd == "Monday"
  sig <- if (!is_mon) {
    monday_df$Sig_Bonf[monday_df$Comparison == sprintf("Monday vs %s", wd)]
  } else NA
  data.frame(
    Weekday = wd, Mean = mean(r), SE = se, N = n,
    CI_lo   = mean(r) - 1.96 * se,
    CI_hi   = mean(r) + 1.96 * se,
    Sig_vs_Monday = ifelse(is_mon, NA, sig),
    stringsAsFactors = FALSE
  )
}))
wd_means$Weekday <- factor(wd_means$Weekday, levels = rev(WEEKDAY_LEVELS))

p_forest <- ggplot2::ggplot(wd_means,
    ggplot2::aes(x = Mean, y = Weekday)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                       colour = "grey50") +
  ggplot2::geom_errorbarh(ggplot2::aes(xmin = CI_lo, xmax = CI_hi),
                           height = 0.25, colour = "steelblue",
                           linewidth = 0.8) +
  ggplot2::geom_point(size = 4,
                       colour = ifelse(
                         wd_means$Weekday == "Monday", "firebrick",
                         "steelblue")) +
  ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d", N)),
                     hjust = -0.2, vjust = -0.6, size = 3) +
  ggplot2::labs(
    title    = "Weekday Mean Daily Returns — NIFTY 50 Index",
    subtitle = "Points: sample mean; bars: 95% CI (±1.96 SE)",
    x = "Mean Daily Return", y = NULL
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5))
save_gg(file.path(DIR_FIGURES, "hyp_02_weekday_mean_returns_forest.png"),
        p_forest, width = 8, height = 5)

cat(sprintf("\n[Hypothesis Testing Complete]\n"))
cat(sprintf("  Tables saved to  : %s\n", DIR_TABLES))
cat(sprintf("  Figures saved to : %s\n", DIR_FIGURES))
cat(sprintf("  Finished: %s\n\n", Sys.time()))
