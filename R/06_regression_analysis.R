# =============================================================================
# Script  : 06_regression_analysis.R
# Project : A Statistical Investigation of Stock Market Crashes and
#           Day-of-the-Week Effects: Evidence from NIFTY 50 Historical
#           Returns, 2000-2026
# Purpose : OLS regression of returns on weekday dummies (Monday = reference);
#           logistic regression of crash indicators on weekday dummies;
#           HAC-robust standard errors; full diagnostics.
# Inputs  : data/processed/features/nifty50_index_features.csv
#           data/processed/features/all_constituents_features.csv
# Outputs : results/tables/reg_*.csv
#           results/figures/reg_*.png
# Packages: ggplot2, sandwich (HAC SEs), lmtest (coeftest), car (vif/avPlots)
# Run     :
#   setwd("C:/Users/hp/OneDrive/Desktop/NIFTY50_Crash_Weekday_Analysis")
#   source("R/06_regression_analysis.R")
# =============================================================================
#
# MODEL SPECIFICATIONS
# --------------------
# M1 (OLS): ReturnPrice ~ Weekday    (Monday omitted; no intercept suppressed)
#   Monday is the base category.  Coefficients on Tue-Fri represent the
#   differential return relative to Monday.
#
# M2 (OLS + lagged vol): ReturnPrice ~ Weekday + RollVol_20_lag1
#   RollVol_20 is shifted forward 1 period before inclusion to avoid
#   look-ahead bias (the volatility is known BEFORE the return is realised).
#
# M3 (Logit -3%): CrashFlag_3pct ~ Weekday   (binary outcome)
# M4 (Logit -5%): CrashFlag_5pct ~ Weekday   (binary outcome)
#
# ROBUST INFERENCE
# ----------------
# HAC standard errors (Newey-West) are used for OLS models because financial
# returns exhibit autocorrelation and heteroskedasticity (ARCH effects).
# The sandwich package computes these.  OLS SEs are also reported for
# comparison.  Logit models use heteroskedasticity-consistent (HC3) SEs.
# =============================================================================

# ---------------------------------------------------------------------------
# 0. Setup
# ---------------------------------------------------------------------------
source("R/utils_analysis.R")
require_pkg("ggplot2")

# sandwich + lmtest for HAC robust SEs
for (pkg in c("sandwich", "lmtest")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop(sprintf(
      "Package '%s' required but not installed.\n  Run: install.packages('%s')",
      pkg, pkg))
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# car for VIF and residual plots (optional — degraded gracefully if absent)
HAS_CAR <- requireNamespace("car", quietly = TRUE)
if (HAS_CAR) suppressPackageStartupMessages(library(car))

# Significance level — defined locally so 06 can be sourced independently
ALPHA <- 0.05

create_results_dirs()

cat("\n", rep("=", 70), "\n", sep = "")
cat("  06_regression_analysis.R\n")
cat(sprintf("  Started: %s\n", Sys.time()))
cat(rep("=", 70), "\n\n", sep = "")

# ---------------------------------------------------------------------------
# 1. Load data
# ---------------------------------------------------------------------------
cat("[Step 1] Loading data ...\n")
idx   <- load_index()
idx_r <- valid_returns(idx)

# Relevel: Monday as reference category (already done in prepare_df via factor
# with WEEKDAY_LEVELS, but relevel explicitly for lm())
idx_r[[COL_WEEKDAY]] <- relevel(idx_r[[COL_WEEKDAY]], ref = "Monday")

cat(sprintf("  Index valid returns: %d rows\n", nrow(idx_r)))

# ---------------------------------------------------------------------------
# Helper: extract tidy coefficient table from a model + coeftest result
# ---------------------------------------------------------------------------
tidy_coeftest <- function(ct, conf_level = 0.95) {
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha / 2)
  coef_names <- rownames(ct)
  data.frame(
    Term       = coef_names,
    Estimate   = ct[, 1],
    Std_Error  = ct[, 2],
    Statistic  = ct[, 3],
    P_value    = ct[, 4],
    CI_lower   = ct[, 1] - z_crit * ct[, 2],
    CI_upper   = ct[, 1] + z_crit * ct[, 2],
    Significant = ct[, 4] < 0.05,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Helper: save model diagnostics
# ---------------------------------------------------------------------------
save_diagnostics <- function(model, label, idx_data) {
  diag_path <- file.path(DIR_FIGURES, sprintf("reg_%s_diagnostics.png", label))
  save_png(diag_path, {
    par(mfrow = c(2, 2))
    plot(model, which = 1:4, main = sprintf("Diagnostics — %s", label))
    par(mfrow = c(1, 1))
  }, width = 1600, height = 1200, res = 150)
}

# ---------------------------------------------------------------------------
# 2. Model M1 — OLS: Returns ~ Weekday (HAC robust SEs)
# ---------------------------------------------------------------------------
cat("\n[Step 2] Model M1 — OLS: ReturnPrice ~ Weekday (Monday = ref) ...\n")

m1 <- lm(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY)), data = idx_r)

# OLS (classical) SEs
m1_ols_ct  <- lmtest::coeftest(m1)
m1_ols_tbl <- tidy_coeftest(m1_ols_ct)
m1_ols_tbl$SE_type <- "OLS"

# HAC robust SEs (Newey-West, lag = floor(4*(n/100)^(2/9)) default)
m1_hac     <- sandwich::NeweyWest(m1)
m1_hac_ct  <- lmtest::coeftest(m1, vcov = m1_hac)
m1_hac_tbl <- tidy_coeftest(m1_hac_ct)
m1_hac_tbl$SE_type <- "HAC_NeweyWest"

m1_coef_combined <- rbind(m1_ols_tbl, m1_hac_tbl)
m1_coef_combined$Model <- "M1_OLS_Weekday"
save_table(m1_coef_combined, "reg_m1_ols_weekday_coefficients.csv")

# Model summary statistics
m1_sum <- summary(m1)
m1_fit <- data.frame(
  Model      = "M1",
  Spec       = "ReturnPrice ~ Weekday (Mon=ref)",
  N          = nrow(idx_r),
  R_squared  = m1_sum$r.squared,
  Adj_R2     = m1_sum$adj.r.squared,
  F_stat_OLS = m1_sum$fstatistic[1],
  F_df1      = m1_sum$fstatistic[2],
  F_df2      = m1_sum$fstatistic[3],
  F_p_OLS    = pf(m1_sum$fstatistic[1], m1_sum$fstatistic[2],
                   m1_sum$fstatistic[3], lower.tail = FALSE),
  RMSE       = sqrt(mean(residuals(m1)^2)),
  stringsAsFactors = FALSE
)
save_table(m1_fit, "reg_m1_model_fit.csv")

cat("  M1 HAC-robust coefficient estimates:\n")
for (i in seq_len(nrow(m1_hac_tbl))) {
  r <- m1_hac_tbl[i, ]
  cat(sprintf("    %-25s  est=%+.6f  SE=%.6f  p=%s  %s\n",
              r$Term, r$Estimate, r$Std_Error,
              format(r$P_value, digits = 3, scientific = TRUE),
              ifelse(r$Significant, "*", "")))
}
cat(sprintf("  R² = %.6f  |  Adj. R² = %.6f  |  N = %d\n",
            m1_fit$R_squared, m1_fit$Adj_R2, m1_fit$N))

# ---------------------------------------------------------------------------
# 3. Model M2 — OLS + lagged rolling volatility
# ---------------------------------------------------------------------------
cat("\n[Step 3] Model M2 — OLS + lagged 20-day rolling volatility ...\n")
cat("  Note: RollVol_20 shifted forward 1 row (lag-1) to avoid look-ahead.\n")
cat("  The volatility at t-1 is known before return at t is realised.\n")

idx_r$RollVol20_lag1 <- c(NA_real_, head(idx_r[[COL_ROLLVOL_20]], -1))
m2_data <- idx_r[!is.na(idx_r$RollVol20_lag1), ]
m2 <- lm(as.formula(paste0(COL_RETURN, " ~ ", COL_WEEKDAY,
                             " + RollVol20_lag1")),
          data = m2_data)

m2_hac    <- sandwich::NeweyWest(m2)
m2_hac_ct <- lmtest::coeftest(m2, vcov = m2_hac)
m2_hac_tbl <- tidy_coeftest(m2_hac_ct)
m2_hac_tbl$SE_type <- "HAC_NeweyWest"
m2_hac_tbl$Model   <- "M2_OLS_Weekday_LagVol"
save_table(m2_hac_tbl, "reg_m2_ols_weekday_lagvol_coefficients.csv")

m2_sum <- summary(m2)
m2_fit <- data.frame(
  Model     = "M2",
  Spec      = "ReturnPrice ~ Weekday + RollVol20_lag1",
  N         = nrow(m2_data),
  R_squared = m2_sum$r.squared,
  Adj_R2    = m2_sum$adj.r.squared,
  RMSE      = sqrt(mean(residuals(m2)^2)),
  stringsAsFactors = FALSE
)
save_table(m2_fit, "reg_m2_model_fit.csv")

cat(sprintf("  M2 N=%d  R²=%.6f  Adj.R²=%.6f\n",
            m2_fit$N, m2_fit$R_squared, m2_fit$Adj_R2))

# VIF check (multicollinearity)
if (HAS_CAR) {
  vif_m2 <- car::vif(m2)
  vif_df <- data.frame(
    Term = names(vif_m2), VIF = unname(vif_m2),
    stringsAsFactors = FALSE)
  save_table(vif_df, "reg_m2_vif.csv")
  cat("  VIF (M2):\n")
  for (i in seq_len(nrow(vif_df)))
    cat(sprintf("    %-30s  VIF=%.3f\n", vif_df$Term[i], vif_df$VIF[i]))
} else {
  message("  car package not available; VIF check skipped.")
}

# ---------------------------------------------------------------------------
# 4. Model M3 — Logistic regression: CrashFlag_3pct ~ Weekday
# ---------------------------------------------------------------------------
cat("\n[Step 4] Model M3 — Logistic: CrashFlag_3pct ~ Weekday ...\n")
cat("  Outcome: 1 if ReturnPrice <= -3%, 0 otherwise\n")
cat("  Reference category: Monday\n")

# Ensure outcome is integer 0/1
idx_r$Crash3_bin <- as.integer(idx_r[[COL_CRASH3]] == TRUE)
idx_r$Crash5_bin <- as.integer(idx_r[[COL_CRASH5]] == TRUE)

# Minimum sample check for logistic regression
n_crash3 <- sum(idx_r$Crash3_bin == 1, na.rm = TRUE)
n_crash5 <- sum(idx_r$Crash5_bin == 1, na.rm = TRUE)
MIN_CRASH_FOR_LOGIT <- 10L

run_logit <- function(outcome_bin, outcome_name, model_id, threshold_label,
                       data_r) {
  n_events <- sum(data_r[[outcome_bin]] == 1, na.rm = TRUE)
  n_total  <- nrow(data_r)
  cat(sprintf("  %s: %d crash events / %d observations (%.2f%%)\n",
              threshold_label, n_events, n_total,
              100 * n_events / n_total))

  if (n_events < MIN_CRASH_FOR_LOGIT) {
    warning(sprintf("Too few crash events (%d) for logistic regression in %s",
                    n_events, threshold_label))
    return(NULL)
  }

  frm <- as.formula(paste0(outcome_bin, " ~ ", COL_WEEKDAY))
  gm  <- glm(frm, data = data_r, family = binomial(link = "logit"))

  # HC3 robust SEs for logistic regression
  hc3   <- sandwich::vcovHC(gm, type = "HC3")
  gm_ct <- lmtest::coeftest(gm, vcov = hc3)
  gm_tbl <- tidy_coeftest(gm_ct)
  gm_tbl$OR         <- exp(gm_tbl$Estimate)
  gm_tbl$OR_CI_lo   <- exp(gm_tbl$CI_lower)
  gm_tbl$OR_CI_hi   <- exp(gm_tbl$CI_upper)
  gm_tbl$SE_type    <- "HC3_robust"
  gm_tbl$Model      <- model_id
  save_table(gm_tbl, sprintf("reg_%s_logit_coefficients.csv",
                              tolower(model_id)))

  # Model fit
  gm_sum <- summary(gm)
  null_dev  <- gm_sum$null.deviance
  res_dev   <- gm_sum$deviance
  mcfadden  <- 1 - res_dev / null_dev
  fit_row <- data.frame(
    Model        = model_id,
    Threshold    = threshold_label,
    N_total      = n_total,
    N_crashes    = n_events,
    Pct_crashes  = round(100 * n_events / n_total, 3),
    Null_deviance = round(null_dev, 3),
    Residual_deviance = round(res_dev, 3),
    McFadden_R2  = round(mcfadden, 5),
    AIC          = round(AIC(gm), 3),
    stringsAsFactors = FALSE
  )
  save_table(fit_row, sprintf("reg_%s_model_fit.csv", tolower(model_id)))

  cat(sprintf("  %s: McFadden R²=%.5f  AIC=%.2f\n",
              model_id, mcfadden, AIC(gm)))
  cat("  Odds ratios (HC3 robust SEs):\n")
  for (i in seq_len(nrow(gm_tbl))) {
    r <- gm_tbl[i, ]
    cat(sprintf("    %-25s  OR=%.4f  [%.4f, %.4f]  p=%s  %s\n",
                r$Term, r$OR, r$OR_CI_lo, r$OR_CI_hi,
                format(r$P_value, digits = 3, scientific = TRUE),
                ifelse(r$Significant, "*", "")))
  }

  list(model = gm, coef_table = gm_tbl, fit = fit_row)
}

m3_result <- run_logit("Crash3_bin", COL_CRASH3, "M3", "-3% crash", idx_r)
m4_result <- run_logit("Crash5_bin", COL_CRASH5, "M4", "-5% crash", idx_r)

# ---------------------------------------------------------------------------
# 5. Residual diagnostics — M1 (OLS)
# ---------------------------------------------------------------------------
cat("\n[Step 5] Residual diagnostics for M1 ...\n")

save_diagnostics(m1, "M1_OLS", idx_r)

# Breusch-Pagan heteroskedasticity test
bp_m1 <- lmtest::bptest(m1)
# Durbin-Watson autocorrelation test
dw_m1 <- lmtest::dwtest(m1)

diag_df <- data.frame(
  Model   = "M1_OLS",
  BP_stat = round(bp_m1$statistic, 4),
  BP_df   = bp_m1$parameter,
  BP_p    = signif(bp_m1$p.value, 4),
  BP_heterosk = bp_m1$p.value < ALPHA,
  DW_stat = round(dw_m1$statistic, 4),
  DW_p    = signif(dw_m1$p.value, 4),
  DW_autocorr = dw_m1$p.value < ALPHA,
  stringsAsFactors = FALSE
)
save_table(diag_df, "reg_m1_diagnostics_tests.csv")

cat(sprintf("  Breusch-Pagan: stat=%.4f  p=%s  Heteroskedastic=%s\n",
            bp_m1$statistic, format(bp_m1$p.value, digits = 3),
            bp_m1$p.value < ALPHA))
cat(sprintf("  Durbin-Watson: DW=%.4f  p=%s  Autocorrelated=%s\n",
            dw_m1$statistic, format(dw_m1$p.value, digits = 3),
            dw_m1$p.value < ALPHA))
cat("  HAC standard errors address both issues in coefficient inference.\n")

# ---------------------------------------------------------------------------
# 6. Influential observations — Cook's distance
# ---------------------------------------------------------------------------
cat("\n[Step 6] Influential observations (Cook's distance) ...\n")

cooks  <- cooks.distance(m1)
# Threshold: 4/n (common heuristic)
thresh <- 4 / nrow(idx_r)
infl_idx <- which(cooks > thresh)
infl_df <- data.frame(
  RowIndex    = infl_idx,
  Date        = as.character(idx_r[[COL_DATE]][infl_idx]),
  Return      = idx_r[[COL_RETURN]][infl_idx],
  Weekday     = as.character(idx_r[[COL_WEEKDAY]][infl_idx]),
  CooksD      = cooks[infl_idx],
  CrashFlag3  = idx_r[[COL_CRASH3]][infl_idx],
  stringsAsFactors = FALSE
)
infl_df <- infl_df[order(-infl_df$CooksD), ]
save_table(infl_df, "reg_m1_influential_observations.csv")
cat(sprintf("  %d observations exceed Cook's D threshold (4/n = %.5f)\n",
            nrow(infl_df), thresh))
if (nrow(infl_df) > 0) {
  cat("  Top 5 influential observations:\n")
  top5_infl <- head(infl_df, 5)
  for (i in seq_len(nrow(top5_infl))) {
    r <- top5_infl[i, ]
    cat(sprintf("    %s  %s  ret=%.4f  CookD=%.5f  Crash3=%s\n",
                r$Date, r$Weekday, r$Return, r$CooksD, r$CrashFlag3))
  }
}

# Cook's D plot
save_png(file.path(DIR_FIGURES, "reg_01_cooks_distance.png"), {
  plot(cooks, type = "h", col = ifelse(cooks > thresh, "firebrick", "grey60"),
       xlab = "Observation index", ylab = "Cook's distance",
       main = "Cook's Distance — M1 OLS (ReturnPrice ~ Weekday)")
  abline(h = thresh, lty = 2, col = "firebrick")
  legend("topright", legend = sprintf("Threshold = %.5f (4/n)", thresh),
         lty = 2, col = "firebrick", bty = "n")
})

# ---------------------------------------------------------------------------
# 7. Coefficient plot — M1 HAC estimates
# ---------------------------------------------------------------------------
cat("\n[Step 7] Coefficient plot ...\n")

m1_plot_df <- m1_hac_tbl[m1_hac_tbl$Term != "(Intercept)", ]
m1_plot_df$Label <- gsub(paste0("^", COL_WEEKDAY), "", m1_plot_df$Term)
m1_plot_df$Label <- ifelse(nchar(m1_plot_df$Label) == 0,
                            m1_plot_df$Term, m1_plot_df$Label)

p_coef <- ggplot2::ggplot(m1_plot_df,
    ggplot2::aes(x = Estimate, y = reorder(Label, Estimate),
                 colour = Significant)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                       colour = "grey40") +
  ggplot2::geom_point(size = 4) +
  ggplot2::geom_errorbarh(
    ggplot2::aes(xmin = CI_lower, xmax = CI_upper), height = 0.25) +
  ggplot2::scale_colour_manual(
    values = c("TRUE" = "firebrick", "FALSE" = "steelblue"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
    name   = "Significance") +
  ggplot2::labs(
    title    = "Weekday Return Differentials vs Monday (HAC-Robust SEs)",
    subtitle = "M1: ReturnPrice ~ Weekday | Monday = reference category",
    x = "Estimated return differential (relative to Monday)",
    y = "Weekday"
  ) +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                    hjust = 0.5),
                 legend.position = "bottom")
save_gg(file.path(DIR_FIGURES, "reg_02_weekday_coefficients.png"), p_coef,
        width = 8, height = 5)

# ---------------------------------------------------------------------------
# 8. Odds ratio plot — M3 (logistic -3%)
# ---------------------------------------------------------------------------
if (!is.null(m3_result)) {
  cat("\n[Step 8] Odds ratio plot for M3 (-3% crash logistic) ...\n")
  or_df <- m3_result$coef_table[m3_result$coef_table$Term != "(Intercept)", ]
  or_df$Label <- gsub(paste0("^", COL_WEEKDAY), "", or_df$Term)

  p_or <- ggplot2::ggplot(or_df,
      ggplot2::aes(x = OR, y = reorder(Label, OR),
                   colour = Significant)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                         colour = "grey40") +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = OR_CI_lo, xmax = OR_CI_hi), height = 0.25) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "firebrick", "FALSE" = "steelblue"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
      name   = "Significance") +
    ggplot2::labs(
      title    = "Odds Ratios for Crash Probability by Weekday vs Monday",
      subtitle = "M3: CrashFlag_3pct ~ Weekday (HC3 robust SEs)",
      x = "Odds Ratio (relative to Monday)",
      y = "Weekday"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5),
                   legend.position = "bottom")
  save_gg(file.path(DIR_FIGURES, "reg_03_crash3_odds_ratios.png"), p_or,
          width = 8, height = 5)
}

# ---------------------------------------------------------------------------
# 9. Model comparison summary
# ---------------------------------------------------------------------------
cat("\n[Step 9] Model comparison table ...\n")

# rbind_fill: base-R equivalent of dplyr::bind_rows — fills missing cols with NA
rbind_fill <- function(...) {
  dfs  <- Filter(Negate(is.null), list(...))
  if (length(dfs) == 0) return(data.frame())
  cols <- unique(unlist(lapply(dfs, colnames)))
  do.call(rbind, lapply(dfs, function(df) {
    missing <- setdiff(cols, colnames(df))
    if (length(missing) > 0)
      df[missing] <- NA
    df[, cols, drop = FALSE]
  }))
}

model_comparison <- rbind_fill(
  m1_fit,
  m2_fit,
  if (!is.null(m3_result)) m3_result$fit else NULL,
  if (!is.null(m4_result)) m4_result$fit else NULL
)
save_table(model_comparison, "reg_model_comparison.csv")

cat("\n  MODEL COMPARISON:\n")
for (i in seq_len(nrow(model_comparison))) {
  r <- model_comparison[i, ]
  n_val  <- if (!is.na(r$N))       as.character(r$N) else
            if (!is.na(r$N_total)) as.character(r$N_total) else "N/A"
  r2_val <- if (!is.na(r$R_squared))  sprintf("%.5f", r$R_squared) else
            if (!is.na(r$McFadden_R2)) sprintf("%.5f (McF)", r$McFadden_R2) else
            "N/A"
  spec_val <- if (!is.na(r$Spec)) r$Spec else
              if (!is.na(r$Threshold)) paste0("Crash ~ Weekday (", r$Threshold, ")") else ""
  cat(sprintf("    %-6s  N=%-6s  R²/McF=%-14s  %s\n",
              r$Model, n_val, r2_val, spec_val))
}

cat(sprintf("\n[Regression Analysis Complete]\n"))
cat(sprintf("  Tables saved to  : %s\n", DIR_TABLES))
cat(sprintf("  Figures saved to : %s\n", DIR_FIGURES))
cat(sprintf("  Finished: %s\n\n", Sys.time()))

# Expose key objects for downstream scripts (robustness checks)
assign("m1_model",   m1,    envir = .GlobalEnv)
assign("m1_hac_vcov", m1_hac, envir = .GlobalEnv)
assign("idx_r_reg",  idx_r,  envir = .GlobalEnv)
