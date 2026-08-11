# =============================================================================
# R/report_helpers.R
#
# Study-specific helper for the manuscript report: the cohort summary table.
#
# MOVED TO omopReportToolkit (2026-08-10, docs/MIGRATION_PLAN_REPO_SPLIT.md
# Phase 1). Everything generic used to live in this file — bibliography
# formatting, ECE, ROC/calibration figures, .build_table1() — and has been
# extracted to the shared package, verified byte-identical there, with one
# real bug fixed in the process (.compute_ece() — see that package's commit
# history). This file now holds only what genuinely differs per study:
#
#   .build_cohort_summary_table()       — cohort-level summary stats flextable
#
# Usage:
#   This file is sourced automatically by R/report_extended.R (the dispatcher),
#   which also calls library(omopReportToolkit) for the generic functions
#   above. Template files (report_prognostic.R, etc.) DO NOT source it
#   directly — rely on the dispatcher to have sourced it first.
#
# Dependencies: officer, flextable, ggplot2, pROC — library()'d here (NOT
#   inside omopReportToolkit itself, which is a package and must not call
#   library()) so that report_prognostic.R's own bare, unqualified calls to
#   these packages resolve. Sourced scripts share one global search path;
#   packages do not get one for free.
#
# NOT sourced here: R/cohort_demographics.R. Its comment used to claim
# fetch_demographics_from_omop() lived there and was needed by every template
# with a Table 1 — false on inspection (that function was never in that file,
# and neither report_prognostic.R, report_extended.R, nor this file calls
# anything cohort_demographics.R defines). That file's real functions
# (fetch_subgroup_labels(), fetch_proc_type_labels()) are DB-touching and
# belong with the scoring step — pad-amp-nhd-prog's R/risk_score_pipeline.R
# already sources it independently, with its own graceful skip-if-missing
# guard. This repo must never source a file that queries a database; removed
# rather than carried over as dead weight when this file was extracted.
# =============================================================================

library(officer)
library(flextable)
library(ggplot2)
library(pROC)

# Greyscale figure styling + generic report helpers: .gs_scales(),
# theme_manuscript(), save_figure(), .calibration_axis_limits(),
# .calibration_reference_line(), .append_references_section(), .compute_ece(),
# .save_roc_plot(), .save_dual_roc_plot(), .save_calibration_plot_from_table(),
# .save_calibration_plot_from_vectors(), .save_dual_calibration_plot(),
# .build_table1(), .strip_heading_autonumbering(). Pinned to a commit in
# renv.lock, not a branch — see that package's README before bumping it.
library(omopReportToolkit)

# -----------------------------------------------------------------------------
# .build_cohort_summary_table()
#
# Builds a flextable summary of overall cohort statistics from the person-level
# scores data frame.  Presented as Table 1 (overall cohort summary) in the Word
# report.
#
# Statistics included:
#   - Total procedures (rows in person_level)
#   - Unique patients
#   - outcome events and incidence rate
#   - Mean total risk score (SD) and median total score (IQR)
# -----------------------------------------------------------------------------
.build_cohort_summary_table <- function(person_level_df) {
  # Build cohort characteristics summary table from person_level scores dataframe
  # Assumes columns: subject_id, outcome, total_score, age (if available)

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  n_procedures <- nrow(person_level_df)
  n_patients <- length(unique(person_level_df$subject_id))
  n_ssi <- sum(person_level_df$outcome, na.rm = TRUE)
  ssi_rate <- 100 * n_ssi / n_procedures

  # Create summary statistics
  summary_data <- data.frame(
    Characteristic = c(
      "Total number of procedures",
      "Number of unique patients",
      "Number of outcome events",
      "Outcome incidence rate (%)",
      "Mean total score (SD)",
      "Median total score (IQR)"
    ),
    Value = c(
      n_procedures,
      n_patients,
      n_ssi,
      paste0(round(ssi_rate, 1), "%"),
      paste0(
        round(mean(person_level_df$total_score, na.rm = TRUE), 2), " (",
        round(sd(person_level_df$total_score, na.rm = TRUE), 2), ")"
      ),
      paste0(
        round(median(person_level_df$total_score, na.rm = TRUE), 2), " (",
        round(quantile(person_level_df$total_score, 0.25, na.rm = TRUE), 2), " – ",
        round(quantile(person_level_df$total_score, 0.75, na.rm = TRUE), 2), ")"
      )
    ),
    stringsAsFactors = FALSE
  )

  ft <- flextable(summary_data) |>
    set_header_labels(
      Characteristic = "Characteristic",
      Value = "Value"
    ) |>
    bold(part = "header") |>
    fontsize(size = 10, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Characteristic", width = 3.0) |>
    width(j = "Value", width = 2.0) |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    padding(padding = 4, part = "all")

  ft
}


# =============================================================================
# .save_roc_plot_from_points()
#
# Draws one or more ROC curves from PRE-COMPUTED (fpr, tpr) points instead of
# from patient-level outcome/prediction vectors.
#
# WHY THIS EXISTS (2026-08-11). omopReportToolkit's .save_roc_plot() and
# .save_dual_roc_plot() both take y/p vectors -- i.e. one row per patient --
# which is exactly the dependency this repo was converting away from. The
# curve points themselves are aggregate: pad-amp-nhd-prog's aggregate step
# derives them from per-score-value event counts and emits agg_roc_points.csv,
# verified to reproduce the person-level curve exactly (identical point set,
# AUC matching metrics.csv to 10 decimal places).
#
# CANDIDATE FOR PROMOTION to omopReportToolkit: nothing here is study-specific.
# It lives in this repo for now only to avoid a toolkit version bump in the
# middle of the conversion; move it when the toolkit is next revised, and
# delete this copy rather than leaving both.
#
# @param pts     data frame with fpr, tpr, curve_label, and optionally auc.
# @param output_folder  directory for the PNG.
# @param file_name      output file name.
# @param title          plot title.
# @return the written file path, or NULL.
# =============================================================================
.save_roc_plot_from_points <- function(pts, output_folder,
                                       file_name = "roc_curve.png",
                                       title = "Receiver Operating Characteristic") {
  if (is.null(pts) || nrow(pts) == 0 ||
      !all(c("fpr", "tpr", "curve_label") %in% names(pts))) {
    message("[report] ROC plot skipped: agg_roc_points.csv missing or malformed.")
    return(NULL)
  }

  pts$fpr <- as.numeric(pts$fpr)
  pts$tpr <- as.numeric(pts$tpr)
  pts <- pts[!is.na(pts$fpr) & !is.na(pts$tpr), , drop = FALSE]
  if (nrow(pts) == 0) return(NULL)

  # Legend label carries each curve's AUC, taken from the artifact rather than
  # recomputed here -- the aggregate step already reconciled it against
  # metrics.csv, so recomputing would only create a way for the two to drift.
  labs_map <- vapply(split(pts, pts$curve_label), function(d) {
    a <- if ("auc" %in% names(d)) suppressWarnings(as.numeric(d$auc[1])) else NA_real_
    if (is.na(a)) d$curve_label[1] else sprintf("%s (AUC %.3f)", d$curve_label[1], a)
  }, character(1))
  pts$Curve <- factor(unname(labs_map[pts$curve_label]),
                      levels = unname(labs_map[sort(names(labs_map))]))

  # Sort within curve so geom_line/geom_step connects points in curve order
  # rather than row order.
  pts <- pts[order(pts$Curve, pts$fpr, pts$tpr), , drop = FALSE]

  gs <- .gs_scales(levels(pts$Curve))

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = fpr, y = tpr,
                                         colour = Curve, linetype = Curve)) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         colour = "grey70", linetype = "dotted", linewidth = 0.6) +
    ggplot2::geom_line(linewidth = 0.9) +
    gs$colour + gs$linetype +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +
    ggplot2::scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
    ggplot2::labs(
      title = title,
      x = "1 - Specificity (false positive rate)",
      y = "Sensitivity (true positive rate)",
      colour = NULL, linetype = NULL
    ) +
    theme_manuscript() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())

  tryCatch(
    save_figure(p, output_folder, file_name, width = 5.5, height = 5.5),
    error = function(e) {
      message("[report] Could not save ROC plot: ", conditionMessage(e))
      NULL
    }
  )
}
