# =============================================================================
# config.R
#
# This repo has no study_params.yaml of its own, and that is deliberate, not
# an oversight. The ~9 config$ fields the report code actually reads (verified
# by grepping every config$ access across R/report_prognostic.R,
# R/report_extended.R, and R/report_helpers.R — not assumed) are written by
# pad-amp-nhd-val's R/extract_report_inputs.R into
# <report_inputs_dir>/_report_config.yaml as part of the same extract step
# that writes every other CSV this repo reads.
#
# Duplicating those fields into a second study_params.yaml here would create
# exactly the drift risk this whole repo split is designed to avoid: someone
# changes prediction_window_days in the analysis repo and the report's methods
# text silently keeps describing the old value. There is exactly one place
# these values can come from — the analysis run that actually produced the
# results this report describes.
#
# Nothing in _report_config.yaml implies database access: no schema names,
# no cohort ids, no credentials. See pad-amp-nhd-val's
# R/extract_report_inputs.R for the field list and the reasoning.
# =============================================================================

#' Read the report-relevant config fields from an extract's output directory.
#'
#' @param report_inputs_dir Path to the `report_inputs/` directory produced by
#'   pad-amp-nhd-val's extract_report_inputs(). Typically
#'   `<results_dir>/report_inputs`.
#' @return A named list matching the shape .report_prognostic() expects.
get_report_config <- function(report_inputs_dir) {
  path <- file.path(report_inputs_dir, "_report_config.yaml")
  if (!file.exists(path)) {
    stop(
      "_report_config.yaml not found in ", report_inputs_dir, ".\n",
      "This file is written by pad-amp-nhd-val's extract_report_inputs() — ",
      "run that (or the full StrategusCodeToRun.R pipeline) against the CDM ",
      "before rendering, or point report_inputs_dir at a results export that ",
      "already has it."
    )
  }
  yaml::read_yaml(path)
}
