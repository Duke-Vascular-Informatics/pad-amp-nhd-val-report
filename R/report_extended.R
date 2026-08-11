# =============================================================================
# R/report_extended.R
#
# Dispatcher for study-design-specific Word report templates.
# Routes generate_manuscript_report() to the correct template based on
# config$study_design.  Shared helper functions live in R/report_helpers.R.
#
# To add a new template:
#   1. Create R/report_<design>.R defining .<design>_report() (see existing stubs).
#   2. Uncomment its source() line below.
#   3. Add the else-if branch in generate_manuscript_report() below.
#
# Entry points (called by StrategusCodeToRun.R):
#   generate_manuscript_report(output_dir, score_output_dir, mfi5_output_dir,
#                              report_inputs_dir, config, citations)
#   generate_word_report(output_dir, score_output_dir)  — backwards-compatible simple report
# =============================================================================

# Null-coalescing operator — defined first so it is available in all sourced templates.
if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

source("R/report_helpers.R")
source("R/report_prognostic.R")
# source("R/report_descriptive.R")   # TODO [TEMPLATE]: uncomment when implemented
# source("R/report_causal.R")        # TODO [TEMPLATE]: uncomment when implemented

# -----------------------------------------------------------------------------
# generate_manuscript_report()
#
# Public entry point for full manuscript-format Word report generation.
# Routes to the appropriate template based on config$study_design.
#
# Parameters:
#   output_dir           — directory for the output .docx file
#   score_output_dir     — directory containing pipeline output CSVs (default: output_dir)
#   mfi5_output_dir      — directory containing mFI-5 pipeline outputs
#   vqifs_output_dir     — directory containing sVQI-FS pipeline outputs
#   report_inputs_dir    — directory of CSV artifacts written by
#                          extract_report_inputs() (R/extract_report_inputs.R).
#                          Phase 0 replaced connection_details with this for the
#                          prognostic path: the report reads files, never a
#                          database. NULL means "no artifacts" and degrades each
#                          affected table to its documented "N/A" state.
#   connection_details   — STILL REQUIRED by the descriptive and causal_inference
#                          templates, which have not been split yet. Do not
#                          thread it into the prognostic branch to make the
#                          signatures uniform; that would reintroduce exactly the
#                          coupling Phase 0 removed. Those two templates are
#                          Phase 4 work — see docs/MIGRATION_PLAN_REPO_SPLIT.md.
#   config               — named list from get_validation_config(); drives all routing
#   citations            — optional character vector of citation strings
# -----------------------------------------------------------------------------
generate_manuscript_report <- function(output_dir         = "output",
                                       score_output_dir   = output_dir,
                                       mfi5_output_dir    = NULL,
                                       vqifs_output_dir   = NULL,
                                       report_inputs_dir  = NULL,
                                       connection_details = NULL,
                                       config             = NULL,
                                       citations          = NULL) {

  design <- config$study_design %||% "prognostic_model"

  if (design == "prognostic_model") {
    # NOTE: connection_details is deliberately NOT passed. .report_prognostic()
    # does not accept it any more.
    .report_prognostic(
      output_dir         = output_dir,
      score_output_dir   = score_output_dir,
      mfi5_output_dir    = mfi5_output_dir,
      vqifs_output_dir   = vqifs_output_dir,
      report_inputs_dir  = report_inputs_dir,
      config             = config,
      citations          = citations
    )

  } else if (design %in% c("descriptive", "cohort_characterization")) {
    # Source lazily so the stub error fires only when this path is actually called.
    source("R/report_descriptive.R", local = TRUE)
    .report_descriptive(
      output_dir         = output_dir,
      connection_details = connection_details,
      config             = config
    )

  } else if (design == "causal_inference") {
    source("R/report_causal.R", local = TRUE)
    .report_causal(
      output_dir         = output_dir,
      connection_details = connection_details,
      config             = config
    )

  } else {
    stop(
      "Unknown study_design: '", design, "'.\n",
      "  Must be one of: prognostic_model, descriptive, cohort_characterization, ",
      "causal_inference\n",
      "  Check the study_design field in study_params.yaml."
    )
  }
}

# -----------------------------------------------------------------------------
# generate_word_report()
#
# Backwards-compatible simple report (no live CDM queries).
# Reads pipeline output CSVs from score_output_dir and assembles a Word document.
# Delegates to .report_word_simple() defined in R/report_prognostic.R.
# -----------------------------------------------------------------------------
generate_word_report <- function(output_dir, score_output_dir = output_dir,
                                 mfi5_output_dir = NULL, vqifs_output_dir = NULL) {
  .report_word_simple(output_dir = output_dir, score_output_dir = score_output_dir,
                      mfi5_output_dir = mfi5_output_dir, vqifs_output_dir = vqifs_output_dir)
}
