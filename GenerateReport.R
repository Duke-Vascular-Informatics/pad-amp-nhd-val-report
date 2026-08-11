################################################################################
# GenerateReport.R — render the pad-amp-nhd-prog manuscript report
#
# Renders from result artifacts only. No database connection, no VPN, no
# credentials — this script must remain runnable on a laptop with nothing but
# a clone of this repo and a results/ directory copied over (e.g. from a PRCC
# export or a local Strategus run of pad-amp-nhd-prog).
#
# INPUT LAYOUT — a results directory (RESULTS_DIR below) containing:
#   iannuzzi/person_level_scores.csv, calibration_table_*.csv, ...
#   mfi5/person_level_scores.csv, ...
#   vqifs/person_level_scores.csv, ...
#   report_inputs/*.csv, report_inputs/_report_config.yaml
#     (all written by pad-amp-nhd-prog's R/extract_report_inputs.R)
#
# This is exactly the shape of pad-amp-nhd-prog's own `output/` folder — point
# RESULTS_DIR at that directory directly for a local dev-container run, or at
# a copied-out results export for anywhere else.
#
# USAGE
#   RESULTS_DIR=/path/to/results Rscript GenerateReport.R
#   RESULTS_DIR defaults to ../pad-amp-nhd-prog/output, i.e. this repo cloned
#   as a sibling of pad-amp-nhd-prog in the same workspace.
################################################################################

if (file.exists("renv/activate.R")) source("renv/activate.R")

results_dir <- Sys.getenv("RESULTS_DIR", unset = file.path("..", "pad-amp-nhd-prog", "output"))
output_dir  <- Sys.getenv("REPORT_OUTPUT_DIR", unset = results_dir)

if (!dir.exists(results_dir)) {
  stop(
    "RESULTS_DIR not found: ", results_dir, "\n",
    "Set RESULTS_DIR to a directory containing iannuzzi/, mfi5/, vqifs/, and ",
    "report_inputs/ subfolders (pad-amp-nhd-prog's own output/ folder has ",
    "this shape)."
  )
}

report_inputs_dir <- file.path(results_dir, "report_inputs")

source("config.R")
config <- get_report_config(report_inputs_dir)

source("R/report_extended.R")

message("Generating Word report from ", results_dir, " ...")
report_path <- generate_manuscript_report(
  output_dir         = output_dir,
  score_output_dir   = file.path(results_dir, "iannuzzi"),
  mfi5_output_dir    = file.path(results_dir, "mfi5"),
  vqifs_output_dir   = file.path(results_dir, "vqifs"),
  report_inputs_dir  = report_inputs_dir,
  config             = config
)
message("Done: ", report_path)
