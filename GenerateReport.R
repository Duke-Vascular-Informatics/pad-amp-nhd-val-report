################################################################################
# GenerateReport.R — render the pad-amp-nhd-prog manuscript report
#
# Renders from result artifacts only. No database connection, no VPN, no
# credentials — this script must remain runnable on a laptop with nothing but
# a clone of this repo and a results directory copied over (e.g. a Duke PRCC
# export, or a local Strategus run of pad-amp-nhd-prog).
#
# WHERE THE DATA COMES FROM — resolved in this order, first match wins:
#
#   1. RESULTS_DIR, if set. An explicit path always wins; nothing below is
#      consulted. Use this for one-off renders.
#
#   2. A .zip in prcc_data/ (gitignored). Drop a Duke PRCC export archive
#      there and it is extracted to prcc_data/.extracted/ and rendered from.
#      This is the normal way to render real Duke results: copy the approved
#      archive in, run this script, done. The newest .zip wins if several are
#      present, and it is re-extracted whenever the .zip is newer than the
#      previous extraction.
#
#   3. Already-extracted content in prcc_data/ (i.e. you unzipped by hand).
#
#   4. ../pad-amp-nhd-prog/output — the synthetic dev-container run. This is
#      the fallback, not the default-in-spirit: it exists so the repo stays
#      runnable in the workspace with no export present.
#
# The chosen source, and whether it is real or synthetic data, is announced
# loudly on startup. That banner is the point of the ordering above: the
# failure mode worth engineering against is rendering synthetic numbers and
# believing they are Duke's.
#
# INPUT LAYOUT — whichever directory is chosen must contain:
#   iannuzzi/  mfi5/  vqifs/     (metrics, calibration tables, subgroup bias)
#   report_inputs/               (agg_*.csv + _report_config.yaml)
# This is exactly the shape of pad-amp-nhd-prog's own output/ folder, and of a
# duke-prcc-deploy export archive once unzipped.
#
# USAGE
#   Rscript GenerateReport.R                       # auto-resolve (see above)
#   RESULTS_DIR=/path/to/results Rscript GenerateReport.R
#   REPORT_OUTPUT_DIR=/where/to/write Rscript GenerateReport.R
################################################################################

if (file.exists("renv/activate.R")) source("renv/activate.R")

PRCC_DIR      <- "prcc_data"
EXTRACT_DIR   <- file.path(PRCC_DIR, ".extracted")
SYNTHETIC_DIR <- file.path("..", "pad-amp-nhd-prog", "output")

# A results directory is only usable if it has the two things every table and
# figure needs. Checked before selecting a source rather than after, so a
# half-copied or wrongly-nested export is reported as such instead of failing
# later inside the report with a confusing message about one missing CSV.
.looks_like_results <- function(d) {
  !is.null(d) && dir.exists(d) &&
    dir.exists(file.path(d, "report_inputs")) &&
    dir.exists(file.path(d, "iannuzzi"))
}

# Some zip archives contain a single top-level folder rather than the results
# layout at their root. Descend through single-directory wrappers so both
# shapes work without the user having to know which they have.
.descend_to_results <- function(d) {
  for (i in 1:3) {
    if (.looks_like_results(d)) return(d)
    subs <- list.dirs(d, full.names = TRUE, recursive = FALSE)
    subs <- subs[!grepl("(^|/)__MACOSX$", subs)]
    if (length(subs) != 1) break
    d <- subs[[1]]
  }
  if (.looks_like_results(d)) d else NULL
}

.resolve_source <- function() {
  # ---- 1. Explicit override -------------------------------------------------
  env_dir <- Sys.getenv("RESULTS_DIR", unset = NA_character_)
  if (!is.na(env_dir) && nzchar(trimws(env_dir))) {
    if (!dir.exists(env_dir)) {
      stop("RESULTS_DIR was set but does not exist: ", env_dir)
    }
    return(list(dir = env_dir, label = "RESULTS_DIR (explicit)", synthetic = FALSE))
  }

  # ---- 2. A .zip dropped into prcc_data/ ------------------------------------
  zips <- list.files(PRCC_DIR, pattern = "\\.zip$", full.names = TRUE)
  if (length(zips) > 0) {
    zips  <- zips[order(file.mtime(zips), decreasing = TRUE)]
    zipf  <- zips[[1]]
    if (length(zips) > 1) {
      message("[report] ", length(zips), " archives in ", PRCC_DIR,
              "/ — using the newest: ", basename(zipf))
    }
    stamp_file <- file.path(EXTRACT_DIR, ".source")
    prior      <- if (file.exists(stamp_file)) readLines(stamp_file, warn = FALSE)[1] else ""
    want       <- paste0(basename(zipf), "|", as.numeric(file.mtime(zipf)))
    if (!identical(prior, want) || !dir.exists(EXTRACT_DIR)) {
      message("[report] Extracting ", basename(zipf), " -> ", EXTRACT_DIR, " ...")
      unlink(EXTRACT_DIR, recursive = TRUE)
      dir.create(EXTRACT_DIR, recursive = TRUE, showWarnings = FALSE)
      # utils::unzip is base R -- no dependency on the `zip` package, which
      # this repo does not otherwise need.
      utils::unzip(zipf, exdir = EXTRACT_DIR)
      writeLines(want, stamp_file)
    } else {
      message("[report] Reusing existing extraction of ", basename(zipf),
              " (archive unchanged).")
    }
    found <- .descend_to_results(EXTRACT_DIR)
    if (is.null(found)) {
      stop(
        "Extracted ", basename(zipf), " but it does not contain a results layout.\n",
        "Expected report_inputs/ and iannuzzi/ (directly, or inside a single\n",
        "top-level folder). Found at the archive root:\n  ",
        paste(list.files(EXTRACT_DIR), collapse = "\n  "), "\n\n",
        "Is this the export_results_for_review.R archive from duke-prcc-deploy?"
      )
    }
    return(list(dir = found, label = paste0("PRCC archive ", basename(zipf)),
                synthetic = FALSE))
  }

  # ---- 3. Hand-unzipped content in prcc_data/ -------------------------------
  if (dir.exists(PRCC_DIR)) {
    found <- .descend_to_results(PRCC_DIR)
    if (!is.null(found)) {
      return(list(dir = found, label = paste0("unpacked contents of ", PRCC_DIR),
                  synthetic = FALSE))
    }
  }

  # ---- 4. Synthetic fallback ------------------------------------------------
  if (.looks_like_results(SYNTHETIC_DIR)) {
    return(list(dir = SYNTHETIC_DIR, label = "synthetic dev-container run",
                synthetic = TRUE))
  }

  stop(
    "No results found.\n\n",
    "Do one of:\n",
    "  - copy a Duke PRCC export .zip into ", PRCC_DIR, "/ (created for you), or\n",
    "  - set RESULTS_DIR to a directory containing report_inputs/ and iannuzzi/, or\n",
    "  - run pad-amp-nhd-prog's StrategusCodeToRun.R so ", SYNTHETIC_DIR,
    " exists.\n"
  )
}

if (!dir.exists(PRCC_DIR)) dir.create(PRCC_DIR, showWarnings = FALSE)

src         <- .resolve_source()
results_dir <- src$dir
output_dir  <- Sys.getenv("REPORT_OUTPUT_DIR", unset = results_dir)

# Announce the data source unmissably. Rendering synthetic numbers while
# believing they are Duke's is the expensive mistake here, and it is silent
# unless something says so out loud.
bar <- strrep("=", 78)
message("\n", bar)
if (isTRUE(src$synthetic)) {
  message("  DATA SOURCE: SYNTHETIC (development data — NOT Duke PRCC results)")
  message("  ", src$label)
  message("  ", results_dir)
  message("")
  message("  To render real Duke results, copy the approved PRCC export .zip")
  message("  into ", PRCC_DIR, "/ and re-run this script.")
} else {
  message("  DATA SOURCE: ", src$label)
  message("  ", normalizePath(results_dir, mustWork = FALSE))
}
message(bar, "\n")

report_inputs_dir <- file.path(results_dir, "report_inputs")

source("config.R")
config <- get_report_config(report_inputs_dir)

# The database this run actually describes, straight from the artifact the
# extract step wrote. A second, independent check that the numbers about to be
# rendered come from where you think: a Duke render says so here.
if (!is.null(config$cdm_database_name)) {
  message("[report] CDM described by these artifacts: ", config$cdm_database_name)
}

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
if (isTRUE(src$synthetic)) {
  message("\nNOTE: this report was built from SYNTHETIC data. Do not circulate ",
          "it as a Duke result.")
}
