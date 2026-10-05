# =============================================================================
# R/report_prognostic.R
#
# Parameterized Word report template for prognostic model validation studies.
# Handles both integer risk scores and LASSO models within a single template,
# routing on config$score_type.
#
# This file is sourced by R/report_extended.R (the dispatcher).
# Do NOT source R/report_helpers.R here — the dispatcher handles that.
#
# Entry points:
#   .report_prognostic(output_dir, score_output_dir, mfi5_output_dir,
#                      connection_details, config, citations)
#     — full manuscript-format Word report; called by generate_manuscript_report()
#       in R/report_extended.R.
#   .report_word_simple(output_dir, score_output_dir)
#     — lightweight report (no live CDM queries); called by generate_word_report()
#       in R/report_extended.R.
#
# Parameterization branches (inside .report_prognostic()):
#   Branch 1 — Table 2 predictor definitions:
#     lasso  → .covariate_table_data_lasso(config)
#     integer → .covariate_table_data()
#   Branch 2 — Table 3 covariate activation table:
#     lasso  → .build_combined_covariate_table(covariate_summary)
#     integer → .build_combined_component_table(covariate_summary)
#   Branch 3 — Annual / monthly outcome rate plots:
#     lasso  → (removed 2026-09-06 — see the stop() in that branch)
#     integer → .save_nhd_rate_by_year_plot() / .save_nhd_rate_by_month_plot()
#   Branch 4 — Methods §2.2 narrative text:
#     lasso  → LASSO-specific paragraph
#     integer → integer-score-specific paragraph
#
# Dependencies:
#   R/report_helpers.R  — sourced by dispatcher before this file
#   dplyr               — used by .covariate_table_data_lasso()
#   readr               — used by .report_prognostic() for subgroup_bias.csv
# =============================================================================

# ---------------------------------------------------------------------------
# Integer score functions (lineage: the SSI validation study this was ported from)
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .covariate_table_data()
#
# Returns a static data frame defining the PAD risk score covariates.
# Used as Table 2 in the Word report for integer score_type.
# Each row contains covariate_id, variable, points, lookback, omop_domain, derivation.
# This table is static (not queried from the CDM) — if covariates.csv is updated,
# this function must be kept in sync manually.
# -----------------------------------------------------------------------------
.covariate_table_data <- function() {
  data.frame(
    covariate_id = c(
      "female",
      "overweight",
      "obese",
      "urgnt",
      "abi_35",
      "prrevasc_any",
      "prolong_abx",
      "optime4h",
      "mFI_high",
      "indicationClaudication"
    ),
    variable = c(
      "Female sex",
      "Overweight (BMI 25 to <30)",
      "Obese (BMI ≥30)",
      "Urgent / emergency case",
      "Low ankle-brachial index (ABI ≤0.35)",
      "Prior revascularization (any)",
      "Prolonged antibiotic exposure",
      "Operative time ≥4 hours",
      "High modified Frailty Index (mFI)",
      "Indication: claudication"
    ),
    points = c(
      "+1", "+1", "+3", "+1", "+1",
      "+1", "+2", "+1", "+1", "−1"
    ),
    lookback = c(
      "Any time",
      "365 days",
      "365 days",
      "30 days",
      "365 days",
      "10 years",
      "90 days",
      "Index date",
      "365 days",
      "365 days"
    ),
    omop_domain = c(
      "Person",
      "Measurement",
      "Measurement",
      "Observation / Visit",
      "Measurement",
      "Procedure",
      "Drug Exposure",
      "Procedure",
      "Condition (composite)",
      "Condition"
    ),
    derivation = c(
      paste0(
        "Concept 8532 (Female) matched to person.gender_concept_id. ",
        "No lookback required; demographic attribute."
      ),
      paste0(
        "BMI resolved with three-tier priority: ",
        "(1) direct BMI measurement (LOINC 3038553, 36304833); ",
        "(2) computed from weight (LOINC 3025315, 3013762, 3011054, 3026600) ",
        "and height (LOINC 3036277, 3023540, 3015514) as weight_kg / height_m\u00b2. ",
        "Flagged when 25 \u2264 BMI < 30."
      ),
      paste0(
        "Same BMI resolution as Overweight (direct preferred, weight/height fallback). ",
        "Flagged when BMI \u2265 30. Mutually exclusive with Overweight."
      ),
      paste0(
        "Concepts 4158569 (Emergency procedure) and 4250892 (Urgent procedure), ",
        "plus all descendants via concept_ancestor, in procedure_occurrence or ",
        "observation within 30 days before or on the index date."
      ),
      paste0(
        "Concepts 40489833 and 46237026 (ABI measurement), plus descendants, in ",
        "the measurement table. Record is counted when value_as_number < 0.35."
      ),
      paste0(
        "Concepts 4236706 (Arterial bypass of lower limb artery) and 4225375 ",
        "(Endarterectomy of lower limb artery) and all descendants in procedure_occurrence. ",
        "Captures any prior lower-extremity arterial bypass or endarterectomy within a 10-year lookback."
      ),
      paste0(
        "Concept 21603553 (systemic antibiotic agent) and descendants in ",
        "drug_exposure. Counted when drug_exposure_start_date ≤ index − 1 day ",
        "and total exposure duration > 2 days (non-prophylactic heuristic)."
      ),
      paste0(
        "Operative duration derived from procedure_occurrence: ",
        "DATEDIFF(MINUTE, procedure_start_datetime, procedure_end_datetime) > 240. ",
        "Supplemented by measurement-table operative-time concepts when available."
      ),
      paste0(
        "Composite index of 5 sub-components: diabetes (201820), COPD (255573), ",
        "congestive heart failure (316139), hypertension (316866), and functional ",
        "status impairment (4215267), each with descendants in condition_occurrence. ",
        "Flagged when ≥2 conditions are present (mFI score > 0.25)."
      ),
      paste0(
        "Concept 442774 (Intermittent claudication) and descendants in ",
        "condition_occurrence. Negative point value — claudication as the ",
        "operative indication is a protective factor for post-operative non-home discharge (NHD)."
      )
    ),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# .build_combined_component_table()
#
# Merges the static covariate definitions with observed prevalence counts from
# the pipeline's covariate_summary.csv to produce a combined flextable for
# Table 3 in the Word report (integer score_type).
# Renamed from .build_combined_covariate_table() in the original monolith.
# -----------------------------------------------------------------------------
.build_combined_component_table <- function(covariate_summary_df) {
  # Build combined covariate table with definitions and prevalence
  # Input: covariate_summary dataframe with columns: covariate_name, n_positive, n_total

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Map covariates to their definitions and derivation methods.
  # Components: Iannuzzi JC et al. J Vasc Surg 2020;71:889-95 (NHD integer score).
  # Point values and component names match covariates/covariates.csv exactly.
  covariate_defs <- list(
    "Age 60-69 years at index" = list(
      points = "+2", definition = "Age 60–69 years at the index amputation date",
      derivation = "Computed from person.year_of_birth; age 60–69 at index date"
    ),
    "Age 70-79 years at index" = list(
      points = "+4", definition = "Age 70–79 years at the index amputation date",
      derivation = "Computed from person.year_of_birth; age 70–79 at index date"
    ),
    "Age 80 or more years at index" = list(
      points = "+6", definition = "Age ≥80 years at the index amputation date",
      derivation = "Computed from person.year_of_birth; age ≥80 at index date"
    ),
    "Female sex" = list(
      points = "+1", definition = "Female gender recorded in OMOP person table",
      derivation = "person.gender_concept_id = 8532"
    ),
    "Non-White race" = list(
      points = "+2", definition = "Non-White race recorded in OMOP person table",
      derivation = "person.race_concept_id ≠ 8527 and ≠ 0 (unknown)"
    ),
    "Ambulatory deficit (use of any ambulatory device)" = list(
      points = "+3", definition = "Use of an ambulatory aid (walking aid, wheelchair, walker, walking frame, crutch) or documented impaired ambulation (unable to walk, walking disability, bed-ridden, confined to chair, dependent for walking)",
      derivation = "observation_occurrence and device_exposure records with ambulatory-status concepts within 365 days before index (day −365 to −1)"
    ),
    "Tissue loss (CLI indication — wound ulcer or gangrene)" = list(
      points = "+3", definition = "Critical limb ischaemia indication: wound, ulcer, or gangrene",
      derivation = "condition_occurrence within 365 days before or on the index amputation date"
    ),
    "Anemia (Hgb less than 10 g/dL)" = list(
      points = "+2", definition = "Pre-operative anemia: hemoglobin < 10 g/dL",
      derivation = "Most recent Hgb in measurement table within 365 days before index; g/L values converted"
    ),
    "Insulin-dependent diabetes mellitus" = list(
      points = "+2", definition = "Insulin use as proxy for insulin-dependent diabetes mellitus",
      derivation = "drug_exposure with qualifying insulin ancestor within 365 days before index"
    )
  )

  combined_data <- data.frame(
    Component = character(),
    Points = character(),
    Definition = character(),
    Count = integer(),
    Total = integer(),
    Prevalence = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def <- covariate_defs[[cov_name]]

    if (is.null(cov_def)) {
      cov_def <- list(
        points = "—", definition = cov_name, derivation = "—"
      )
    }

    combined_data <- rbind(combined_data, data.frame(
      Component = cov_name,
      Points = cov_def$points,
      Definition = cov_def$definition,
      Count = covariate_summary_df$n_positive[i],
      Total = covariate_summary_df$n_total[i],
      Prevalence = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  ft <- flextable(combined_data) |>
    set_header_labels(
      Component = "Component",
      Points = "Points",
      Definition = "Definition",
      Count = "Count",
      Total = "Total",
      Prevalence = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component", width = 2.4) |>
    width(j = "Points", width = 0.6) |>
    width(j = "Definition", width = 3.0) |>
    width(j = "Count", width = 0.7) |>
    width(j = "Total", width = 0.7) |>
    width(j = "Prevalence", width = 0.9) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")

  ft
}


# -----------------------------------------------------------------------------
# .build_mfi5_component_table()
#
# Builds a flextable for Table 3a: Subramaniam 2018 mFI-5 components with
# point values, OMOP definitions, and observed activation in the cohort.
# Input: covariate_summary dataframe (covariate_name, n_positive, n_total)
#        from the mFI-5 pipeline run.
# -----------------------------------------------------------------------------
.build_mfi5_component_table <- function(covariate_summary_df) {

  border_h   <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Component definitions: Subramaniam S et al. J Am Coll Surg 2018;226(2):173-181.
  # Each item = 1 point; mFI-5 = sum of positive items (range 0–5).
  covariate_defs <- list(
    "Dependent functional status (ADL dependence)" = list(
      points = "+1",
      definition = "Partial or total dependence for activities of daily living (dependent for, or needs help with, bathing, dressing, feeding, grooming, hygiene, or mobility; requires assistance with daily activities), bed-ridden, confined to chair, or severe frailty",
      derivation = "observation_occurrence and condition_occurrence records within 365 days before index (day −365 to −1)"
    ),
    "Diabetes mellitus (any type)" = list(
      points = "+1",
      definition = "Any type of diabetes mellitus",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "COPD or current pneumonia" = list(
      points = "+1",
      definition = "Chronic obstructive pulmonary disease within 365 days before index, or pneumonia within 30 days before index (current pneumonia); either qualifies",
      derivation = "condition_occurrence: COPD within 365 days OR pneumonia within 30 days before index"
    ),
    "Congestive heart failure (within 30 days prior)" = list(
      points = "+1",
      definition = "Congestive heart failure within 30 days before the index amputation date",
      derivation = "condition_occurrence within 30 days before index (lookback_start_day = −30)"
    ),
    "Hypertension" = list(
      points = "+1",
      definition = "Hypertensive disorder (medication requirement not enforced — documented limitation)",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    )
  )

  combined_data <- data.frame(
    Component    = character(),
    Points       = character(),
    Definition   = character(),
    Count        = integer(),
    Total        = integer(),
    Prevalence   = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def  <- covariate_defs[[cov_name]]
    if (is.null(cov_def)) {
      cov_def <- list(points = "—", definition = cov_name, derivation = "—")
    }
    combined_data <- rbind(combined_data, data.frame(
      Component    = cov_name,
      Points       = cov_def$points,
      Definition   = cov_def$definition,
      Count        = covariate_summary_df$n_positive[i],
      Total        = covariate_summary_df$n_total[i],
      Prevalence   = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }

  ft <- flextable(combined_data) |>
    set_header_labels(
      Component    = "Component",
      Points       = "Points",
      Definition   = "Definition",
      Count        = "Count",
      Total        = "Total",
      Prevalence   = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component",    width = 2.4) |>
    width(j = "Points",       width = 0.6) |>
    width(j = "Definition",   width = 3.0) |>
    width(j = "Count",        width = 0.7) |>
    width(j = "Total",        width = 0.7) |>
    width(j = "Prevalence",   width = 0.9) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")

  ft
}

# -----------------------------------------------------------------------------
# .build_vqifs_component_table()
#
# Builds a flextable for Table 3c: Kraiss 2022 sVQI-FS components with
# point values, OMOP definitions, and observed activation in the cohort.
# Input: covariate_summary dataframe (covariate_name, n_positive, n_total)
#        from the sVQI-FS pipeline run.
# -----------------------------------------------------------------------------
.build_vqifs_component_table <- function(covariate_summary_df) {

  border_h   <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  # Component definitions: Kraiss LW et al. J Vasc Surg 2022;76:1325-34.
  # Each item = 1 point; sVQI-FS (implemented) = sum of positive items (range 0–10).
  # 10 of the paper's 11 items — non-home residence omitted (see covariates_vqifs.csv header).
  covariate_defs <- list(
    "Hypertension" = list(
      points = "+1",
      definition = "Hypertensive disorder",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "Congestive heart failure" = list(
      points = "+1",
      definition = "Congestive heart failure",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "Coronary artery disease" = list(
      points = "+1",
      definition = "Coronary artery disease",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "Peripheral vascular disease" = list(
      points = "+1",
      definition = "Peripheral arterial occlusive disease (diagnosis proxy; paper uses ABI <0.7 or prior arterial intervention/amputation)",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "Diabetes mellitus (any type)" = list(
      points = "+1",
      definition = "Any type of diabetes mellitus",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    "Chronic obstructive pulmonary disease" = list(
      points = "+1",
      definition = "COPD",
      derivation = "condition_occurrence within 365 days before index (day −365 to −1)"
    ),
    # RENAL IMPAIRMENT — key updated 2026-07-26 to match the covariate_name in
    # covariates_vqifs.csv after the definition itself changed. The dict key
    # is looked up by exact covariate_name match below (cov_def <-
    # covariate_defs[[cov_name]]); leaving the OLD key here after
    # covariates_vqifs.csv was updated would have silently fallen through to
    # the generic "—" placeholder for every field, since a missed lookup
    # returns NULL rather than an error.
    "Renal impairment (creatinine > 1.8 mg/dL or dialysis)" = list(
      points = "+1",
      definition = "Creatinine > 1.8 mg/dL or dialysis, the paper's own lab-value definition (no longer a diagnosis proxy — CORRECTED 2026-07-26 after the prior CKD-diagnosis proxy activated in 224/225 patients, 99.6%, due to an over-broad concept-ancestor expansion)",
      derivation = "query_renal_impairment_covariate_counts() in R/risk_score_pipeline.R: measurement value threshold OR procedure/condition dialysis presence, within 365 days before index"
    ),
    # ANEMIA — key updated 2026-07-26, same reason as renal impairment above.
    "Anemia (sex-specific Hgb threshold: <13 g/dL M / <12 g/dL F)" = list(
      points = "+1",
      definition = "Hgb < 13 g/dL (male) / < 12 g/dL (female), the paper's own sex-specific lab threshold (no longer a diagnosis proxy — CORRECTED 2026-07-26 after the prior anemia-diagnosis proxy activated in only 3/225 patients, 1.3%, under-capturing relative to the lab threshold)",
      derivation = "query_anemia_covariate_counts(sex_specific = TRUE) in R/risk_score_pipeline.R: measurement value threshold, sex resolved from person.gender_concept_id, within 365 days before index; missing_is_negative = FALSE (unmeasured Hgb is unknown, not \"not anaemic\")"
    ),
    "Underweight (BMI < 18.5)" = list(
      points = "+1",
      definition = "BMI < 18.5, the paper's own cut-point (direct measurement preferred; weight/height fallback)",
      derivation = "measurement within 365 days before index; missing_is_negative = FALSE (unmeasured BMI is unknown, not \"not underweight\")"
    ),
    "Non-ambulatory / impaired ambulation status" = list(
      points = "+1",
      definition = "Impaired ambulation: walking aid, wheelchair, walker, walking-frame or crutch use, unable to walk, walking disability, bed-ridden, or confined to chair",
      derivation = paste0(
        "observation_occurrence and device_exposure records with ambulatory-status concepts ",
        "(the same set as the Iannuzzi ambulatory-deficit component), within 365 days before ",
        "index (day −365 to −1)"
      )
    )
  )

  combined_data <- data.frame(
    Component    = character(),
    Points       = character(),
    Definition   = character(),
    Count        = integer(),
    Total        = integer(),
    Prevalence   = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def  <- covariate_defs[[cov_name]]
    if (is.null(cov_def)) {
      cov_def <- list(points = "—", definition = cov_name, derivation = "—")
    }
    combined_data <- rbind(combined_data, data.frame(
      Component    = cov_name,
      Points       = cov_def$points,
      Definition   = cov_def$definition,
      Count        = covariate_summary_df$n_positive[i],
      Total        = covariate_summary_df$n_total[i],
      Prevalence   = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }

  ft <- flextable(combined_data) |>
    set_header_labels(
      Component    = "Component",
      Points       = "Points",
      Definition   = "Definition",
      Count        = "Count",
      Total        = "Total",
      Prevalence   = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component",    width = 2.4) |>
    width(j = "Points",       width = 0.6) |>
    width(j = "Definition",   width = 3.0) |>
    width(j = "Count",        width = 0.7) |>
    width(j = "Total",        width = 0.7) |>
    width(j = "Prevalence",   width = 0.9) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")

  ft
}


# -----------------------------------------------------------------------------
# .save_nhd_rate_by_year_plot()
#
# One panel, all series overlaid: NHD rate (%) by procedure year, one line
# per disposition type (SNF, IRF, Hospice, LTAC, Other NHD) plus an overall
# NHD line (any of those dispositions), distinguished by grey level +
# linetype + shape via .gs_scales() -- the same idiom used for the ROC/
# calibration overlays elsewhere in this file. REDESIGNED 2026-09-13 from a
# facet_wrap (one disposition type per panel, no Overall line) back to a
# single overlaid chart per Adam's request; the "Overall" series is
# expected in agg_nhd_by_year.csv as of the same date (see
# pad-amp-nhd-val/R/aggregate_report_inputs.R).
# Returns the output file path, or NULL if the plot cannot be generated.
# -----------------------------------------------------------------------------
.save_nhd_rate_by_year_plot <- function(yr_type_tbl, output_folder) {
  # AGGREGATE-ONLY INPUT (2026-08-11). This used to take the person_level frame
  # and do the year x disposition tabulation itself, which is why the report
  # needed index_date and a per-subject discharge_type. That tabulation moved
  # to pad-amp-nhd-val's R/aggregate_report_inputs.R, which applies the same
  # >= 11-patients-per-year minimum plus small-cell suppression and emits
  # agg_nhd_by_year.csv. This function now only plots.
  #
  # Suppressed rows arrive with events/nhd_rate as NA (n is retained; the rate
  # is blanked alongside the count so it cannot be back-multiplied). They are
  # dropped from the plotted series rather than rendered as zero -- a
  # suppressed cell is "not shown", never "none occurred".
  if (is.null(yr_type_tbl) || nrow(yr_type_tbl) == 0) {
    message("[report] NHD-by-year plot skipped: agg_nhd_by_year.csv missing or empty.")
    return(NULL)
  }
  if (!all(c("year", "n", "discharge_type", "events", "nhd_rate") %in% names(yr_type_tbl))) {
    message("[report] NHD-by-year plot skipped: agg_nhd_by_year.csv missing required columns.")
    return(NULL)
  }

  nhd_levels <- c("SNF", "IRF", "Hospice", "LTAC", "Other NHD")

  n_suppressed <- sum(is.na(yr_type_tbl$nhd_rate))
  yr_type_tbl  <- yr_type_tbl[!is.na(yr_type_tbl$nhd_rate), , drop = FALSE]
  if (n_suppressed > 0) {
    message("[report] NHD-by-year: ", n_suppressed,
            " small-cell-suppressed point(s) omitted from the plot.")
  }

  if (nrow(yr_type_tbl) == 0 || length(unique(yr_type_tbl$year)) < 2) {
    message("[report] NHD-by-year plot skipped: fewer than 2 years of unsuppressed data.")
    return(NULL)
  }

  # "Overall" (any NHD disposition) is always included, in the most
  # prominent palette slot, alongside whichever named disposition types have
  # any events (dropped otherwise, same rule as before). All series are
  # overlaid on one panel via .gs_scales() (grey level + linetype + shape) --
  # replacing the previous facet_wrap, which gave up the ability to show an
  # overall line at all in exchange for a legibility workaround this repo no
  # longer needs now that .gs_series_palette has 10 slots (up from 2 model +
  # 2 reference greys when the facet design was chosen).
  present_levels <- nhd_levels[
    vapply(nhd_levels, function(tp) sum(yr_type_tbl$events[yr_type_tbl$discharge_type == tp]) > 0,
           logical(1))
  ]
  if (length(present_levels) == 0) present_levels <- nhd_levels
  all_levels  <- c("Overall", present_levels)
  yr_type_tbl <- yr_type_tbl[yr_type_tbl$discharge_type %in% all_levels, ]
  yr_type_tbl$discharge_type <- factor(yr_type_tbl$discharge_type, levels = all_levels)
  total_procs <- sum(yr_type_tbl$n[!duplicated(yr_type_tbl$year)])
  total_years <- length(unique(yr_type_tbl$year))

  gs <- .gs_scales(all_levels, slots = seq_along(all_levels))

  p <- ggplot2::ggplot(yr_type_tbl,
      ggplot2::aes(x = year, y = nhd_rate, colour = discharge_type,
                   linetype = discharge_type, shape = discharge_type)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    gs$colour + gs$linetype + gs$shape +
    ggplot2::scale_x_continuous(breaks = sort(unique(yr_type_tbl$year))) +
    ggplot2::scale_y_continuous(limits = c(0, NA),
                                labels = function(x) paste0(round(x, 1), "%")) +
    ggplot2::labs(
      title    = "Non-Home Discharge Rate by Disposition Type and Amputation Year",
      x        = "Year of index amputation",
      y        = "NHD rate (%)",
      colour = NULL, linetype = NULL, shape = NULL,
      caption  = paste0("N = ", total_procs, " patients across ", total_years,
                        " years; years with < 11 patients suppressed.")
    ) +
    theme_manuscript() +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      plot.caption     = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank()
    )

  tryCatch({
    save_figure(p, output_folder, "nhd_rate_by_year.png", width = 7, height = 5)
  }, error = function(e) {
    message("[report] Could not save NHD-by-year plot: ", conditionMessage(e))
    NULL
  })
}

# -----------------------------------------------------------------------------
# .save_nhd_rate_by_month_plot()
#
# Bar chart of 90-day outcome rate (%) by calendar month (Jan-Dec), pooled across
# all years.  Used for integer score_type studies.
# -----------------------------------------------------------------------------
.save_nhd_rate_by_month_plot <- function(mo_counts, output_folder) {
  # AGGREGATE-ONLY INPUT (2026-08-11) -- see the sibling by-year function above
  # for the rationale. The month x outcome tabulation and its >= 5-patient
  # minimum moved to pad-amp-nhd-val's aggregate step (agg_nhd_by_month.csv);
  # the Wilson CI is still computed here, since it is a display concern derived
  # from the two counts rather than something the analysis repo needs to emit.
  if (is.null(mo_counts) || nrow(mo_counts) == 0 ||
      !all(c("month", "n", "events") %in% names(mo_counts))) {
    message("[report] outcome-by-month plot skipped: agg_nhd_by_month.csv missing or malformed.")
    return(NULL)
  }

  # Keep all 12 months so the x-axis is always Jan-Dec; suppressed months
  # arrive with n/events already NA and simply plot as a gap.
  mo_tbl <- do.call(rbind, lapply(1:12, function(m) {
    row <- mo_counts[mo_counts$month == m, , drop = FALSE]
    n      <- if (nrow(row) == 1) suppressWarnings(as.numeric(row$n[1]))      else NA_real_
    events <- if (nrow(row) == 1) suppressWarnings(as.numeric(row$events[1])) else NA_real_
    if (!is.na(n) && !is.na(events) && n > 0) {
      rate   <- 100 * events / n
      ci_obj <- tryCatch(
        prop.test(events, n, conf.level = 0.95, correct = FALSE)$conf.int,
        error = function(e) c(NA_real_, NA_real_)
      )
      ci_lo  <- 100 * ci_obj[1]
      ci_hi  <- 100 * ci_obj[2]
    } else {
      rate   <- NA_real_
      ci_lo  <- NA_real_
      ci_hi  <- NA_real_
    }
    data.frame(
      month    = m,
      n        = if (is.na(n)) NA_integer_ else as.integer(n),
      events   = if (is.na(events)) NA_integer_ else as.integer(events),
      ssi_rate = rate,
      ci_lo    = ci_lo,
      ci_hi    = ci_hi,
      stringsAsFactors = FALSE
    )
  }))

  mo_tbl$month_label <- factor(
    mo_tbl$month,
    levels = 1:12,
    labels = c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
  )

  if (all(is.na(mo_tbl$ssi_rate))) {
    message("[report] NHD-by-month plot skipped: all months suppressed (< 5 procedures each).")
    return(NULL)
  }

  # Greyscale (2026-09-06). This was the last live figure still emitting a hue
  # (#1F3864 navy) and the only one bypassing the manuscript figure pipeline —
  # it used theme_minimal() and a bare ggsave() at 150 dpi instead of
  # theme_manuscript() + save_figure(), so it also shipped no 600 dpi TIFF or
  # vector PDF. A single-series bar chart is legible once desaturated, so this
  # was never a discrimination problem; it was a consistency one.
  #
  # grey35 rather than black: the bars sit behind black error bars and black
  # count labels, and a black-on-black bar loses both. grey35 keeps the bar
  # clearly subordinate to its annotations while staying dark enough to read
  # against the panel at print size.
  p <- ggplot2::ggplot(mo_tbl, ggplot2::aes(x = month_label, y = ssi_rate)) +
    ggplot2::geom_col(fill = "grey35", width = 0.7, na.rm = TRUE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lo, ymax = ci_hi),
      width = 0.25, colour = "black", na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(!is.na(ssi_rate), paste0("n=", n), "")),
      vjust = -0.4, size = 2.8, colour = "black", na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      labels = function(x) paste0(round(x, 1), "%")
    ) +
    ggplot2::labs(
      title   = "NHD Rate by Month of Amputation",
      x       = "Month of index amputation",
      y       = "NHD rate (%)",
      caption = paste0("Pooled across all study years. ",
                       "Error bars = 95% Wilson CI. ",
                       "Months with < 5 patients suppressed.")
    ) +
    theme_manuscript() +
    ggplot2::theme(
      plot.caption       = ggplot2::element_text(size = 8),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )

  # File name corrected 2026-09-06: this wrote "ssi_rate_by_month.png", a
  # leftover from the surgical-site-infection study this function was ported
  # from. It is the NHD-by-month figure and had nothing to do with SSI. Routed
  # through save_figure() so it gets the same 600 dpi TIFF + vector PDF as
  # every other manuscript figure; save_figure() returns the .png path, which
  # is what body_add_img() embeds.
  tryCatch({
    save_figure(p, output_folder, "nhd_rate_by_month.png", width = 7, height = 4.5)
  }, error = function(e) {
    message("[report] Could not save NHD-by-month plot: ", conditionMessage(e))
    NULL
  })
}


# ---------------------------------------------------------------------------
# LASSO model functions (from MACCE validation study)
# ---------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# .covariate_table_data_lasso()
#
# Reads included LASSO predictors from model/varImp.rds and returns a data frame
# for inclusion in the Word report as Table 2 (lasso score_type).
# Renamed from .covariate_table_data() in the original MACCE monolith.
# Now accepts config as first argument and reads config$var_imp_file instead of
# the hardcoded default path.
# -----------------------------------------------------------------------------
.covariate_table_data_lasso <- function(config, n_top = Inf) {
  varImp_path <- config$var_imp_file %||% "model/varImp.rds"
  if (!file.exists(varImp_path)) {
    stop("varImp.rds not found at: ", varImp_path,
         "\nSet varImp_path= or ensure the model artifact exists before generating the report.")
  }

  vi <- readRDS(varImp_path)

  # Retain only model-included covariates (included == 1 flags the LASSO-selected set).
  vi <- vi[!is.na(vi$included) & vi$included == 1, ]

  if (nrow(vi) == 0) {
    stop("No covariates with included == 1 found in ", varImp_path)
  }

  # Derive OMOP domain from analysisId encoding used by FeatureExtraction:
  #   2xx = condition_era, 4xx = drug_era, 9xx = measurement value,
  #   5xx = demographics,  7xx = visit,    other = miscellaneous
  get_domain <- function(aid) {
    ifelse(aid >= 200L & aid < 300L, "Condition",
    ifelse(aid >= 400L & aid < 500L, "Drug",
    ifelse(aid >= 900L & aid < 1000L, "Measurement",
    ifelse(aid >= 500L & aid < 600L, "Demographics",
    ifelse(aid >= 700L & aid < 800L, "Visit",
    "Other")))))
  }

  # Derive human-readable lookback window from analysisId.
  # Window codes follow FeatureExtraction default temporal analysis settings.
  get_lookback <- function(aid) {
    dplyr::case_when(
      aid %in% c(210L, 410L) ~ "365 to 3 days before index",
      aid %in% c(211L, 411L) ~ "180 to 3 days before index",
      aid %in% c(212L, 412L) ~ "30 to 3 days before index",
      aid %in% c(501L, 502L, 503L, 504L) ~ "At index (demographic)",
      aid %in% c(706L, 707L, 708L) ~ "365 days before index (visit)",
      aid %in% c(901L, 904L) ~ "Most recent value in 365 days before index",
      aid %in% c(998L, 999L) ~ "Aggregated risk score",
      TRUE ~ paste0("analysisId=", aid)
    )
  }

  vi$domain   <- get_domain(vi$analysisId)
  vi$lookback <- get_lookback(vi$analysisId)

  # Strip the FeatureExtraction time-window prefix from covariateName.
  # E.g. "condition_era group during day -365 through -3 days relative to index: Angina pectoris"
  # becomes "Angina pectoris".
  vi$variable <- sub("^[^:]+:\\s*", "", vi$covariateName)

  # Derivation note: OMOP CDM table and concept hierarchy source.
  vi$derivation <- ifelse(
    vi$domain == "Condition",
    paste0("Concept ", vi$conceptId, " + descendants in condition_occurrence (condition_era)."),
    ifelse(vi$domain == "Drug",
      paste0("Concept ", vi$conceptId, " + descendants in drug_exposure (drug_era)."),
      ifelse(vi$domain == "Measurement",
        paste0("Concept ", vi$conceptId, " in measurement; most recent value in lookback window."),
        paste0("FeatureExtraction analysisId=", vi$analysisId, ", conceptId=", vi$conceptId, ".")
      )
    )
  )

  # Sort by absolute coefficient value descending so the most predictive
  # covariates appear first.
  vi <- vi[order(-abs(vi$covariateValue)), ]

  # Trim to top n_top rows if requested.
  if (is.finite(n_top) && n_top < nrow(vi)) {
    vi <- vi[seq_len(as.integer(n_top)), ]
  }

  # Coerce covariateId to plain numeric so that merge() with covariate_summary
  # (read from CSV via read.csv(), which returns numeric) produces matches.
  # PLP RDS files store covariateId as bit64::integer64; read.csv() returns
  # numeric (double).  R's merge() treats these as different types and finds
  # zero intersections without explicit coercion.
  data.frame(
    covariate_id = as.numeric(vi$covariateId),
    variable     = vi$variable,
    weight       = vi$covariateValue,
    lookback     = vi$lookback,
    domain       = vi$domain,
    derivation   = vi$derivation,
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# .build_combined_covariate_table()
#
# Builds a combined covariate table for LASSO score_type studies (PLP mode).
# Shows top predictors by |coefficient| with prevalence counts.
# Copied verbatim from pad_oler_macce_val/R/report_extended.R.
# -----------------------------------------------------------------------------
.build_combined_covariate_table <- function(covariate_summary_df) {
  # Build combined covariate table with definitions and prevalence.
  # Input: covariate_summary dataframe.
  # PLP mode: detected when "feature_importance" column is present.
  #   Columns: Feature, Importance (LASSO weight), Count, Prevalence.
  # Integer-score mode: covariate_name, n_positive, n_total, no feature_importance.
  #   Columns: Component, Points, Definition, OMOP_Concept, Count, Total, Prevalence.

  border_h  <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  border_out <- officer::fp_border(color = "#1F3864", width = 1.5)

  is_plp_mode <- "feature_importance" %in% names(covariate_summary_df)

  if (is_plp_mode) {
    # ---- PLP mode: show top features by |LASSO weight| with prevalence ----
    n_total <- if ("n_total" %in% names(covariate_summary_df))
                 covariate_summary_df$n_total[1] else NA_integer_

    # Strip FeatureExtraction time-window prefix from covariate name for display.
    # E.g. "condition_era group during day -365 through -3 days relative to index: Angina"
    # becomes "Angina".
    display_name <- sub("^[^:]+:\\s*", "", covariate_summary_df$covariate_name)

    plp_data <- data.frame(
      Feature      = display_name,
      Importance   = round(covariate_summary_df$feature_importance, 4),
      Count        = as.integer(covariate_summary_df$n_positive),
      Prevalence   = paste0(
        ifelse(!is.na(n_total) & n_total > 0,
               round(100 * covariate_summary_df$n_positive / n_total, 1),
               NA_real_), "%"),
      stringsAsFactors = FALSE
    )

    ft <- flextable::flextable(plp_data) |>
      flextable::set_header_labels(
        Feature    = "Feature (top predictors by |coefficient|)",
        Importance = "LASSO\nCoefficient",
        Count      = "Count\n(n positive)",
        Prevalence = "Prevalence %"
      ) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 9, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::width(j = "Feature",    width = 3.5) |>
      flextable::width(j = "Importance", width = 0.8) |>
      flextable::width(j = "Count",      width = 0.7) |>
      flextable::width(j = "Prevalence", width = 0.8) |>
      flextable::align(j = c("Importance", "Count", "Prevalence"),
                       align = "center", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::hline(border = border_h, part = "body") |>
      flextable::border_outer(border = border_out, part = "all") |>
      flextable::set_table_properties(layout = "fixed") |>
      flextable::padding(padding = 3, part = "all")

    return(ft)
  }

  # ---- Integer-score mode: full covariate definition table ----

  # Map covariates to their definitions and derivation methods
  covariate_defs <- list(
    "Female sex" = list(
      points = "+1",
      definition = "Female gender",
      omop_concept = "Concept 8532 (Female)",
      derivation = "person.gender_concept_id matches concept 8532"
    ),
    "Overweight (BMI 25 to <30)" = list(
      points = "+1",
      definition = "BMI between 25 and <30 kg/m\u00b2",
      omop_concept = paste0("Direct BMI: 3038553, 36304833; ",
                            "Weight: 3025315, 3013762, 3011054, 3026600; ",
                            "Height: 3036277, 3023540, 3015514"),
      derivation = paste0("Direct BMI measurement preferred (LOINC 39156-5 / 59574-4); ",
                          "computed from weight/height as fallback. 25 \u2264 BMI < 30.")
    ),
    "Obese (BMI \u226530)" = list(
      points = "+3",
      definition = "BMI \u2265 30 kg/m\u00b2",
      omop_concept = paste0("Direct BMI: 3038553, 36304833; ",
                            "Weight: 3025315, 3013762, 3011054, 3026600; ",
                            "Height: 3036277, 3023540, 3015514"),
      derivation = paste0("Direct BMI measurement preferred (LOINC 39156-5 / 59574-4); ",
                          "computed from weight/height as fallback. BMI \u2265 30.")
    ),
    "Urgent / emergency case" = list(
      points = "+1",
      definition = "Urgent or emergency procedure",
      omop_concept = "Concepts 4158569, 4250892 + descendants",
      derivation = "Procedure types in procedure_occurrence or observation within 30 days"
    ),
    "Low ankle-brachial index (ABI ≤0.35)" = list(
      points = "+1",
      definition = "ABI ≤ 0.35",
      omop_concept = "Concepts 40489833, 46237026 (ABI measurement)",
      derivation = "ABI measurement value < 0.35 in measurement table"
    ),
    "Prior revascularization (any)" = list(
      points = "+1",
      definition = "Any prior lower-extremity revascularization procedure",
      omop_concept = "Concepts 4236706 + 4225375 + descendants",
      derivation = "Procedure_occurrence within 10-year lookback"
    ),
    "Prolonged antibiotic exposure" = list(
      points = "+2",
      definition = "Non-prophylactic antibiotic exposure >2 days",
      omop_concept = "Concept 21603553 (systemic antibiotic) + descendants",
      derivation = "drug_exposure duration > 2 days within 90 days before index date"
    ),
    "Operative time ≥4 hours" = list(
      points = "+1",
      definition = "Operative duration ≥ 240 minutes",
      omop_concept = "procedure_occurrence timestamps (procedure_start/end_datetime)",
      derivation = "DATEDIFF(MINUTE, start, end) > 240"
    ),
    "High modified Frailty Index (mFI)" = list(
      points = "+1",
      definition = "Modified Frailty Index > 0.25 (≥2 of 5 conditions)",
      omop_concept = "Concepts 201820, 255573, 316139, 316866, 4215267",
      derivation = "Condition_occurrence: diabetes, COPD, CHF, hypertension, functional impairment"
    ),
    "Indication: claudication" = list(
      points = "−1",
      definition = "Intermittent claudication as operative indication",
      omop_concept = "Concept 442774 + descendants",
      derivation = "Condition_occurrence within 365 days"
    )
  )
  
  combined_data <- data.frame(
    Component = character(),
    Points = character(),
    Definition = character(),
    OMOP_Concept = character(),
    Count = integer(),
    Total = integer(),
    Prevalence = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(covariate_summary_df))) {
    cov_name <- covariate_summary_df$covariate_name[i]
    cov_def <- covariate_defs[[cov_name]]

    if (is.null(cov_def)) {
      cov_def <- list(
        points = "—", definition = cov_name, omop_concept = "—", derivation = "—"
      )
    }

    combined_data <- rbind(combined_data, data.frame(
      Component = cov_name,
      Points = cov_def$points,
      Definition = cov_def$definition,
      OMOP_Concept = cov_def$omop_concept,
      Count = covariate_summary_df$n_positive[i],
      Total = covariate_summary_df$n_total[i],
      Prevalence = paste0(
        round(100 * covariate_summary_df$n_positive[i] / covariate_summary_df$n_total[i], 1), "%"
      ),
      stringsAsFactors = FALSE
    ))
  }
  
  ft <- flextable(combined_data) |>
    set_header_labels(
      Component = "Component",
      Points = "Points",
      Definition = "Definition",
      OMOP_Concept = "OMOP Standard Concept ID(s)",
      Count = "Count",
      Total = "Total",
      Prevalence = "Prevalence %"
    ) |>
    bold(part = "header") |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Calibri", part = "all") |>
    width(j = "Component", width = 1.8) |>
    width(j = "Points", width = 0.5) |>
    width(j = "Definition", width = 1.8) |>
    width(j = "OMOP_Concept", width = 1.8) |>
    width(j = "Count", width = 0.6) |>
    width(j = "Total", width = 0.6) |>
    width(j = "Prevalence", width = 0.8) |>
    align(j = c("Points", "Count", "Total", "Prevalence"), align = "center", part = "all") |>
    bg(part = "header", bg = "#1F3864") |>
    color(part = "header", color = "white") |>
    hline(border = border_h, part = "body") |>
    border_outer(border = border_out, part = "all") |>
    set_table_properties(layout = "fixed") |>
    padding(padding = 3, part = "all")
  
  ft
}




# ===========================================================================
# .report_word_simple()
#
# Lightweight report that reads pipeline CSV outputs and assembles a Word
# document.  No live CDM queries.  Called by generate_word_report() in
# R/report_extended.R.
#
# Renamed from generate_word_report() in the original synthea-omop-template monolith.
# Body is verbatim — no parameterization applied to the simple report.
# ===========================================================================
.report_word_simple <- function(output_dir = "output/risk_score_eval",
                                 score_output_dir = "output/risk_score_eval") {

  # DISABLED 2026-08-11 -- this is the one code path left in this repo that
  # reads person_level_scores.csv (subject_id, index_date, per-patient outcome
  # and predicted risk). The manuscript report was converted to render purely
  # from aggregate artifacts; this legacy "simple report" was not, and leaving
  # it callable would mean the repo still contains a working patient-level
  # reader -- exactly the property the conversion was meant to remove.
  #
  # It is not reachable from GenerateReport.R (which calls
  # generate_manuscript_report()), and its own helper .compute_ece() was found
  # earlier to carry a latent bug precisely because nothing exercised it. So
  # this fails loudly rather than being quietly converted: if the simple report
  # is genuinely wanted again, convert it against agg_*.csv the same way
  # .report_prognostic() was, and delete this guard as part of that work.
  stop(
    "generate_word_report() / .report_word_simple() is disabled.\n",
    "It renders from person_level_scores.csv (patient-level), which this repo ",
    "no longer reads -- see R/aggregate_report_inputs.R in pad-amp-nhd-val.\n",
    "Use generate_manuscript_report() instead (that is what GenerateReport.R ",
    "calls), or convert this function to the agg_*.csv artifacts first."
  )

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Load pipeline outputs if available
  person_level <- NULL
  covariate_summary <- NULL
  metrics <- NULL
  calibration_plot_files <- list()
  roc_plot_file <- NULL

  if (file.exists(file.path(score_output_dir, "person_level_scores.csv"))) {
    person_level <- read.csv(file.path(score_output_dir, "person_level_scores.csv"), stringsAsFactors = FALSE)
  }
  if (file.exists(file.path(score_output_dir, "covariate_summary.csv"))) {
    covariate_summary <- read.csv(file.path(score_output_dir, "covariate_summary.csv"), stringsAsFactors = FALSE)
  }
  if (file.exists(file.path(score_output_dir, "metrics.csv"))) {
    metrics <- read.csv(file.path(score_output_dir, "metrics.csv"), stringsAsFactors = FALSE)
  }

  # Ensure calibration PNGs exist for report insertion by backfilling from CSV
  # tables (preferred) or person-level predictions (fallback).
  lookup_plot_path <- file.path(score_output_dir, "calibration_lookup.png")
  recal_plot_path <- file.path(score_output_dir, "calibration_recalibrated.png")
  lookup_table_path <- file.path(score_output_dir, "calibration_table_lookup.csv")
  recal_table_path <- file.path(score_output_dir, "calibration_table_recalibrated.csv")

  if (!file.exists(lookup_plot_path) && file.exists(lookup_table_path)) {
    .save_calibration_plot_from_table(
      calibration_table_path = lookup_table_path,
      output_folder = score_output_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
  }
  if (!file.exists(recal_plot_path) && file.exists(recal_table_path)) {
    .save_calibration_plot_from_table(
      calibration_table_path = recal_table_path,
      output_folder = score_output_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
  }

  if (!file.exists(lookup_plot_path) && !is.null(person_level) &&
      all(c("outcome", "predicted_risk_lookup") %in% names(person_level))) {
    .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_lookup,
      output_folder = score_output_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
  }
  if (!file.exists(recal_plot_path) && !is.null(person_level) &&
      all(c("outcome", "predicted_risk_recalibrated") %in% names(person_level))) {
    .save_calibration_plot_from_vectors(
      y = person_level$outcome,
      p = person_level$predicted_risk_recalibrated,
      output_folder = score_output_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
  }
  
  # Check for calibration plot files
  cal_files <- list.files(score_output_dir, pattern = "^calibration_.*\\.png$", full.names = TRUE)
  if (length(cal_files) > 0) {
    calibration_plot_files <- setNames(cal_files, 
                                         gsub(".*calibration_|\\.png$", "", cal_files))
  }
  
  # Generate ROC plot if we have the data
  if (!is.null(person_level)) {
    roc_plot_file <- .save_roc_plot(
      y = person_level$outcome,
      p = if ("predicted_risk_recalibrated" %in% names(person_level)) 
          person_level$predicted_risk_recalibrated
        else person_level$total_score / max(person_level$total_score, na.rm = TRUE),
      output_folder = score_output_dir
    )
  }

  doc <- read_docx()

  # ---- Title ---------------------------------------------------------------
  today_str <- format(Sys.Date(), "%B %d, %Y")
  doc <- body_add_par(doc,
    paste0("PAD / Major Lower-Extremity Amputation — Non-Home Discharge Risk Score"),
    style = "heading 1")
  doc <- body_add_par(doc,
    paste0("External Validation Report: Iannuzzi 2020 and Subramaniam mFI-5"),
    style = "heading 1")
  doc <- body_add_par(doc, paste("Report Generated:", today_str), style = "heading 2")
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 1. Executive Summary ------------------------------------------------
  doc <- body_add_par(doc, "1.  Executive Summary", style = "heading 2")
  
  if (!is.null(person_level)) {
    n_patients   <- length(unique(person_level$subject_id))
    n_procedures <- nrow(person_level)
    n_nhd_events <- sum(person_level$outcome, na.rm = TRUE)
    nhd_rate     <- round(100 * n_nhd_events / n_procedures, 1)
    mean_score   <- round(mean(person_level$total_score, na.rm = TRUE), 2)

    doc <- body_add_par(doc,
      paste0(
        "This study externally validated two published integer risk scores for non-home ",
        "discharge (NHD) following major lower-extremity amputation in patients with ",
        "peripheral arterial disease, diabetes mellitus, or a lower-extremity wound indication. ",
        "The scores evaluated were the Iannuzzi 2020 NHD integer score (9 components, 0–18 points) ",
        "and the Subramaniam 2018 modified Frailty Index — 5-item (mFI-5, 0–5 points). ",
        "The analysis was performed on ",
        config$cdm_database_name %||% "the validation database",
        " (OMOP CDM v5.4) containing ", n_patients, " unique patients with ",
        n_procedures, " eligible amputation encounters. ",
        "Overall NHD incidence was ", n_nhd_events, " events (", nhd_rate, "%). ",
        "The mean Iannuzzi integer score was ", mean_score, " points."
      ),
      style = "Normal"
    )
  } else {
    doc <- body_add_par(doc,
      paste0(
        "This study externally validated two published integer risk scores for non-home ",
        "discharge (NHD) following major lower-extremity amputation: the Iannuzzi 2020 ",
        "NHD integer score (9 components, 0–18 points) and the Subramaniam 2018 mFI-5 ",
        "(5 components, 0–5 points)."
      ),
      style = "Normal"
    )
  }
  
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 2. Methods ----------------------------------------------------------
  doc <- body_add_par(doc, "2.  Methods", style = "heading 2")

  # 2.1 Study Population and Data Source
  doc <- body_add_par(doc, "2.1  Study Population and Data Source", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "We conducted an external validation study of two published integer risk scores for ",
      "non-home discharge (NHD) following major lower-extremity amputation using the ",
      config$cdm_database_name %||% "validation", " database, which contains records mapped ",
      "to the Observational Medical Outcomes Partnership (OMOP) Common Data Model version 5.4. ",
      "The study period spanned ", config$study_start_date %||% "2017-01-01",
      " through ", config$study_end_date %||% "2025-12-31", "."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "The target cohort comprised adults aged 18 years or older who underwent an inpatient ",
      "major lower-extremity amputation. The index event was defined by six OMOP procedure ",
      "concept ancestors and all descendants via the concept_ancestor table: above-knee ",
      "amputation (concept 4195136), amputation through tibia and fibula / below-knee ",
      "(concept 4338257), through-knee amputation (concept 4143795), hip disarticulation ",
      "(concept 4242396), ankle disarticulation (concept 4264289), and hemipelvectomy ",
      "(concept 36675618). At least one of the following indications was required to be ",
      "documented on or before the index date: peripheral arterial disease (SNOMED 399957001 ",
      "and descendants), diabetes mellitus (SNOMED 73211009 and descendants), or a ",
      "lower-extremity wound (ulcers, open wounds, gangrene, soft-tissue infection, ",
      "osteomyelitis, or diabetic foot — 25 SNOMED ancestor concept IDs). These eligibility ",
      "criteria exclude traumatic, burn, and oncologic amputations. Patients were required to ",
      "have at least ", config$min_prior_observation_days %||% 365,
      " days of prior observation before the index date. ",
      "All patients were enrolled from inpatient visits (OMOP visit_concept_id 9201) only."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "The outcome was non-home discharge at the end of the index hospitalization, defined as ",
      "any discharge destination other than home (UB-04 codes 01/06 and CMS place-of-service ",
      "code 8536 = “home”). Discharge destination was ascertained from ",
      "visit_occurrence.discharged_to_concept_id using a dynamic vocabulary query resolved ",
      "at runtime. The outcome was attributed to the index visit when discharge occurred within ",
      config$prediction_window_days %||% 90,
      " days of the index date (prediction_window_days). The dataset contained ",
      if (!is.na(n_target)) n_target else "N",
      " patients with at least one qualifying amputation within the study window."
    ),
    style = "Normal"
  )

  # 2.2 Risk Score Computation
  doc <- body_add_par(doc, "2.2  Risk Score Computation", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "Two published integer risk scores were evaluated. ",
      "For each score, covariates were extracted from the OMOP CDM using component-specific ",
      "lookback windows relative to the index date. A patient meeting the minimum event ",
      "threshold for a given component received the full integer point value; those below ",
      "threshold received zero. Missing data were treated as zero evidence (absence of ",
      "component). The total score is the arithmetic sum of all component values."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Iannuzzi 2020 non-home discharge integer risk score (score range 0–18 points). ",
      "This nine-component score was originally derived and internally validated in patients ",
      "undergoing open lower-extremity revascularisation for peripheral artery disease. ",
      "Components and point values are: age 60–69 years (+2), age 70–79 years (+4), ",
      "age ≥80 years (+6), female sex (+1; OMOP concept 8532), non-White race (+2; ",
      "concept 8527 [White] as reference), ambulatory deficit — use of an ambulatory ",
      "aid or documented impaired ambulation (+3; walking-aid, wheelchair, walker, frame and ",
      "crutch use, and unable-to-walk, bed-ridden and confined-to-chair findings), ",
      "tissue loss / critical limb ischaemia indication — wound, ulcer, or gangrene (+3; ",
      "concepts 4029926, 4291464 and descendants), anemia (hemoglobin <10 g/dL; +2; ",
      "LOINC 718-7, OMOP concept 3000963), and insulin-dependent diabetes mellitus (+2; ",
      "ATC insulins and analogues, OMOP concept 21600713 and descendants). The score is ",
      "mapped to predicted NHD risk probabilities via a refitted score-to-risk lookup table ",
      "(see Performance Evaluation section below for refitting rationale)."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Subramaniam 2018 modified Frailty Index — 5-item (mFI-5; score range 0–5 points). ",
      "This five-component frailty index was derived in a large surgical registry (ACS NSQIP) ",
      "and assigns one point for each of: dependent functional status (activities-of-daily-living ",
      "dependence, bed-ridden, confined to chair, or severe frailty), diabetes mellitus of any type ",
      "(concept 201820 and descendants), chronic obstructive pulmonary disease within 365 days ",
      "(concept 255573 and descendants) or current pneumonia within 30 days (concept 255848 and ",
      "descendants), congestive heart failure within 30 days prior to the index ",
      "date (concept 316139 and descendants), and hypertension (concept 316866 and ",
      "descendants). No published NHD probability mapping exists for the mFI-5 in this ",
      "population; the raw integer score is used directly for discrimination analysis."
    ),
    style = "Normal"
  )

  # 2.3 Performance Evaluation
  doc <- body_add_par(doc, "2.3  Performance Evaluation", style = "heading 3")
  doc <- body_add_par(doc,
    paste0(
      "Three model specifications were evaluated. ",
      "(1) Iannuzzi 2020 — refitted lookup: the published Iannuzzi lookup table ",
      "(Iannuzzi et al. 2020, Table III) exhibits a non-monotonic inversion in its tail ",
      "(score 17 = 50%% NHD risk, below score 16 = 87.5%%), a small-sample artefact from ",
      "the derivation cohort. To ensure higher scores map to higher predicted risk, we refitted ",
      "the lookup as a monotone isotonic regression (using both derivation and validation ",
      "series) via stats::isoreg(). This produces a mathematically monotone curve while ",
      "incorporating all available observed rates. Predicted NHD probabilities are drawn from ",
      "this refitted table. ",
      "(2) Iannuzzi 2020 — temporal recalibration: a logistic regression of the total ",
      "integer score on the observed binary NHD outcome was fitted on the chronologically ",
      "earlier half of the cohort (training set) and all reported metrics were evaluated ",
      "on the later half (test set). This specification quantifies the improvement in ",
      "calibration obtainable by re-anchoring the score’s probability scale to the local ",
      "event rate. ",
      "(3) mFI-5 — raw integer score: because no published NHD probability mapping exists ",
      "for the mFI-5, discrimination was assessed from the raw score only; calibration ",
      "metrics are not applicable and are reported as N/A."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Discrimination was quantified using the area under the receiver operating ",
      "characteristic curve (AUROC) computed with pROC and the area under the ",
      "precision-recall curve (AUPRC) computed with PRROC. Calibration was assessed ",
      "using four metrics: (a) Brier score (mean squared error of predicted probabilities); ",
      "(b) expected calibration error (ECE) — the probability-weighted mean absolute ",
      "difference between mean predicted probability and observed event rate across ",
      "10 equal-frequency bins; (c) calibration intercept; and (d) calibration slope, ",
      "derived from a logistic regression of the observed outcome on the log-odds of the ",
      "predicted probability. Perfect calibration corresponds to intercept = 0 and slope = 1. ",
      "All point estimates are accompanied by 95% bootstrap percentile confidence intervals ",
      "(B = 500 resamples)."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    paste0(
      "Temporal train/test split. Patients were sorted chronologically by index date and ",
      "divided at the midpoint (floor(n / 2) training rows). The recalibration logistic ",
      "regression was fitted exclusively on the training set (earlier half) and all ",
      "discrimination and calibration metrics — for all three model specifications — ",
      "were evaluated on the test set (later half) only. This approach prevents optimistic ",
      "bias arising from evaluating a locally fitted model on the data used to fit it, and ",
      "provides an estimate of prospective performance on future patients from the same source."
    ),
    style = "Normal"
  )
  # Append model-specific split sample sizes when split_info.csv was found.
  for (si_pair in list(
    list(si = split_info_iannuzzi, label = "Iannuzzi 2020"),
    list(si = split_info_mfi5,     label = "mFI-5")
  )) {
    sent <- split_sentence(si_pair$si, si_pair$label)
    if (!is.null(sent)) {
      doc <- body_add_par(doc, sent, style = "Normal")
    }
  }
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 3. Table 1: Cohort Summary ------------------------------------------
  doc <- body_add_par(doc, "3.  Study Cohort Characteristics", style = "heading 2")
  
  if (!is.null(person_level)) {
    doc <- body_add_par(doc,
      paste0(
        "Table 1 presents baseline demographic and clinical characteristics of the validation cohort. ",
        "Variables are summarized across all eligible procedures (N = ", nrow(person_level), ")."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 1.  Cohort characteristics (baseline demographics and clinical features).",
      style = "Normal"
    )
    
    cohort_summary_df <- .build_cohort_summary_table(person_level)
    doc <- body_add_flextable(doc, cohort_summary_df)
  }
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- 4. Table 2: Risk Score Components -----------------------------------
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "4.  Risk Model Variables", style = "heading 2")
  doc <- body_add_par(doc,
    paste0(
      "Table 2 lists the components of the integer risk score, the point value assigned to each, ",
      "the lookback window applied, and the OMOP concept-based derivation method used in this validation."
    ),
    style = "Normal"
  )
  doc <- body_add_par(doc,
    "Table 2.  Risk score components, point values, and OMOP CDM derivation method.",
    style = "Normal"
  )
  doc <- body_add_flextable(doc, .build_table1(.covariate_table_data()))
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- Figure 1. ROC Curve (placed immediately after Tables 1-2) ----------
  if (!is.null(roc_plot_file) && file.exists(roc_plot_file)) {
    doc <- body_add_par(doc,
      "Figure 1.  Receiver operating characteristic (ROC) curve for non-home discharge (NHD) risk prediction.",
      style = "Normal"
    )
    doc <- body_add_img(doc, src = roc_plot_file, width = 5, height = 3.5)
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 5. Table 3: Covariate Summary & Cohort Counts ----------------------
  if (!is.null(covariate_summary)) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "5.  Covariate Prevalence in the Validation Cohort", style = "heading 2")
    doc <- body_add_par(doc,
      paste0(
        "Table 3 displays the prevalence of each risk score covariate in the validation cohort, ",
        "alongside the covariate definitions and OMOP concept derivation. ",
        "Covariate counts and prevalence percentages are computed across all eligible procedures."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 3.  Risk score covariates with OMOP derivation and prevalence in the validation cohort.",
      style = "Normal"
    )

    # Build combined flextable for covariate summary with definitions
    combined_cov_df <- .build_combined_covariate_table(covariate_summary)
    doc <- body_add_flextable(doc, combined_cov_df)
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 6. Results & Performance Metrics ------------------------------------
  if (!is.null(metrics) && nrow(metrics) > 0) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "6.  Discrimination and Calibration Metrics", style = "heading 2")
    
    doc <- body_add_par(doc,
      paste0(
        "Table 4 presents the discrimination (AUROC, AUPRC, Brier score) and calibration ",
        "(calibration-in-the-large intercept and slope) metrics across three model specifications: ",
        "lookup-based score-to-risk table, and recalibrated logistic regression."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc,
      "Table 4.  Discrimination and calibration metrics.",
      style = "Normal"
    )
    
    # Build a formatted "value (95% CI)" string per metric-model cell, then
    # pivot wide so each model becomes one column.  Falls back to just the
    # point estimate when ci_lower / ci_upper are absent (legacy CSV format).
    has_ci <- all(c("ci_lower", "ci_upper") %in% names(metrics)) &&
              any(!is.na(metrics$ci_lower))

    fmt3 <- function(x) format(round(as.numeric(x), 3), nsmall = 3, trim = TRUE)

    metrics_disp <- metrics
    metrics_disp$cell <- if (has_ci) {
      ifelse(
        !is.na(metrics$ci_lower) & !is.na(metrics$ci_upper),
        paste0(fmt3(metrics$value),
               " (", fmt3(metrics$ci_lower), "\u2013", fmt3(metrics$ci_upper), ")"),
        fmt3(metrics$value)
      )
    } else {
      fmt3(metrics$value)
    }

    metrics_wide <- metrics_disp[, c("metric", "cell", "model")]
    metrics_wide <- reshape(metrics_wide, idvar = "metric", timevar = "model", direction = "wide")
    names(metrics_wide) <- gsub("cell\\.", "", names(metrics_wide))

    ft_metrics <- flextable(metrics_wide) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      align(j = seq_along(names(metrics_wide))[-1], align = "center", part = "all") |>
      set_header_labels(
        metric       = "Metric",
        score_only   = "Score only",
        lookup       = "Lookup (published)",
        recalibrated = "Recalibrated"
      )

    if (has_ci) {
      ft_metrics <- add_footer_lines(ft_metrics,
        "Values shown as point estimate (95% bootstrap percentile CI, B\u2009=\u2009500 resamples).")
      ft_metrics <- fontsize(ft_metrics, size = 8, part = "footer")
      ft_metrics <- font(ft_metrics, fontname = "Calibri", part = "footer")
    }

    doc <- body_add_flextable(doc, ft_metrics)
    doc <- body_add_par(doc, "", style = "Normal")
    
    # Interpretation
    auroc_lookup <- metrics$value[metrics$metric == "AUROC" & metrics$model == "lookup"]
    if (length(auroc_lookup) > 0 && !is.na(auroc_lookup)) {
      interp <- if (auroc_lookup > 0.8) "excellent" 
                else if (auroc_lookup > 0.7) "good" 
                else if (auroc_lookup > 0.6) "fair" 
                else "poor"
      doc <- body_add_par(doc,
        paste0(
          "The model demonstrates an AUROC of ", round(auroc_lookup, 3), 
          " when using lookup-based probabilities, indicating ", interp, 
          " discriminative ability."
        ),
        style = "Normal"
      )
    }
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 8. Calibration Plots -----------------------------------------------
  if (length(calibration_plot_files) > 0) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "8.  Calibration: Observed vs. Predicted Risk", style = "heading 2")
    doc <- body_add_par(doc,
      paste0(
        "Figures 2+ display calibration plots for the lookup-based and recalibrated model specifications. ",
        "The solid line represents perfect calibration (predicted = observed risk). Points above the line ",
        "indicate overprediction; points below indicate underprediction."
      ),
      style = "Normal"
    )
    
    fig_num <- 2
    for (model_name in names(calibration_plot_files)) {
      plot_file <- calibration_plot_files[[model_name]]
      if (file.exists(plot_file)) {
        cap <- paste0(
          "Figure ", fig_num, ".  Calibration plot (",
          gsub("_", " ", model_name), " model). Points represent deciles of predicted risk, ",
          "with error bars showing 95% confidence intervals around the observed event rate."
        )
        doc <- body_add_par(doc, cap, style = "Normal")
        doc <- body_add_img(doc, src = plot_file, width = 5, height = 3.5)
        doc <- body_add_par(doc, "", style = "Normal")
        fig_num <- fig_num + 1
      }
    }
  }

  # ---- 9. Expected Calibration Error (ECE) --------------------------------
  if (!is.null(person_level) && "predicted_risk_recalibrated" %in% names(person_level)) {
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "", style = "Normal")
    doc <- body_add_par(doc, "9.  Expected Calibration Error", style = "heading 2")
    
    # Compute ECE
    y <- person_level$outcome
    p <- person_level$predicted_risk_recalibrated
    ece_result <- .compute_ece(y, p, n_bins = 10)
    ece_value <- ece_result$ece
    
    doc <- body_add_par(doc,
      paste0(
        "Expected calibration error (ECE) quantifies the average absolute difference between predicted ",
        "and observed risk probabilities across deciles of risk. For the recalibrated model, ECE = ",
        round(ece_value, 4), ", indicating ",
        if (ece_value < 0.05) "excellent" else if (ece_value < 0.10) "good" else "moderate",
        " calibration."
      ),
      style = "Normal"
    )
    doc <- body_add_par(doc, "", style = "Normal")
  }

  # ---- 10. Discussion & Conclusion -----------------------------------------
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "10.  Discussion and Conclusion", style = "heading 2")
  doc <- body_add_par(doc,
    paste0(
      "This external validation demonstrates the applicability of the Iannuzzi 2020 NHD integer risk score ",
      "and the Subramaniam mFI-5 to an OMOP CDM v5.4 dataset. All score components were successfully mapped ",
      "to OMOP standard concept IDs using transparent, scriptable SQL against the concept_ancestor and concept ",
      "tables. The target cohort comprised adult patients undergoing inpatient major lower-extremity amputation, ",
      "and the non-home discharge (NHD) outcome was ascertained from visit_occurrence.discharged_to_concept_id ",
      "using a dynamic vocabulary query at runtime. Performance metrics indicate ",
      if (!is.null(metrics)) {
        auroc <- metrics$value[metrics$metric == "AUROC" & metrics$model == "lookup"]
        if (length(auroc) > 0 && !is.na(auroc[1])) {
          if (auroc[1] > 0.75) "promising discriminative and calibration properties"
          else if (auroc[1] > 0.60) "moderate discriminative and calibration properties"
          else "modest discriminative properties that warrant further investigation"
        } else "good performance"
      } else "reasonable",
      ", supporting the continued evaluation of these scores as perioperative clinical decision-support ",
      "tools for patients undergoing major lower-extremity amputation."
    ),
    style = "Normal"
  )

  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc,
    paste0(
      "The fully reproducible workflow — implemented as executable R scripts with SqlRender-parameterised ",
      "cohort SQL — enables validation teams to audit all cohort inclusion criteria, concept mappings, ",
      "lookback windows, and statistical calculations end-to-end. This transparency aligns with OHDSI ",
      "best practices for network studies and external validation. Future steps include applying this ",
      "validated pipeline to de-identified real-world vascular surgery registry data."
    ),
    style = "Normal"
  )

  # ---- Write output -------------------------------------------------------
  out_path <- file.path(output_dir, "ssi_validation_report.docx")
  print(doc, target = out_path)
  message("Report written to: ", normalizePath(out_path))
  invisible(out_path)
}


# ===========================================================================
# .report_prognostic()
#
# Full manuscript-format Word report. Parameterized by config$score_type
# ("integer" | "lasso") with 4 branches.
# Called by generate_manuscript_report() in R/report_extended.R.
#
# Renamed from generate_manuscript_report() in the original synthea-omop-template
# monolith.  All 4 parameterization branches have been applied; the rest of
# the body is verbatim.
#
# RENDER ONLY — NO DATABASE (see charon's "Multi-Repo Analysis Pipeline" section, https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline).
#
# This function used to take `connection_details` and query the CDM live for
# Table 1, Table 2, Figure 1, the cdm_source metadata line, three supplemental
# tables, and a PHI-bearing edge-case export. It no longer does, and the
# argument is gone rather than merely unused: an absent parameter is a
# guarantee the compiler enforces, whereas an ignored one is a comment the
# next contributor can quietly re-wire.
#
# Everything it needs now arrives as CSV artifacts in `report_inputs_dir`,
# written by extract_report_inputs() where the data lives. This function must
# stay runnable on a laptop with no VPN, no credentials, and no driver — if you
# find yourself wanting a connection here, the query belongs in
# R/extract_report_inputs.R and its result belongs in a new artifact.
#
# @param report_inputs_dir Directory of extract artifacts. NULL is permitted and
#   means "no artifacts": every affected table degrades to its documented "N/A"
#   state, exactly as it did when a connection was unavailable before.
# ===========================================================================
.report_prognostic <- function(output_dir        = "output",
                                       score_output_dir   = "output",
                                       mfi5_output_dir    = NULL,
                                       vqifs_output_dir   = NULL,
                                       report_inputs_dir  = NULL,
                                       config             = NULL,
                                       citations          = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  temp_figure_dir <- tempfile("report_figures_")
  dir.create(temp_figure_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_figure_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # ---------------------------------------------------------------------------
  # Section and supplemental-label counters.
  #
  # Section numbers used to be hardcoded into each heading string, which drifted
  # out of order as sections were added (the decision-curve section rendered as
  # "2.8" between "2.5" and "2.6") and double-numbered against the Word
  # template's own automatic heading numbering. Supplemental labels had the same
  # problem: "Supplemental Figure S6" was hardcoded inside a loop, so two
  # figures shared the label and a third got none.
  #
  # These closures are the single source of numbering for the whole document.
  # Call them in render order; never hardcode a number in a heading or caption.
  # ---------------------------------------------------------------------------
  .sec_major <- 0L   # level-2 heading counter  (1. Methods, 2. Results, ...)
  .sec_minor <- 0L   # level-3 heading counter, reset by each major section
  .supp_tbl  <- 0L   # Supplemental Table  S<n>
  .supp_fig  <- 0L   # Supplemental Figure S<n>

  # Start a new major section: "1. Methods". Resets the minor counter.
  section_major <- function(title) {
    .sec_major <<- .sec_major + 1L
    .sec_minor <<- 0L
    paste0(.sec_major, ". ", title)
  }
  # Next subsection within the current major section: "1.3. Risk score evaluation".
  section_num <- function(title) {
    .sec_minor <<- .sec_minor + 1L
    paste0(.sec_major, ".", .sec_minor, ". ", title)
  }
  # ---------------------------------------------------------------------------
  # Supplemental label plan.
  #
  # Supplemental tables and figures share ONE S-number sequence (S1 = a table,
  # S5 = a figure, ...), matching the established convention for this report.
  #
  # Labels are pre-allocated here rather than counted at render time for two
  # reasons: the Methods section cross-references subgroup tables/figures that
  # are not rendered until much later, and the subgroup sections physically
  # render before the "Supplemental Material" section even though readers
  # expect S1 to be the CDM metadata table. A pre-computed plan gives unique,
  # stable labels that both the cross-reference and the render site agree on.
  #
  # Which entries exist depends on which score pipelines supplied output, so the
  # plan is built after the pipeline outputs are loaded (see .init_supp_labels()
  # below). Look up a label with supp("<key>").
  #
  # A KEY MUST ONLY BE ALLOCATED IF ITS ITEM WILL ACTUALLY RENDER.
  # Allocating one for an item that then does not render produces a dangling
  # cross-reference: the Methods promises "Supplemental Table S9" and no S9
  # exists. That is exactly what happened on Duke data, where the subgroup keys
  # were gated on "did the mFI-5 score run?" while the render site was gated on
  # "does subgroup_bias.csv exist?" — two different questions with two different
  # answers. Gate both on the same predicate; see .read_subgroup_bias() below.
  # ---------------------------------------------------------------------------
  .supp_labels <- list()

  # ---------------------------------------------------------------------------
  # .read_subgroup_bias()
  #
  # Single source of truth for "is there a usable subgroup bias analysis for
  # this score?". Returns the parsed data frame, or NULL.
  #
  # Both the supplemental-label plan and add_subgroup_section() call this, so
  # the label allocation and the rendering decision cannot disagree. Do not
  # reimplement the file/parse/row check anywhere else — that divergence is the
  # bug this exists to prevent.
  #
  # NULL when the analysis did not run on THIS dataset for any reason: the
  # scoring step skipped it, the CSV is unreadable, or every subgroup fell below
  # the event-suppression floor. In all of those cases the report must render no
  # subgroup section, no caption, and no Methods cross-reference to one.
  # ---------------------------------------------------------------------------
  .read_subgroup_bias <- function(dir_path) {
    if (is.null(dir_path)) return(NULL)
    bias_path <- file.path(dir_path, "subgroup_bias.csv")
    if (!file.exists(bias_path)) return(NULL)
    df <- tryCatch(readr::read_csv(bias_path, show_col_types = FALSE),
                   error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df
  }

  .init_supp_labels <- function(has_mfi5, has_vqifs, has_mfi5_bias) {
    # The mFI-5 subgroup bias table/figures were promoted to main-text
    # Table/Figure numbering 2026-09-13 (were "Supplemental Table/Figure S#")
    # -- see the hardcoded "Table 7"/"Figure 6"/"Figure 7" captions at their
    # render site below, and the Methods cross-reference in "Subgroup
    # analysis and bias assessment" above. No key is allocated for them here
    # any more; has_mfi5_bias still gates whether that section renders at
    # all (used directly at the render site), just not the S-numbering.
    keys <- c(
      # "Supplemental Material" section, in render order
      "cdm_metadata", "concept_set_inventory", "model_descriptions", "cpt_codes", "discharge_codes",
      "nhd_rate_by_month", "score_dist_iannuzzi",
      if (has_mfi5)  "score_dist_mfi5",
      if (has_vqifs) "score_dist_vqifs"
    )
    .supp_labels <<- stats::setNames(paste0("S", seq_along(keys)), keys)
  }

  # Return the bare label ("S7") or a prefixed one ("Supplemental Table S7").
  supp <- function(key, kind = NULL) {
    id <- .supp_labels[[key]]
    if (is.null(id)) id <- "S?"   # never abort a report over a label
    if (is.null(kind)) id else paste0("Supplemental ", kind, " ", id)
  }
  supp_table  <- function(key) supp(key, "Table")
  supp_figure <- function(key) supp(key, "Figure")

  make_doc_run <- function(text, bold = FALSE, font_size = 10) {
    officer::ftext(
      text,
      officer::fp_text(bold = bold, font.size = font_size, font.family = "Calibri")
    )
  }

  add_doc_caption <- function(doc, title, details = NULL) {
    caption_runs <- list(make_doc_run(title, bold = TRUE, font_size = 10))
    if (!is.null(details) && nzchar(details)) {
      caption_runs[[length(caption_runs) + 1L]] <- make_doc_run(
        paste0(" ", details), bold = FALSE, font_size = 10
      )
    }
    caption_par <- do.call(
      officer::fpar,
      c(caption_runs, list(fp_p = officer::fp_par(text.align = "left")))
    )
    officer::body_add_fpar(doc, value = caption_par, style = "Normal")
  }

  # NOTE (2026-08-11): person_level_scores.csv is no longer read here. Every
  # figure and table that used it now renders from the agg_*.csv artifacts
  # written by pad-amp-nhd-val's R/aggregate_report_inputs.R, so this repo
  # needs no patient-level data at all. covariate_summary.csv and metrics.csv
  # are already aggregate (per-covariate counts; model-level metrics).
  covariate_summary_path <- file.path(score_output_dir, "covariate_summary.csv")
  metrics_path <- file.path(score_output_dir, "metrics.csv")
  lookup_calibration_plot <- file.path(score_output_dir, "calibration_lookup.png")
  recalibrated_calibration_plot <- file.path(score_output_dir, "calibration_recalibrated.png")
  calibration_table_lookup_path <- file.path(score_output_dir, "calibration_table_lookup.csv")
  calibration_table_recalibrated_path <- file.path(score_output_dir, "calibration_table_recalibrated.csv")
  lookup_calibration_plot_temp <- file.path(temp_figure_dir, "calibration_lookup.png")
  recalibrated_calibration_plot_temp <- file.path(temp_figure_dir, "calibration_recalibrated.png")

  if (!file.exists(covariate_summary_path) || !file.exists(metrics_path)) {
    stop("Missing one or more required pipeline outputs in ", score_output_dir)
  }

  # ---------------------------------------------------------------------------
  # REPORT INPUT READERS  (Phase 0 — the render half of the extract/render split)
  #
  # These replace three fetch_*_from_omop() closures that used to live here and
  # queried the CDM directly. The SQL moved verbatim to R/extract_report_inputs.R;
  # nothing below opens a connection, and .report_prognostic() no longer takes a
  # connection_details argument, so it cannot.
  #
  # NULL SEMANTICS ARE THE CONTRACT. The old fetchers returned NULL when a query
  # failed, and every call site downstream guards with is.null(). The extract
  # step preserves that by writing no file for a failed query, so:
  #
  #     file absent      -> NULL          (query failed / never ran)
  #     header-only file -> 0-row df      (query succeeded, found nothing)
  #
  # Those two are different facts and must stay distinguishable. Do not "helpfully"
  # collapse an empty file to NULL.
  #
  # check.names = FALSE is required: the extract half preserves whatever column
  # casing DatabaseConnector produced, and the lookups below are case-insensitive
  # against those exact names.
  # ---------------------------------------------------------------------------
  .report_inputs_dir <- report_inputs_dir

  # col_classes: named vector, e.g. c(nubc_code = "character"), for columns
  # that must not go through read.csv()'s automatic type inference. Base R's
  # write.csv()/read.csv() round-trip does NOT preserve "this was a string" —
  # a column of source codes that all happen to look numeric ("01", "03")
  # comes back as integer with the leading zero silently dropped, quoting in
  # the CSV notwithstanding. Found via a byte-for-byte docx diff against a
  # pre-split render (Supplemental Table S4 nubc_code "01"/"03" -> "1"/"3");
  # not caught by nhd-val's own Phase 0 verification only because that
  # dataset's supp_discharge_destinations extract happened to be 0 rows.
  read_report_input <- function(name, col_classes = NULL) {
    if (is.null(.report_inputs_dir)) return(NULL)
    path <- file.path(.report_inputs_dir, paste0(name, ".csv"))
    if (!file.exists(path)) return(NULL)
    # read.csv(colClasses = NULL) errors (rep_len(NULL, cols)) — NOT the same
    # as omitting the argument, so the two-branch call is load-bearing, not
    # a style choice.
    tryCatch(
      if (is.null(col_classes)) {
        utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                        colClasses = col_classes)
      },
      error = function(e) {
        message("[report] could not read ", basename(path), ": ", conditionMessage(e))
        NULL
      }
    )
  }

  covariate_summary <- read.csv(covariate_summary_path, stringsAsFactors = FALSE)
  metrics <- read.csv(metrics_path, stringsAsFactors = FALSE)
  names(metrics) <- tolower(names(metrics))

  # --- mFI-5 pipeline outputs (loaded when mfi5_output_dir is supplied) ------
  person_level_mfi5      <- NULL
  covariate_summary_mfi5 <- NULL
  metrics_mfi5           <- NULL

  if (!is.null(mfi5_output_dir) && dir.exists(mfi5_output_dir)) {
    mfi5_cs_path  <- file.path(mfi5_output_dir, "covariate_summary.csv")
    mfi5_met_path <- file.path(mfi5_output_dir, "metrics.csv")
    if (file.exists(mfi5_cs_path))  covariate_summary_mfi5 <- read.csv(mfi5_cs_path,  stringsAsFactors = FALSE)
    if (file.exists(mfi5_met_path)) {
      metrics_mfi5 <- read.csv(mfi5_met_path, stringsAsFactors = FALSE)
      if (!"ci_lower" %in% names(metrics_mfi5)) metrics_mfi5$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics_mfi5)) metrics_mfi5$ci_upper <- NA_real_
    }
    message("[report] mFI-5 pipeline outputs loaded from: ", mfi5_output_dir)
  }

  # --- sVQI-FS pipeline outputs (loaded when vqifs_output_dir is supplied) ---
  person_level_vqifs      <- NULL
  covariate_summary_vqifs <- NULL
  metrics_vqifs           <- NULL

  if (!is.null(vqifs_output_dir) && dir.exists(vqifs_output_dir)) {
    vqifs_cs_path  <- file.path(vqifs_output_dir, "covariate_summary.csv")
    vqifs_met_path <- file.path(vqifs_output_dir, "metrics.csv")
    if (file.exists(vqifs_cs_path))  covariate_summary_vqifs <- read.csv(vqifs_cs_path,  stringsAsFactors = FALSE)
    if (file.exists(vqifs_met_path)) {
      metrics_vqifs <- read.csv(vqifs_met_path, stringsAsFactors = FALSE)
      if (!"ci_lower" %in% names(metrics_vqifs)) metrics_vqifs$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics_vqifs)) metrics_vqifs$ci_upper <- NA_real_
    }
    message("[report] sVQI-FS pipeline outputs loaded from: ", vqifs_output_dir)
  }

  # Presence flags. These used to be `!is.null(person_level_mfi5)` -- i.e. "did
  # that score's person-level file load?". Since 2026-08-11 the report reads no
  # person-level file at all, so presence is keyed off each score's metrics.csv
  # instead, which is aggregate and is written by the same scoring run.
  has_mfi5  <- !is.null(metrics_mfi5)
  has_vqifs <- !is.null(metrics_vqifs)

  # Does a usable mFI-5 subgroup bias analysis exist for THIS dataset? Resolved
  # here, before the label plan is fixed, because the Methods cross-reference
  # and the S-number allocation both depend on it. Read once and reused at the
  # render site so the two cannot drift apart.
  subgroup_bias_mfi5 <- .read_subgroup_bias(mfi5_output_dir)
  has_mfi5_bias      <- !is.null(subgroup_bias_mfi5)
  if (has_mfi5  && !has_mfi5_bias) {
    message("[report] No usable subgroup_bias.csv for mFI-5 in ",
            if (is.null(mfi5_output_dir)) "<none>" else mfi5_output_dir,
            " — the subgroup bias section, its supplemental table/figure, and the ",
            "Methods cross-reference to them are all omitted for this dataset.")
  }

  # Now that we know which score pipelines produced output, fix the supplemental
  # S-number plan. Everything downstream refers to labels via supp()/supp_table()
  # /supp_figure() so Methods cross-references and render sites cannot diverge.
  .init_supp_labels(
    has_mfi5      = has_mfi5,
    has_vqifs     = has_vqifs,
    has_mfi5_bias = has_mfi5_bias
  )

  # --- Temporal split metadata (written by evaluate_integer_risk_score()) ----
  # split_info.csv contains split_date, n_train, n_test for each model.
  # Used to populate the methods sentence and Table 4 caption.
  split_info_iannuzzi <- NULL
  split_info_mfi5     <- NULL
  split_info_vqifs    <- NULL
  si_path_iannuzzi    <- file.path(score_output_dir, "split_info.csv")
  if (file.exists(si_path_iannuzzi)) {
    split_info_iannuzzi <- read.csv(si_path_iannuzzi, stringsAsFactors = FALSE)
  }
  if (!is.null(mfi5_output_dir)) {
    si_path_mfi5 <- file.path(mfi5_output_dir, "split_info.csv")
    if (file.exists(si_path_mfi5)) {
      split_info_mfi5 <- read.csv(si_path_mfi5, stringsAsFactors = FALSE)
    }
  }
  if (!is.null(vqifs_output_dir)) {
    si_path_vqifs <- file.path(vqifs_output_dir, "split_info.csv")
    if (file.exists(si_path_vqifs)) {
      split_info_vqifs <- read.csv(si_path_vqifs, stringsAsFactors = FALSE)
    }
  }

  # Helper: build a one-sentence split description for the methods section.
  # Returns NULL when no split was applied (pre-existing outputs without split).
  # Build the split-sample-size sentence. model_label = NULL renders the shared
  # form: the temporal split is identical across all scores (same patients, same
  # index dates), so the report states it once rather than repeating a
  # near-verbatim sentence per model.
  split_sentence <- function(si, model_label = NULL) {
    if (is.null(si) || nrow(si) == 0) return(NULL)
    prefix <- if (is.null(model_label)) "\tPatients" else sprintf("\tFor %s, patients", model_label)
    sprintf(
      paste0("%s were sorted chronologically by index date and ",
             "divided into a training set (n = %d; through %s) ",
             "used only to fit the recalibration models and a test set ",
             "(n = %d; after %s) on which the recalibrated and raw-score ",
             "specifications were evaluated. The same split applies to every score."),
      prefix,
      si$n_train[1], si$split_date[1],
      si$n_test[1],  si$split_date[1]
    )
  }

  # Backfill ECE if it is not present in the metrics file.
  compute_ece <- function(y, p, n_bins = 10) {
    ok <- !(is.na(y) | is.na(p))
    y <- as.numeric(y[ok])
    p <- as.numeric(p[ok])
    if (length(y) == 0) {
      return(NA_real_)
    }

    eps <- 1e-6
    p[p < eps] <- eps
    p[p > (1 - eps)] <- 1 - eps

    probs <- unique(stats::quantile(p, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
    if (length(probs) < 3) {
      probs <- c(0, 1)
    }
    bins <- cut(p, breaks = probs, include.lowest = TRUE)
    bin_n <- as.numeric(table(bins))
    if (length(bin_n) == 0) {
      return(NA_real_)
    }

    pred_mean <- tapply(p, bins, mean)
    obs_mean <- tapply(y, bins, mean)
    as.numeric(sum(abs(pred_mean - obs_mean) * bin_n) / sum(bin_n))
  }

  if (!any(metrics$metric == "ECE")) {
    ece_rows <- data.frame(
      metric = character(), value = numeric(),
      ci_lower = numeric(), ci_upper = numeric(),
      model = character(), stringsAsFactors = FALSE
    )

    # AGGREGATE-ONLY (2026-08-11). ECE was computed here from patient-level
    # outcome/prediction vectors. It is now derived from the calibration TABLES
    # the scoring step already writes (calibration_table_lookup.csv /
    # _recalibrated.csv), which carry per-bin predicted and observed rates --
    # exactly the quantities ECE averages over. Bin sizes are used as weights
    # when the table carries them; otherwise bins are weighted equally, which
    # is what an unweighted table can support.
    .ece_from_table <- function(path) {
      if (!file.exists(path)) return(NA_real_)
      tb <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE),
                     error = function(e) NULL)
      if (is.null(tb) || nrow(tb) == 0) return(NA_real_)
      if (!all(c("predicted", "observed") %in% names(tb))) return(NA_real_)
      w <- if ("n" %in% names(tb)) suppressWarnings(as.numeric(tb$n)) else rep(1, nrow(tb))
      if (all(is.na(w)) || sum(w, na.rm = TRUE) == 0) w <- rep(1, nrow(tb))
      pr <- suppressWarnings(as.numeric(tb$predicted))
      ob <- suppressWarnings(as.numeric(tb$observed))
      ok <- !is.na(pr) & !is.na(ob) & !is.na(w)
      if (!any(ok)) return(NA_real_)
      sum(w[ok] * abs(pr[ok] - ob[ok])) / sum(w[ok])
    }
    for (.spec in list(list(p = calibration_table_lookup_path,       m = "lookup"),
                       list(p = calibration_table_recalibrated_path, m = "recalibrated"))) {
      .v <- .ece_from_table(.spec$p)
      if (!is.na(.v)) {
        ece_rows <- rbind(ece_rows, data.frame(
          metric = "ECE", value = .v, ci_lower = NA_real_, ci_upper = NA_real_,
          model = .spec$m, stringsAsFactors = FALSE
        ))
      }
    }

    if (nrow(ece_rows) > 0) {
      # Ensure metrics has ci_lower/ci_upper before binding, so column sets match.
      if (!"ci_lower" %in% names(metrics)) metrics$ci_lower <- NA_real_
      if (!"ci_upper" %in% names(metrics)) metrics$ci_upper <- NA_real_
      metrics <- rbind(metrics, ece_rows)
    }
  } else {
    # Ensure ci_lower/ci_upper exist even when ECE was already present in the CSV.
    if (!"ci_lower" %in% names(metrics)) metrics$ci_lower <- NA_real_
    if (!"ci_upper" %in% names(metrics)) metrics$ci_upper <- NA_real_
  }

  # add_doc_page_break(): inserts a manual Word page break.
  add_doc_page_break <- function(doc) {
    officer::body_add_break(doc)
  }

  fmt <- function(x, digits = 3) {
    format(round(as.numeric(x), digits), nsmall = digits)
  }

  metric_value <- function(metric_name, model_name) {
    row <- metrics[metrics$metric == metric_name & metrics$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) {
      return(NA_real_)
    }
    as.numeric(row$value[1])
  }

  # Returns a formatted "(lower–upper)" CI string for a given metric/model pair.
  # Pulls ci_lower and ci_upper from the metrics data frame (present when
  # compute_bootstrap_cis() was run during the pipeline).  Returns "—" when the
  # columns are absent or the values are NA (e.g. legacy metrics.csv files).
  metric_ci <- function(metric_name, model_name) {
    row <- metrics[metrics$metric == metric_name & metrics$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  # Metric lookup helpers for the mFI-5 model (reads from metrics_mfi5).
  metric_value_mfi5 <- function(metric_name, model_name) {
    if (is.null(metrics_mfi5)) return(NA_real_)
    row <- metrics_mfi5[metrics_mfi5$metric == metric_name & metrics_mfi5$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return(NA_real_)
    as.numeric(row$value[1])
  }

  metric_ci_mfi5 <- function(metric_name, model_name) {
    if (is.null(metrics_mfi5)) return("\u2014")
    row <- metrics_mfi5[metrics_mfi5$metric == metric_name & metrics_mfi5$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  # Metric lookup helpers for the sVQI-FS model (reads from metrics_vqifs).
  metric_value_vqifs <- function(metric_name, model_name) {
    if (is.null(metrics_vqifs)) return(NA_real_)
    row <- metrics_vqifs[metrics_vqifs$metric == metric_name & metrics_vqifs$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return(NA_real_)
    as.numeric(row$value[1])
  }

  metric_ci_vqifs <- function(metric_name, model_name) {
    if (is.null(metrics_vqifs)) return("\u2014")
    row <- metrics_vqifs[metrics_vqifs$metric == metric_name & metrics_vqifs$model == model_name, , drop = FALSE]
    if (nrow(row) == 0) return("\u2014")
    if (!all(c("ci_lower", "ci_upper") %in% names(row))) return("\u2014")
    lo <- as.numeric(row$ci_lower[1])
    hi <- as.numeric(row$ci_upper[1])
    if (is.na(lo) || is.na(hi)) return("\u2014")
    paste0("(", fmt(lo), "\u2013", fmt(hi), ")")
  }

  # Cohort totals from agg_cohort_summary.csv (was nrow()/sum() over the
  # person-level frame).
  .cohort_sum <- read_report_input("agg_cohort_summary")
  .cs_row <- if (!is.null(.cohort_sum) && "score_id" %in% names(.cohort_sum)) {
    r <- .cohort_sum[.cohort_sum$score_id == "iannuzzi", , drop = FALSE]
    if (nrow(r) == 0) .cohort_sum[1, , drop = FALSE] else r
  } else NULL
  n_target  <- if (!is.null(.cs_row)) as.integer(.cs_row$n_total[1])  else NA_integer_
  n_outcome <- if (!is.null(.cs_row)) as.integer(.cs_row$n_events[1]) else NA_integer_
  outcome_prev <- if (n_target > 0) 100 * n_outcome / n_target else NA_real_

  results_tbl <- data.frame(
    Metric = c(
      "AUROC",
      "AUPRC",
      "Brier score",
      "Estimated calibration error",
      "Calibration intercept",
      "Calibration slope"
    ),
    Value = c(
      fmt(metric_value("AUROC", "lookup")),
      fmt(metric_value("AUPRC", "lookup")),
      fmt(metric_value("Brier", "lookup")),
      fmt(metric_value("ECE", "lookup")),
      fmt(metric_value("CalibrationIntercept", "lookup")),
      fmt(metric_value("CalibrationSlope", "lookup"))
    ),
    "95% CI" = c(
      metric_ci("AUROC",                "lookup"),
      metric_ci("AUPRC",                "lookup"),
      metric_ci("Brier",                "lookup"),
      metric_ci("ECE",                  "lookup"),
      metric_ci("CalibrationIntercept", "lookup"),
      metric_ci("CalibrationSlope",     "lookup")
    ),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )

  normalize_label <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("[()\\[\\]\\{\\}]", " ", x)
    x <- gsub("[^a-z0-9]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  # Branch 1 — Table 2 covariate/predictor definitions.
  # LASSO: read from varImp.rds artifact via .covariate_table_data_lasso().
  # Integer: use the static component specification table .covariate_table_data().
  tbl2_data <- if (identical(config$score_type, "lasso")) {
    .covariate_table_data_lasso(config)
  } else {
    .covariate_table_data()
  }
  # Build predictor_ref from whichever table was selected above.
  # Column names differ: integer has "points"; LASSO has "weight".
  if ("points" %in% names(tbl2_data) && "covariate_id" %in% names(tbl2_data)) {
    predictor_ref <- tbl2_data[, c("covariate_id", "variable", "points", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  } else if ("weight" %in% names(tbl2_data) && "covariate_id" %in% names(tbl2_data)) {
    predictor_ref <- tbl2_data[, c("covariate_id", "variable", "weight", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  } else {
    predictor_ref <- .covariate_table_data()[, c("covariate_id", "variable", "points", "lookback", "derivation")]
    names(predictor_ref) <- c("covariate_id", "Predictor", "Points", "Lookback", "Definition")
  }

  has_missing_col <- "n_missing" %in% names(covariate_summary)

  if ("covariate_id" %in% names(covariate_summary)) {
    keep_cols <- c("covariate_id", "n_positive", "mean_points",
                   if (has_missing_col) "n_missing")
    covariate_act <- covariate_summary[, keep_cols, drop = FALSE]
    predictor_tbl <- merge(predictor_ref, covariate_act, by = "covariate_id", all.x = TRUE, sort = FALSE)
  } else {
    predictor_ref$key <- normalize_label(predictor_ref$Predictor)
    keep_cols <- c("covariate_name", "n_positive", "mean_points",
                   if (has_missing_col) "n_missing")
    covariate_act <- covariate_summary[, keep_cols, drop = FALSE]
    covariate_act$key <- normalize_label(covariate_act$covariate_name)
    covariate_act <- covariate_act[, c("key", "n_positive", "mean_points",
                                       if (has_missing_col) "n_missing"), drop = FALSE]
    predictor_tbl <- merge(predictor_ref, covariate_act, by = "key", all.x = TRUE, sort = FALSE)
  }

  predictor_tbl$n_positive[is.na(predictor_tbl$n_positive)] <- 0
  predictor_tbl$mean_points[is.na(predictor_tbl$mean_points)] <- 0
  if (has_missing_col) predictor_tbl$n_missing[is.na(predictor_tbl$n_missing)] <- 0

  base_cols <- c("Predictor", "Points", "Lookback", "Definition", "n_positive", "mean_points")
  if (has_missing_col) base_cols <- c(base_cols, "n_missing")
  predictor_tbl <- predictor_tbl[, base_cols]

  if (has_missing_col) {
    names(predictor_tbl) <- c("Predictor", "Points", "Lookback", "Definition",
                               "PositiveCount", "MeanPoints", "MissingCount")
    predictor_tbl$MissingCount <- as.integer(predictor_tbl$MissingCount)
    predictor_tbl$MissingPct   <- paste0(
      round(100 * predictor_tbl$MissingCount / max(n_target, 1), 1), "%")
  } else {
    names(predictor_tbl) <- c("Predictor", "Points", "Lookback", "Definition",
                               "PositiveCount", "MeanPoints")
  }
  predictor_tbl$PositiveCount <- as.integer(predictor_tbl$PositiveCount)
  predictor_tbl$MeanPoints <- round(as.numeric(predictor_tbl$MeanPoints), 4)

  fmt_n_pct <- function(n, denom, digits = 1) {
    n <- suppressWarnings(as.numeric(n))
    denom <- suppressWarnings(as.numeric(denom))
    if (is.na(n) || is.na(denom) || denom <= 0) {
      return("0 (0.0%)")
    }
    paste0(format(round(n, 0), scientific = FALSE, trim = TRUE),
           " (", format(round(100 * n / denom, digits), nsmall = digits, trim = TRUE), "%)")
  }


  # Rebuilds the named list fetch_demographics_from_omop() used to return.
  # Returns NULL only when NONE of the six parts is present, matching the old
  # behaviour where a failed connection produced a single NULL rather than a
  # list of NULLs.
  read_demographics <- function() {
    parts <- c("age", "sex", "race", "ethnicity", "indication", "procedure_type")
    out <- stats::setNames(lapply(parts, function(p) read_report_input(paste0("demographics_", p))),
                           parts)
    if (all(vapply(out, is.null, logical(1)))) return(NULL)
    out
  }

  # Rebuilds fetch_nhd_outcomes_from_omop()'s list: six scalars plus dest_df.
  # The scalars round-trip through a one-row CSV; NA survives, and the integer
  # columns are re-coerced because read.csv widens an all-NA column to logical.
  read_nhd_outcomes <- function() {
    s <- read_report_input("nhd_outcomes_scalars")
    if (is.null(s) || nrow(s) == 0) return(NULL)
    list(
      n_nhd         = as.integer(s$n_nhd[1]),
      dest_df       = read_report_input("nhd_outcomes_destinations"),
      median_los    = as.numeric(s$median_los[1]),
      los_p25       = as.numeric(s$los_p25[1]),
      los_p75       = as.numeric(s$los_p75[1]),
      n_readmission = as.integer(s$n_readmission[1]),
      n_death       = as.integer(s$n_death[1])
    )
  }

  # Rebuilds fetch_discharge_types_from_omop()'s data frame (subject_id,
  # discharge_type). Merged into person_level by subject_id downstream, so the
  # column must come back as an integer, not a character.
  read_discharge_types <- function() {
    df <- read_report_input("discharge_types")
    if (is.null(df) || nrow(df) == 0) return(df)
    if ("subject_id" %in% names(df)) df$subject_id <- as.integer(df$subject_id)
    df
  }

  append_distribution_rows <- function(tbl, dist_df, label_prefix, denom, max_rows = 6L) {
    if (is.null(dist_df) || nrow(dist_df) == 0) {
      return(tbl)
    }

    n_col <- names(dist_df)[tolower(names(dist_df)) == "n"][1]
    cat_col <- names(dist_df)[tolower(names(dist_df)) == "category"][1]
    if (is.na(n_col) || is.na(cat_col)) {
      return(tbl)
    }

    dist_df$n_value <- as.numeric(dist_df[[n_col]])
    dist_df$cat_value <- as.character(dist_df[[cat_col]])
    keep_n <- min(nrow(dist_df), max_rows)
    shown <- dist_df[seq_len(keep_n), , drop = FALSE]

    for (i in seq_len(nrow(shown))) {
      tbl <- rbind(
        tbl,
        data.frame(
          Item = paste0(label_prefix, " - ", shown$cat_value[i]),
          Value = fmt_n_pct(shown$n_value[i], denom),
          Definition = paste0(
            "Distribution among target-cohort patients based on OMOP person ",
            if (label_prefix == "Race") "race_concept_id" else if (label_prefix == "Ethnicity") "ethnicity_concept_id" else "gender_concept_id",
            "."
          ),
          stringsAsFactors = FALSE
        )
      )
    }

    if (nrow(dist_df) > keep_n) {
      other_n <- sum(as.numeric(dist_df$n_value[(keep_n + 1):nrow(dist_df)]), na.rm = TRUE)
      tbl <- rbind(
        tbl,
        data.frame(
          Item = paste0(label_prefix, " - Other"),
          Value = fmt_n_pct(other_n, denom),
          Definition = "Combined frequency of remaining categories not shown individually.",
          stringsAsFactors = FALSE
        )
      )
    }

    tbl
  }

  # Phase 0: `connection_details` dropped from the signature. Demographics now
  # come from the extract artifacts via read_demographics(), which returns the
  # same named list this function already expected.
  build_table1_cohort <- function(n_target, n_outcome, config) {

    # Helper: build one row; is_header=TRUE makes the row a section label
    row1 <- function(char, val = "", header = FALSE) {
      data.frame(
        Characteristic = char,
        Value          = val,
        is_header      = header,
        stringsAsFactors = FALSE
      )
    }
    sub_row <- function(label, n, denom) {
      row1(paste0("    ", label), fmt_n_pct(n, denom))
    }
    # Look up count for a fixed OMOP concept_id in a distribution data frame
    # returned by distribution_sql() (columns: CONCEPT_ID, CATEGORY, N).
    # All matching is done on the integer concept_id — no string/regex logic.
    lookup_concept <- function(df, cid) {
      if (is.null(df) || nrow(df) == 0) return(0L)
      n_col  <- names(df)[toupper(names(df)) == "N"][1]
      id_col <- names(df)[toupper(names(df)) == "CONCEPT_ID"][1]
      if (is.na(n_col) || is.na(id_col)) return(0L)
      idx <- which(as.integer(df[[id_col]]) == as.integer(cid))
      if (length(idx) == 0) return(0L)
      sum(as.integer(df[[n_col]][idx]), na.rm = TRUE)
    }
    # Look up count by exact category label (used for indication/procedure rows
    # where the SQL itself sets the category string, so it is stable).
    lookup_n <- function(df, category_value) {
      if (is.null(df) || nrow(df) == 0) return(0L)
      n_col   <- names(df)[toupper(names(df)) == "N"][1]
      cat_col <- names(df)[toupper(names(df)) == "CATEGORY"][1]
      if (is.na(n_col) || is.na(cat_col)) return(0L)
      idx <- which(trimws(df[[cat_col]]) == category_value)
      if (length(idx) == 0) return(0L)
      as.integer(df[[n_col]][idx[1]])
    }

    demog <- read_demographics()

    # ---- Age ------------------------------------------------------------------
    # AGGREGATE-ONLY (2026-08-11). This previously read demographics_age.csv --
    # one row per patient, every exact age -- purely to compute a median and
    # IQR. Those three numbers are now computed in pad-amp-nhd-val's aggregate
    # step and arrive as a one-row agg_age_summary.csv, so the report never sees
    # an individual age.
    age_row <- row1("Age, median (IQR), years", "N/A")
    age_summary <- read_report_input("agg_age_summary")
    if (!is.null(age_summary) && nrow(age_summary) > 0 &&
        all(c("median", "p25", "p75") %in% names(age_summary))) {
      med <- suppressWarnings(as.numeric(age_summary$median[1]))
      p25 <- suppressWarnings(as.numeric(age_summary$p25[1]))
      p75 <- suppressWarnings(as.numeric(age_summary$p75[1]))
      if (!any(is.na(c(med, p25, p75)))) {
        age_row <- row1(
          "Age, median (IQR), years",
          paste0(as.integer(floor(med)), " (",
                 as.integer(floor(p25)), "\u2013", as.integer(floor(p75)), ")")
        )
      }
    }

    # ---- Sex ------------------------------------------------------------------
    # Standard OMOP Gender domain concept IDs (vocabulary_id = 'Gender'):
    #   8507 = MALE
    #   8532 = FEMALE
    male_n   <- lookup_concept(demog$sex, 8507L)
    female_n <- lookup_concept(demog$sex, 8532L)

    # ---- Race / Ethnicity (four requested categories) ------------------------
    # Standard OMOP Race domain concept IDs (vocabulary_id = 'Race'):
    #   8527 = White
    #   8516 = Black or African American
    #   8515 = Asian
    # Standard OMOP Ethnicity domain concept IDs (vocabulary_id = 'Ethnicity'):
    #   38003563 = Hispanic or Latino
    #   38003564 = Not Hispanic or Latino
    #
    # RACE AND ETHNICITY ARE SEPARATE OMOP DOMAINS AND MUST NOT SHARE A TABLE
    # SECTION (fixed 2026-07-26): Hispanic/Latino (ethnicity_concept_id) was
    # previously rendered as a fourth row under the "Race" header alongside
    # White/Black/Asian (race_concept_id). Since a patient can be e.g. both
    # White (race) AND Hispanic (ethnicity), summing all four rows' percentages
    # as if they were one mutually-exclusive block made the "Race" section sum
    # to well over 100% — not a data error, just a table-layout error that
    # conflated two independent OMOP fields. latino_n now renders in its own
    # "Ethnicity" section below, matching how Race/Ethnicity are reported in
    # Supplemental Table S7 (subgroup bias), which already treats them as
    # distinct subgroup variables.
    white_n   <- lookup_concept(demog$race,      8527L)
    black_n   <- lookup_concept(demog$race,      8516L)
    asian_n   <- lookup_concept(demog$race,      8515L)
    latino_n  <- lookup_concept(demog$ethnicity, 38003563L)
    non_latino_n <- lookup_concept(demog$ethnicity, 38003564L)

    # ---- Indication (PAD/DM/wound, from concept-ancestor rollup before index date) -----
    ind_df  <- demog$indication
    pad_n   <- lookup_n(ind_df, "PAD")
    dm_n    <- lookup_n(ind_df, "Diabetes mellitus")
    wound_n <- lookup_n(ind_df, "LE wound / gangrene")

    # ---- Amputation level -----------------------------------------------------
    pt_df       <- demog$procedure_type
    aka_n       <- lookup_n(pt_df, "Above-knee amputation (AKA)")
    bka_n       <- lookup_n(pt_df, "Below-knee amputation (BKA)")
    other_amp_n <- lookup_n(pt_df, "Other amputation")

    # ---- Assemble table -------------------------------------------------------
    tbl <- rbind(
      age_row,
      # Sex
      row1("Sex", header = TRUE),
      sub_row("Male",   male_n,   n_target),
      sub_row("Female", female_n, n_target),
      # Race (mutually exclusive OMOP race_concept_id categories)
      row1("Race", header = TRUE),
      sub_row("White",             white_n,  n_target),
      sub_row("Black",             black_n,  n_target),
      sub_row("Asian",             asian_n,  n_target),
      # Ethnicity — a SEPARATE OMOP field from race (a patient may be e.g. both
      # White and Hispanic); rendered as its own section rather than folded into
      # Race, which previously made the Race section's percentages sum to over
      # 100% (see the note above white_n/black_n/asian_n/latino_n).
      row1("Ethnicity", header = TRUE),
      sub_row("Hispanic / Latino",     latino_n,     n_target),
      sub_row("Not Hispanic / Latino", non_latino_n, n_target),
      # Indication (PAD, DM, and/or LE wound documented before index; not mutually exclusive)
      row1("Indication (not mutually exclusive)", header = TRUE),
      sub_row("Peripheral arterial disease (PAD)", pad_n,   n_target),
      sub_row("Diabetes mellitus",                  dm_n,    n_target),
      sub_row("Lower-extremity wound / gangrene",   wound_n, n_target),
      # Amputation level (mutually exclusive: AKA > BKA > Other)
      row1("Amputation level", header = TRUE),
      sub_row("Above-knee (AKA)", aka_n,       n_target),
      sub_row("Below-knee (BKA)", bka_n,       n_target),
      sub_row("Other",            other_amp_n, n_target),
      # Outcome
      row1("Outcome", header = TRUE),
      sub_row("Non-home discharge (NHD)", n_outcome, n_target),
      # Total (last row)
      row1("Total cohort", fmt_n_pct(n_target, n_target))
    )

    tbl
  }

  # Use passed-in config; fall back to get_validation_config() for callers that
  # do not supply it. config is still needed here for schema names in captions
  # and for study parameters — it does NOT imply database access.
  if (is.null(config) && exists("get_validation_config", mode = "function")) {
    config <- tryCatch(get_validation_config(), error = function(e) NULL)
  }

  # PHASE 0, DELIBERATE REMOVAL — do not restore.
  #
  # This is where the report used to call build_connection_details(config) to
  # manufacture its own connection when the caller did not pass one. That single
  # fallback is what made the whole render path database-dependent: dropping the
  # connection_details *argument* would have achieved nothing while this stayed,
  # because the report could always rebuild what it was no longer given.
  #
  # The report now consumes artifacts written by extract_report_inputs(). If an
  # artifact is missing, the affected table or figure degrades to its documented
  # "N/A" state — it does not reach for a database.

  cohort_tbl <- build_table1_cohort(n_target, n_outcome, config)

  simple_ft <- function(df) {
    flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 4, part = "all") |>
      autofit()
  }

  # Two-column Table 1 formatter.
  # Expects df with columns: Characteristic, Value, is_header (logical).
  # is_header rows are rendered bold with no indentation; sub-rows are indented.
  # The is_header column is dropped before the flextable is built.
  table1_ft <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
      return(flextable(data.frame(Characteristic = character(), Value = character())))
    }

    header_rows <- which(df$is_header)
    sub_rows    <- which(!df$is_header)
    total_row   <- nrow(df)   # last row is always "Total cohort"

    # Strip the helper column before passing to flextable
    display_df <- df[, c("Characteristic", "Value"), drop = FALSE]

    ft <- flextable(display_df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      align(align = "left",  part = "all") |>
      valign(valign = "top", part = "all") |>
      padding(padding = 3, part = "all") |>
      # Section-label rows: bold, no left indent, light grey background
      bold(i = header_rows, part = "body") |>
      bg(i = header_rows, bg = "#F2F2F2", part = "body") |>
      # Sub-rows: extra left padding to simulate indent
      padding(i = sub_rows, j = "Characteristic", padding.left = 18, part = "body") |>
      # Total row: bold
      bold(i = total_row, part = "body") |>
      # Column widths
      width(j = "Characteristic", width = 2.8) |>
      width(j = "Value",          width = 1.4) |>
      set_table_properties(layout = "fixed")

    ft
  }

  wrapped_definition_ft <- function(df) {
    ft <- flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 10, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 4, part = "all") |>
      align(align = "left", part = "all") |>
      valign(valign = "top", part = "all") |>
      width(j = "Item", width = 1.4) |>
      width(j = "Value", width = 0.9) |>
      width(j = "Definition", width = 4.7) |>
      set_table_properties(layout = "fixed")

    ft
  }

  wrapped_predictor_ft <- function(df) {
    has_missing <- "MissingPct" %in% names(df)
    ft <- flextable(df) |>
      bold(part = "header") |>
      fontsize(size = 9, part = "all") |>
      font(fontname = "Calibri", part = "all") |>
      bg(part = "header", bg = "#1F3864") |>
      color(part = "header", color = "white") |>
      padding(padding = 3, part = "all") |>
      align(align = "left", part = "all") |>
      valign(valign = "top", part = "all") |>
      width(j = "Predictor",    width = 1.3) |>
      width(j = "Points",       width = 0.5) |>
      width(j = "Lookback",     width = 0.75) |>
      width(j = "Definition",   width = if (has_missing) 3.0 else 3.5) |>
      width(j = "PositiveCount", width = 0.75) |>
      width(j = "MeanPoints",   width = 0.7)
    if (has_missing) {
      ft <- ft |>
        width(j = "MissingCount", width = 0.65) |>
        width(j = "MissingPct",   width = 0.65) |>
        set_header_labels(
          MissingCount = "No CDM\nRecord\nn",
          MissingPct   = "No CDM\nRecord\n%"
        )
    }
    ft |> set_table_properties(layout = "fixed")
  }

  # ---------------------------------------------------------------------------
  # save_subgroup_forest_plot()
  #
  # Builds a forest plot of a subgroup performance metric (95% CI) by subgroup
  # from the subgroup_bias data frame.  Each subgroup variable is drawn as a
  # labelled section (via ggplot2 faceting on subgroup_var).  A dashed vertical
  # reference line shows the overall metric value for the assessed model
  # (read from metrics.csv).
  #
  # Arguments:
  #   bias_df       — data frame from subgroup_bias.csv
  #   overall_value — numeric overall metric value for the reference line
  #                   (overall ECE when metric = "ece", overall AUROC when
  #                   metric = "auroc")
  #   output_folder — directory where the PNG will be written
  #   metric        — "ece" (calibration, default) or "auroc" (discrimination).
  #                   Selects which pair of value/ci_lower/ci_upper columns is
  #                   plotted, and the axis label/title/caption wording.
  #
  # Rows with a missing value for the selected metric are dropped before
  # plotting — AUROC can be NA for a subgroup whose outcome happened to be
  # constant within a bootstrap resample even when ECE is not.
  #
  # Returns the path to the saved PNG, or NULL on failure.
  # ---------------------------------------------------------------------------
  save_subgroup_forest_plot <- function(bias_df, overall_value, output_folder,
                                        file_name = "subgroup_forest_plot.png",
                                        metric = c("ece", "auroc")) {

    metric <- match.arg(metric)
    value_col <- if (metric == "auroc") "auroc" else "ece"
    lo_col    <- if (metric == "auroc") "auroc_ci_lower" else "ci_lower"
    hi_col    <- if (metric == "auroc") "auroc_ci_upper" else "ci_upper"

    if (is.null(bias_df) || nrow(bias_df) == 0) return(NULL)

    bias_df <- bias_df[!is.na(bias_df[[value_col]]), , drop = FALSE]
    if (nrow(bias_df) == 0) return(NULL)

    bias_df$.value <- bias_df[[value_col]]
    bias_df$.lo    <- bias_df[[lo_col]]
    bias_df$.hi    <- bias_df[[hi_col]]

    # Impose Table 1 ordering on subgroup facets.
    var_order <- c("age_group", "sex", "race", "ethnicity",
                   "indication", "proc_type", "year")
    present_vars <- var_order[var_order %in% bias_df$subgroup_var]
    extra_vars   <- setdiff(unique(bias_df$subgroup_var), present_vars)
    ordered_vars <- c(present_vars, extra_vars)
    bias_df$subgroup_var <- factor(bias_df$subgroup_var, levels = ordered_vars)

    # Build a combined label: "Sex: Female", "Race: Black", etc.
    bias_df$label <- paste0(
      tools::toTitleCase(gsub("_", " ", as.character(bias_df$subgroup_var))),
      ": ",
      bias_df$subgroup_level
    )

    # Order labels within each facet by the selected metric (ascending) for
    # readability.
    bias_df$label <- factor(
      bias_df$label,
      levels = bias_df$label[order(bias_df$subgroup_var, bias_df$.value)]
    )

    # Facet labels: capitalise the subgroup variable name for display.
    facet_labels <- setNames(
      tools::toTitleCase(gsub("_", " ", levels(bias_df$subgroup_var))),
      levels(bias_df$subgroup_var)
    )

    x_label <- if (metric == "auroc") "AUROC (95% CI)" else "Expected Calibration Error (95% CI)"
    title   <- if (metric == "auroc") "Subgroup Discrimination (AUROC)" else "Subgroup Calibration (ECE)"
    caption <- if (metric == "auroc") {
      paste0(
        "Dashed line = overall AUROC (", round(overall_value, 3), "). ",
        "Groups with < 10 events suppressed. ",
        "CIs from 200 bootstrap resamples."
      )
    } else {
      paste0(
        "Dashed line = overall ECE (", round(overall_value, 3), "). ",
        "Groups with < 10 events suppressed. ",
        "CIs from 200 bootstrap resamples."
      )
    }

    p <- ggplot2::ggplot(bias_df,
           ggplot2::aes(x = .value, y = label)) +
      ggplot2::geom_point(size = 2, colour = "black") +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .lo, xmax = .hi),
        width = 0.25, colour = "grey40", orientation = "y"
      ) +
      ggplot2::geom_vline(
        xintercept = overall_value,
        linetype   = "dashed",
        colour     = "black"
      ) +
      ggplot2::facet_grid(
        subgroup_var ~ .,
        scales   = "free_y",
        space    = "free_y",
        labeller = ggplot2::as_labeller(facet_labels)
      ) +
      ggplot2::labs(
        x     = x_label,
        y     = NULL,
        title = title,
        caption = caption
      ) +
      theme_manuscript() +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        strip.text         = ggplot2::element_text(face = "bold"),
        plot.caption       = ggplot2::element_text(size = 8)
      )

    # Already black/grey40 with no colour mapping — greyscale-safe as-is.
    # Routed through save_figure() only for the 600 dpi TIFF + vector PDF.
    tryCatch({
      save_figure(p, output_folder, file_name,
                 width  = 7,
                 height = max(4, nrow(bias_df) * 0.35 + 1.5))
    }, error = function(e) {
      message("[report] Could not save subgroup forest plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # save_score_distribution_plot()
  # Stacked count histogram of integer risk scores by outcome (NHD vs not).
  # model_label is shown in the plot title to identify which score is plotted.
  # ---------------------------------------------------------------------------
  save_score_distribution_plot <- function(dist_df, output_folder,
                                           model_label = "Integer risk score") {
    # AGGREGATE-ONLY INPUT (2026-08-11). Was a person_level frame binned by
    # geom_histogram(); now takes pre-counted (total_score, outcome, count) rows
    # from agg_score_distribution.csv and draws them with geom_col(), which is
    # the same picture from counts instead of rows. Suppressed cells arrive with
    # count = NA and are dropped rather than drawn as zero.
    if (is.null(dist_df) || nrow(dist_df) == 0 ||
        !all(c("total_score", "outcome", "count") %in% names(dist_df))) return(NULL)
    df <- dist_df[!is.na(dist_df$count), , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    n_shown <- sum(as.numeric(df$count), na.rm = TRUE)

    df$Outcome <- factor(as.character(df$outcome), levels = c("No NHD", "NHD"))
    df$total_score <- as.numeric(df$total_score)
    df$count <- as.numeric(df$count)

    score_range <- range(df$total_score, na.rm = TRUE)
    # Greyscale fill: the previous #4472C4 / #C00000 pair has near-identical
    # luminance and merges into one band once desaturated. grey25/grey80 is
    # the two-level table used everywhere else in this repo's fills (see
    # .gs_series_palette, in omopReportToolkit's R/figure_style.R) — far enough apart on the
    # grey ramp to stay legible after a bad photocopy.
    p <- ggplot2::ggplot(df, ggplot2::aes(x = total_score, y = count, fill = Outcome)) +
      ggplot2::geom_col(position = "stack", colour = "white", width = 1) +
      ggplot2::scale_fill_manual(values = c("No NHD" = "grey80", "NHD" = "grey25")) +
      ggplot2::scale_x_continuous(
        breaks = seq(floor(score_range[1]), ceiling(score_range[2]), by = 1)
      ) +
      ggplot2::labs(
        title   = paste0("Score Distribution by Outcome — ", model_label),
        x       = "Integer risk score",
        y       = "Count",
        fill    = NULL,
        caption = paste0(
          "N = ", n_shown, " patients. ",
          # Stated as a shade->group mapping rather than a stacking order:
          # ggplot2 draws factor level 1 ("No NHD") at the TOP of a stack, so
          # the previous wording ("No NHD below, NHD above") was inverted.
          "NHD = non-home discharge. Dark grey = NHD; light grey = no NHD."
        )
      ) +
      theme_manuscript() +
      ggplot2::theme(
        legend.position  = "top",
        plot.caption     = ggplot2::element_text(size = 8),
        panel.grid.minor = ggplot2::element_blank()
      )

    fig_name <- paste0("score_distribution_",
                       gsub("[^A-Za-z0-9]", "_", tolower(model_label)), ".png")
    tryCatch({
      save_figure(p, output_folder, fig_name, width = 6, height = 3.8)
    }, error = function(e) {
      message("[report] Could not save score distribution plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # save_dca_plot()
  # Decision curve analysis: net benefit vs. threshold probability.
  # Accepts a named list of predicted-risk vectors (models) so that multiple
  # model curves can be overlaid. Threshold range 1-70% for NHD studies.
  #
  # Parameters:
  #   y            — integer vector of observed outcomes (0/1)
  #   p            — numeric vector (single model) OR named list of numeric
  #                  vectors (multiple models); e.g.:
  #                  list("Iannuzzi Lookup" = v1, "Iannuzzi Recal" = v2,
  #                       "mFI-5 Recal" = v3)
  #   output_folder — directory for the PNG file
  # ---------------------------------------------------------------------------
  save_dca_plot <- function(dca_df, dca_meta, output_folder, threshold_max_pct = NULL) {
    # AGGREGATE-ONLY INPUT (2026-08-11). This used to take patient-level y and
    # a named list of per-patient prediction vectors, and compute net benefit
    # here. That computation moved to pad-amp-nhd-val's aggregate step (it is
    # exactly reproducible from per-score-value counts -- verified equal to
    # machine epsilon), which emits agg_dca_net_benefit.csv with the reference
    # strategies included, plus agg_dca_meta.csv carrying the two cohort
    # statistics the axis logic below needs (prevalence, and the largest
    # predicted risk across models) that can no longer be derived here.
    if (is.null(dca_df) || nrow(dca_df) == 0 ||
        !all(c("threshold", "net_benefit", "strategy") %in% names(dca_df))) {
      message("[report] DCA plot skipped: agg_dca_net_benefit.csv missing or malformed.")
      return(NULL)
    }

    dca_src <- dca_df
    dca_df <- data.frame(
      threshold   = as.numeric(dca_df$threshold),
      net_benefit = as.numeric(dca_df$net_benefit),
      Strategy    = as.character(dca_df$strategy),
      stringsAsFactors = FALSE
    )
    thresholds <- sort(unique(dca_df$threshold))

    prev <- if (!is.null(dca_meta) && "prevalence" %in% names(dca_meta))
              suppressWarnings(as.numeric(dca_meta$prevalence[1])) else NA_real_
    max_pred <- if (!is.null(dca_meta) && "max_predicted_risk_any" %in% names(dca_meta))
              suppressWarnings(as.numeric(dca_meta$max_predicted_risk_any[1])) else NA_real_

    # Model curves keep the artifact's own order; the two reference strategies
    # are pinned last so they always take the subordinate palette slots below.
    ref_names      <- c("Treat all", "Treat none")
    model_names    <- setdiff(unique(dca_df$Strategy), ref_names)
    all_strategies <- c(model_names, intersect(ref_names, unique(dca_df$Strategy)))

    # ---- Displayed threshold range -------------------------------------------
    # Plotting the full 1-99% range is the unconventional choice, not the
    # truncation: standard DCA practice is to show only clinically plausible
    # thresholds (Vickers & Elkin, Med Decis Making 2006), and Vickers' own
    # `dca` package defaults to 0-50%. Here the untruncated axis actively
    # misleads — beyond the largest predicted risk, no patient is classified
    # positive, every model's net benefit is identically 0, and that dead flat
    # tail reads as a model result.
    #
    # The bound is chosen so nothing informative can be cropped:
    #   (a) past the largest predicted risk of ANY model — beyond that point
    #       no model classifies anyone, so every curve is flat 0 by
    #       construction;
    #   (b) past the outcome prevalence, which is where the treat-all
    #       reference stops being positive and therefore stops being
    #       interesting;
    #   (c) past the last threshold at which ANY curve is still non-zero, so
    #       a negative excursion (a model doing HARM) is never hidden — that
    #       is a finding, not noise;
    #   then rounded up to a 10% gridline with a 5-point margin, and floored
    #   at 30% so a degenerate cohort cannot produce an absurdly narrow panel.
    #
    # config$dca_threshold_max_pct overrides all of this. Use it when the
    # clinically plausible range is known: a data-driven bound must not crop
    # the range clinicians actually act over just because this cohort's models
    # never predict that high — the impact strip is there precisely to show
    # "no patients here" honestly rather than hiding the region.
    nonzero <- dca_df$threshold[abs(dca_df$net_benefit) > 1e-9]
    auto_max <- max(c(
      max_pred,                                                   # (a)
      prev,                                                       # (b)
      if (length(nonzero)) max(nonzero) else 0                    # (c)
    ), na.rm = TRUE)
    x_max <- if (!is.null(threshold_max_pct) && is.finite(threshold_max_pct)) {
      as.numeric(threshold_max_pct)
    } else {
      max(30, min(100, ceiling((auto_max * 100 + 5) / 10) * 10))
    }
    message("[report] DCA threshold axis displayed as 0-", x_max, "% ",
            if (is.null(threshold_max_pct)) "(chosen from data)"
            else "(from config$dca_threshold_max_pct)", ".")

    # ---- Net benefit values, tabulated ---------------------------------------
    # The figure encodes net benefit as a y-axis POSITION, which is the right
    # display for comparing shapes but cannot be read off precisely — least of
    # all in the 30-50% region where four curves overlap. Labelling points
    # directly is not the answer (a number on every point is unreadable, and
    # selective labels on four interleaved curves collide); the conventional
    # pairing is a table of net benefit at a few thresholds.
    #
    # Written as CSV next to the figure, matching how the pipeline already
    # emits calibration_table_*.csv: the Word report reads it back to render
    # the table, and it persists to output/figures/ as a machine-readable
    # artifact for anyone re-analysing without re-running the pipeline.
    tbl_pts <- seq(0.10, x_max / 100, by = 0.10)
    tbl_pts <- tbl_pts[tbl_pts <= max(thresholds)]
    if (length(tbl_pts) > 0) {
      # save_figure() creates output_folder, but that runs after this write —
      # don't assume the caller pre-created it.
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
      }
      nb_wide <- do.call(rbind, lapply(all_strategies, function(st) {
        rows <- dca_df[dca_df$Strategy == st, , drop = FALSE]
        # Snap to the nearest computed threshold rather than recomputing, so
        # the table can never disagree with the plotted curve.
        vals <- vapply(tbl_pts, function(pt) {
          rows$net_benefit[which.min(abs(rows$threshold - pt))]
        }, numeric(1L))
        data.frame(
          Strategy = st,
          as.list(stats::setNames(round(vals, 4), sprintf("%g%%", tbl_pts * 100))),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }))
      utils::write.csv(nb_wide,
                       file.path(output_folder, "decision_curve_net_benefit.csv"),
                       row.names = FALSE)
      message("[report] decision_curve_net_benefit.csv written (",
              length(tbl_pts), " thresholds x ", length(all_strategies),
              " strategies).")
    }

    # Greyscale encoding via .gs_scales() (R/report_helpers.R): up to 4 model
    # curves take slots 1-4 (grey level + linetype + shape), and the two
    # reference strategies take slots 5-6 (lighter grey70, undecorated
    # circle/triangle) so they stay visually subordinate to the model curves
    # even without colour. This replaces a hue-only palette that, while
    # already CVD-checked (see prior version of this comment), still relied
    # on colour as the only channel — no linetype/shape redundancy, and
    # entirely unreadable once desaturated for the greyscale journal
    # submission requirement.
    # Slots are pinned explicitly rather than left to default 1..N: the two
    # reference strategies must always take the reserved subordinate slots
    # 5-6 (grey70), whatever the model count. With the default numbering a
    # single-model run would give "Treat all" slot 2 — black, filled triangle
    # — making the reference line more prominent than the model curve.
    dca_df$Strategy <- factor(dca_df$Strategy, levels = all_strategies)
    gs              <- .gs_scales(all_strategies,
                                  slots = c(seq_along(model_names), 5L, 6L))

    # geom_line() alone is too dense along these smooth curves for point
    # shapes to read cleanly; subsample ~12 points per strategy so the shape
    # channel is visible without cluttering the curve (same approach as the
    # dual ROC plot in R/report_helpers.R).
    #
    # Subsample over the VISIBLE range only. Spacing them across the full
    # 1-99% computation and then zooming to x_max would leave only a handful
    # of markers on screen, weakening the shape channel exactly where the
    # figure is read.
    dca_points <- do.call(rbind, lapply(
      split(dca_df[dca_df$threshold * 100 <= x_max, , drop = FALSE],
            dca_df$Strategy[dca_df$threshold * 100 <= x_max], drop = TRUE),
      function(d) d[seq(1, nrow(d), by = max(1, floor(nrow(d) / 12))), , drop = FALSE]
    ))

    # Model curves drawn heavier than the two reference strategies (0.9 vs 1.15)
    # so the reference lines recede visually behind the curves being compared,
    # on top of the existing grey/linetype/shape channels -- a small addition
    # for readers who find the lines hard to tell apart even with those.
    p_dca <- ggplot2::ggplot(dca_df,
        ggplot2::aes(x = threshold * 100, y = net_benefit,
                     colour = Strategy, linetype = Strategy)) +
      ggplot2::geom_line(data = dca_df[dca_df$Strategy %in% model_names, , drop = FALSE],
                         linewidth = 1.15) +
      ggplot2::geom_line(data = dca_df[dca_df$Strategy %in% ref_names, , drop = FALSE],
                         linewidth = 0.7) +
      ggplot2::geom_point(data = dca_points, ggplot2::aes(shape = Strategy), size = 1.6) +
      gs$colour + gs$linetype + gs$shape +
      # coord_cartesian(), not scale limits: zooming keeps every computed row
      # so lines stay continuous to the panel edge. Setting limits on the
      # scale would DROP the out-of-range rows, emitting "Removed N rows"
      # warnings and clipping each line short of the boundary.
      ggplot2::scale_x_continuous(
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::coord_cartesian(xlim = c(0, x_max)) +
      ggplot2::labs(
        title   = "Decision Curve Analysis",
        x       = NULL,
        y       = "Net benefit"
      ) +
      theme_manuscript() +
      ggplot2::theme(
        legend.position  = "top",
        panel.grid.minor = ggplot2::element_blank(),
        # The clinical impact strip below shares this x-axis and carries the
        # tick labels for both panels, so suppress them here — otherwise the
        # identical axis is drawn twice in the middle of one figure.
        axis.text.x      = ggplot2::element_blank(),
        axis.ticks.x     = ggplot2::element_blank()
      )

    # --- Clinical impact strip -------------------------------------------------
    # Percentage of the cohort a model would actually classify as high risk at
    # each threshold — i.e. how many patients the decision above is even about.
    #
    # WHY this and not a rug of predicted risks like Figure 4: the DCA x-axis is
    # THRESHOLD probability, not predicted risk, and net benefit at threshold pt
    # is determined by the patients with predicted risk >= pt. The cumulative
    # "% classified high risk" therefore reads directly against the curve above
    # it, whereas a rug of individual risks would not. This is the established
    # DCA companion plot (the clinical impact curve — Kerr et al., J Clin Oncol
    # 2016; rmda::plot_clinical_impact), so it is a conventional addition, not
    # an invention.
    #
    # It also fixes a real honesty problem in this study's figure. Predicted
    # risk tops out at 0.489 (Iannuzzi recalibrated), 0.536 (mFI-5) and 0.466
    # (sVQI-FS), so above roughly a 50% threshold NO patient is classified
    # positive and every model's net benefit is identically zero. Without this
    # strip, the flat right-hand half of the DCA looks like a model result; it
    # is really just an empty cohort.
    # pct_high arrives on the aggregate artifact (it is a weighted proportion of
    # the same per-score-value counts net benefit is computed from), rather than
    # being recomputed here as mean(pv >= t) over per-patient predictions.
    impact_df <- if ("pct_high" %in% names(dca_src)) {
      d <- dca_src[dca_src$strategy %in% model_names &
                     !is.na(dca_src$pct_high), , drop = FALSE]
      if (nrow(d) == 0) NULL else data.frame(
        threshold = as.numeric(d$threshold),
        pct_high  = as.numeric(d$pct_high),
        Strategy  = as.character(d$strategy),
        stringsAsFactors = FALSE
      )
    } else NULL
    # The clinical-impact strip is optional: if the artifact predates pct_high
    # (or every value is NA), the DCA panel is still rendered on its own rather
    # than the whole figure failing.
    if (is.null(impact_df) || nrow(impact_df) == 0) {
      message("[report] DCA clinical-impact strip omitted: no pct_high in ",
              "agg_dca_net_benefit.csv.")
      out_file <- tryCatch(
        save_figure(p_dca, output_folder, "decision_curve_analysis.png",
                    width = 6.5, height = 5.0),
        error = function(e) {
          message("[report] Could not save DCA plot: ", conditionMessage(e)); NULL })
      return(out_file)
    }

    impact_df$Strategy <- factor(impact_df$Strategy, levels = all_strategies)

    impact_visible <- impact_df[impact_df$threshold * 100 <= x_max, , drop = FALSE]
    impact_points <- do.call(rbind, lapply(
      split(impact_visible, impact_visible$Strategy, drop = TRUE), function(d) {
        d[seq(1, nrow(d), by = max(1, floor(nrow(d) / 12))), , drop = FALSE]
      }))

    p_impact <- ggplot2::ggplot(impact_df,
        ggplot2::aes(x = threshold * 100, y = pct_high,
                     colour = Strategy, linetype = Strategy)) +
      ggplot2::geom_line(linewidth = 0.7, show.legend = FALSE) +
      ggplot2::geom_point(data = impact_points, ggplot2::aes(shape = Strategy),
                          size = 1.3, show.legend = FALSE) +
      gs$colour + gs$linetype + gs$shape +
      # Same zoom as the panel above, so the two x-axes stay in register.
      ggplot2::scale_x_continuous(
        breaks = seq(0, 100, by = 10),
        labels = function(x) paste0(x, "%")
      ) +
      ggplot2::scale_y_continuous(breaks = c(0, 50, 100),
                                  labels = function(x) paste0(x, "%")) +
      ggplot2::coord_cartesian(xlim = c(0, x_max), ylim = c(0, 100)) +
      ggplot2::labs(
        x       = "Threshold probability (%)",
        y       = "Classified\nhigh risk",
        # Hard-wrapped: ggplot2 does not wrap plot.caption, so a single long
        # string is silently clipped at the panel edge. Keep lines under ~95
        # characters at this 6.5in width if this text is edited.
        # "pt" is written in plain ASCII rather than the "pₜ" subscript glyph,
        # which the ragg TIFF device does not render in the default font.
        caption = paste0(
          "Net benefit = TP/N - FP/N x (pt / (1-pt)), where pt is the threshold probability.\n",
          "Treat-all and treat-none are reference strategies (lighter grey).\n",
          "Lower panel: percentage of the cohort each model classifies as high risk at that\n",
          "threshold. Where a line reaches 0%, no patient is treated and net benefit is\n",
          "necessarily 0 rather than meaningfully worse than the alternatives.\n",
          # State the displayed range AND why it stops there, so a reader can
          # see the axis was not cropped to flatter the models.
          #
          # The "nothing beyond this point" claim is only true of the
          # data-derived bound, which is constructed to guarantee it. A
          # pre-specified clinical bound may well cut across live curves — as
          # a 60% bound does in this cohort, where Iannuzzi lookup still
          # classifies ~8% of patients — so asserting it there would have the
          # caption contradicting the impact strip directly above it.
          "Net benefit was computed over thresholds of 1-99%; the axis is shown to ",
          sprintf("%g%%", x_max),
          if (is.null(threshold_max_pct)) paste0(
            ",\nbeyond which no model classifies any patient as high risk and all curves ",
            "are identically 0."
          ) else
            ",\nthe pre-specified clinically plausible range for this study."
        )
      ) +
      theme_manuscript() +
      ggplot2::theme(
        legend.position  = "none",
        plot.caption     = ggplot2::element_text(size = 8, hjust = 0),
        panel.grid.minor = ggplot2::element_blank()
      )

    combined_dca <- patchwork::wrap_plots(p_dca, p_impact, ncol = 1,
                                          heights = c(3.4, 1))

    tryCatch({
      save_figure(combined_dca, output_folder, "decision_curve.png",
                  width = 6.5, height = 6.1)
    }, error = function(e) {
      message("[report] Could not save DCA plot: ", conditionMessage(e))
      NULL
    })
  }

  # ---------------------------------------------------------------------------
  # next_report_file()
  #
  # Returns the target .docx path for the new report.  archive_old_reports()
  # always runs before this, so the dated filename is free to use directly.
  # ---------------------------------------------------------------------------
  next_report_file <- function(output_dir, base_name) {
    file.path(output_dir, paste0(base_name, ".docx"))
  }

  # ---------------------------------------------------------------------------
  # archive_old_reports()
  #
  # Moves all existing .docx files in output_dir to output_dir/archive/ before
  # each report run so only the latest report is visible at the top level.
  # Files are renamed with an incrementing suffix (_2, _3 …) when an archive
  # entry with the same name already exists (e.g. two runs on the same date).
  # ---------------------------------------------------------------------------
  archive_old_reports <- function(output_dir) {
    existing <- list.files(output_dir, pattern = "\\.docx$",
                           full.names = TRUE, all.files = FALSE)
    if (length(existing) == 0L) return(invisible(NULL))

    archive_dir <- file.path(output_dir, "archive")
    dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

    for (f in existing) {
      nm   <- basename(f)
      dest <- file.path(archive_dir, nm)

      # Avoid silently overwriting an existing archive entry on same-day re-runs.
      if (file.exists(dest)) {
        base <- tools::file_path_sans_ext(nm)
        ext  <- tools::file_ext(nm)
        i    <- 2L
        repeat {
          dest <- file.path(archive_dir, paste0(base, "_", i, ".", ext))
          if (!file.exists(dest)) break
          i <- i + 1L
        }
      }

      file.rename(f, dest)
    }

    invisible(NULL)
  }

  # Figure 3 — ROC. Rendered from pre-computed curve points (aggregate) rather
  # than from patient-level outcome/prediction vectors; see
  # .save_roc_plot_from_points() in R/report_helpers.R and the aggregate step
  # in pad-amp-nhd-val for why the two are equivalent. The published Iannuzzi
  # lookup curve is the primary one for this figure, matching what this report
  # plotted before the conversion.
  .roc_pts <- read_report_input("agg_roc_points")
  roc_plot_file <- if (!is.null(.roc_pts) && "curve_label" %in% names(.roc_pts)) {
    lk <- .roc_pts[.roc_pts$curve_label == "Iannuzzi (Lookup)", , drop = FALSE]
    if (nrow(lk) == 0) lk <- .roc_pts[.roc_pts$curve_label ==
                                        "Iannuzzi 2020 (Score)", , drop = FALSE]
    .save_roc_plot_from_points(lk, temp_figure_dir, "roc_curve.png")
  } else NULL

  # Branch 3 — Annual and monthly outcome rate plots.
  # NHD integer: faceted per-disposition trend via .save_nhd_rate_by_year_plot().
  if (identical(config$score_type, "lasso")) {
    # The LASSO/MACCE branch is inherited scaffolding this study never runs
    # (score_type is "integer"). Its two figure functions were DELETED on
    # 2026-09-06: they were unreachable, had never been converted to aggregate
    # inputs, and were the last figure code in this file still hardcoding a hue
    # (#1F3864 navy) rather than the greyscale palette. Recover them from git
    # history if a LASSO study is ever rendered here -- and convert them on both
    # counts before re-enabling this branch.
    stop("score_type = 'lasso' is not supported by this report repo. Its figure ",
         "functions were removed 2026-09-06 (unreachable, patient-level-only, ",
         "and not greyscale-safe). Recover .save_macce_rate_by_*_plot() from git ",
         "history, convert them to aggregate inputs the way .save_nhd_rate_by_*_plot() ",
         "were, and apply theme_manuscript() + save_figure() before enabling this branch.")
  } else {
    nhd_year_plot_file  <- .save_nhd_rate_by_year_plot(
      read_report_input("agg_nhd_by_year"), temp_figure_dir)
    nhd_month_plot_file <- .save_nhd_rate_by_month_plot(
      read_report_input("agg_nhd_by_month"), temp_figure_dir)
  }

  # Score distribution plots — one supplemental figure per model. All three
  # come from one aggregate artifact, split by score_id.
  .score_dist <- function(score_id) {
    d <- read_report_input("agg_score_distribution")
    if (is.null(d) || !"score_id" %in% names(d)) return(NULL)
    d[d$score_id == score_id, , drop = FALSE]
  }
  score_dist_plot_file_iannuzzi <- save_score_distribution_plot(
    .score_dist("iannuzzi"), temp_figure_dir, model_label = "Iannuzzi 2020")
  score_dist_plot_file_mfi5 <- if (has_mfi5) {
    save_score_distribution_plot(
      .score_dist("mfi5"), temp_figure_dir, model_label = "mFI-5")
  } else NULL
  score_dist_plot_file_vqifs <- if (has_vqifs) {
    save_score_distribution_plot(
      .score_dist("vqifs"), temp_figure_dir, model_label = "sVQI-FS")
  } else NULL
  score_dist_plot_file <- score_dist_plot_file_iannuzzi  # keep backward compat name

  # Returns a logical index selecting the temporal test partition of a
  # person_level data frame (split_set == "test"); TRUE for every row when no
  # split was applied. Shared by the DCA plot below and the risk-tier table
  # further down, so every place that needs "the evaluation rows for a
  # recalibrated specification" uses the same definition.
  .test_rows <- function(df) {
    if (is.null(df)) return(logical(0))
    if ("split_set" %in% names(df)) df$split_set == "test" else rep(TRUE, nrow(df))
  }

  # Decision curve analysis plot (Figure 5) — restricted to the three
  # recalibrated model specifications (Iannuzzi/mFI-5/sVQI-FS) plus the
  # 'treat-all'/'treat-none' reference strategies. The published Iannuzzi
  # lookup curve is computed by the aggregate step and present in
  # agg_dca_net_benefit.csv (strategy == "Iannuzzi (Lookup)") but deliberately
  # excluded from this figure so it shows only the recalibrated specifications
  # the study is actually comparing against each other.
  #
  # EVALUATION SET: restricted to the temporal test partition, matching Table 5
  # (recalibrated tiers) and the subgroup ECE analysis — every curve on this
  # plot compares net benefit across the same patients.
  # Every curve, including the two reference strategies and the cross-model
  # alignment onto a common evaluation set, is resolved in the aggregate
  # step -- the per-subject_id join that alignment used to require is exactly
  # the kind of operation this repo should no longer be doing.
  dca_df <- read_report_input("agg_dca_net_benefit")
  if (!is.null(dca_df) && "strategy" %in% names(dca_df)) {
    dca_df <- dca_df[dca_df$strategy != "Iannuzzi (Lookup)", , drop = FALSE]
  }
  dca_plot_file <- tryCatch(
    save_dca_plot(
      dca_df,
      read_report_input("agg_dca_meta"),
      temp_figure_dir,
      threshold_max_pct = config$dca_threshold_max_pct
    ),
    error = function(e) {
      message("[report] DCA plot failed: ", conditionMessage(e))
      NULL
    })

  if (file.exists(lookup_calibration_plot)) {
    file.copy(lookup_calibration_plot, lookup_calibration_plot_temp, overwrite = TRUE)
  } else if (file.exists(calibration_table_lookup_path)) {
    lookup_generated <- .save_calibration_plot_from_table(
      calibration_table_path = calibration_table_lookup_path,
      output_folder = temp_figure_dir,
      file_name = "calibration_lookup.png",
      plot_title = "Calibration Plot: Lookup Model"
    )
    if (!is.null(lookup_generated) && file.exists(lookup_generated)) {
      lookup_calibration_plot_temp <- lookup_generated
    }
  }

  if (file.exists(recalibrated_calibration_plot)) {
    file.copy(recalibrated_calibration_plot, recalibrated_calibration_plot_temp, overwrite = TRUE)
  } else if (file.exists(calibration_table_recalibrated_path)) {
    recal_generated <- .save_calibration_plot_from_table(
      calibration_table_path = calibration_table_recalibrated_path,
      output_folder = temp_figure_dir,
      file_name = "calibration_recalibrated.png",
      plot_title = "Calibration Plot: Recalibrated Model"
    )
    if (!is.null(recal_generated) && file.exists(recal_generated)) {
      recalibrated_calibration_plot_temp <- recal_generated
    }
  }

  # Calibration-plot fallbacks. These previously re-derived a plot from
  # patient-level vectors when the scoring step's PNG was missing; they now
  # rebuild it from that step's calibration TABLE instead, which is aggregate
  # and is written alongside the PNG by the same run.
  for (.cal in list(
        list(temp = "lookup_calibration_plot_temp", tbl = calibration_table_lookup_path,
             file = "calibration_lookup.png",       title = "Calibration Plot: Lookup Model"),
        list(temp = "recalibrated_calibration_plot_temp", tbl = calibration_table_recalibrated_path,
             file = "calibration_recalibrated.png", title = "Calibration Plot: Recalibrated Model"))) {
    if (!file.exists(get(.cal$temp)) && file.exists(.cal$tbl)) {
      .gen <- tryCatch(
        .save_calibration_plot_from_table(
          table_path = .cal$tbl, output_folder = temp_figure_dir,
          file_name = .cal$file, plot_title = .cal$title),
        error = function(e) { message("[report] calibration fallback failed: ",
                                      conditionMessage(e)); NULL })
      if (!is.null(.gen) && file.exists(.gen)) assign(.cal$temp, .gen)
    }
  }


  project_name <- basename(normalizePath(getwd(), winslash = "/", mustWork = FALSE))
  project_name <- gsub("[^A-Za-z0-9_-]", "_", project_name)
  report_base_name <- paste(project_name, "report", format(Sys.Date(), "%Y%m%d"), sep = "_")

  archive_old_reports(output_dir)

  report_file <- next_report_file(output_dir, report_base_name)

  # CDM source metadata for the methods text. Phase 0: this used to be a live
  # cdm_source query issued from inside the report — a database dependency
  # that called DatabaseConnector directly rather than going through a
  # fetch_*_from_omop() helper, easy to miss for exactly that reason. It now
  # reads the artifact written by extract_report_inputs().
  #
  # Empty strings are the documented "unknown" value here, not NA: the methods
  # text interpolates these directly, and the old code left them "" whenever the
  # query failed or no connection was supplied.
  cdm_version_str        <- ""
  vocabulary_version_str <- ""
  meta_raw <- read_report_input("cdm_source_metadata")
  if (!is.null(meta_raw) && nrow(meta_raw) > 0) {
    names(meta_raw)        <- tolower(names(meta_raw))
    cdm_version_str        <- as.character(meta_raw$cdm_version[1])
    vocabulary_version_str <- as.character(meta_raw$vocabulary_version[1])
  }

  doc <- read_docx()
  doc <- officer::body_set_default_section(
    doc,
    officer::prop_section(
      page_margins = officer::page_mar(
        top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
        header = 0.3, footer = 0.3, gutter = 0
      )
    )
  )
  doc <- body_add_par(doc, "Manuscript Draft: Methods and Results", style = "heading 1")
  doc <- body_add_par(doc, paste0(
    "PAD Major Lower-Extremity Amputation and Non-Home Discharge: ",
    "External Validation of the Iannuzzi 2020 Score, the Subramaniam mFI-5, ",
    "and the Kraiss 2022 sVQI-FS"
  ), style = "Normal")
  doc <- body_add_par(doc, paste("Date:", format(Sys.Date(), "%Y-%m-%d")), style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")

  doc <- body_add_par(doc, section_major("Methods"), style = "heading 2")
  doc <- body_add_par(doc, section_num("Data source"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThis analysis used patient-level data mapped to the Observational Medical Outcomes Partnership ",
    "Common Data Model (OMOP CDM",
    if (nzchar(cdm_version_str))        paste0("; CDM version: ", cdm_version_str)        else "",
    if (nzchar(vocabulary_version_str)) paste0("; vocabulary release: ", vocabulary_version_str) else "",
    "). The study window spanned ",
    if (!is.null(config$study_start_date)) format(as.Date(config$study_start_date), "%B %d, %Y") else "N/A",
    " to ",
    if (!is.null(config$study_end_date))   format(as.Date(config$study_end_date),   "%B %d, %Y") else "N/A",
    ". Full data source metadata are reported in ", supp_table("cdm_metadata"), ". Every cohort, ",
    "outcome, and covariate definition used in this analysis is registered in OHDSI ATLAS and listed, ",
    "with its standard-concept logic and source OMOP table(s), in ", supp_table("concept_set_inventory"), "."
  ), style = "Normal")
  doc <- body_add_par(doc, section_num("Cohort, outcome, and covariate definitions"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThe target cohort comprised adults aged 18 years or older undergoing an inpatient major ",
    "lower-extremity amputation, with peripheral arterial disease, diabetes mellitus, or a ",
    "lower-extremity wound documented on or before the index date, and at least ",
    config$min_prior_observation_days %||% 1,
    " day(s) of prior observation (a minimal requirement accommodating emergency presentations). ",
    "Traumatic, burn, and oncologic amputations were excluded, as were patients admitted from ",
    "another hospital or a skilled nursing facility, restricting the cohort to patients admitted ",
    "from home (including elective admissions coordinated through an outpatient clinic referral). ",
    "The outcome, non-home discharge ",
    "(NHD), was ascertained from each patient's discharge disposition at the end of the index ",
    "hospitalization and classified as non-home for any unambiguous institutional destination ",
    "(skilled nursing facility, inpatient rehabilitation, long-term acute care, hospice, or other ",
    "institutional care). NHD is a complete-case classification: every target-cohort patient is ",
    "retained, and a discharge is classified as not-NHD by default whenever the destination ",
    "cannot be confirmed non-home — a conservative choice that may under-count true NHD events, ",
    "discussed in the Limitations. Hospice discharges are classified as a single non-home category ",
    "regardless of setting (home vs. facility); the Iannuzzi 2020 score's own NHD definition does ",
    "not need to resolve this distinction, but not distinguishing the two here is noted as a ",
    "limitation. The dataset contained ",
    if (!is.na(n_target)) n_target else "N",
    " patients with at least one qualifying amputation within the study window."
  ), style = "Normal")

  n_scores <- 1L + (has_mfi5) + (has_vqifs)
  score_word <- c("One", "Two", "Three")[min(n_scores, 3)]

  doc <- body_add_par(doc, paste0(
    "\t", score_word, " published integer risk scores were evaluated as candidate predictors of ",
    "NHD: the Iannuzzi 2020 NHD score (range 0–18 points; its published four-level sex/race ",
    "interaction is implemented as two additive binary components, female +1 and non-White +2, ",
    "reproducing the published point totals exactly)",
    if (has_mfi5) ", the Subramaniam 2018 modified Frailty Index-5 (mFI-5, range 0–5 points)" else "",
    if (has_vqifs) paste0(
      # NOTE: this list must match covariates/covariates_vqifs.csv exactly — ten items,
      # NOT the paper's eleven. Non-home residence is deliberately omitted (near-circular
      # with the NHD outcome; no clean standard concept in this vocabulary build).
      ", and the Kraiss 2022 simple VQI Frailty Score (sVQI-FS, implemented as 0–10 points; the ",
      "published eleventh item, non-home residence, is omitted as near-circular with the outcome, ",
      "and the equally weighted form is evaluated rather than the authors' differentially weighted ",
      "variant — both addressed in the Limitations)"
    ) else "",
    ". Every component was ascertained from clinical documentation on or before the index date ",
    "over a 365-day lookback window (index date excluded) unless the score's own publication ",
    "defines the item differently (mFI-5 congestive heart failure and current pneumonia, 30 days; ",
    "Iannuzzi tissue loss, index date included); a patient meeting a component's evidence ",
    "threshold received its full published point value, and an absent record was treated as zero ",
    "evidence.",
    if (has_vqifs) paste0(
      " The source publication's rule suppressing sVQI-FS when fewer than five frailty domains ",
      "have data was not applied, since ascertainment here is presence/absence of coded records ",
      "rather than an explicit missing state."
    ) else "",
    " Component definitions, point values, and observed activation are in Tables 3a–c; the ",
    "ATLAS concept-set/cohort inventory underlying every definition is in ",
    supp_table("concept_set_inventory"), "."
  ), style = "Normal")

  # ---------------------------------------------------------------------------
  # Methods: implementation and code deployment.
  #
  # Consolidated 2026-09-13 (was 4 separate paragraphs) into one shorter
  # subsection, now that the analysis runs on real Duke data and the emphasis
  # has shifted from "how this was built" toward the results themselves.
  # ---------------------------------------------------------------------------
  doc <- body_add_par(doc, section_num("Implementation and code deployment"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tThe analysis was implemented as an OHDSI Strategus (v1.5.0) study package: cohort ",
    "construction, cohort diagnostics, and baseline characterization run through standard HADES ",
    "modules (CohortGenerator, CohortDiagnostics, Characterization) from a single declarative ",
    "specification, so the same code executes unmodified at any OMOP CDM v5 site with no ",
    "site-specific SQL editing, credentials, or institution-specific identifiers embedded. ",
    "Predictor ascertainment is likewise cohort-based: every score component resolves to a named, ",
    "cohort definition authored from that score's own concept set (", supp_table("concept_set_inventory"), "); ",
    "one cohort may serve several score items, with the lookback window held in a separate mapping. Three ",
    "elements fall outside what Strategus/Circe can express and are implemented as documented ",
    "extensions: the non-home discharge outcome (Circe has no discharge-disposition criterion), ",
    "application of the three published scoring rules (Strategus's prediction module fits new ",
    "models rather than applying fixed published weights), and arithmetic on measurement values ",
    "(body mass index, laboratory unit normalization). To verify that cohort-based ascertainment ",
    "measures the same thing as a direct CDM query, both routes are retained and an automated ",
    "regression test requires every predictor's per-patient values to agree exactly between them, ",
    "and a second automated check confirms each cohort's concept set still matches the concept set ",
    "recorded for that score component."
  ), style = "Normal")

  # ---------------------------------------------------------------------------
  # Methods: model evaluation.
  #
  # NEW subsection (2026-09-13), consolidating what was previously split across
  # the back half of "Risk score evaluation" (model specifications, train/test
  # split, discrimination/calibration definitions) with a new decision-curve-
  # analysis paragraph (previously described only in Figure 5's caption) so
  # discrimination, calibration, and DCA methodology all live in one place.
  # Score component descriptions moved to "Cohort, outcome, and covariate
  # definitions" above, since they are covariate definitions, not evaluation
  # methodology.
  # ---------------------------------------------------------------------------
  doc <- body_add_par(doc, section_num("Model evaluation"), style = "heading 3")

  # --- Transportability framing ---------------------------------------------
  # Peer review flagged that "external validation" overstates what is being done
  # for two of the three scores: only Iannuzzi 2020 predicts NHD, and none was
  # derived in an amputation population. This paragraph states the actual aim.
  doc <- body_add_par(doc, paste0(
    "\tNo published risk score exists for non-home discharge after major lower-extremity ",
    "amputation. Each instrument evaluated here was derived for a different outcome, a ",
    "different population, or both: the Iannuzzi 2020 score predicts NHD but was derived in ",
    "patients undergoing elective lower extremity bypass; the Subramaniam mFI-5 is a general ",
    "frailty index derived to predict mortality, postoperative complications, and unplanned ",
    "readmission rather than discharge destination; and the Kraiss 2022 simple VQI Frailty Score ",
    "(sVQI-FS) was derived to predict 9-month mortality in non-emergent vascular surgery cases. ",
    "We therefore evaluated whether any of these existing instruments transports to NHD ",
    "prediction in amputation patients. This is a screening step, undertaken as a benchmark ",
    "before committing to de novo model development; it is not an external validation of the ",
    "scores against the outcomes and populations for which they were designed."
  ), style = "Normal")

  # (Score component descriptions now live in "Cohort, outcome, and covariate
  # definitions" above, since they are covariate definitions, not evaluation
  # methodology; n_scores/score_word are computed there.)

  # Model specifications + train/test split, merged into one paragraph
  # 2026-09-13 (was two) -- they are one topic, how each model was fit and
  # evaluated. Discrimination/calibration and DCA paragraphs tightened at the
  # same time: Table 4/Figure 4/Figure 5's own captions already carry the
  # specifics, so this prose states each method once without restating it.
  doc <- body_add_par(doc, paste0(
    "\tFor each score, two kinds of specification were considered: the published mapping, where ",
    "one exists, and a temporal recalibration — a logistic regression of the score's total integer ",
    "value on the observed NHD outcome. Iannuzzi 2020's published score-to-risk lookup required a ",
    "monotone (isotonic) fit pooling the derivation and validation columns of the source ",
    "publication's Table III, since the published table's tail is non-monotonic in the sparse ",
    "high-score cells (both raw columns are retained in covariates/risk_lookup.csv for provenance); ",
    "no equivalent published mapping exists for the mFI-5 or sVQI-FS. Patients were sorted ",
    "chronologically by index amputation date and divided at the midpoint; each recalibration was ",
    "fitted on the earlier half only and evaluated on the later half, preventing optimistic bias ",
    "from evaluating a locally fitted model on the data used to fit it. The Iannuzzi published-",
    "lookup specification is the exception: as a fixed external mapping with no parameters ",
    "estimated from these data, it is evaluated over the full cohort rather than discarding half ",
    "the available events for no bias-reduction benefit (Table 4 states the evaluation set for ",
    "every column). Because recalibration is a monotone transform of a single predictor, AUROC and ",
    "AUPRC do not differ between a score's published/raw and recalibrated specifications; Table 4 ",
    "and Figure 4 report the recalibrated specification for every score, and Figure 3 reports ",
    "discrimination for the published/raw specification."
  ), style = "Normal")
  # Append the split sample sizes when split_info.csv was found. The split is
  # identical across scores by construction (same patients, same index dates),
  # so report it once rather than repeating a near-identical sentence per model.
  split_sent <- NULL
  for (si in list(split_info_iannuzzi, split_info_mfi5, split_info_vqifs)) {
    if (is.null(split_sent)) split_sent <- split_sentence(si, NULL)
  }
  if (!is.null(split_sent)) doc <- body_add_par(doc, split_sent, style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tDiscrimination was summarised using AUROC and AUPRC, and calibration using the Brier score, ",
    "expected calibration error (ECE), calibration intercept, and calibration slope — each with 95% ",
    "bootstrap percentile confidence intervals (B = 500 resamples). ECE is the probability-weighted ",
    "mean absolute difference between predicted and observed NHD rates across quantile-based bins, ",
    "which the calibration plots also depict directly (dashed diagonal = perfect calibration)."
  ), style = "Normal")
  doc <- body_add_par(doc, paste0(
    "\tClinical utility was assessed using decision curve analysis (DCA): net benefit of using a ",
    "model to guide a binary treat/do-not-treat decision, across threshold probabilities from 1% to ",
    "99%, on the same temporal test partition, compared against 'treat-all' and 'treat-none' ",
    "reference strategies."
  ), style = "Normal")
  doc <- body_add_par(doc, section_num("Subgroup analysis and bias assessment"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "\tModel calibration and discrimination were assessed across prespecified subgroups — ",
    "biological sex, race, ethnicity, age group (<65, 65–74, ≥75 years), amputation level ",
    "(above-knee, below-knee, other), and calendar year of the index procedure — to identify ",
    "populations where the recalibrated scores may be miscalibrated or discriminate less reliably. ",
    "Expected calibration error (ECE) and AUROC were computed within each subgroup using the same ",
    "recalibrated predicted probabilities and temporal test partition as the overall figures in ",
    "Table 4, with 95% CIs from 200 bootstrap resamples. Subgroup levels with fewer than 10 observed ",
    "NHD events were suppressed; AUROC was additionally suppressed where the outcome was constant ",
    "within that subgroup. ",
    # Gated on has_mfi5_bias, not has_mfi5 — see .init_supp_labels(). When the
    # subgroup analysis produced nothing for this dataset the sentence is
    # replaced rather than dropped silently, so a reader is told the assessment
    # was attempted and did not yield reportable results, instead of finding the
    # paragraph describing a method that then points nowhere.
    if (has_mfi5_bias)
      paste0("Subgroup results are reported for the mFI-5 (Recalibrated) model in ",
             "Table 7 and Figures 6 (calibration) and 7 (discrimination).")
    else
      # Deliberately states only the FACT, not a cause. An earlier draft of this
      # sentence said "no subgroup met the minimum event threshold", which is a
      # specific empirical claim the report is in no position to make: the
      # subgroup analysis can yield nothing for several reasons (the scoring
      # step skipped it, the demographic lookup failed against this CDM, or the
      # event floor genuinely suppressed every level), and this repo sees only
      # the absence of a file. On the 2026-08-12 Duke export, with 698 patients
      # and 449 events, the event floor is almost certainly NOT the explanation
      # -- so that wording would have printed something false.
      paste0("Subgroup calibration results are not reported for this data source.")
  ), style = "Normal")

  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_major("Results"), style = "heading 2")

  # ---- Figure 1: CONSORT-style patient-flow diagram ------------------------
  # Reads agg_consort_flow.csv (pad-amp-nhd-val's R/extract_report_inputs.R),
  # a cumulative cohort-attrition funnel built from CohortGenerator's own
  # cg_cohort_attrition.csv/cg_cohort_inclusion.csv plus the facility-admission
  # exclusion step's before/after counts. Absent in any export produced before
  # that extraction was added -- read_report_input() and
  # .save_consort_flow_plot() both degrade to NULL in that case, so this
  # block is skipped rather than erroring, the same graceful-omission
  # convention used for subgroup_bias.csv elsewhere in this file.
  consort_flow <- read_report_input("agg_consort_flow")
  consort_plot_file <- .save_consort_flow_plot(consort_flow, temp_figure_dir)
  if (!is.null(consort_plot_file) && file.exists(consort_plot_file)) {
    doc <- body_add_img(doc, src = consort_plot_file, width = 5.5, height = 6.5)
    doc <- add_doc_caption(doc,
      "Figure 1. Patient flow diagram.",
      paste0(
        "Cumulative cohort attrition from entry criteria to the final validation ",
        "cohort. The first five stages are the target cohort's inclusion/exclusion ",
        "rules, applied in the order evaluated; the final stage additionally excludes ",
        "patients whose index admission originated from another hospital or a skilled ",
        "nursing facility (see Methods)."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 1 (CONSORT patient-flow diagram) added.")
  } else {
    message("[report] Figure 1 (CONSORT patient-flow diagram) skipped: agg_consort_flow.csv not available.")
  }

  # ---- Table 1: Demographics -----------------------------------------------
  doc <- body_add_par(doc, section_num("Cohort characteristics"), style = "heading 3")
  doc <- body_add_par(doc, paste0(
    "The final target cohort included ", n_target, " patients, of whom ",
    n_outcome, " experienced non-home discharge (NHD), corresponding to an observed ",
    # 1 decimal place, matching every other percentage in this report (Table 3b/3c
    # prevalence, Table 5 tier rates, etc. all use round(..., 1)) — this line
    # previously used fmt(outcome_prev, 2), producing "35.11%" here against
    # "35.1%" everywhere else describing the identical rate.
    "NHD rate of ", fmt(outcome_prev, 1), "%."
  ), style = "Normal")
  doc <- body_add_flextable(doc, table1_ft(cohort_tbl))
  doc <- add_doc_caption(doc,
    "Table 1. Demographics and clinical characteristics of the validation cohort.",
    paste0(
      "Values are n (%) unless stated. Age is summarised as median (IQR). ",
      "Indication rows are not mutually exclusive; a patient may have more than one. ",
      "Amputation level is assigned hierarchically (AKA, then BKA, then Other) and is mutually ",
      "exclusive. Concept-set definitions underlying every row are in ",
      supp_table("concept_set_inventory"), "."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")

  # ---- Table 2: NHD Patient Outcomes ----------------------------------------
  doc <- add_doc_page_break(doc)
  nhd_outcomes <- read_nhd_outcomes()

  # Defensive consistency check: Table 2's NHD count (from
  # fetch_nhd_outcomes_from_omop()'s own SQL, run against the outcome cohort
  # table with the same prediction_window_days bound as get_outcomes()) must
  # equal Table 1's NHD count (sum(person_level$outcome), from
  # calculate_scores()/get_outcomes()). These previously disagreed (79 vs 80)
  # because one query lacked the prediction-window bound the other applied.
  # Both queries now apply that bound identically, so they SHOULD always
  # agree going forward — this warning exists so a future edit to either
  # query that reintroduces a divergence is caught immediately rather than
  # discovered by a peer reviewer reading the generated report.
  if (!is.null(nhd_outcomes) && !is.na(nhd_outcomes$n_nhd) && !is.na(n_outcome)) {
    n_nhd_table1 <- n_outcome
    if (nhd_outcomes$n_nhd != n_nhd_table1) {
      warning(sprintf(
        paste0("[report] Table 1 and Table 2 NHD counts disagree (Table 1 = %d, ",
               "Table 2 = %d). These must come from the same population — check that ",
               "fetch_nhd_outcomes_from_omop()'s nhd_cte and get_outcomes() apply an ",
               "identical prediction_window_days bound."),
        n_nhd_table1, nhd_outcomes$n_nhd
      ), call. = FALSE)
    }
  }

  if (!is.null(nhd_outcomes) && !is.na(nhd_outcomes$n_nhd) && nhd_outcomes$n_nhd > 0) {
    n_nhd_denom <- nhd_outcomes$n_nhd

    los_str <- if (!is.na(nhd_outcomes$median_los)) {
      paste0(as.integer(round(nhd_outcomes$median_los)), " days",
             " (IQR: ", as.integer(round(nhd_outcomes$los_p25)),
             "\u2013", as.integer(round(nhd_outcomes$los_p75)), ")")
    } else "N/A"

    # Build static rows for NHD outcome table
    nhd_static_rows <- data.frame(
      Outcome = c(
        "Index hospitalization length of stay, median (IQR)",
        "Discharge destination",
        "90-day readmission, n (%)",
        "90-day mortality, n (%)"
      ),
      Value = c(
        los_str,
        "",
        fmt_n_pct(nhd_outcomes$n_readmission, n_nhd_denom),
        fmt_n_pct(nhd_outcomes$n_death,        n_nhd_denom)
      ),
      stringsAsFactors = FALSE
    )

    # Insert dynamic discharge destination rows after the "Discharge destination" header row.
    # Destinations are returned sorted descending by n from the SQL query.
    if (!is.null(nhd_outcomes$dest_df) && nrow(nhd_outcomes$dest_df) > 0) {
      dest_rows <- data.frame(
        Outcome = paste0("    ", nhd_outcomes$dest_df$destination),
        Value   = vapply(nhd_outcomes$dest_df$n, function(x) fmt_n_pct(x, n_nhd_denom),
                         character(1L)),
        stringsAsFactors = FALSE
      )
      # Splice: rows 1-2 | destination sub-rows | rows 3-4
      nhd_outcome_tbl <- rbind(
        nhd_static_rows[1:2, ],
        dest_rows,
        nhd_static_rows[3:4, ]
      )
    } else {
      nhd_outcome_tbl <- nhd_static_rows
    }

    # Row formatting indices
    nhd_header_rows <- which(nhd_outcome_tbl$Outcome == "Discharge destination")
    nhd_indent_rows <- grep("^    ", nhd_outcome_tbl$Outcome)

    nhd_out_ft <- flextable::flextable(nhd_outcome_tbl) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::align(align = "left", part = "all") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::bold(i = nhd_header_rows, part = "body") |>
      flextable::bg(i = nhd_header_rows, bg = "#F2F2F2", part = "body") |>
      flextable::padding(i = nhd_indent_rows, j = "Outcome",
                         padding.left = 18, part = "body") |>
      flextable::width(j = "Outcome", width = 3.5) |>
      flextable::width(j = "Value",   width = 1.5) |>
      flextable::set_table_properties(layout = "fixed")

    doc <- body_add_par(doc, section_num("NHD patient outcomes"), style = "heading 3")
    doc <- body_add_par(doc,
      paste0(
        "Among the ", n_nhd_denom, " patients who experienced non-home discharge (NHD) at ",
        "the end of the index hospitalization, Table 2 summarises key post-discharge outcomes."
      ),
      style = "Normal"
    )
    doc <- body_add_flextable(doc, nhd_out_ft)
    doc <- add_doc_caption(doc,
      "Table 2. NHD patient outcomes at the index hospitalization and within 90 days.",
      paste0(
        "Denominator is all patients with non-home discharge at the index ",
        "hospitalization (n\u00a0=\u00a0", n_nhd_denom, "). ",
        "Index hospitalization LOS: length of the inpatient visit (visit_concept_id 9201) ",
        "containing the index procedure date, calculated as DATEDIFF(DAY, visit_start_date, ",
        "COALESCE(visit_end_date, visit_start_date)). ",
        "Discharge destination: destination concept name from visit_occurrence.discharged_to_concept_id ",
        "joined to the vocabulary concept table; destinations with fewer than 11 patients are suppressed. ",
        "90-day readmission: any inpatient visit (visit_concept_id 9201) starting after the index ",
        "hospitalization's discharge date and within 90 days of that discharge date (anchored on ",
        "discharge, not admission, so patients with a longer index stay are not given a shorter ",
        "effective follow-up window). ",
        "90-day mortality: any death record on or after the discharge date and within 90 days of it."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 2 (NHD outcomes) added.")
  }

  # ---- Figure 2: NHD rate by year (placed after Tables 1-2) ---------------
  if (!is.null(nhd_year_plot_file) && file.exists(nhd_year_plot_file)) {
    doc <- body_add_img(doc, src = nhd_year_plot_file, width = 5.5, height = 3.5)
    doc <- add_doc_caption(doc,
      "Figure 2. Non-home discharge (NHD) rate by disposition type and amputation year.",
      paste0(
        "Non-home discharge (NHD) rate (%) by calendar year of index amputation. The Overall line ",
        "is the combined NHD rate across all dispositions; the remaining lines break it out by ",
        "disposition (SNF = Skilled Nursing Facility; IRF = Inpatient Rehabilitation Facility; ",
        "LTAC = Long-term Acute Care; Hospice; Other NHD), distinguished by grey level, line type, ",
        "and point shape. Years with fewer than 11 patients are suppressed (small-cell privacy ",
        "rule, matching Table 2's discharge-destination suppression threshold). Disposition is ",
        "mapped from UB-04 discharged_to_source_value codes."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 2 (NHD rate by year) added.")
  }

  # ---- Table 3a: mFI-5 component activation --------------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Predictor activation"), style = "heading 3")
  if (!is.null(covariate_summary_mfi5) && nrow(covariate_summary_mfi5) > 0) {
    doc <- body_add_flextable(doc, .build_mfi5_component_table(covariate_summary_mfi5))
    doc <- add_doc_caption(doc,
      "Table 3a. Subramaniam mFI-5: component definitions and activation summary.",
      paste0(
        "Point value (range 0–5) and observed activation rate for each of the five mFI-5 ",
        "components. All use a 365-day lookback except congestive heart failure and the pneumonia ",
        "arm of the combined COPD-or-pneumonia item (30 days each, per the original publication); ",
        "absent records count as true negatives. Concept-set definitions are in ",
        supp_table("concept_set_inventory"), "."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 3a (mFI-5 components) added.")
  }

  # ---- Table 3b: Iannuzzi NHD component activation --------------------------
  tbl3b_data <- if (identical(config$score_type, "lasso")) {
    .build_combined_covariate_table(covariate_summary)
  } else {
    .build_combined_component_table(covariate_summary)
  }
  doc <- body_add_flextable(doc, tbl3b_data)
  doc <- add_doc_caption(doc,
    "Table 3b. Iannuzzi 2020 NHD score: component definitions and activation summary.",
    paste0(
      "Point values (range 0–18) and observed activation rates for the nine Iannuzzi 2020 ",
      "components, all with a 365-day lookback (tissue loss includes the index date). Anemia ",
      "(Hgb < 10 g/dL) treats an absent measurement as unknown ",
      "(missing_is_negative = FALSE); all other components treat it as a true negative. ",
      "Concept-set definitions are in ", supp_table("concept_set_inventory"), "."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")
  message("[report] Table 3b (Iannuzzi components) added.")

  # ---- Table 3c: sVQI-FS component activation --------------------------------
  if (!is.null(covariate_summary_vqifs) && nrow(covariate_summary_vqifs) > 0) {
    doc <- body_add_flextable(doc, .build_vqifs_component_table(covariate_summary_vqifs))
    doc <- add_doc_caption(doc,
      "Table 3c. Kraiss 2022 sVQI-FS: component definitions and activation summary.",
      paste0(
        "Point value (range 0–10 as implemented; non-home residence, the paper's eleventh item, ",
        "is omitted — see covariates/covariates_vqifs.csv) and observed activation rate for each ",
        "of ten sVQI-FS components, all with a 365-day lookback. ",
        # Updated to match the 2026-07-26 switch from diagnosis proxies to the paper's
        # own lab thresholds (query_renal_impairment_covariate_counts() and
        # query_anemia_covariate_counts(sex_specific = TRUE) in R/risk_score_pipeline.R).
        # This footnote previously still claimed diagnosis proxies, understating fidelity.
        "Renal impairment and anemia use the source publication's own laboratory thresholds ",
        "rather than diagnosis proxies; peripheral vascular disease remains a diagnosis-based ",
        "proxy rather than the paper's ankle-brachial index definition. Absent records count as ",
        "true negatives except for underweight and anemia, where an unmeasured value is treated ",
        "as unknown. Concept-set definitions are in ", supp_table("concept_set_inventory"), "."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Table 3c (sVQI-FS components) added.")
  }

  # ---- Table 4: Dual-model performance comparison ---------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Model performance"), style = "heading 3")
  # Build a dynamic caption note about the temporal split if metadata is available.
  split_note_iannuzzi <- if (!is.null(split_info_iannuzzi) && nrow(split_info_iannuzzi) > 0) {
    sprintf("Iannuzzi 2020: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_iannuzzi$n_test[1], split_info_iannuzzi$split_date[1])
  } else NULL
  split_note_mfi5 <- if (!is.null(split_info_mfi5) && nrow(split_info_mfi5) > 0) {
    sprintf("mFI-5: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_mfi5$n_test[1], split_info_mfi5$split_date[1])
  } else NULL
  split_note_vqifs <- if (!is.null(split_info_vqifs) && nrow(split_info_vqifs) > 0) {
    sprintf("sVQI-FS: metrics reported on the test set (n\u2009=\u2009%d; after %s).",
            split_info_vqifs$n_test[1], split_info_vqifs$split_date[1])
  } else NULL
  split_caption <- paste(Filter(Negate(is.null), list(split_note_iannuzzi, split_note_mfi5, split_note_vqifs)),
                         collapse = " ")

  perf_tbl <- data.frame(
    Metric = c(
      "AUROC",
      "AUPRC",
      "Brier score",
      "Expected calibration error",
      "Calibration intercept",
      "Calibration slope"
    ),
    "Iannuzzi Recalibrated" = c(
      paste0(fmt(metric_value("AUROC",                "recalibrated")), " ", metric_ci("AUROC",                "recalibrated")),
      paste0(fmt(metric_value("AUPRC",                "recalibrated")), " ", metric_ci("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value("Brier",                "recalibrated")), " ", metric_ci("Brier",                "recalibrated")),
      paste0(fmt(metric_value("ECE",                  "recalibrated")), " ", metric_ci("ECE",                  "recalibrated")),
      paste0(fmt(metric_value("CalibrationIntercept", "recalibrated")), " ", metric_ci("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value("CalibrationSlope",     "recalibrated")), " ", metric_ci("CalibrationSlope",     "recalibrated"))
    ),
    "mFI-5 Recalibrated" = if (!is.null(metrics_mfi5)) c(
      paste0(fmt(metric_value_mfi5("AUROC",                "recalibrated")), " ", metric_ci_mfi5("AUROC",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("AUPRC",                "recalibrated")), " ", metric_ci_mfi5("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("Brier",                "recalibrated")), " ", metric_ci_mfi5("Brier",                "recalibrated")),
      paste0(fmt(metric_value_mfi5("ECE",                  "recalibrated")), " ", metric_ci_mfi5("ECE",                  "recalibrated")),
      paste0(fmt(metric_value_mfi5("CalibrationIntercept", "recalibrated")), " ", metric_ci_mfi5("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value_mfi5("CalibrationSlope",     "recalibrated")), " ", metric_ci_mfi5("CalibrationSlope",     "recalibrated"))
    ) else rep("—", 6),
    "sVQI-FS Recalibrated" = if (!is.null(metrics_vqifs)) c(
      paste0(fmt(metric_value_vqifs("AUROC",                "recalibrated")), " ", metric_ci_vqifs("AUROC",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("AUPRC",                "recalibrated")), " ", metric_ci_vqifs("AUPRC",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("Brier",                "recalibrated")), " ", metric_ci_vqifs("Brier",                "recalibrated")),
      paste0(fmt(metric_value_vqifs("ECE",                  "recalibrated")), " ", metric_ci_vqifs("ECE",                  "recalibrated")),
      paste0(fmt(metric_value_vqifs("CalibrationIntercept", "recalibrated")), " ", metric_ci_vqifs("CalibrationIntercept", "recalibrated")),
      paste0(fmt(metric_value_vqifs("CalibrationSlope",     "recalibrated")), " ", metric_ci_vqifs("CalibrationSlope",     "recalibrated"))
    ) else rep("—", 6),
    check.names     = FALSE,
    stringsAsFactors = FALSE
  )

  has_vqifs_metrics <- !is.null(metrics_vqifs)

  perf_ft <- flextable::flextable(perf_tbl) |>
    flextable::set_header_labels(
      Metric                   = "Metric",
      "Iannuzzi Recalibrated"  = "Iannuzzi 2020\n(Recalibrated, 95% CI)",
      "mFI-5 Recalibrated"     = "mFI-5\n(Recalibrated, 95% CI)",
      "sVQI-FS Recalibrated"   = "sVQI-FS\n(Recalibrated, 95% CI)"
    ) |>
    flextable::bold(part = "header") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::font(fontname = "Calibri", part = "all") |>
    flextable::bg(part = "header", bg = "#1F3864") |>
    flextable::color(part = "header", color = "white") |>
    flextable::padding(padding = 3, part = "all") |>
    flextable::width(j = "Metric",                  width = 1.6) |>
    flextable::width(j = "Iannuzzi Recalibrated",   width = 1.8) |>
    flextable::width(j = "mFI-5 Recalibrated",      width = 1.8) |>
    flextable::width(j = "sVQI-FS Recalibrated",    width = 1.8) |>
    flextable::align(j = c("Iannuzzi Recalibrated", "mFI-5 Recalibrated", "sVQI-FS Recalibrated"),
                     align = "center", part = "all") |>
    flextable::set_table_properties(layout = "fixed")

  doc <- body_add_flextable(doc, perf_ft)
  doc <- add_doc_caption(doc,
    paste0(
      "Table 4. Model performance: discrimination and calibration metrics for the ",
      "temporally recalibrated Iannuzzi 2020, Subramaniam mFI-5, and Kraiss 2022 sVQI-FS ",
      "specifications."
    ),
    paste0(
      "Each recalibrated specification is a logistic regression of the score's total integer ",
      "value on the observed NHD outcome, fitted on the chronologically earlier half of the ",
      "cohort and evaluated on the later half; metrics reported here are on the test half only. ",
      "AUROC and AUPRC are unchanged from the score's published-lookup or raw-score ",
      "specification (recalibration is a monotone transform of a single predictor) and are also ",
      "shown in Figure 3. ",
      if (nchar(split_caption) > 0) paste0(split_caption, " ") else "",
      "95% CI = 95% bootstrap percentile confidence interval (B = 500 resamples). ",
      "— indicates the specification is unavailable for this run."
    )
  )
  doc <- body_add_par(doc, "", style = "Normal")
  message("[report] Table 4 (recalibrated-only performance) added.")

  # ---- Figure 3: Combined ROC curve (Iannuzzi + mFI-5 + sVQI-FS) ------------
  # Multi-model ROC, drawn from the same aggregate curve-point artifact as
  # Figure 3's single-model version. The pre-conversion code normalised each
  # raw integer score to [0, 1] before plotting; that was only ever a way to
  # put the scores on a common predictor scale for pROC, and it does not
  # change a ROC curve at all (the curve is rank-based). The aggregate step
  # ranks by the raw score directly, so these curves are identical to the
  # ones this figure showed before -- and their AUCs now provably agree with
  # metrics.csv, which the normalised version did not guarantee.
  dual_roc_file <- NULL
  .want_roc <- c("Iannuzzi (Lookup)",
                 if (has_mfi5)  "mFI-5 (Score)",
                 if (has_vqifs) "sVQI-FS (Score)")
  if (has_mfi5 && !is.null(.roc_pts) && "curve_label" %in% names(.roc_pts)) {
    multi <- .roc_pts[.roc_pts$curve_label %in% .want_roc, , drop = FALSE]
    if (nrow(multi) > 0 && length(unique(multi$curve_label)) > 1) {
      dual_roc_file <- tryCatch(
        .save_roc_plot_from_points(multi, temp_figure_dir, "roc_curve_multi.png"),
        error = function(e) {
          message("[report] Multi-model ROC plot skipped: ", conditionMessage(e))
          NULL
        }
      )
    }
  }

  has_vqifs_roc <- has_vqifs

  fig2_file    <- if (!is.null(dual_roc_file) && file.exists(dual_roc_file)) dual_roc_file else roc_plot_file
  fig2_caption <- if (!is.null(dual_roc_file) && file.exists(dual_roc_file) && has_vqifs_roc) {
    paste0("ROC curves for the Iannuzzi 2020 NHD score (published lookup), ",
           "the Subramaniam mFI-5 (raw integer score 0–5), and the Kraiss 2022 ",
           "sVQI-FS (raw integer score) predicting non-home discharge. Curves are ",
           "distinguished by grey level and line type; see the legend, which reports each AUC. ",
           "AUROC with 95% bootstrap percentile CI (B = 500 resamples).")
  } else if (!is.null(dual_roc_file) && file.exists(dual_roc_file)) {
    paste0("ROC curves for the Iannuzzi 2020 NHD score (published lookup) ",
           "and the Subramaniam mFI-5 (raw integer score 0\u20135) predicting non-home ",
           "discharge. Curves are distinguished by grey level and line type; see the legend. ",
           "AUROC with 95% bootstrap percentile CI (B\u2009=\u2009500 resamples).")
  } else {
    paste0("ROC curve for the Iannuzzi 2020 lookup model predicting non-home ",
           "discharge. AUROC with 95% bootstrap percentile CI (B = 500 resamples). ",
           "Dashed diagonal = no-discrimination reference line.")
  }

  if (!is.null(fig2_file) && file.exists(fig2_file)) {
    doc <- body_add_img(doc, src = fig2_file, width = 5.0, height = 4.5)
    doc <- add_doc_caption(doc, "Figure 3. Receiver operating characteristic (ROC) curves.", fig2_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 3 (ROC) added.")
  }

  # ---- Figure 4: Calibration — recalibrated specifications only -------------
  # Recalibrated-only, matching Table 4, Table 5, and Figure 5 (DCA): the
  # Iannuzzi published-lookup curve is never requested here (lookup_source
  # is always NULL below). Attempt a triple-curve combined plot (Iannuzzi +
  # mFI-5 + sVQI-FS recalibrated); fall back to dual, or single-curve when
  # mFI-5/sVQI-FS calibration data are unavailable.
  mfi5_calibration_table_path <- if (!is.null(mfi5_output_dir)) {
    file.path(mfi5_output_dir, "calibration_table_recalibrated.csv")
  } else NULL

  vqifs_calibration_table_path <- if (!is.null(vqifs_output_dir)) {
    file.path(vqifs_output_dir, "calibration_table_recalibrated.csv")
  } else NULL

  dual_cal_file <- tryCatch(
    .save_dual_calibration_plot(
      lookup_source = NULL,
      recal_source  = if (file.exists(calibration_table_recalibrated_path))
                        calibration_table_recalibrated_path
                      else NULL,
      mfi5_source   = if (!is.null(mfi5_calibration_table_path) &&
                          file.exists(mfi5_calibration_table_path))
                        mfi5_calibration_table_path
                      else NULL,
      vqifs_source  = if (!is.null(vqifs_calibration_table_path) &&
                          file.exists(vqifs_calibration_table_path))
                        vqifs_calibration_table_path
                      else NULL,
      output_folder = temp_figure_dir,
      file_name     = "calibration_dual.png"
    ),
    error = function(e) {
      message("[report] Calibration plot skipped: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(dual_cal_file) && file.exists(dual_cal_file)) {
    # Combined plot (up to four curves).
    has_mfi5_cal <- !is.null(mfi5_calibration_table_path) &&
                    file.exists(mfi5_calibration_table_path)
    has_vqifs_cal <- !is.null(vqifs_calibration_table_path) &&
                     file.exists(vqifs_calibration_table_path)
    fig3_title <- if (has_mfi5_cal && has_vqifs_cal)
      "Figure 4. NHD risk score calibration — three recalibrated specifications."
    else if (has_mfi5_cal)
      "Figure 4. NHD risk score calibration — two recalibrated specifications."
    else
      "Figure 4. Iannuzzi 2020: temporal recalibration calibration plot."
    fig3_caption <- if (has_mfi5_cal && has_vqifs_cal)
      paste0("Calibration curves for the temporally recalibrated Iannuzzi 2020, mFI-5, and ",
             "sVQI-FS specifications (Table 4), each a logistic regression of the score's total ",
             "integer value fitted on the chronologically earlier half of the cohort and ",
             "evaluated on the later half. Mean predicted NHD risk (x-axis) vs. observed NHD ",
             "rate (y-axis) by quantile bin. Dotted diagonal = perfect calibration.")
    else if (has_mfi5_cal)
      paste0("Calibration curves for the temporally recalibrated Iannuzzi 2020 and mFI-5 ",
             "specifications (Table 4), each a logistic regression of the score's total integer ",
             "value fitted on the chronologically earlier half of the cohort and evaluated on the ",
             "later half. Mean predicted NHD risk (x-axis) vs. observed NHD rate (y-axis) by ",
             "quantile bin. Dotted diagonal = perfect calibration.")
    else
      paste0("Calibration of the Iannuzzi 2020 temporal recalibration specification (Table 4): ",
             "a logistic regression of the total integer score fitted on the chronologically ",
             "earlier half of the cohort and evaluated on the later half. Mean predicted NHD ",
             "risk (x-axis) vs. observed NHD rate (y-axis) by quantile bin. Dotted diagonal = ",
             "perfect calibration.")
    doc <- body_add_img(doc, src = dual_cal_file, width = 5.0, height = 5.0)
    doc <- add_doc_caption(doc, fig3_title, fig3_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 4 (calibration) added.")
  } else if (file.exists(recalibrated_calibration_plot_temp)) {
    # Fallback: single-curve, Iannuzzi 2020 temporal recalibration only (the
    # published-lookup plot is never used for Figure 4 — see header comment).
    cal_caption <- paste0(
      "Calibration plot for the Iannuzzi 2020 temporal recalibration model. ",
      "Mean predicted non-home discharge risk (x-axis) vs. observed NHD rate (y-axis) ",
      "by quantile bin. Dashed diagonal = perfect calibration."
    )
    doc <- body_add_img(doc, src = recalibrated_calibration_plot_temp, width = 4.5, height = 4.5)
    doc <- add_doc_caption(doc, "Figure 4. Iannuzzi 2020: temporal recalibration calibration plot.", cal_caption)
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 4 (single calibration fallback) added.")
  }

    # ---- Risk tier table (after Figure 4) ------------------------------------
  # Multi-model risk tier analysis across every available score specification.
  # Tier thresholds: Low <60%, High >60% (two tiers; see pad-amp-nhd-val's
  # aggregate_report_inputs.R for the actual boundary -- not restated as a
  # number here a second time after this comment went stale across two
  # earlier threshold changes without being updated).
  # Each model section contributes one row per tier (now two, was three)
  # with a header separator row.
  #
  # EVALUATION SET: every recalibrated specification is restricted to the test
  # partition (split_set == "test"), matching Table 4. Tiering the recalibrated
  # models over the full cohort re-included the rows their own recalibration was
  # fitted on. The published-lookup specification is exempt — nothing is fitted,
  # so it is tiered over the full cohort, consistent with Table 4.
  # (.test_rows() itself is defined earlier in this function, near the DCA plot,
  # since that is its first point of use.)
  # AGGREGATE-ONLY (2026-08-11). Tiering itself -- assigning each patient to a
  # risk band and counting -- moved to pad-amp-nhd-val's aggregate step, which
  # emits agg_risk_tiers.csv with one row per (model, tier) plus the published
  # Iannuzzi strata and their published rates. This function now only formats
  # those counts into the table's two-level layout.
  #
  # Suppressed tiers arrive with n/events as NA and render as "—" rather than
  # as 0, so a small cell is never mistaken for an empty one.
  build_tier_rows_from_agg <- function(agg, model_label) {
    if (is.null(agg) || nrow(agg) == 0) return(NULL)
    d <- agg[agg$model_label == model_label, , drop = FALSE]
    if (nrow(d) == 0) return(NULL)

    header_row <- data.frame(
      "Risk Tier"             = model_label,
      "N"                     = "",
      "NHD Events"            = "",
      "Observed NHD Rate (%)" = "",
      is_model_header         = TRUE,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    # Column percents (added 2026-10-05): each tier's share of THIS model's
    # patients (N column) and of THIS model's NHD events (NHD Events column),
    # i.e. the denominator is the model's own column total, not the whole
    # table. The total is only knowable when no tier in the model was
    # small-cell-suppressed (a suppressed cell arrives as NA); computing a
    # percent against a total that silently omits it would be wrong, so the
    # percents are left off for the whole model section in that case.
    n_all  <- suppressWarnings(as.numeric(d$n))
    ev_all <- suppressWarnings(as.numeric(d$events))
    n_col_total  <- if (!anyNA(n_all)  && sum(n_all)  > 0) sum(n_all)  else NA_real_
    ev_col_total <- if (!anyNA(ev_all) && sum(ev_all) > 0) sum(ev_all) else NA_real_
    with_col_pct <- function(x, total) {
      if (is.na(x)) return("\u2014")                       # suppressed
      txt <- as.character(as.integer(x))
      if (is.na(total)) txt else paste0(txt, " (", round(100 * x / total, 1), "%)")
    }
    tier_rows <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
      n_tier <- suppressWarnings(as.numeric(d$n[i]))
      n_ev   <- suppressWarnings(as.numeric(d$events[i]))
      pub    <- if ("published_rate_pct" %in% names(d))
                  suppressWarnings(as.numeric(d$published_rate_pct[i])) else NA_real_
      obs_rate <- if (is.na(n_tier) || is.na(n_ev)) {
        "\u2014"                      # suppressed
      } else if (n_tier > 0) {
        paste0(round(100 * n_ev / n_tier, 1), "%",
               if (!is.na(pub)) paste0(" (published ", pub, "%)") else "")
      } else "N/A"
      data.frame(
        "Risk Tier"             = paste0("  ", d$tier[i]),
        "N"                     = with_col_pct(n_tier, n_col_total),
        "NHD Events"            = with_col_pct(n_ev, ev_col_total),
        "Observed NHD Rate (%)" = obs_rate,
        is_model_header         = FALSE,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }))
    rbind(header_row, tier_rows)
  }

  agg_tiers <- read_report_input("agg_risk_tiers")

  # Restricted to the three recalibrated specifications -- the published
  # Iannuzzi lookup and the Iannuzzi 2020 publication's own raw-score strata
  # are still computed and present in agg_risk_tiers.csv (see
  # pad-amp-nhd-val's R/aggregate_report_inputs.R) but deliberately not
  # rendered here, so this table compares only the recalibrated models
  # against each other.
  tier_sections <- list()
  for (.lbl in c("Iannuzzi 2020 (Recalibrated, test set)",
                 "mFI-5 (Recalibrated, test set)",
                 "sVQI-FS (Recalibrated, test set)")) {
    sec <- build_tier_rows_from_agg(agg_tiers, .lbl)
    if (!is.null(sec)) tier_sections[[.lbl]] <- sec
  }

  tier_sections <- Filter(Negate(is.null), tier_sections)

  # The caption's tier-threshold description is derived directly from the
  # tier labels actually present in the data (e.g. "Low (<50%)") rather than
  # hardcoded here, so it can never drift out of sync with
  # aggregate_report_inputs.R's actual bin edges -- exactly the kind of
  # two-sources-of-truth bug this report has been bitten by before (see the
  # subgroup-bias supplemental-label history elsewhere in this file).
  # n_tiers (and the "two"/"three" word derived from it below) is likewise
  # counted from the data rather than hardcoded -- a caption once claimed
  # "three tiers" through two separate threshold-scheme changes (3 tiers,
  # then 2) before anyone noticed it never read the actual row count.
  n_tiers <- if (length(tier_sections) > 0) {
    sum(!tier_sections[[1]]$is_model_header)
  } else {
    2L
  }
  tier_threshold_desc <- if (length(tier_sections) > 0) {
    first_sec <- tier_sections[[1]]
    paste(trimws(first_sec[["Risk Tier"]][!first_sec$is_model_header]), collapse = ", ")
  } else {
    "Low and High"
  }
  tier_count_word <- c("one", "two", "three", "four", "five")[min(n_tiers, 5)]

  if (length(tier_sections) > 0) {
    tier_all <- do.call(rbind, tier_sections)
    row.names(tier_all) <- NULL

    # Row indices for model-header rows (bold + shaded)
    header_idx  <- which(tier_all$is_model_header)
    tier_display <- tier_all[, c("Risk Tier", "N", "NHD Events", "Observed NHD Rate (%)"),
                              drop = FALSE]

    tier_ft <- flextable::flextable(tier_display) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::bold(i = header_idx, part = "body") |>
      flextable::bg(i = header_idx, bg = "#F2F2F2", part = "body") |>
      flextable::align(j = c("N", "NHD Events", "Observed NHD Rate (%)"),
                       align = "center", part = "all") |>
      flextable::width(j = "Risk Tier",              width = 2.2) |>
      flextable::width(j = "N",                      width = 1.1) |>
      flextable::width(j = "NHD Events",             width = 1.2) |>
      flextable::width(j = "Observed NHD Rate (%)",  width = 1.6) |>
      flextable::set_table_properties(layout = "fixed")

    n_tier_models <- length(tier_sections)
    # tier_sections is now keyed by the model label itself (it is built by
    # looping over labels read from agg_risk_tiers.csv), so the caption list is
    # just its names. This previously mapped four short keys ("lookup",
    # "recal", "mfi5", "vqifs", "iannuzzi_score_bands") to display strings; when
    # the keys became labels that lookup silently matched nothing and the
    # caption rendered as "five specifications are shown in separate sections: ."
    model_names_used <- names(tier_sections)
    n_word <- c("one", "two", "three", "four", "five")[min(n_tier_models, 5)]

    doc <- body_add_par(doc, section_num("Risk tier analysis"), style = "heading 3")
    doc <- body_add_flextable(doc, tier_ft)
    doc <- add_doc_caption(doc,
      paste0(
        "Table 5. Risk tier classification by model specification. ",
        "Patients are stratified into ", tier_count_word, " tiers based on each recalibrated ",
        "model's predicted NHD risk: ", tier_threshold_desc, ". ",
        "The observed NHD rate within each tier provides a direct assessment of clinical utility."
      ),
      paste0(
        n_word, " specification", if (n_tier_models > 1) "s are" else " is",
        " shown in separate sections: ",
        paste(model_names_used, collapse = "; "), ". ",
        "EVALUATION SET: all three specifications are restricted to the temporal test ",
        "partition, matching Table 4, because the training half was used to fit their ",
        "recalibration. ",
        "N = number of patients assigned to that tier by the corresponding model. ",
        "NHD Events = number with non-home discharge. ",
        "Percentages in parentheses are column percents: the share of that model's ",
        "patients (N) or of its NHD events (NHD Events) falling in each tier. ",
        "Observed NHD Rate = NHD Events / N (the row percent). ",
        "Note that logistic recalibration of a weakly discriminating integer score compresses ",
        "the predicted probability range toward the cohort base rate; when this places every ",
        "patient in a single probability tier, that is a substantive finding about the score's ",
        "limited spread, not a computational artefact."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Risk tier table (Table 5, ", n_tier_models, " sections) added.")
  }

  # ---- Table 6: NHD rate by mFI-5 point score -------------------------------
  # Purely descriptive (not a model-evaluation metric like Table 4/5), so it
  # combines the train+test partitions to describe the full validation cohort
  # rather than restricting to the test half. Built from
  # agg_score_value_counts.csv, which already carries one row per
  # (split_set, total_score, score_id) -- no new upstream artifact needed.
  # A score value's row is combined (train n + test n) only when NEITHER
  # split's count was small-cell-suppressed; if either was, the combined row
  # is suppressed too, since summing a known count with an unknown
  # (suppressed) one would not be a safe disclosure.
  sv_counts <- read_report_input("agg_score_value_counts")
  sv_mfi5   <- if (!is.null(sv_counts) && "score_id" %in% names(sv_counts)) {
    sv_counts[sv_counts$score_id == "mfi5", , drop = FALSE]
  } else NULL

  if (!is.null(sv_mfi5) && nrow(sv_mfi5) > 0) {
    scores <- sort(unique(as.numeric(sv_mfi5$total_score)))
    score_rate_tbl <- do.call(rbind, lapply(scores, function(s) {
      rows      <- sv_mfi5[as.numeric(sv_mfi5$total_score) == s, , drop = FALSE]
      train_row <- rows[rows$split_set == "train", , drop = FALSE]
      test_row  <- rows[rows$split_set == "test",  , drop = FALSE]
      # A missing row means zero patients at that score in that split (a true
      # zero); a present row with NA n_total means suppressed. These are NOT
      # the same thing, so absence defaults to 0, not NA.
      n_train <- if (nrow(train_row) == 1) suppressWarnings(as.numeric(train_row$n_total[1]))  else 0
      n_test  <- if (nrow(test_row)  == 1) suppressWarnings(as.numeric(test_row$n_total[1]))   else 0
      e_train <- if (nrow(train_row) == 1) suppressWarnings(as.numeric(train_row$n_events[1])) else 0
      e_test  <- if (nrow(test_row)  == 1) suppressWarnings(as.numeric(test_row$n_events[1]))  else 0
      suppressed <- is.na(n_train) || is.na(n_test)
      n_comb <- if (suppressed) NA_real_ else n_train + n_test
      e_comb <- if (suppressed) NA_real_ else e_train + e_test
      data.frame(
        Score  = as.integer(s),
        N      = if (is.na(n_comb)) "—" else as.character(n_comb),
        Events = if (is.na(n_comb)) "—" else as.character(e_comb),
        Rate   = if (is.na(n_comb) || n_comb == 0) "—"
                 else paste0(round(100 * e_comb / n_comb, 1), "%"),
        stringsAsFactors = FALSE
      )
    }))

    score_rate_ft <- flextable::flextable(score_rate_tbl) |>
      flextable::set_header_labels(
        Score = "mFI-5 Score", N = "N", Events = "NHD Events", Rate = "Observed NHD Rate"
      ) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::align(j = c("Score", "N", "Events", "Rate"), align = "center", part = "all") |>
      flextable::width(j = "Score",  width = 1.2) |>
      flextable::width(j = "N",      width = 1.0) |>
      flextable::width(j = "Events", width = 1.2) |>
      flextable::width(j = "Rate",   width = 1.4) |>
      flextable::set_table_properties(layout = "fixed")

    doc <- body_add_flextable(doc, score_rate_ft)
    doc <- add_doc_caption(doc,
      "Table 6. Non-home discharge rate by mFI-5 point score.",
      paste0(
        "Observed NHD rate at each mFI-5 integer score value (0–5), combining the ",
        "temporal train and test partitions to describe the full validation cohort. ",
        "N = patients with that score; NHD Events = number with non-home discharge. ",
        "Score values with fewer than 5 patients in either partition are suppressed."
      )
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] mFI-5 score-rate table (Table 6) added.")
  }

  # ---- Figure 5: Decision curve analysis ------------------------------------
  doc <- add_doc_page_break(doc)
  doc <- body_add_par(doc, section_num("Decision curve analysis"), style = "heading 3")
  if (!is.null(dca_plot_file) && file.exists(dca_plot_file)) {
    doc <- body_add_img(doc, src = dca_plot_file, width = 5.5, height = 3.8)
    doc <- add_doc_caption(doc, "Figure 5. Decision curve analysis.", paste0(
      "Decision curve analysis for the recalibrated Iannuzzi 2020, mFI-5, and sVQI-FS ",
      "specifications available in this run, evaluated on the temporal test partition so ",
      "that all curves compare net benefit across the same patients. ",
      "Net benefit is plotted across threshold probabilities from 1% to 99%. ",
      "Model curves are compared with the 'treat-all' (dashed) and ",
      "'treat-none' (zero reference) strategies. Threshold probabilities correspond ",
      "to the minimum predicted NHD risk at which a clinician would recommend an intervention."
    ))
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Figure 5 (DCA) added.")

    # ---- Net benefit at selected thresholds (exact values) -------------------
    # save_dca_plot() already computes this table purely to write it as a
    # sidecar CSV (decision_curve_net_benefit.csv, next to the figure) --
    # reading it back and rendering it here gives readers exact numbers at
    # the thresholds where the curves above are hardest to tell apart, on top
    # of the existing grey level/linetype/shape encoding and heavier model
    # (vs. reference) line weight. No new computation; same values plotted.
    nb_csv_path <- file.path(temp_figure_dir, "decision_curve_net_benefit.csv")
    if (file.exists(nb_csv_path)) {
      nb_tbl <- tryCatch(
        utils::read.csv(nb_csv_path, check.names = FALSE, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (!is.null(nb_tbl) && nrow(nb_tbl) > 0) {
        pct_cols  <- setdiff(names(nb_tbl), "Strategy")
        pct_width <- max(0.45, min(0.75, 5.0 / max(1, length(pct_cols))))
        nb_ft <- flextable::flextable(nb_tbl) |>
          flextable::colformat_double(j = pct_cols, digits = 3) |>
          flextable::bold(part = "header") |>
          flextable::fontsize(size = 8, part = "all") |>
          flextable::font(fontname = "Calibri", part = "all") |>
          flextable::bg(part = "header", bg = "#1F3864") |>
          flextable::color(part = "header", color = "white") |>
          flextable::padding(padding = 3, part = "all") |>
          flextable::width(j = "Strategy", width = 1.6) |>
          flextable::width(j = pct_cols, width = pct_width) |>
          flextable::align(j = pct_cols, align = "center", part = "all") |>
          flextable::set_table_properties(layout = "fixed")
        doc <- body_add_flextable(doc, nb_ft)
        doc <- add_doc_caption(doc,
          "Net benefit at selected threshold probabilities.",
          paste0(
            "Exact net benefit for each strategy in Figure 5, at 10-percentage-point increments ",
            "of the threshold probability, for comparing curves that are close together. Values ",
            "match the plotted curves exactly."
          )
        )
        doc <- body_add_par(doc, "", style = "Normal")
        message("[report] DCA net-benefit table added.")
      }
    }
  }

  # Helper: load, sort, and render a subgroup_bias.csv as a table + two forest
  # plots (calibration ECE, then discrimination AUROC). Used for both Iannuzzi
  # recalibrated and mFI-5 recalibrated subgroup sections. Takes the
  # ALREADY-READ data frame (from .read_subgroup_bias()), not a path. It used
  # to re-read and re-test the file itself, which is how the render decision
  # drifted away from the supplemental-label decision: two reads, two
  # predicates, two answers. One read, one predicate, passed in.
  #
  # `auroc` / `auroc_ci_lower` / `auroc_ci_upper` may be absent in a stale
  # subgroup_bias.csv written before compute_subgroup_bias() assessed
  # discrimination \u2014 the AUROC column/figure are skipped gracefully rather
  # than erroring in that case.
  add_subgroup_section <- function(doc, df, overall_ece_val, overall_auroc_val,
                                   section_heading, table_caption, figure_caption,
                                   figure_caption_auroc, forest_file_name,
                                   forest_file_name_auroc) {
    if (is.null(df) || nrow(df) == 0) return(doc)

    has_auroc <- all(c("auroc", "auroc_ci_lower", "auroc_ci_upper") %in% names(df))

    subgroup_order <- c(age_group = 1, sex = 2, race = 3, ethnicity = 4,
                        indication = 5, proc_type = 6, year = 7)
    sort_key <- subgroup_order[match(df$subgroup_var, names(subgroup_order))]
    sort_key[is.na(sort_key)] <- 99L
    df <- df[order(sort_key, df$subgroup_level), ]

    bias_display <- data.frame(
      Subgroup  = tools::toTitleCase(gsub("_", " ", df$subgroup_var)),
      Level     = df$subgroup_level,
      N         = df$n,
      Events    = df$n_events,
      ECE       = round(df$ece, 3),
      "ECE 95% CI"  = paste0("(", round(df$ci_lower, 3),
                         "\u2013",
                         round(df$ci_upper, 3), ")"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    if (has_auroc) {
      bias_display$AUROC <- ifelse(is.na(df$auroc), "\u2014", round(df$auroc, 3))
      bias_display[["AUROC 95% CI"]] <- ifelse(
        is.na(df$auroc_ci_lower) | is.na(df$auroc_ci_upper), "\u2014",
        paste0("(", round(df$auroc_ci_lower, 3), "\u2013", round(df$auroc_ci_upper, 3), ")")
      )
    }

    center_cols <- c("N", "Events", "ECE", "ECE 95% CI")
    if (has_auroc) center_cols <- c(center_cols, "AUROC", "AUROC 95% CI")

    bias_ft <- flextable::flextable(bias_display) |>
      flextable::bold(part = "header") |>
      flextable::fontsize(size = 10, part = "all") |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::bg(part = "header", bg = "#1F3864") |>
      flextable::color(part = "header", color = "white") |>
      flextable::padding(padding = 4, part = "all") |>
      flextable::align(j = center_cols, align = "center", part = "all") |>
      flextable::width(j = "Subgroup",     width = 1.2) |>
      flextable::width(j = "Level",        width = 1.6) |>
      flextable::width(j = "N",            width = 0.6) |>
      flextable::width(j = "Events",       width = 0.7) |>
      flextable::width(j = "ECE",          width = 0.7) |>
      flextable::width(j = "ECE 95% CI",   width = 1.1) |>
      flextable::set_table_properties(layout = "fixed")
    if (has_auroc) {
      bias_ft <- bias_ft |>
        flextable::width(j = "AUROC",        width = 0.7) |>
        flextable::width(j = "AUROC 95% CI", width = 1.1)
    }

    doc <- body_add_par(doc, section_heading, style = "heading 3")
    doc <- body_add_flextable(doc, bias_ft)
    doc <- add_doc_caption(doc,
      table_caption,
      paste0("Subgroups with fewer than 10 observed NHD events are suppressed. ",
             "Overall ECE = ", round(overall_ece_val, 3),
             if (has_auroc) paste0(". Overall AUROC = ", round(overall_auroc_val, 3)) else "",
             ". 95% CI = bootstrap percentile interval (B\u2009=\u2009200 resamples).")
    )
    doc <- body_add_par(doc, "", style = "Normal")

    forest_png <- save_subgroup_forest_plot(df, overall_ece_val,
                                            temp_figure_dir,
                                            file_name = forest_file_name,
                                            metric = "ece")
    if (!is.null(forest_png) && file.exists(forest_png)) {
      plot_height <- max(4.0, nrow(df) * 0.35 + 1.5)
      doc <- body_add_img(doc, src = forest_png,
                          width  = 5.5,
                          height = min(plot_height, 9.0))
      doc <- add_doc_caption(doc,
        figure_caption,
        paste0("Expected calibration error (ECE) with 95% bootstrap percentile CIs ",
               "(B\u2009=\u2009200) by subgroup. ",
               "Dashed vertical line = overall ECE. ",
               "Subgroups with < 10 NHD events are suppressed.")
      )
      doc <- body_add_par(doc, "", style = "Normal")
    }

    if (has_auroc) {
      auroc_df <- df[!is.na(df$auroc), , drop = FALSE]
      forest_png_auroc <- save_subgroup_forest_plot(auroc_df, overall_auroc_val,
                                                     temp_figure_dir,
                                                     file_name = forest_file_name_auroc,
                                                     metric = "auroc")
      if (!is.null(forest_png_auroc) && file.exists(forest_png_auroc)) {
        plot_height <- max(4.0, nrow(auroc_df) * 0.35 + 1.5)
        doc <- body_add_img(doc, src = forest_png_auroc,
                            width  = 5.5,
                            height = min(plot_height, 9.0))
        doc <- add_doc_caption(doc,
          figure_caption_auroc,
          paste0("AUROC with 95% bootstrap percentile CIs ",
                 "(B\u2009=\u2009200) by subgroup. ",
                 "Dashed vertical line = overall AUROC. ",
                 "Subgroups with < 10 NHD events, or a constant outcome within ",
                 "a subgroup, are suppressed.")
        )
        doc <- body_add_par(doc, "", style = "Normal")
      }
    }
    doc
  }

  # ---- Subgroup bias assessment \u2014 mFI-5 ONLY --------------------------------
  #
  # Iannuzzi 2020 and sVQI-FS subgroup sections were removed 2026-09-06 at the
  # study team's request: the report presents subgroup calibration for the
  # mFI-5 alone. Their subgroup_bias.csv files are still WRITTEN by the scoring
  # step (all three scores produce one now) and remain available for inspection
  # in output/<score>/ -- they are simply not rendered. The supplemental label
  # plan in .init_supp_labels() allocates no keys for them; if either section is
  # ever restored, restore its keys there in the same change.

  # ---- Subgroup \u2014 mFI-5 recalibrated ----------------------------------------
  # Gated on has_mfi5_bias (resolved once, near the top, by .read_subgroup_bias)
  # rather than on the output directory merely existing. This is the same flag
  # that decided whether to allocate the S-numbers and whether to write the
  # Methods cross-reference, so all three agree by construction.
  if (has_mfi5_bias) {
    mfi5_recal_metrics <- tryCatch({
      mfi5_met <- readr::read_csv(file.path(mfi5_output_dir, "metrics.csv"),
                                  show_col_types = FALSE)
      names(mfi5_met) <- tolower(names(mfi5_met))
      list(
        ece   = as.numeric(mfi5_met$value[mfi5_met$metric == "ECE" &
                                            mfi5_met$model == "recalibrated"][1]),
        auroc = as.numeric(mfi5_met$value[mfi5_met$metric == "AUROC" &
                                            mfi5_met$model == "recalibrated"][1])
      )
    }, error = function(e) list(ece = NA_real_, auroc = NA_real_))
    ece_mfi5_recal   <- if (is.na(mfi5_recal_metrics$ece))   0.0 else mfi5_recal_metrics$ece
    auroc_mfi5_recal <- if (is.na(mfi5_recal_metrics$auroc)) 0.0 else mfi5_recal_metrics$auroc

    doc <- add_subgroup_section(
      doc,
      df                = subgroup_bias_mfi5,
      overall_ece_val   = ece_mfi5_recal,
      overall_auroc_val = auroc_mfi5_recal,
      section_heading   = section_num("Subgroup bias assessment \u2014 mFI-5 (Recalibrated)"),
      table_caption     = "Table 7. ECE and AUROC by subgroup \u2014 mFI-5 (Recalibrated).",
      figure_caption    = "Figure 6. Subgroup calibration forest plot \u2014 mFI-5 (Recalibrated).",
      figure_caption_auroc = "Figure 7. Subgroup discrimination forest plot \u2014 mFI-5 (Recalibrated).",
      forest_file_name  = "subgroup_forest_mfi5.png",
      forest_file_name_auroc = "subgroup_forest_mfi5_auroc.png"
    )
    message("[report] Subgroup section (mFI-5 recalibrated) rendered: ",
            nrow(subgroup_bias_mfi5), " subgroup rows.")
  }

  # ---- Supplemental section ------------------------------------------------
  doc <- body_add_par(doc, section_major("Supplemental Material"), style = "heading 2")

  # ---- S1–S3: DB-sourced tables (own connection; order: CDM, CPT, ICD) ------
  if (!is.null(config)) {
    tryCatch({

      # ---- Supplemental Table S1 — CDM Source --------------------------------
      tryCatch({
        cdm_src_raw <- read_report_input("supp_cdm_source")
        names(cdm_src_raw) <- tolower(names(cdm_src_raw))
        if (nrow(cdm_src_raw) > 0) {
          cdm_src_display <- data.frame(
            Field = c("CDM Source Name", "Source Abbreviation", "CDM Holder",
                      "Source Release Date", "CDM Release Date",
                      "CDM Version", "Vocabulary Version",
                      "Study Start Date", "Study End Date"),
            Value = c(as.character(cdm_src_raw$cdm_source_name[1]),
                      as.character(cdm_src_raw$cdm_source_abbreviation[1]),
                      as.character(cdm_src_raw$cdm_holder[1]),
                      as.character(cdm_src_raw$source_release_date[1]),
                      as.character(cdm_src_raw$cdm_release_date[1]),
                      as.character(cdm_src_raw$cdm_version[1]),
                      as.character(cdm_src_raw$vocabulary_version[1]),
                      if (!is.null(config$study_start_date)) as.character(config$study_start_date) else "N/A",
                      if (!is.null(config$study_end_date))   as.character(config$study_end_date)   else "N/A"),
            stringsAsFactors = FALSE
          )
          cdm_src_ft <- flextable::flextable(cdm_src_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 10, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 4, part = "all") |>
            flextable::width(j = "Field", width = 2.0) |>
            flextable::width(j = "Value", width = 4.0) |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "CDM source metadata", style = "heading 3")
          doc <- body_add_flextable(doc, cdm_src_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("cdm_metadata"), ". CDM source metadata."),
            paste0("Metadata from the cdm_source table of the OMOP CDM instance ",
                   "used for this analysis. CDM Version and Vocabulary Version confirm ",
                   "compliance with OMOP CDM v5.4 and the Athena vocabulary release used ",
                   "during ETL.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S1 (CDM source) added.")
        }
      }, error = function(e) {
        message("[report] CDM source table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table — ATLAS concept sets and cohorts ---------------
      #
      # Added 2026-09-13, replacing the concept IDs and OMOP table/column names
      # that used to be listed inline in Methods and in Table 1's/Table S3's
      # captions, now that every cohort and concept set this study uses is
      # registered in OHDSI ATLAS (atlas-demo.ohdsi.org). One row per cohort in
      # pad-amp-nhd-val/inst/Cohorts.csv, plus a final row group for the four
      # predictors resolved by a direct concept/demographic query rather than a
      # cohort (age bands, sex, race, and sVQI-FS's BMI-derived underweight
      # item) — included for completeness, marked accordingly in the ATLAS ID
      # column. This table is static (it describes the study design, not a
      # data source), so it is built directly rather than read from a CSV.
      tryCatch({
        # Concept IDs below are the cohort's ACTUAL primary/anchor concept(s)
        # as authored in pad-amp-nhd-val/inst/cohorts/*.json and inst/Cohorts.csv
        # (verified against those files directly, not recalled from memory --
        # e.g. the target cohort's PAD indication is anchored on 3654996 with
        # 13 related concepts, a DIFFERENT and broader set than cohort 9100003's
        # 317309, which is used only for the sVQI-FS pvd covariate; conflating
        # the two would have been a real transcription error). Large concept
        # sets are given as "anchor + N others" rather than enumerated in full
        # -- the full expansion is in the source repo's inst/cohorts/*.json.
        atlas_inventory <- data.frame(
          "ATLAS ID" = c(
            "9100011", "9100001",
            "9100024", "9100023", "9100022", "9100021",
            "9100002", "9100003", "9100004", "9100026", "9100027", "9100025",
            "9100007", "9100008", "9100009", "9100010",
            "1797941",
            "N/A", "N/A", "N/A", "N/A"
          ),
          "Cohort / Concept Set" = c(
            "[DVI] Major LE Amputation (dysvascular, trauma/cancer excluded) — target",
            "[DVI] Non-Home Discharge — outcome",
            "[DVI] Coronary Artery Disease (risk score item)",
            "[DVI] Heart Failure (risk score item)",
            "[DVI] Hypertension (risk score item)",
            "[DVI] Diabetes Mellitus (risk score item)",
            "[DVI] COPD (risk score item)",
            "[DVI] Peripheral Arterial Occlusive Disease",
            "[DVI] Ischemic Tissue Loss",
            "[DVI] Dependent Functional Status (ADL dependence)",
            "[DVI] Ambulatory Status (aid use / impaired ambulation)",
            "[DVI] Pneumonia (mFI-5 current pneumonia)",
            "[DVI] Insulin-Treated Diabetes",
            "[DVI] Anemia (Hgb <10 g/dL)",
            "[DVI] Anemia (sex-specific threshold)",
            "[DVI] Renal Impairment",
            "[DVI] Major LE Amputation — superseded target, kept for lineage",
            "Age bands (60–69, 70–79, ≥80)",
            "Female sex",
            "Non-White race",
            "Underweight (BMI, sVQI-FS)"
          ),
          "Concept ID(s)" = c(
            "Procedure: 4195136 + 8 others; PAD: 3654996 + 13 others; DM: 201820, 442793; Wound: 197304 + 23 others; Trauma excl.: 194229, 72487, 197751, 4095264, 4187096; Malignancy excl.: 4177242",
            "None — dynamic vocabulary lookup",
            "4185932",
            "316139",
            "316866",
            "201820",
            "255573",
            "317309",
            "4029926, 4291464",
            "Observation: 4044722 + 22 others; Condition: 45770280",
            "Observation: 4012645, 4044714, 439405, 4086548, 4058155, 4058154 + 7 others; Device: 4240470, 37165652, 4141765, 4251933",
            "255848",
            "21600713",
            "3000963",
            "3000963",
            "3016723, 4146536, 4032243",
            "Same procedure concepts as 9100011 above",
            "None — computed from person.year_of_birth",
            "8532",
            "Excludes 8527 (White)",
            "3038553 + 8 others (weight/height fallback)"
          ),
          "Standard-Concept Logic" = c(
            "Major LE amputation procedure with a qualifying PAD, diabetes, or lower-extremity wound indication; excludes limb-trauma or lower-limb-malignancy codes on the index visit",
            "Discharge disposition resolved dynamically against the local vocabulary; no fixed concept id",
            "Ischemic heart disease diagnosis",
            "Heart failure diagnosis",
            "Hypertension diagnosis and descendants",
            "Diabetes mellitus diagnosis and descendants",
            "COPD diagnosis and descendants",
            "Peripheral arterial disease diagnosis and descendants",
            "Ischemic ulcer or ischemic gangrene diagnosis",
            "Activities-of-daily-living dependence finding, bed-ridden, confined to chair, or severe frailty",
            "Walking-aid, wheelchair, walker, walking-frame or crutch use, or a finding of impaired ambulation",
            "Pneumonia diagnosis and descendants (30-day window; OR'd with COPD for the mFI-5 item)",
            "Insulin exposure",
            "Hemoglobin measurement below a fixed threshold",
            "Hemoglobin measurement below a sex-specific threshold",
            "Elevated creatinine, or dialysis (procedure or diagnosis)",
            "Inpatient visit containing a major LE amputation procedure",
            "Computed from year of birth relative to the index date",
            "Gender concept",
            "Race concept (non-White)",
            "Computed from height and weight measurements"
          ),
          "OMOP Table(s)" = c(
            "ProcedureOccurrence, ConditionOccurrence, VisitOccurrence",
            "VisitOccurrence",
            "ConditionOccurrence", "ConditionOccurrence", "ConditionOccurrence", "ConditionOccurrence",
            "ConditionOccurrence", "ConditionOccurrence", "ConditionOccurrence",
            "ConditionOccurrence, Observation",
            "Observation, DeviceExposure",
            "ConditionOccurrence",
            "DrugExposure", "Measurement", "Measurement",
            "Measurement, ProcedureOccurrence, ConditionOccurrence",
            "ProcedureOccurrence, VisitOccurrence",
            "Person", "Person", "Person", "Measurement"
          ),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        atlas_ft <- flextable::flextable(atlas_inventory) |>
          flextable::bold(part = "header") |>
          flextable::fontsize(size = 8, part = "all") |>
          flextable::font(fontname = "Calibri", part = "all") |>
          flextable::bg(part = "header", bg = "#1F3864") |>
          flextable::color(part = "header", color = "white") |>
          flextable::padding(padding = 3, part = "all") |>
          flextable::width(j = "ATLAS ID", width = 0.6) |>
          flextable::width(j = "Cohort / Concept Set", width = 1.6) |>
          flextable::width(j = "Concept ID(s)", width = 2.4) |>
          flextable::width(j = "Standard-Concept Logic", width = 2.0) |>
          flextable::width(j = "OMOP Table(s)", width = 1.2) |>
          flextable::set_table_properties(layout = "fixed")
        doc <- body_add_par(doc, "ATLAS concept sets and cohorts", style = "heading 3")
        doc <- body_add_flextable(doc, atlas_ft)
        doc <- add_doc_caption(doc,
          paste0(supp_table("concept_set_inventory"), ". ATLAS concept sets and cohorts used in this analysis."),
          paste0("Every cohort and concept set referenced in the Methods is named with a [DVI] prefix, ",
                 "following the convention of the shared OHDSI ATLAS instance (atlas-demo.ohdsi.org). Cohorts numbered 9100xxx are local ",
                 "to this study's reserved id block and not yet pushed to the shared ATLAS instance; ",
                 "1797941 is a shared, previously registered cohort. The final four rows ",
                 "are predictors resolved by a direct concept or demographic query rather than a cohort ",
                 "definition (ATLAS ID: N/A). Concept ID(s) lists each definition's primary standard ",
                 "concept(s); a set with more than a few members is given as an anchor concept plus a ",
                 "count of additional descendants/related concepts, with the full expansion available in ",
                 "the study repository's cohort definitions.")
        )
        doc <- body_add_par(doc, "", style = "Normal")
        message("[report] Supplemental Table (ATLAS concept sets and cohorts) added.")
      }, error = function(e) {
        message("[report] ATLAS concept set inventory table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S2 — Model descriptions -------------------------
      #
      # Read from covariates/model_metadata.yaml — the single source of truth
      # for score derivation details — rather than a hardcoded data.frame().
      #
      # WHY: the previous hardcoded block described a DIFFERENT score entirely
      # (NSQIP-derived, amputation cohort, 9 components including BMI/ABI/
      # operative time, AUROC 0.73) than the one Table 3b actually implements
      # (VSGNE-derived, elective lower-extremity bypass, C statistic 0.77).
      # Table 3b was always correct because it is generated from
      # covariates/covariates.csv; S2 was wrong because nothing forced it to
      # agree with that same source. Reading both from data (Table 3b from the
      # covariates CSVs, S2 from model_metadata.yaml, both curated files this
      # report treats as ground truth) can't re-diverge the way two
      # independently hand-typed R blocks did.
      tryCatch({
        model_meta <- yaml::read_yaml("covariates/model_metadata.yaml")

        # Build the Field/Description rows for one score's section. Optional
        # fields (derivation_n, derivation_nhd_rate, derivation_exclusions,
        # prediction_window, c_statistic_*) are omitted when NULL/empty in the
        # YAML rather than rendered as blank rows — e.g. the mFI-5 and sVQI-FS
        # entries have no published NHD derivation_n because they were not
        # derived to predict NHD.
        .fmt_field <- function(label, value) {
          if (is.null(value) || (is.character(value) && !nzchar(trimws(value)))) return(NULL)
          if (is.list(value)) value <- paste(unlist(value), collapse = "; ")
          # YAML ">" folded block scalars join lines with spaces but keep a
          # trailing newline; collapse any remaining whitespace runs (including
          # that trailing \n) to single spaces so it doesn't render as a stray
          # blank line inside the flextable cell.
          value <- gsub("\\s+", " ", trimws(as.character(value)))
          data.frame(Field = label, Description = value, stringsAsFactors = FALSE)
        }

        .build_score_rows <- function(meta) {
          derivation_cohort <- paste0(
            meta$derivation_registry,
            if (!is.null(meta$derivation_years)) paste0(", ", meta$derivation_years) else "",
            if (!is.null(meta$derivation_procedure)) paste0("; ", meta$derivation_procedure) else "",
            if (!is.null(meta$derivation_n)) paste0(" (n = ", format(meta$derivation_n, big.mark = ","), ")") else ""
          )
          exclusions_str <- if (length(meta$derivation_exclusions) > 0)
            paste0("Derivation exclusions: ", paste(unlist(meta$derivation_exclusions), collapse = "; "), ".")
          else NULL
          c_stat <- c(
            if (!is.null(meta$c_statistic_derivation))
              paste0("Derivation: ", meta$c_statistic_derivation) else NULL,
            if (!is.null(meta$c_statistic_validation))
              paste0("Validation: ", meta$c_statistic_validation) else NULL
          )
          c_stat_str <- if (length(c_stat) > 0) paste(c_stat, collapse = "; ") else NULL

          do.call(rbind, Filter(Negate(is.null), list(
            .fmt_field("Score name",        meta$score_name),
            .fmt_field("Reference",         meta$reference),
            .fmt_field("Derivation cohort", derivation_cohort),
            .fmt_field("Derivation exclusions", exclusions_str),
            .fmt_field("Outcome definition", meta$outcome_definition),
            .fmt_field("Prediction window",  meta$prediction_window),
            .fmt_field("Score design",       meta$score_design),
            .fmt_field("Score range",        meta$score_range),
            .fmt_field("Original discrimination (C statistic / AUC)", c_stat_str)
          )))
        }

        # Only include scores whose pipeline actually ran this report (mirrors
        # the has_mfi5/has_vqifs gating used everywhere else in this function).
        score_sections <- Filter(Negate(is.null), list(
          .build_score_rows(model_meta$iannuzzi),
          if (has_mfi5)  .build_score_rows(model_meta$mfi5),
          if (has_vqifs) .build_score_rows(model_meta$vqifs)
        ))

        # Interleave a blank spacer row between (not after) sections, then drop
        # it for the single-score-only case.
        model_desc_display <- score_sections[[1]]
        if (length(score_sections) > 1) {
          for (i in 2:length(score_sections)) {
            model_desc_display <- rbind(
              model_desc_display,
              data.frame(Field = "", Description = "", stringsAsFactors = FALSE),
              score_sections[[i]]
            )
          }
        }

        mdesc_ft <- flextable::flextable(model_desc_display) |>
          flextable::bold(part = "header") |>
          flextable::fontsize(size = 9, part = "all") |>
          flextable::font(fontname = "Calibri", part = "all") |>
          flextable::bg(part = "header", bg = "#1F3864") |>
          flextable::color(part = "header", color = "white") |>
          flextable::padding(padding = 3, part = "all") |>
          flextable::width(j = "Field",       width = 1.8) |>
          flextable::width(j = "Description", width = 4.7) |>
          flextable::bold(i = which(model_desc_display$Field %in%
                                      c("Score name")), part = "body") |>
          flextable::bg(i = which(model_desc_display$Field %in%
                                    c("Score name")), bg = "#F2F2F2", part = "body") |>
          flextable::set_table_properties(layout = "fixed")
        doc <- body_add_par(doc, "Model descriptions", style = "heading 3")
        doc <- body_add_flextable(doc, mdesc_ft)
        doc <- add_doc_caption(doc,
          paste0(supp_table("model_descriptions"), ". Description of integer risk score models evaluated."),
          paste0(
            "Derivation details for each score are read from covariates/model_metadata.yaml. ",
            "None of these instruments was derived to predict non-home discharge after major ",
            "lower-extremity amputation (see Methods 1.3 for the transportability framing this ",
            "study uses instead of describing itself as an external validation). ",
            "The Iannuzzi 2020 score's own outcome definition (rehabilitation or SNF discharge ",
            "only) is narrower than this study's NHD definition, which additionally counts ",
            "hospice and LTAC discharges as non-home — see the Limitations discussion. ",
            "The Iannuzzi 2020 NHD score uses a published lookup table to convert integer score ",
            "to predicted probability; a temporally recalibrated version is also evaluated. ",
            if (has_mfi5) paste0(
              "The Subramaniam mFI-5 — derived to predict mortality and postoperative ",
              "complications, not NHD — is evaluated via temporal logistic recalibration only ",
              "(no published NHD lookup table exists). "
            ) else "",
            if (has_vqifs) paste0(
              "The Kraiss 2022 sVQI-FS — derived to predict 9-month mortality, not NHD — is ",
              "likewise evaluated via temporal logistic recalibration only. "
            ) else ""
          )
        )
        doc <- body_add_par(doc, "", style = "Normal")
        message("[report] Supplemental Table S2 (model descriptions) added.")
      }, error = function(e) {
        message("[report] Model description table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S3 — CPT codes by procedure subgroup -----------
      # Uses concept_relationship ('Mapped from') to find CPT4 source codes
      # that map to SNOMED standard descendants — CPT4 codes are source codes
      # (standard_concept IS NULL), not in concept_ancestor as descendants.
      #
      # TWO BUGS FIXED HERE (2026-07-26), both of which independently produced
      # an all-zero table that contradicted the cohort's own existence:
      #
      # 1. NOT SCOPED TO THE COHORT: the original query counted ANY
      #    procedure_occurrence row anywhere in the CDM matching a CPT4/HCPCS
      #    concept, not procedures belonging to this study's target-cohort
      #    patients at their index date. Fixed by adding a target_population
      #    CTE (the same pattern used throughout R/risk_score_pipeline.R) and
      #    joining procedure_occurrence on person_id + procedure_date =
      #    cohort_start_date (verified against cohorts/target_amputation.sql:
      #    cohort_start_date IS the qualifying procedure's procedure_date).
      #
      # 2. procedure_source_concept_id MAY NEVER BE POPULATED WITH A CPT4
      #    CONCEPT: Synthea's procedures.csv (see synthea/modules/pad_amp_nhd.json)
      #    emits amputation procedures with their SNOMED display name directly —
      #    there is no evidence this ETL ever writes a CPT4 concept into
      #    procedure_source_concept_id for these rows; it may instead be 0,
      #    NULL, or the same SNOMED concept as procedure_concept_id. Added an
      #    OR'd fallback match against procedure_source_value = the CPT4
      #    concept_code text, in case the source system recorded the CPT4 code
      #    as a raw string without a successful concept mapping.
      #
      #    If BOTH bugs are fixed and every row is still 0 (i.e., this ETL
      #    genuinely never records CPT4 codes for these procedures — expected
      #    for Synthea-generated SNOMED-native procedure data), this function
      #    now says so explicitly in the report rather than rendering a table
      #    that silently implies the cohort itself is empty. See the
      #    total_cases == 0 branch below.
      tryCatch({
        cpt_raw <- read_report_input("supp_cpt_codes")
        names(cpt_raw) <- tolower(names(cpt_raw))
        total_cases <- sum(cpt_raw$case_count, na.rm = TRUE)
        if (nrow(cpt_raw) > 0 && total_cases == 0) {
          # Both scoping bugs above are fixed, and the table is STILL all
          # zero: this ETL genuinely never records a CPT4/HCPCS concept or
          # source value for these procedures (expected when procedures are
          # sourced from Synthea's native SNOMED procedure codes). Say so
          # explicitly — loudly, per the plan's sanity-check requirement —
          # rather than rendering a table whose all-zero rows would otherwise
          # read as evidence the cohort itself is empty.
          message("[report] Supplemental Table S3 skipped: 0 procedure_occurrence rows matched ",
                  "any CPT4/HCPCS concept or source value for the target cohort at its index date ",
                  "after scoping the query correctly. This ETL likely records index amputation ",
                  "procedures using native SNOMED codes only (see synthea/modules/pad_amp_nhd.json) ",
                  "and never populates a CPT4 procedure_source_concept_id/procedure_source_value.")
          doc <- body_add_par(doc, "Index procedure CPT codes", style = "heading 3")
          doc <- body_add_par(doc, paste0(
            "No CPT-4/HCPCS-coded procedure records were found for the target cohort at their ",
            "index amputation date. Index amputations in this dataset are captured using native ",
            "standard (SNOMED) procedure concepts rather than CPT-4 source codes (see Table 1 and ",
            "the amputation-level concept IDs cited in its footnote); a CPT-4 crosswalk table is ",
            "therefore not applicable to this data source and is omitted rather than shown as an ",
            "uninformative all-zero table."
          ), style = "Normal")
          doc <- body_add_par(doc, "", style = "Normal")
        } else if (nrow(cpt_raw) > 0) {
          cpt_display <- data.frame(
            "Procedure Group" = cpt_raw$proc_group,
            "CPT Code"        = cpt_raw$cpt_code,
            "Description"     = cpt_raw$cpt_description,
            "Cases (n)"       = cpt_raw$case_count,
            check.names = FALSE, stringsAsFactors = FALSE
          )
          cpt_ft <- flextable::flextable(cpt_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 9, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 3, part = "all") |>
            flextable::width(j = "Procedure Group", width = 1.7) |>
            flextable::width(j = "CPT Code",        width = 0.9) |>
            flextable::width(j = "Description",     width = 3.5) |>
            flextable::width(j = "Cases (n)",       width = 0.8) |>
            flextable::align(j = "Cases (n)", align = "right", part = "all") |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "Index procedure CPT codes", style = "heading 3")
          doc <- body_add_flextable(doc, cpt_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("cpt_codes"), ". CPT codes for index amputation subgroups."),
            paste0("CPT-4 codes mapped from each amputation subgroup's standard concept (see ",
                   supp_table("concept_set_inventory"), "). Cases (n) = number of distinct patients ",
                   "with that source concept. A code may appear in more than one group.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S3 (CPT codes) added.")
        } else {
          # combined itself has zero rows — distinct from the total_cases == 0
          # branch above: here, the vocab query never found ANY CPT4/HCPCS
          # concept related to the amputation ancestors at all (no descendant
          # in concept_ancestor, no 'Maps to' relationship), independent of
          # whether the cohort has matching procedure_occurrence rows. Worth
          # its own log line since it points to a vocabulary build issue
          # rather than an ETL/source-value issue.
          message("[report] Supplemental Table S3 skipped: no CPT4/HCPCS concept was found ",
                  "related to any amputation-level ancestor in this vocabulary build (0 rows from ",
                  "the concept_ancestor/concept_relationship join, before any cohort join).")
        }
      }, error = function(e) {
        message("[report] CPT supplemental table skipped: ", conditionMessage(e))
      })

      # ---- Supplemental Table S4 — Discharge destination source codes ---------
      # For NHD, outcomes are determined from visit_occurrence.discharged_to_source_value
      # (UB-04 NUBC codes) and secondarily from discharged_to_concept_id.  This table
      # enumerates ALL distinct source codes observed in the target cohort's index
      # visits — including rows with NULL source values — so that the full cohort is
      # accounted for and site-specific coding patterns are transparent.
      # Modelled after the equivalent fetch in pad-oler-nhd-val/R/report_prognostic.R
      # (fetch_nhd_disposition_codes).
      tryCatch({
        # nubc_code carries source values like "01"/"03" — force character so
        # read.csv()'s automatic type inference cannot drop the leading zero.
        dest_supp_raw <- read_report_input("supp_discharge_destinations",
                                           col_classes = c(nubc_code = "character"))
        names(dest_supp_raw) <- tolower(names(dest_supp_raw))
        if (nrow(dest_supp_raw) > 0) {
          dest_supp_display <- data.frame(
            "NUBC Code"       = dest_supp_raw$nubc_code,
            # Concept ID is an IDENTIFIER, not a quantity — kept as character
            # so flextable's default colformat_num() (big.mark = ",") does not
            # render it as "581,476" etc. Peer review flagged this exact
            # artefact; format(..., big.mark = "", scientific = FALSE) also
            # guards against any accidental scientific notation on large IDs.
            "OMOP Concept ID" = format(dest_supp_raw$omop_concept_id, big.mark = "", scientific = FALSE, trim = TRUE),
            "Concept Name"    = dest_supp_raw$concept_name,
            "Classification"  = dest_supp_raw$classification,
            "Visits (n)"      = dest_supp_raw$visit_count,
            "Persons (n)"     = dest_supp_raw$person_count,
            check.names = FALSE, stringsAsFactors = FALSE
          )
          dest_supp_ft <- flextable::flextable(dest_supp_display) |>
            flextable::bold(part = "header") |>
            flextable::fontsize(size = 9, part = "all") |>
            flextable::font(fontname = "Calibri", part = "all") |>
            flextable::bg(part = "header", bg = "#1F3864") |>
            flextable::color(part = "header", color = "white") |>
            flextable::padding(padding = 3, part = "all") |>
            flextable::width(j = "NUBC Code",       width = 0.9) |>
            flextable::width(j = "OMOP Concept ID", width = 1.1) |>
            flextable::width(j = "Concept Name",    width = 2.2) |>
            flextable::width(j = "Classification",  width = 1.3) |>
            flextable::width(j = "Visits (n)",      width = 0.7) |>
            flextable::width(j = "Persons (n)",     width = 0.7) |>
            flextable::align(j = c("Visits (n)", "Persons (n)"), align = "right", part = "all") |>
            flextable::set_table_properties(layout = "fixed")
          doc <- body_add_par(doc, "Discharge destination source codes", style = "heading 3")
          doc <- body_add_flextable(doc, dest_supp_ft)
          doc <- add_doc_caption(doc,
            paste0(supp_table("discharge_codes"), ". UB-04 NUBC discharge disposition codes observed in the validation cohort."),
            paste0("All distinct values of visit_occurrence.discharged_to_source_value ",
                   "for the index inpatient visit (visit_concept_id 9201) among target cohort patients, ",
                   "including rows with no source code recorded ('(none / NULL)'). ",
                   "OMOP Concept ID and Concept Name are drawn from discharged_to_concept_id. ",
                   "Classification: Home = confirmed home discharge (not-NHD); SNF/IRF/Hospice/LTAC/Other NHD = ",
                   "confirmed non-home discharge; HO = classified Home (source value treated as authoritative) ",
                   "even when the ETL has assigned a conflicting non-home concept_id; ",
                   "AM = Against Medical Advice, classified as home discharge (not-NHD); ",
                   "Other / Unknown = destination not classifiable from source code or concept ID, also ",
                   "classified not-NHD (presumed home) — a conservative default, not a removal from the ",
                   "analytic denominator; see Methods 1.2. ",
                   "Visits (n) = total index hospitalizations; Persons (n) = distinct patients.")
          )
          doc <- body_add_par(doc, "", style = "Normal")
          message("[report] Supplemental Table S4 (discharge destination source codes) added.")
        }
      }, error = function(e) {
        message("[report] Discharge destination supplemental table skipped: ", conditionMessage(e))
      })

    }, error = function(e) {
      message("[report] Supplemental DB tables skipped: ", conditionMessage(e))
    })
  }

  # ---- S4: NHD rate by month of year ----------------------------------------
  if (!is.null(nhd_month_plot_file) && file.exists(nhd_month_plot_file)) {
    doc <- body_add_par(doc, "NHD rate by month", style = "heading 3")
    doc <- body_add_img(doc, src = nhd_month_plot_file, width = 5.5, height = 3.5)
    doc <- add_doc_caption(doc,
      paste0(supp_figure("nhd_rate_by_month"), ". Non-home discharge (NHD) rate by calendar month of amputation."),
      paste0("Observed non-home discharge rate (%) for each calendar month ",
             "(January through December), pooled across all study years. Bar height represents the ",
             "NHD rate; numbers above each bar show the total procedure count for that month. ",
             "Months with fewer than 5 procedures are suppressed.")
    )
    doc <- body_add_par(doc, "", style = "Normal")
    message("[report] Supplemental Figure S5 (NHD by month) added.")
  }

  # ---- Score distribution plots (one per model) ------------------------------
  # Each model gets its own supplemental figure number from the pre-allocated
  # plan. Previously the label "Supplemental Figure S6" was hardcoded inside
  # this loop, so Iannuzzi and mFI-5 both rendered as S6 and sVQI-FS — which was
  # not in the loop at all — had no figure.
  for (dist_info in list(
    list(file = score_dist_plot_file_iannuzzi, label = "Iannuzzi 2020", key = "score_dist_iannuzzi"),
    list(file = score_dist_plot_file_mfi5,     label = "mFI-5",         key = "score_dist_mfi5"),
    list(file = score_dist_plot_file_vqifs,    label = "sVQI-FS",       key = "score_dist_vqifs")
  )) {
    if (!is.null(dist_info$file) && file.exists(dist_info$file)) {
      fig_label <- supp_figure(dist_info$key)
      doc <- body_add_par(doc, paste0("Score distribution — ", dist_info$label),
                          style = "heading 3")
      doc <- body_add_img(doc, src = dist_info$file, width = 5.5, height = 3.5)
      doc <- add_doc_caption(doc,
        paste0(fig_label, ". Distribution of ",
               dist_info$label, " integer risk score by outcome group."),
        paste0("Stacked count histogram of ", dist_info$label, " integer scores. ",
               "Blue bars = patients without NHD; red bars = patients with NHD. ",
               "Bars are stacked so the total bar height equals the count of all patients at that score. ",
               "NHD = non-home discharge.")
      )
      doc <- body_add_par(doc, "", style = "Normal")
      message("[report] ", fig_label, " (", dist_info$label, " score distribution) added.")
    }
  }

  # ---- Edge cases — CSV export --------------------------------------------
  # MOVED TO THE EXTRACT SIDE (Phase 0). This block used to open its own
  # connection here and write output/pad_amp_nhd_edge_<date>.csv, which contains
  # MRN, age, and procedure date. That is PHI, and it is the single strongest
  # reason the render half must not hold a connection: a report anyone can run
  # off a results export must not be able to produce a re-identifiable file.
  #
  # It now lives in export_edge_cases() in R/extract_report_inputs.R, runs where
  # the data lives, and writes to the study output folder — never to
  # report_inputs/, and never as an input to render. See that function's
  # header for why it does not belong in a shareable analysis-core repo
  # long-term (reserved for duke-prcc-deploy).

  doc <- .append_references_section(doc, citations)

  print(doc, target = report_file)
  # Section numbers are generated explicitly by section_major()/section_num();
  # remove the Word template's competing automatic heading numbering so headings
  # are not double-numbered ("1.1.1. 1.1. Data source").
  .strip_heading_autonumbering(report_file)
  message("Manuscript report written to: ", normalizePath(report_file))

  # ---------------------------------------------------------------------------
  # Persist manuscript figures before temp_figure_dir is cleaned up.
  #
  # WHY: every figure above is built inside temp_figure_dir (a tempfile()
  # directory, created at the top of this function with
  # on.exit(unlink(temp_figure_dir, ...)) already registered) purely so it can
  # be embedded into the Word document via officer::body_add_img(). Without
  # this step the .tiff/.pdf/.png trio that save_figure() (R/report_helpers.R)
  # writes for journal submission is deleted the moment this function returns
  # — there is no standalone Figure 2/3/4/5 file to actually upload anywhere.
  # Copy the whole directory into output_dir/figures/ now, while
  # temp_figure_dir still exists (on.exit runs after this point, not before).
  #
  # Ported from pad-amp-nhd-val (#42) — same defect, same fix, this repo
  # simply predates the greyscale figure work that added it there.
  # ---------------------------------------------------------------------------
  figures_dir <- file.path(output_dir, "figures")
  tryCatch({
    if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
    fig_files <- list.files(temp_figure_dir, full.names = TRUE)
    file.copy(fig_files, figures_dir, overwrite = TRUE)
    message("[report] ", length(fig_files), " manuscript figure file(s) persisted to: ",
            normalizePath(figures_dir, winslash = "/", mustWork = FALSE))
  }, error = function(e) {
    message("[report] Could not persist manuscript figures (non-fatal): ", conditionMessage(e))
  })

  invisible(report_file)
}
