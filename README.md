# pad-amp-nhd-val-report

Word manuscript report for `pad-amp-nhd-val` — external validation of three
published integer risk scores (Iannuzzi 2020, Subramaniam mFI-5, Kraiss
sVQI-FS) against non-home discharge after major lower extremity amputation.

**Renders from result artifacts only. No database connection.** This repo
must remain runnable on a laptop with nothing but a clone of it and a
`results/` directory copied over — no VPN, no credentials, no JDBC driver, no
`DatabaseConnector`.

## Why this is a separate repo

[`pad-amp-nhd-val`](https://github.com/Duke-Vascular-Informatics/pad-amp-nhd-val)
is meant to be Strategus-faithful: cohorts, Strategus spec, the retained
custom scoring step, and the extract layer that turns CDM queries into CSV
artifacts. It produces results. It should never import `ggplot2`, `officer`,
or `flextable`, and it should never build a Word document — that coupling is
exactly what this split removes.

This repo owns the opposite half: given the CSVs `pad-amp-nhd-val` wrote,
produce the manuscript. It shares generic figure/table helpers with other
studies via [`omopReportToolkit`](https://github.com/Duke-Vascular-Informatics/omop-report-toolkit)
(bucket 3 of `docs/MIGRATION_PLAN_REPO_SPLIT.md` in `omop-dev-workspace`); the
report composition here — which tables, which figures, the clinical
narrative — is specific to this study and stays here, not in the toolkit.

## Usage

```r
# from a clone of this repo, sibling to a clone of pad-amp-nhd-val:
RESULTS_DIR=../pad-amp-nhd-val/output Rscript GenerateReport.R

# or against a results export copied from anywhere (PRCC, a colleague's run):
RESULTS_DIR=/path/to/results REPORT_OUTPUT_DIR=/path/to/write/report Rscript GenerateReport.R
```

`RESULTS_DIR` must contain `iannuzzi/`, `mfi5/`, `vqifs/` (each with
`person_level_scores.csv`, `calibration_table_*.csv`, `subgroup_bias.csv`,
`metrics.csv`) and `report_inputs/` (CSVs + `_report_config.yaml`, written by
`pad-amp-nhd-val`'s `R/extract_report_inputs.R`). This is exactly the shape
of `pad-amp-nhd-val`'s own `output/` folder.

## Config — deliberately not duplicated here

There is no `study_params.yaml` in this repo. The ~9 config fields the report
actually reads (narrative dates, the DCA display threshold, template routing —
verified by grepping every `config$` access in `R/*.R`, not assumed) are
written by the analysis repo's extract step into
`report_inputs/_report_config.yaml`. See `config.R`'s header for why: having
two copies of these fields in two repos is exactly the drift this split is
meant to avoid.

## Provenance

`R/report_prognostic.R`, `R/report_extended.R`, `R/report_helpers.R`, and
`covariates/model_metadata.yaml` were moved here from `pad-amp-nhd-val`
2026-08-11, verbatim except for one dead-code removal (an unconditional
`source("R/cohort_demographics.R")` that neither this repo's code nor
`pad-amp-nhd-val`'s own report code ever called — that file's real,
DB-touching functions belong to and are already sourced independently by
`pad-amp-nhd-val`'s scoring step).

Verified by running the full report end-to-end from this repo against
`pad-amp-nhd-val`'s existing `output/` directory and diffing the result
against the last report generated before the split: 31,901 paragraphs,
identical except the generation date.
