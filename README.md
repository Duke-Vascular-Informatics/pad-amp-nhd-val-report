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
(bucket 3 of charon's [Multi-Repo Analysis Pipeline](https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline)); the
report composition here — which tables, which figures, the clinical
narrative — is specific to this study and stays here, not in the toolkit.

## Usage

This repo needs no database, no VPN and no credentials. It needs R and a copy of the results.

### Steps

1. **Restore the R packages** from the lockfile:

   ```bash
   Rscript -e 'renv::restore(prompt = FALSE)'
   ```

2. **Choose where the results come from.** Use one of these:
   - **Your own run:** clone `pad-amp-nhd-val` next to this repo and point at its `output/` folder:

     ```bash
     RESULTS_DIR=../pad-amp-nhd-val/output Rscript GenerateReport.R
     ```

   - **An approved Duke export:** drop the `.zip` into `prcc_data/` and run `Rscript GenerateReport.R`. The script takes the timestamped archive, checks it, and extracts it. See `prcc_data/README.md`.
   - **A results folder copied from anywhere:**

     ```bash
     RESULTS_DIR=/path/to/results REPORT_OUTPUT_DIR=/path/to/write/report Rscript GenerateReport.R
     ```

3. **Read the banner.** The script prints `DATA SOURCE:` before it renders. If it says `SYNTHETIC`, the report is development output and must not be circulated as Duke results.

Expected: a Word report is written to `REPORT_OUTPUT_DIR`, or to `reports/` when the source is an extracted export. The default output is the results folder itself when it is not an export.

`RESULTS_DIR` must contain `iannuzzi/`, `mfi5/`, `vqifs/` (each with
`person_level_scores.csv`, `calibration_table_*.csv`, `subgroup_bias.csv`,
`metrics.csv`) and `report_inputs/` (CSVs + `_report_config.yaml`, written by
`pad-amp-nhd-val`'s `R/extract_report_inputs.R`). This is exactly the shape
of `pad-amp-nhd-val`'s own `output/` folder.

If the script stops with `STALE EXPORT`, the archive came from a run that did not finish. Rerun on the analysis side and export again. `REPORT_ALLOW_STALE_EXPORT=1` exists only to inspect such an archive; do not circulate a report made with it.

Tests: this repo has no test suite yet. The check is the render itself, plus comparing the output against the previous report.

## Config — deliberately not duplicated here

There is no `study_params.yaml` in this repo. The ~9 config fields the report
actually reads (narrative dates, the DCA display threshold, template routing —
verified by grepping every `config$` access in `R/*.R`, not assumed) are
written by the analysis repo's extract step into
`report_inputs/_report_config.yaml`. See `config.R`'s header for why: having
two copies of these fields in two repos is exactly the drift this split is
meant to avoid.

## Provenance

The report code (`R/report_prognostic.R`, `R/report_extended.R`, `R/report_helpers.R`) and
`covariates/model_metadata.yaml` moved here from `pad-amp-nhd-val` on 2026-08-11, so that the
analysis repo never imports plotting or Word packages. The move was verified by rendering the
full report from this repo and diffing it against the last report made before the split: 31,901
paragraphs, identical except the generation date. One dead line was removed, an unconditional
`source()` of a file this report never used; the details are in `CHANGELOG.md`.

## Citing

If you use this code, please cite it: see [`CITATION.cff`](CITATION.cff) (GitHub's
**Cite this repository** button reads it). Each release is archived on Zenodo, which
provides the DOI. The analysis that produces the report's inputs is
[`pad-amp-nhd-val`](https://github.com/Duke-Vascular-Informatics/pad-amp-nhd-val).

Developed in an institution-specific configuration of the
[Charon](https://github.com/Duke-Vascular-Informatics/charon) container
(*Containerized HADES Analytics for Research in OHDSI Networks*, v1.2.1,
DOI [10.5281/zenodo.23874907](https://doi.org/10.5281/zenodo.23874907)); please cite it too:
Johnson AP. Charon [software]. Zenodo; 2026.

## Funding

Research reported in this publication was supported by the National Center For Advancing
Translational Sciences of the National Institutes of Health under Award Number K12TR005435.
The content is solely the responsibility of the authors and does not necessarily represent
the official views of the National Institutes of Health.

## License

Copyright 2026 Duke University. All Rights Reserved. The software is hereby licensed under the GNU GPL License v2 (see [LICENSE](LICENSE)). OMOP vocabulary files are subject to the
[Athena license terms](https://www.ohdsi.org/analytic-tools/athena-standardized-vocabularies/). No vocabulary data is distributed with this repo.
