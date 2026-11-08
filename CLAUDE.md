# pad-amp-nhd-val-report — CLAUDE Instructions (Local Wrapper)

Shared baseline (applies first):

- [charon's `CLAUDE.md`](https://github.com/Duke-Vascular-Informatics/charon/blob/main/CLAUDE.md)

## Local Overrides

- **This repo renders a Word report from result artifacts. It must never gain
  a database dependency.** If a change here seems to need `DatabaseConnector`,
  `connection_details`, or any live CDM query, the query belongs in
  `pad-amp-nhd-val`'s `R/extract_report_inputs.R` instead, writing a new CSV
  artifact this repo reads. This is the one rule that must never be broken —
  see charon's [Multi-Repo Analysis Pipeline](https://github.com/Duke-Vascular-Informatics/charon#multi-repo-analysis-pipeline) section for why.
- Consumes [`omopReportToolkit`](https://github.com/Duke-Vascular-Informatics/omop-report-toolkit)
  for generic figure styling and report helpers, pinned to a commit in
  `renv.lock` (never a branch). Bump it deliberately: `renv::install(...)`
  then hand-verify the `renv.lock` diff — do not run a blind
  `renv::snapshot()` afterward. One has already been observed in this
  workspace to silently strip unrelated packages from a lockfile when run
  over an existing project; see that package's own README for the same
  warning.
- No `study_params.yaml` here — see `config.R`'s header. Report parameters
  come from `_report_config.yaml`, written by `pad-amp-nhd-val`'s extract
  step, not duplicated in this repo.
- **Tables 3a-c carry "Lookback window" and "Cohort ID" columns** (the "Parent concepts" column moved
  to the supplemental ATLAS cohort table on 2026-10-30, next to each cohort's role in the study).
  The lookback text is built from `report_inputs/score_component_definitions.csv`, which
  `pad-amp-nhd-val`'s extract step writes from the same covariate CSVs the scoring step used, so it
  cannot drift; the ATLAS cohort table's parent-concept column uses the same rows through
  `.inventory_parents()`. An export that predates that file falls back to a hand-maintained copy
  (`.TBL3_LOOKBACK_PARENTS` in `R/report_prognostic.R`), logged as a message; update it only if you
  must keep rendering old exports. **Cohort ID** (`.COMPONENT_COHORTS`) shows the cohort's OFFICIAL
  ATLAS id on the public demo instance (atlas-demo.ohdsi.org; 15 cohorts published 2026-10-30, the
  target is 1798563; the non-home discharge outcome is hand-written SQL and has none), and the ATLAS
  table's "ATLAS ID" (official id only; local 9100xxx ids are not shown in the report) and "Use in this study" columns are static text. All three mirror
  `pad-amp-nhd-val/covariates/cohort_map.csv` and `inst/Cohorts.csv` (KEEP IN SYNC). Components are matched by name exactly as in `covariate_summary.csv`.
- **Supplemental ATLAS table's "OHDSI PL ID" column** (added 2026-10-30) is static text, like the ATLAS ID and
  "Use in this study" columns: the closest OHDSI Phenotype Library entry and how it compares (equivalent /
  near-equivalent / contained / related / None). It mirrors `ohdsi_pl_links` in the workspace's
  `phenotype_library/catalog.yaml` (KEEP IN SYNC; the overlap evidence is in that file's notes), and the Methods
  "Data source" paragraph summarises it. The comparison is concept sets only (descendants expanded, dev
  vocabulary); the library entries were "Pending"/"Pending peer review" on 2026-10-30, so the report says
  "pending peer review" and must not call them peer reviewed.
  **Export vintage gates.** The report's Methods text describes the analysis code as it stood
  on a given date. An export made earlier can contradict that text, so each gate below says which
  export a circulated report may be rendered from. Each export zip is named by its timestamp
  (`..._YYYYMMDD-HHMMSS.zip`).

  | Gate | What changed in the analysis | Why an older export is wrong | Render only from an export made after |
  |---|---|---|---|
  | Cohort vintage | COPD (PL 1271) and CAD (PL 197) concept sets aligned to the OHDSI Phenotype Library | Old sets lacked COPD concept 258780; CAD used 4185932 only | `..._20261104-163229.zip` (checked 2026-11-04 against `pad-amp-nhd-val/inst/cohorts/*.json`) |
  | ECE | Subgroup ECE uses the same ten-quantile-bin `compute_ece()` as the overall ECE | Earlier exports used equal-width bins, so the Methods sentence would be wrong | the PRCC rerun on that code (later than `..._20261104-163229.zip`) |
  | Lookback and PAD set (2026-11-05) | Any-time-before-index lookback for chronic items; 90 days for anemia and underweight; one PAD concept set for Table 1 and the sVQI-FS item | Earlier exports used 365 days and the old PVD set, so Table 3 and the Methods disagree | the PRCC rerun on that code (later than `..._20261104-213138.zip`) |
  | Table 1 from ATLAS cohorts (2026-11-05) | Indication and amputation-level rows come from cohorts 9100003, 9100021, 9100029 to 9100031, not hand-written concept lists | Earlier exports disagree with Table 3c and Supplemental Table S2 (PAD: 154 patients, 26.7%, against 387, 67.1%, under the sVQI-FS item) | any export made after 2026-11-05 |

  Dated history of these changes is in `CHANGELOG.md`.
- **Stale-export guard (2026-11-05):** `GenerateReport.R` lists the archive's file times and refuses it when `report_inputs/`, `iannuzzi/`,
  `mfi5/` or `vqifs/` are all older than `CohortGeneratorModule/`. A PRCC run that stops after the Strategus modules (for example at the
  cohort-freshness check) still produces a zip, with fresh module output beside scoring and report files left over from an earlier run;
  without this guard it would render without error. `REPORT_ALLOW_STALE_EXPORT=1` overrides it for inspection only.
- **Repositories table (Supplemental Table S11, last supplemental item):** rendered from
  `covariates/study_repositories.csv` (role, repository, version, doi, license, note); Methods 1.3 cites it and gives
  the three repository URLs. A blank `doi` renders "To be assigned at release" and the render prints a WARNING. After
  tagging `pad-amp-nhd-val` and this repo, put their Zenodo version DOIs in the CSV (the generator's v3 DOI and Charon
  v1.2.1 were checked against Zenodo on 2026-11-04; templates and the toolkit are intentionally not listed, they are covered by Charon) and re-render
  before circulating. New supplemental items go AFTER this one only if S11 may move; otherwise insert before it.
- **Decision-curve figure:** the mFI-5 curve is drawn as a fine dotted line (linetype `"11"`),
  overriding the toolkit palette for that one series, because its jagged step-function curve made
  the palette's dash pattern hard to read.
- Repo is **public** (made public 2026-10-02 ahead of publication; GPL-2.0, see `LICENSE`). Nothing patient-level or site-credentialed may ever be committed here.

### Pipeline

| Step | File | What it does |
|------|------|---------------|
| — | `GenerateReport.R` | Entry point. `Rscript GenerateReport.R` — resolves its data source automatically (see below). |

**Data source resolution** (first match wins), implemented in
`GenerateReport.R`:

1. `RESULTS_DIR` env var, if set — explicit always wins.
2. A `.zip` in `prcc_data/` (gitignored) — a Duke PRCC export archive, extracted
   to `prcc_data/.extracted/` and rendered from. **This is the normal way to
   render real Duke results.** Newest archive wins; re-extracts only when the
   archive changes.
3. Already-unzipped content in `prcc_data/`.
4. `../pad-amp-nhd-val/output` — the synthetic dev-container run.

The chosen source is announced in a banner on startup, and a synthetic render
prints an explicit "do not circulate as a Duke result" warning at the end. That
banner is the point of the ordering: silently rendering synthetic numbers and
believing they are Duke's is the expensive mistake here. Never remove it.

`prcc_data/` is gitignored except its README — it holds real Duke results, and
the archive is only aggregate because `duke-prcc-deploy`'s export step made it
so, which is not a property this repo can verify after the fact.

**Rendered output goes to `reports/`** (gitignored) when the source is an
extracted archive, and next to the results otherwise. It must NOT default into
`prcc_data/.extracted/`: that directory is a working copy this script deletes
and re-creates whenever the archive changes, so a report written there is
silently destroyed by the next render of a new export. `REPORT_OUTPUT_DIR`
overrides.

There is no Step 1/2/9 numbering here — that convention belongs to
`pad-amp-nhd-val`'s Strategus pipeline. This repo has exactly one script.

### Version Control Routing

Independent repository; **not** a submodule, and not added to the workspace
root's index.

| Remote | URL | What to push |
|--------|-----|---------------|
| `origin` | `git@github.com:Duke-Vascular-Informatics/pad-amp-nhd-val-report.git` | Full repository |

```bash
BRANCH=$(gh api user --jq .login)
git push origin "$BRANCH"   # then open a PR into main
```

No Duke GitLab routing — this repo has no PRCC deployment concerns. If it
ever needs one (e.g. rendering directly on PRCC rather than from an export),
that is bucket-4 (`duke-prcc-deploy`) scope, not something to add here
directly.
