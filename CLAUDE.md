# pad-amp-nhd-prog-report — CLAUDE Instructions (Local Wrapper)

Shared baseline (applies first):

- `../CLAUDE.md`

## Local Overrides

- **This repo renders a Word report from result artifacts. It must never gain
  a database dependency.** If a change here seems to need `DatabaseConnector`,
  `connection_details`, or any live CDM query, the query belongs in
  `pad-amp-nhd-prog`'s `R/extract_report_inputs.R` instead, writing a new CSV
  artifact this repo reads. This is the one rule that must never be broken —
  see `docs/MIGRATION_PLAN_REPO_SPLIT.md` (in `omop-dev-workspace`) for why.
- Consumes [`omopReportToolkit`](https://github.com/Duke-Vascular-Informatics/omop-report-toolkit)
  for generic figure styling and report helpers, pinned to a commit in
  `renv.lock` (never a branch). Bump it deliberately: `renv::install(...)`
  then hand-verify the `renv.lock` diff — do not run a blind
  `renv::snapshot()` afterward. One has already been observed in this
  workspace to silently strip unrelated packages from a lockfile when run
  over an existing project; see that package's own README for the same
  warning.
- No `study_params.yaml` here — see `config.R`'s header. Report parameters
  come from `_report_config.yaml`, written by `pad-amp-nhd-prog`'s extract
  step, not duplicated in this repo.
- Repo is **private**.

### Pipeline

| Step | File | What it does |
|------|------|---------------|
| — | `GenerateReport.R` | Entry point. `RESULTS_DIR=<path> Rscript GenerateReport.R`. |

There is no Step 1/2/9 numbering here — that convention belongs to
`pad-amp-nhd-prog`'s Strategus pipeline. This repo has exactly one script.

### Version Control Routing

Independent repository; **not** a submodule, and not added to the workspace
root's index.

| Remote | URL | What to push |
|--------|-----|---------------|
| `origin` | `git@github.com:Duke-Vascular-Informatics/pad-amp-nhd-prog-report.git` | Full repository |

```bash
BRANCH=$(gh api user --jq .login)
git push origin "$BRANCH"   # then open a PR into main
```

No Duke GitLab routing — this repo has no PRCC deployment concerns. If it
ever needs one (e.g. rendering directly on PRCC rather than from an export),
that is bucket-4 (`duke-prcc-deploy`) scope, not something to add here
directly.
