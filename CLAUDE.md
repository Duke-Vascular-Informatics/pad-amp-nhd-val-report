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
| — | `GenerateReport.R` | Entry point. `Rscript GenerateReport.R` — resolves its data source automatically (see below). |

**Data source resolution** (first match wins), implemented in
`GenerateReport.R`:

1. `RESULTS_DIR` env var, if set — explicit always wins.
2. A `.zip` in `prcc_data/` (gitignored) — a Duke PRCC export archive, extracted
   to `prcc_data/.extracted/` and rendered from. **This is the normal way to
   render real Duke results.** Newest archive wins; re-extracts only when the
   archive changes.
3. Already-unzipped content in `prcc_data/`.
4. `../pad-amp-nhd-prog/output` — the synthetic dev-container run.

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
