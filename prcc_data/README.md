# prcc_data/ — Duke PRCC export drop-zone

Copy an approved Duke PRCC export archive into this directory and run
`Rscript GenerateReport.R` from the repo root. The report will render from it
instead of the synthetic development data, and will say so on startup.

```bash
cp ~/Downloads/pad_amp_nhd_prog_strategusOutput_*.zip prcc_data/
Rscript GenerateReport.R
```

The archive is produced by `export_results_for_review.R` in `duke-prcc-deploy`
and must have cleared Duke's data-egress review before it leaves PRCC. This
repo does not check that and cannot — it only renders what it is given.

## What happens

- The newest `.zip` here is extracted to `prcc_data/.extracted/` and rendered
  from. Re-extraction happens only when the archive changes, so re-running is
  cheap.
- Already-unzipped content placed here directly works too.
- With this directory empty, the report falls back to the synthetic run at
  `../pad-amp-nhd-prog/output` and prints a prominent warning that the output
  is **not** a Duke result.
- `RESULTS_DIR=<path>` overrides all of the above.

## Everything here is gitignored

Except this README. Do not commit exports, extracted contents, or rendered
documents built from them — this directory holds real Duke results.
