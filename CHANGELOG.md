# Changelog

Dated record of changes to the report, moved here from `CLAUDE.md`. The rules
in `CLAUDE.md` (including the export-vintage gates) state what is true now and
why. This file records when each change happened.

## v1.0.0 (2026-11-06)

- Frozen for the manuscript. Tag `v1.0.0`. This repo's own Zenodo DOI, 10.5281/zenodo.24013550, is recorded in Supplemental Table S11 (commit `e962587`).

## 2026-11-05

- Lookback windows and the PAD concept set changed in `pad-amp-nhd-val`. Table 1 indication and amputation-level rows now come from ATLAS cohorts (9100003, 9100021, 9100029 to 9100031).
- Stale-export guard added to `GenerateReport.R`. A PRCC run that stopped after the Strategus modules still produced a zip; the guard refuses it.
- Decision-curve figure: the mFI-5 curve uses a dotted line, overriding the toolkit palette for that series, because its step-function shape was hard to read with the palette's dash pattern.

## 2026-11-04

- Subgroup ECE switched to ten-quantile bins, matching the overall ECE.
- The Zenodo DOIs for the synthetic-data generator (v3) and Charon v1.2.1 were checked (per `CLAUDE.md`, the Repositories table rule).

## 2026-10-30

- ATLAS cohorts published on the public demo instance (15 cohorts). Cohort IDs now shown in Table 3.
- Supplemental ATLAS table gained an "OHDSI PL ID" column. Library entries were pending peer review on this date.
- The "Parent concepts" column moved to the supplemental ATLAS table.

## 2026-10-02

- Repository made public ahead of the manuscript.

## 2026-08-11

- Report split out of `pad-amp-nhd-val` into this repo, so that the analysis repo never imports plotting or Word packages. Verified by rendering the full report from this repo and diffing it against the last report made before the split (identical except the generation date).
- The unconditional `source("R/cohort_demographics.R")` in the copied code was removed. Nothing in this repo called it, and the file's database functions are sourced by the analysis repo.
