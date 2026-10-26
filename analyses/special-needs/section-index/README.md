# Section Index Workflow

This folder contains a theory-driven section-index workflow for the 2025 safety survey.

## Purpose

Use this workflow to:
- build simple composite section scores alongside the existing factor-analysis outputs
- preserve the factor-analysis workflow unchanged while reusing the same respondent filtering and demographic flags
- generate respondent-level raw and standardized section indices
- compare section indices across the same demographic dichotomies used for factor scores

## Scripts

Run scripts in this order:

1. `section_index_analysis.R`
   - loads completed 2025 safety survey responses using the shared preparation helpers
   - documents candidate items, reverse-coding, exclusions, and completion rules
   - scores section composites as respondent-level means of answered items
   - writes section index scores, item diagnostics, and reliability summaries to `outputs/`

2. `section_index_demographic_comparison.R`
   - uses standardized section index scores from `outputs/`
   - estimates weighted demographic-group comparisons using the survey design
   - writes test results, weighted means, and interpretation summaries to `outputs/`

## Authoritative Outputs

Treat these files as the main section-index outputs:

- `outputs/section_index_labels.csv`
- `outputs/section_index_item_membership.csv`
- `outputs/section_index_notes.csv`
- `outputs/section_index_score_summary.csv`
- `outputs/section_index_item_diagnostics.csv`
- `outputs/section_index_reliability_summary.csv`
- `outputs/section_index_scores.csv`
- `outputs/section_index_scores.rds`
- `outputs/section_index_demographic_tests.csv`
- `outputs/section_index_demographic_weighted_summary.csv`
- `outputs/section_index_demographic_interpretation.csv`

## Notes

- This workflow reuses the shared preparation helpers in `../core_prep_helpers.R`.
- The primary Section 6 index uses only universally applicable items and leaves the structurally gated 6.05 to 6.16 battery out of the all-respondent composite.
- Section 7 is represented by two indices rather than one broad composite: Section 7A for desire for more enforcement and Section 7B for acceptance of the existing system.
- The `safe_clothes` item is documented as a Section 7 candidate but is dropped from scoring because it does not fit cleanly into either Section 7A or 7B.
- Raw composite scores are standardized to z-scores before demographic comparison.
