# Latent Class Analysis Workflow

This folder contains the current latent class companion workflow for the 2025 safety survey.

## Purpose

Use this workflow to:
- audit the safety-survey item inventory
- fit baseline latent class models on the same curated 39-item all-respondent block used by factor analysis
- compare that expanded baseline against the prior smaller 23-item exploratory LCA framing
- test whether the resulting classes are conceptually distinct or mostly a lower-to-higher burden ordering
- summarize the recommended class solution against the current 9-factor structure

The factor-analysis workflow remains the authoritative source for item selection, coding direction, and prepared core data. This folder owns the latent class comparison, interpretation, and documentation outputs.

## Scripts

Run scripts in this order:

1. `lca_item_audit.R`
   - audits available survey variables
   - classifies variables into core, conditional, profiling, and excluded roles
   - writes audit outputs to `outputs/`

2. `lca_model_comparison.R`
   - reads the shared curated selection and model-ready core data from `../factor_analysis/outputs/`
   - keeps the baseline LCA on the all-respondent 39-item block and leaves gated or structural-not-applicable items out unless modeled separately
   - reuses the factor-analysis direction rules so higher coded values consistently indicate greater burden / lower safety / lower confidence where appropriate
   - fits latent class models for both the expanded 39-item block and the legacy 23-item block across 2-7 classes with repeated random starts
   - writes class-count comparison tables, class membership, class-conditional probabilities, class-profile summaries, factor-lens summaries, and a written interpretation summary to `outputs/`

## Authoritative Outputs

Treat these files as the main latent-class outputs:

- `outputs/lca_variable_set_audit.csv`
- `outputs/lca_direction_audit.csv`
- `outputs/lca_candidate_model_summary.csv`
- `outputs/lca_candidate_cross_domain_diagnostics.csv`
- `outputs/lca_candidate_factor_monotonicity.csv`
- `outputs/lca_candidate_item_monotonicity.csv`
- `outputs/lca_framing_comparison.csv`
- `outputs/lca_recommended_class_membership.csv`
- `outputs/lca_recommended_class_probabilities.csv`
- `outputs/lca_recommended_class_item_expected_scores.csv`
- `outputs/lca_recommended_class_factor_summary.csv`
- `outputs/lca_recommended_class_profile_summary.csv`
- `outputs/lca_legacy_best_bic_profile_summary.csv`
- `outputs/lca_analysis_notes.csv`
- `outputs/lca_summary.md`

Compatibility copies of the older filenames are still written for continuity:

- `outputs/lca_factor_model_comparison.csv`
- `outputs/lca_best_class_probabilities.csv`
- `outputs/lca_factor_model_notes.csv`

## Notes

- The factor-analysis outputs in `../factor_analysis/outputs/` are the authoritative source for the shared curated dataset, the 39 kept variables, and the direction-aware coding rules.
- The baseline LCA intentionally excludes `drive_worry`, the instructional attention check, and the structural-not-applicable frequency battery because they are not defensible in the all-respondent complete-case baseline without separate handling.
- The legacy smaller LCA framing is read from `../legacy_mixed_outputs/lca_curated_core_selection.csv` only to support direct comparison against the expanded block.
- Older mixed-output artifacts from before the workflow split remain in `../legacy_mixed_outputs/` for reference only.