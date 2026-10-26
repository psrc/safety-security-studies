# Factor Analysis Workflow

This folder contains the active safety-attitudes dimension workflow for 2025 safety survey analysis.

## Purpose

Use this workflow to:
- prepare the curated first-pass attitudinal item block
- estimate exploratory factor models
- generate respondent-level factor scores
- compare factor dimensions across subgroups such as `disability_person`

## Scripts

Run scripts in this order:

1. `prepare_core_data.R`
   - loads completed 2025 safety survey responses
   - applies ordered value encoding using `../data/value_labels.csv`
   - applies reverse-coding where needed
   - writes the curated analysis dataset to `outputs/`

2. `factor_model_analysis.R`
   - fits the exploratory factor models on the curated item block
   - compares factor-retention diagnostics including BIC, parallel analysis, MAP, and VSS
   - writes factor loadings, factor scores, factor labels, and factor notes to `outputs/`

3. `factor_demographic_comparison.R`
   - uses factor scores from `outputs/`
   - estimates weighted demographic-group comparisons using the survey design
   - writes test results, weighted means, and interpretation summaries to `outputs/`

4. `plot_factor_demographic_ranked_dotplot.R`
   - reads `outputs/factor_demographic_tests.csv`
   - creates a faceted ranked dot plot of weighted standardized mean differences by respondent dichotomy
   - writes publication-ready PNG and PDF files to `outputs/`

## Authoritative Outputs

Treat these files as the main factor-analysis outputs:

- `outputs/curated_core_selection.csv`
- `outputs/core_model_data.rds`
- `outputs/factor_model_summary.csv`
- `outputs/factor_retention_summary.csv`
- `outputs/factor_retention_diagnostics.csv`
- `outputs/best_factor_loadings.csv`
- `outputs/candidate_factor_top_loadings.csv`
- `outputs/factor_dimension_labels.csv`
- `outputs/factor_scores.rds`
- `outputs/factor_demographic_tests.csv`
- `outputs/factor_demographic_weighted_summary.csv`
- `outputs/factor_demographic_interpretation.csv`
- `outputs/factor_demographic_ranked_dotplot.png`
- `outputs/factor_demographic_ranked_dotplot.pdf`

## Notes

- The source view is `Elmer.safety_survey.v_responses_2025`.
- Responses in the labeled source view are character labels, not numeric codes.
- The prepared factor dataset retains the five demographic dichotomies, along with `person_weight` and `sample_segment`, so subgroup comparisons can run directly from factor outputs.
- Factor labels in `outputs/factor_dimension_labels.csv` are heuristic summaries based on top-loading items and can be revised if interpretation changes.
