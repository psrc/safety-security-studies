---

editor_options: 
  markdown: 
    wrap: 72
---

# Quick Start Guide - Safety & Security Studies Analysis

## Running Analyses

### Factor Analysis

**Purpose:** Understand underlying structure of safety/security concerns\
**Entry Point:** `analyses/special-needs/factor-analysis/prepare_core_data.R`

``` r
# Open the project
open "safety-security-studies.Rproj"

# Run in RStudio Console:
source("analyses/special-needs/factor-analysis/prepare_core_data.R")
source("analyses/special-needs/factor-analysis/factor_model_analysis.R")
source("analyses/special-needs/factor-analysis/factor_demographic_comparison.R")

# Generate visualizations:
source("analyses/special-needs/factor-analysis/plot_factor_loadings_heatmap.R")
source("analyses/special-needs/factor-analysis/plot_factor_demographic_ranked_dotplot.R")
```

**Estimated Runtime:** 10-15 minutes\
**Outputs:** Factor scores, loadings, demographic comparisons\
**See:** `analyses/special-needs/factor-analysis/README.md` for details

------------------------------------------------------------------------

### Latent Class Analysis

**Purpose:** Compare factor-based vs legacy variable selection for latent classes\
**Requirements:** Factor analysis must be completed first\
**Entry Point:** `analyses/special-needs/latent-class-analysis/lca_item_audit.R`

``` r
source("analyses/special-needs/latent-class-analysis/lca_item_audit.R")
source("analyses/special-needs/latent-class-analysis/lca_model_comparison.R")
```

**Estimated Runtime:** 5-10 minutes\
**Outputs:** Model comparison statistics, class membership\
**See:** `analyses/special-needs/latent-class-analysis/README.md` for details

------------------------------------------------------------------------

### Paired Tradeoff Analysis

**Purpose:** Examine safety vs security tradeoffs across 6 domains\
**Requirements:** None (standalone)\
**Entry Point:** `analyses/special-needs/paired-tradeoff-analysis/paired_tradeoff_disability_analysis.R`

``` r
source("analyses/special-needs/paired-tradeoff-analysis/paired_tradeoff_disability_analysis.R")
```

**Estimated Runtime:** 3-5 minutes\
**Outputs:** 13 CSV files with domain applicability and contrasts\
**See:** `analyses/special-needs/paired-tradeoff-analysis/README.md` for details

------------------------------------------------------------------------

### Section Index Analysis

**Purpose:** Calculate theory-driven composite section scores\
**Requirements:** None (standalone)\
**Entry Point:** `analyses/special-needs/section-index/section_index_analysis.R`

``` r
source("analyses/special-needs/section-index/section_index_analysis.R")
source("analyses/special-needs/section-index/section_index_demographic_comparison.R")
source("analyses/special-needs/section-index/section_composition_plots.R")
```

**Estimated Runtime:** 5-10 minutes\
**Outputs:** Section scores, demographic comparisons\
**See:** `analyses/special-needs/section-index/README.md` for details

------------------------------------------------------------------------

### Section 5 Linear Regression

**Purpose:** Model the section-level score with weighted and unweighted regression\
**Requirement:** Run the section-index analysis first to create `section_index_scores.rds`

``` r
source("analyses/special-needs/linear-regression/section5_index_regression.R")
quarto::quarto_render("analyses/special-needs/linear-regression/section5-regression.qmd")
```

------------------------------------------------------------------------

### Ordinal Regression Models

**Purpose:** Model safety/security outcomes using ordinal logistic regression\
**Entry Point:** `analyses/special-needs/ordinal-regression/ordinal_logistic.R`

``` r
# Run scripts
source("analyses/special-needs/ordinal-regression/ordinal_logistic.R")

# Generate Quarto reports
quarto::quarto_render("analyses/special-needs/ordinal-regression/ordinal-regression.qmd")
```

**Estimated Runtime:** 15-20 minutes\
**Outputs:** Regression models, statistical reports\
**See:** `analyses/special-needs/ordinal-regression/README.md` for details

------------------------------------------------------------------------

### Exact Matching Analysis

**Purpose:** Compare ordinal travel outcomes after exact matching on categorical confounders\
**Requirement:** PSRC survey data access; input preparation is shared with ordinal regression

``` r
source("analyses/special-needs/propensity-score-matching/disability_foregone_travel_psm.R")
quarto::quarto_render("analyses/special-needs/propensity-score-matching/propensity-score-matching.qmd")
```

The current workflow uses exact matching and does not estimate propensity scores.

## Project Setup

### Initial Setup

``` r
# 1. Install required packages (first time only)
required_packages <- c(
  "tidyverse",    # Data wrangling (ggplot2, dplyr, tidyr, etc.)
  "data.table",   # Fast data manipulation
  "survey",       # Weighted survey analysis
  "psych",        # Factor analysis
  "MASS",         # Ordinal regression (polr function)
  "psrc.travelsurvey",  # PSRC custom survey functions
  "psrcelmer",    # Elmer database access
  "quarto"        # Report generation
)
install.packages(required_packages[!required_packages %in% rownames(installed.packages())])

# 2. Open the RStudio project
open "safety-security-studies.Rproj"

# 3. Verify root discovery without loading packages or connecting to data sources
source("R/project_paths.R")
project_root <- detect_project_root()
cat("Project root:", project_root, "\n")
```

### Data Access

**Option 1: From Elmer Database** (default, requires PSRC network)

``` r
source("R/project_paths.R")
project_root <- detect_project_root()
source(file.path(project_root, "R/data-prep/core_prep_helpers.R"))

# Loads from Elmer: safety_survey.v_responses_2025
safety_data <- load_safety_responses()
```

Each workflow documents its own inputs and cache behavior; there is no repository-wide survey cache path.

------------------------------------------------------------------------

## Data Structure

### Input Data

- **Source:** PSRC Elmer Database (safety_survey.v_responses_2025)
- **Records:** \~1,500 survey respondents
- **Variables:** 23 safety/security concern items + demographics
- **Filter:** complete==2 (fully completed responses only)

### Key Demographic Indicators

Created by `derive_demographic_flags()`: - `age65plus` - Age \> 64 (binary) - `low_income` - Federal poverty threshold (binary) - `race_poc` - Non-White race/ethnicity (binary) - `zero_vehicles` - No household vehicles (binary)

### Key Outputs

| Analysis | Main Output | Format | Location |
|-----------------|---------------------|-----------------|-----------------|
| Factor Analysis | Factor scores | RDS | `analyses/special-needs/factor-analysis/outputs/factor_scores.rds` |
| Factor Analysis | Loadings | CSV | `analyses/special-needs/factor-analysis/outputs/best_factor_loadings.csv` |
| LCA | Model comparison | RDS | `analyses/special-needs/latent-class-analysis/outputs/` |
| Paired Tradeoff | Domain analysis | CSV | `analyses/special-needs/paired-tradeoff-analysis/*.csv` |
| Section Index | Section scores | RDS | `analyses/special-needs/section-index/outputs/section_index_scores.rds` |
| Ordinal Regression | Model objects | RDS | `analyses/special-needs/ordinal-regression/outputs/` |

------------------------------------------------------------------------

## Troubleshooting

### Issue: "Cannot find core_prep_helpers.R"

**Solution:** Ensure you've opened `safety-security-studies.Rproj` in RStudio, and run from within that project.

### Issue: "Cannot connect to Elmer database"

**Solution:** 1. Verify PSRC network connection 2. Check the configuration used by that workflow 3. Use a cached input only if the workflow documents one.

### Issue: "Quarto reports won't render"

**Solution:** Install Quarto CLI from <https://quarto.org/docs/get-started/>, then restart RStudio.

### Issue: "Path errors in analysis scripts"

**Solution:** 1. Verify `R/project_paths.R` is present 2. Run from within the project or set `SURVEY_PROJECT_ROOT` to the repository's root directory.

------------------------------------------------------------------------

## Common Workflows

### Run All Analyses in Sequence

``` r
# Factor analysis (10-15 min)
source("analyses/special-needs/factor-analysis/prepare_core_data.R")
source("analyses/special-needs/factor-analysis/factor_model_analysis.R")

# Latent class (5-10 min, requires factor analysis)
source("analyses/special-needs/latent-class-analysis/lca_item_audit.R")
source("analyses/special-needs/latent-class-analysis/lca_model_comparison.R")

# Independent analyses (can run in parallel)
source("analyses/special-needs/paired-tradeoff-analysis/paired_tradeoff_disability_analysis.R")
source("analyses/special-needs/section-index/section_index_analysis.R")

# Regression (can run anytime)
source("analyses/special-needs/ordinal-regression/ordinal_logistic.R")
```

**Total Runtime:** \~60 minutes

### Generate All Reports

``` r
# Install quarto if needed
install.packages("quarto")

# Render all reports
quarto::quarto_render("analyses/special-needs/factor-analysis/factor-analysis.qmd")
quarto::quarto_render("analyses/special-needs/latent-class-analysis/latent-class-analysis.qmd")
quarto::quarto_render("analyses/special-needs/linear-regression/section5-regression.qmd")
quarto::quarto_render("analyses/special-needs/ordinal-regression/ordinal-regression.qmd")
quarto::quarto_render("analyses/special-needs/paired-tradeoff-analysis/paired-tradeoff-analysis.qmd")
quarto::quarto_render("analyses/special-needs/propensity-score-matching/propensity-score-matching.qmd")
quarto::quarto_render("analyses/special-needs/section-index/section_index.qmd")
quarto::quarto_render("analyses/special-needs/disability-response-comparison.qmd")
```

### Validate Migration

``` r
# Check repository integrity
source("validate-migration.R")
```

------------------------------------------------------------------------

## File Organization Reference

```         
R/                              Shared code
├── .project-setup.R           ← Run this first to set project root
├── project_paths.R             ← Side-effect-free root discovery
├── data-prep/
│   ├── core_prep_helpers.R    ← Core data functions (MAIN)
│   ├── data_processing.R      ← Legacy workflows
│   └── config.yml             ← Database & codebook config
└── functions/
    ├── functions.R            ← General purpose functions
    └── section_index_functions.R  ← Alternative helpers

data/                           Data & codebooks
├── value_labels.csv           ← Master codebook
└── variable_descriptions.csv  ← Variable metadata

analyses/special-needs/        Analysis workflows
├── factor-analysis/           ← FACTOR ANALYSIS
├── latent-class-analysis/     ← LCA (depends on factor analysis)
├── section-index/             ← SECTION SCORING (independent)
├── paired-tradeoff-analysis/  ← TRADEOFF ANALYSIS (independent)
├── ordinal-regression/        ← ORDINAL REGRESSION
├── propensity-score-matching/ ← EXACT MATCHING
└── linear-regression/         ← SECTION 5 REGRESSION

outputs/                        Results
└── legacy_mixed_outputs/      Original outputs

validate-migration.R           Validation checks
```

------------------------------------------------------------------------

## Additional Resources

- **Main Documentation:** README.md
- **Migration Details:** MIGRATION_SUMMARY.md
- **Validation Results:** Run `source("validate-migration.R")`
- **Factor Analysis Methods:** `analyses/special-needs/factor-analysis/README.md`
- **Project Configuration:** `R/data-prep/config.yml`

------------------------------------------------------------------------

**Last Updated:** 8/31/2026\
**For Questions:** See individual analysis README files in `analyses/special-needs/*/`
