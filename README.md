# Safety & Security Studies - Survey Analysis Repository

## Overview

This repository contains a set of R-based statistical analyses for the **PSRC Safety & Security Survey (SSS)**, a specialized component of the Household Travel Survey examining how safety and security concerns influence travel behavior.

## Repository Structure

```
safety-security-studies/
├── R/                             # Shared R functions and configuration
│   ├── .project-setup.R           # Project initialization
│   ├── project_paths.R            # Side-effect-free project root detection
│   ├── data-prep/                 # Shared data preparation layer
│   │   ├── core_prep_helpers.R    # Core data prep functions (tidyverse-style)
│   │   ├── data_processing.R      # Legacy data processing workflows
│   │   └── config.yml             # Database and codebook configuration
│   └── functions/                 # Reusable analytical functions
│       ├── functions.R            # General purpose functions
│       ├── section_index_functions.R  # Section-index helpers
│       └── special_needs_helpers.R    # Shared recodes and ordinal input preparation
│
├── data/                          # Data files and codebooks
│   ├── value_labels.csv           # Master codebook for all variables
│   ├── variable_descriptions.csv  # Variable metadata and descriptions
│   └── [other supporting data files]
│
├── analyses/special-needs/        # Specialized analyses
│   ├── factor-analysis/
│   │   ├── README.md
│   │   ├── prepare_core_data.R    # Load and encode survey responses
│   │   ├── factor_model_analysis.R # Fit exploratory factor model
│   │   ├── factor_demographic_comparison.R # Weighted demographic analysis
│   │   ├── plot_*.R               # 7 visualization scripts
│   │   └── outputs/               # Saved models, scores, and figures
│   │
│   ├── latent-class-analysis/
│   │   ├── README.md
│   │   ├── lca_item_audit.R       # Compare variable selection approaches
│   │   ├── lca_model_comparison.R # Test factor-based vs legacy models
│   │   └── outputs/
│   │
│   ├── paired-tradeoff-analysis/
│   │   ├── README.md
│   │   ├── paired_tradeoff_disability_analysis.R # Main entry point
│   │   ├── paired-tradeoff-analysis.qmd
│   │   └── [13 CSV outputs]       # Domain pairs and contrasts
│   │
│   ├── section-index/
│   │   ├── README.md
│   │   ├── section_index_analysis.R      # Theory-driven composite scoring
│   │   ├── section_index_demographic_comparison.R
│   │   └── outputs/
│   │
│   ├── ordinal-regression/
│   │   ├── ordinal_logistic.R     # Ordinal regression models
│   │   ├── ordinal-regression.qmd # Ordinal model workflow and diagnostics
│   │   └── outputs/
│   ├── propensity-score-matching/
│   │   ├── disability_foregone_travel_psm.R
│   │   └── propensity-score-matching.qmd
│   ├── linear-regression/
│   │   ├── section5_index_regression.R
│   │   └── section5-regression.qmd
│   ├── disability-response-comparison.qmd
│   ├── dashboard.qmd
│   │
│   └── disability_response_sss.R  # Standalone reference script
│
├── outputs/                        # Consolidated results
│   ├── [final plots and figures]
│   └── legacy_mixed_outputs/      # Original analysis outputs
│
├── validate-migration.R            # Validation checks for repository integrity
├── README.md                       # This file
├── LICENSE
└── safety-security-studies.Rproj   # RStudio project file
```

## Key Components

### 1. Shared Data Preparation Layer (`R/data-prep/`)

**`core_prep_helpers.R`** - Core helper functions (tidyverse-style):
- `normalize_safety_source()` - Standardize ID columns and data types
- `filter_completed_responses()` - Keep only fully completed responses (complete==2)
- `derive_demographic_flags()` - Create demographic indicators:
  - `age65plus`: Age > 64
  - `low_income`: Household size × $11,000 + $20,300 (federal poverty threshold)
  - `race_poc`: Race/ethnicity non-White
  - `zero_vehicles`: Household vehicle count = 0
- `load_safety_responses()` - Load survey responses from Elmer database or cached RDS
- `get_ordered_label_map()` - Codebook lookup for ordered factors

**`data_processing.R`** - Legacy workflows and additional preprocessing

**`config.yml`** - Configuration for:
- Database connection (HouseholdTravelSurvey2025)
- Codebook file paths
- Value encoding rules

### 2. Reusable Functions (`R/functions/`)

**`functions.R`** - General-purpose analytical functions

**`section_index_functions.R`** - Functions used by the section-index workflow.

**`special_needs_helpers.R`** - Shared demographic recodes and ordinal-model input preparation used by section-index regression, ordinal regression, and matching.

### 3. Analytical Workflows

#### Factor Analysis (`analyses/special-needs/factor-analysis/`)
Explores underlying structure of safety/security concerns using exploratory factor analysis.

**Workflow:**
1. `prepare_core_data.R` - Load survey responses from Elmer, apply value encoding
2. `factor_model_analysis.R` - Fit exploratory factor model (psych::fa, minres + oblimin rotation)
3. `factor_demographic_comparison.R` - Analyze factor scores across demographic groups
4. `plot_*.R` - Generate visualizations (factor loadings heatmap, demographic comparisons, etc.)

**Key Outputs:**
- `factor_scores.rds` - Respondent-level factor scores
- `best_factor_loadings.csv` - Standardized loadings for 9-factor solution
- `factor_demographic_comparison_*.csv` - Weighted comparisons by demographic group

#### Latent Class Analysis (`analyses/special-needs/latent-class-analysis/`)
Compares latent class models using factor-based vs legacy variable selection.

**Workflow:**
1. `lca_item_audit.R` - Audit variables and compare approaches
2. `lca_model_comparison.R` - Compare model fit and class membership

**Dependency:** Requires factor analysis outputs (`factor-analysis/outputs/curated_core_selection.csv`)

#### Paired Tradeoff Analysis (`analyses/special-needs/paired-tradeoff-analysis/`)
Examines tradeoffs between safety and security concerns across six domains.

**Entry Point:** `paired_tradeoff_disability_analysis.R`

**Outputs:** 13 CSV files including:
- `paired_tradeoff_domain_applicability_summary.csv` - Domain relevance by group
- `paired_tradeoff_paired_contrast_summary.csv` - Safety vs security contrasts

**Report:** [paired-tradeoff-analysis.qmd](analyses/special-needs/paired-tradeoff-analysis/paired-tradeoff-analysis.qmd)

**Note:** Can run independently (no dependencies on other analyses)

#### Section Index Analysis (`analyses/special-needs/section-index/`)
Creates theory-driven composite section scores (not factor-based).

**Workflow:**
1. `section_index_analysis.R` - Compute section-level composite scores
2. `section_index_demographic_comparison.R` - Demographic analysis

**Key Method:** Respondent-level means, standardized to z-scores

**Report:** [section_index.qmd](analyses/special-needs/section-index/section_index.qmd)

#### Linear Regression (`analyses/special-needs/linear-regression/`)
Models a section-level safety and security score with unweighted and survey-weighted linear regression, including an age specification comparison and residual diagnostics.

**Report:** [section5-regression.qmd](analyses/special-needs/linear-regression/section5-regression.qmd)

#### Propensity Score Matching (`analyses/special-needs/propensity-score-matching/`)
Estimates an ATT comparison using exact matching on the available categorical confounders, then checks balance and fits weighted ordinal outcome models. The current script uses exact matching; it does not estimate propensity scores.

**Report:** [propensity-score-matching.qmd](analyses/special-needs/propensity-score-matching/propensity-score-matching.qmd)

#### Ordinal Regression Analysis (`analyses/special-needs/ordinal-regression/`)
Models safety/security outcomes using ordinal logistic regression.

**Key Script:** `ordinal_logistic.R` fits the ordinal regression models.

**Report:** [ordinal-regression.qmd](analyses/special-needs/ordinal-regression/ordinal-regression.qmd)

The root-level [disability-response-comparison.qmd](analyses/special-needs/disability-response-comparison.qmd) compares response patterns by disability status.

## Getting Started

### Prerequisites
- R 4.0+
- RStudio (recommended)
- R Packages: tidyverse, data.table, survey, psych, MASS, MatchIt, cobalt, psrc.travelsurvey, psrcelmer
- Database Access: PSRC Elmer (HouseholdTravelSurvey2025 database)

### Setup
1. Open `safety-security-studies.Rproj` in RStudio
2. Install packages: `install.packages(c("tidyverse", "data.table", "survey", "psych", "MASS"))`
3. Some workflows retrieve data from Elmer; check the workflow's documented input and configuration before running it.

### Running Individual Analyses

**Factor Analysis:**
```r
source("analyses/special-needs/factor-analysis/prepare_core_data.R")
source("analyses/special-needs/factor-analysis/factor_model_analysis.R")
```

**Section Index:**
```r
source("analyses/special-needs/section-index/section_index_analysis.R")
```

**Paired Tradeoff:**
```r
source("analyses/special-needs/paired-tradeoff-analysis/paired_tradeoff_disability_analysis.R")
```

**Section 5 Regression:** Run section-index analysis first, then:
```r
source("analyses/special-needs/linear-regression/section5_index_regression.R")
```

**Exact Matching:**
```r
source("analyses/special-needs/propensity-score-matching/disability_foregone_travel_psm.R")
```

**Ordinal Regression (Quarto):**
```r
quarto::quarto_render("analyses/special-needs/ordinal-regression/ordinal-regression.qmd")
```

The other method reports are in their respective analysis folders. The paired tradeoff and section-index reports are at `analyses/special-needs/paired-tradeoff-analysis/paired-tradeoff-analysis.qmd` and `analyses/special-needs/section-index/section_index.qmd`.

## Project Initialization

The repository uses shared project root detection via `R/project_paths.R`. The helper is side-effect-free; `R/.project-setup.R` also loads shared helpers and packages for legacy workflows:

```r
source("R/project_paths.R")
project_root <- detect_project_root()
```

Set `SURVEY_PROJECT_ROOT` when running from outside the repository and the project root cannot be found by walking up from the script or working directory.

## Data Source

**Primary Source:** PSRC Elmer Database
- Table: `safety_survey.v_responses_2025`
- Access: Requires PSRC network connection and database credentials

**Fallback:** Cached data may be configured in `R/data-prep/config.yml`; follow the input paths used by the workflow being run.

## Known Limitations & Follow-up Work

1. **Root-Level Data Processing:** `R/data-prep/data_processing.R` contains legacy workflows with environment-dependent path references. Review before using in production.

## Related Documentation

- Individual analysis README files in each `analyses/special-needs/*/` subdirectory
- [Factor analysis report](analyses/special-needs/factor-analysis/factor-analysis.qmd)
- [Latent class analysis report](analyses/special-needs/latent-class-analysis/latent-class-analysis.qmd)
- [Linear regression report](analyses/special-needs/linear-regression/section5-regression.qmd)
- [Ordinal regression report](analyses/special-needs/ordinal-regression/ordinal-regression.qmd)
- [Propensity score matching report](analyses/special-needs/propensity-score-matching/propensity-score-matching.qmd)
- [Paired tradeoff report](analyses/special-needs/paired-tradeoff-analysis/paired-tradeoff-analysis.qmd)
- [Section-index report](analyses/special-needs/section-index/section_index.qmd)
- [Disability response comparison](analyses/special-needs/disability-response-comparison.qmd)
- Factor analysis methods: See `analyses/special-needs/factor-analysis/README.md`
- Latent class methodology: See `analyses/special-needs/latent-class-analysis/README.md`
- Section index theory: See `analyses/special-needs/section-index/README.md`
