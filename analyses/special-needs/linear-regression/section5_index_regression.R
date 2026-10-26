library(tidyverse)
library(survey)

`%>%` <- magrittr::`%>%`
read_rds <- readr::read_rds
write_csv <- readr::write_csv
tibble <- tibble::tibble

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

script_path <- commandArgs(trailingOnly = FALSE) %>%
  stringr::str_subset("^--file=") %>%
  stringr::str_remove("^--file=")

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

project_paths_file <- file.path(script_dir, "..", "..", "..", "R", "project_paths.R")
if (!file.exists(project_paths_file)) project_paths_file <- file.path(getwd(), "R", "project_paths.R")
source(project_paths_file)
project_root <- detect_project_root(script_dir)
output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source_view <- "Elmer.safety_survey.v_responses_2025"

source(file.path(project_root, "R", "data-prep", "core_prep_helpers.R"))
source(file.path(project_root, "R", "functions", "special_needs_helpers.R"))

safe_write_csv <- function(data, path) {
  tryCatch(
    {
      write_csv(data, path)
      path
    },
    error = function(e) {
      timestamped_path <- file.path(
        dirname(path),
        paste0(tools::file_path_sans_ext(basename(path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      )
      write_csv(data, timestamped_path)
      timestamped_path
    }
  )
}

index_id <- "section5_safety_anxiety_security_worry"
outcome_var <- paste0(index_id, "_z")
exp_vars <- c("disability", "age_bin5", "hhincome_bin3", "gender_simple", "neighborhood_trips")

# section_index_scores.rds carries the outcome; the four raw demographic fields below are not in it
section_index_scores_path <- file.path(project_root, "analyses", "special-needs", "section-index", "outputs", "section_index_scores.rds")

if (!file.exists(section_index_scores_path)) {
  stop("Expected section_index_scores.rds in section-index/outputs. Run section_index_analysis.R first.")
}

section_index_scores <- read_rds(section_index_scores_path)

if (!outcome_var %in% names(section_index_scores)) {
  stop(sprintf("Missing outcome variable `%s` in section_index_scores.rds.", outcome_var))
}

# Same source view/filtering as section_index_analysis.R, so person_id lines up with section_index_scores
raw_covariate_vars <- c("age", "gender", "hhincome_detailed", "neighborhood_trips")

safety_responses <- load_safety_responses(project_root, source_view = source_view) %>%
  normalize_safety_source() %>%
  filter_completed_responses() %>%
  dplyr::select(any_of(c("person_id", raw_covariate_vars)))

missing_raw_vars <- setdiff(raw_covariate_vars, names(safety_responses))

if (length(missing_raw_vars) > 0L) {
  stop(sprintf("Missing raw explanatory variables in source data: %s", paste(missing_raw_vars, collapse = ", ")))
}

regression_covariates <- safety_responses %>%
  recode_special_needs_covariates() %>%
  dplyr::select(person_id, age, age_bin5, hhincome_bin3, gender_simple, neighborhood_trips)

regression_data <- section_index_scores %>%
  dplyr::mutate(disability = factor(disability_person, levels = c("No", "Yes"))) %>%
  dplyr::select(person_id, person_weight, sample_segment, disability, dplyr::all_of(outcome_var)) %>%
  dplyr::left_join(regression_covariates, by = "person_id")

# age_bin5 vs. age specification comparison: same predictors, age binned vs. treated as linear
exp_vars_age <- c("disability", "age", "hhincome_bin3", "gender_simple", "neighborhood_trips")

valid_predictors <- exp_vars[vapply(dplyr::select(regression_data, dplyr::all_of(exp_vars)), predictor_is_valid, logical(1))]
dropped_predictors <- setdiff(exp_vars, valid_predictors)
valid_predictors_age <- exp_vars_age[vapply(dplyr::select(regression_data, dplyr::all_of(exp_vars_age)), predictor_is_valid, logical(1))]
dropped_predictors_age <- setdiff(exp_vars_age, valid_predictors_age)

if (length(dropped_predictors) > 0L) {
  message("Dropped predictors with <2 observed values: ", paste(dropped_predictors, collapse = ", "))
}

if (length(valid_predictors) == 0L) {
  stop("No explanatory variables with at least two observed values remain after filtering.")
}

# age_bin5 is a deterministic function of age, so listwise deletion is identical either way;
# both specifications are fit on this shared row set so their fit statistics are directly comparable
model_data <- regression_data %>%
  dplyr::select(dplyr::all_of(unique(c(outcome_var, valid_predictors, valid_predictors_age, "sample_segment", "person_weight")))) %>%
  dplyr::filter(complete.cases(.), person_weight > 0) %>%
  droplevels()

n_total <- nrow(regression_data)
n_retained <- nrow(model_data)
message(sprintf("Retained %d of %d respondents after listwise deletion across outcome + predictors.", n_retained, n_total))

model_formula <- reformulate(valid_predictors, response = outcome_var)
model_formula_age <- reformulate(valid_predictors_age, response = outcome_var)

unweighted_model <- lm(model_formula, data = model_data)
unweighted_model_age <- lm(model_formula_age, data = model_data)

old_lonely_psu <- getOption("survey.lonely.psu")
options(survey.lonely.psu = "adjust")
on.exit(options(survey.lonely.psu = old_lonely_psu), add = TRUE)

survey_design <- survey::svydesign(
  ids = ~1,
  strata = ~sample_segment,
  weights = ~person_weight,
  data = model_data,
  nest = TRUE
)

weighted_model <- survey::svyglm(model_formula, design = survey_design, family = stats::gaussian())

tidy_coefficients <- function(model_object) {
  model_coefficients <- coef(summary(model_object))
  data.frame(
    term = rownames(model_coefficients),
    estimate = model_coefficients[, "Estimate"],
    std_error = model_coefficients[, "Std. Error"],
    statistic = model_coefficients[, "t value"],
    p_value = model_coefficients[, "Pr(>|t|)"],
    row.names = NULL
  )
}

coefficient_comparison <- dplyr::bind_rows(
  tidy_coefficients(unweighted_model) %>% dplyr::mutate(model = "unweighted_lm"),
  tidy_coefficients(weighted_model) %>% dplyr::mutate(model = "svyglm_gaussian")
) %>%
  dplyr::relocate(model, .before = term)

model_fit <- tibble(
  model = c("unweighted_lm", "svyglm_gaussian"),
  n = n_retained,
  r_squared = c(summary(unweighted_model)$r.squared, NA_real_),
  adj_r_squared = c(summary(unweighted_model)$adj.r.squared, NA_real_)
)

# age_bin5 (ordered polynomial contrasts) vs. age (1 df, linear) at equal N, so AIC/BIC are directly comparable;
# compared on the unweighted fit since svyglm's pseudo-likelihood has no standard AIC/R2 across specifications
age_specification_comparison <- tibble(
  age_specification = c("age_bin5", "age"),
  predictor_df = c(nlevels(model_data$age_bin5) - 1L, 1L),
  n = n_retained,
  unweighted_r_squared = c(summary(unweighted_model)$r.squared, summary(unweighted_model_age)$r.squared),
  unweighted_adj_r_squared = c(summary(unweighted_model)$adj.r.squared, summary(unweighted_model_age)$adj.r.squared),
  unweighted_aic = c(stats::AIC(unweighted_model), stats::AIC(unweighted_model_age)),
  unweighted_bic = c(stats::BIC(unweighted_model), stats::BIC(unweighted_model_age))
)

sample_summary <- tibble(
  outcome = outcome_var,
  n_total_index_scores = n_total,
  n_retained = n_retained,
  n_dropped = n_total - n_retained,
  dropped_predictors = paste(dropped_predictors, collapse = "; ")
)

residuals_vector <- residuals(unweighted_model)
set.seed(42)
residual_diagnostics <- tibble(
  residual_mean = mean(residuals_vector),
  residual_sd = stats::sd(residuals_vector),
  shapiro_p_value = tryCatch(
    stats::shapiro.test(sample(residuals_vector, min(5000, length(residuals_vector))))$p.value,
    error = function(e) NA_real_
  )
)

coefficient_path <- safe_write_csv(coefficient_comparison, file.path(output_dir, "section5_regression_coefficients.csv"))
fit_path <- safe_write_csv(model_fit, file.path(output_dir, "section5_regression_model_fit.csv"))
sample_path <- safe_write_csv(sample_summary, file.path(output_dir, "section5_regression_sample_summary.csv"))
diagnostics_path <- safe_write_csv(residual_diagnostics, file.path(output_dir, "section5_regression_residual_diagnostics.csv"))
age_spec_path <- safe_write_csv(age_specification_comparison, file.path(output_dir, "section5_regression_age_specification_comparison.csv"))

message("Wrote coefficient comparison to: ", coefficient_path)
message("Wrote model fit summary to: ", fit_path)
message("Wrote sample summary to: ", sample_path)
message("Wrote residual diagnostics to: ", diagnostics_path)
message("Wrote age specification comparison to: ", age_spec_path)

print(coefficient_comparison)
print(model_fit)
print(age_specification_comparison)
