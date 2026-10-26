# Propensity score matching: effect of disability on forgoing travel, as a robustness
# check against the regression-adjusted estimates in ordinal_logistic.R.
library(MatchIt)
library(cobalt)
library(survey)
library(dplyr)

# Reuse the shared input preparation without fitting the ordinal models.
script_path <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_path) > 0L) {
    sub("^--file=", "", script_path[[1]])
} else {
    tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
}
script_dir <- if (length(script_path) > 0L && !is.na(script_path[[1]])) dirname(script_path[[1]]) else getwd()
project_paths_file <- file.path(script_dir, "..", "..", "..", "R", "project_paths.R")
if (!file.exists(project_paths_file)) project_paths_file <- file.path(getwd(), "R", "project_paths.R")
source(project_paths_file)
project_root <- detect_project_root(script_dir)
source(file.path(project_root, "R", "functions", "special_needs_helpers.R"))

ordinal_inputs <- prepare_ordinal_inputs()
ordinal_model_data <- ordinal_inputs$ordinal_model_data
valid_predictors <- ordinal_inputs$valid_predictors
ordinal_outcomes <- ordinal_inputs$ordinal_outcomes

treatment_var <- "disability"
# Home location (rgs_class/urbanicity) is a plausible confounder but is left out for now,
# since it may itself be shaped by neighborhood attributes rather than pre-treatment demographics.
confounders <- setdiff(valid_predictors, treatment_var)

psm_data <- ordinal_model_data %>%
    dplyr::select(all_of(c(ordinal_outcomes, treatment_var, confounders, "person_weight"))) %>%
    dplyr::filter(complete.cases(.), person_weight > 0) %>%
    droplevels()

cat("\nUnique", treatment_var, "= No respondents available before matching:",
    sum(psm_data[[treatment_var]] == "No"), "out of", nrow(psm_data), "\n")

# Checks whether outcome sparsity already exists in the raw comparison pool, before any matching
cat("\n### Pre-Match Weighted Cross-Tab (Diagnostic)\n")
for (outcome_name in ordinal_outcomes) {
    cat("\n", outcome_name, ":\n")
    print(xtabs(psm_data$person_weight ~ psm_data[[treatment_var]] + psm_data[[outcome_name]]))
}

match_formula <- reformulate(confounders, response = treatment_var)

# Exact matching: confounders are all low-cardinality categorical, and distance-based
# nearest-neighbor matching was converging on the same handful of controls for every
# treated unit (poor common support), zeroing out entire outcome categories.
match_out <- MatchIt::matchit(
    match_formula,
    data = psm_data,
    method = "exact",
    s.weights = "person_weight",
    estimand = "ATT"
)

cat("\n### Matching Summary\n")
print(summary(match_out))

# Flags when matches are concentrated on too few unique control respondents to be representative
matched_control_ids <- na.omit(as.vector(match_out$match.matrix))
cat("\nUnique controls reused:", length(unique(matched_control_ids)),
    "out of", sum(psm_data[[treatment_var]] == "No"), "available controls\n")

cat("\n### Covariate Balance Before/After Matching\n")
balance_table <- cobalt::bal.tab(match_out, un = TRUE, s.d.denom = "treated")
print(balance_table)

matched_data <- MatchIt::match.data(match_out)

tidy_svyolr <- function(model_object) {
    estimates <- stats::coef(model_object)
    std_errors <- sqrt(diag(stats::vcov(model_object)))
    statistic <- estimates / std_errors
    p_values <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)

    data.frame(
        term = names(estimates),
        estimate = as.numeric(estimates),
        std_error = as.numeric(std_errors),
        statistic = as.numeric(statistic),
        p_value = as.numeric(p_values),
        odds_ratio = exp(as.numeric(estimates)),
        row.names = NULL
    )
}

fit_matched_ordinal_model <- function(response_var, data) {
    # Empty/near-empty weighted cells (e.g. no matched controls in one outcome level) make svyolr's Hessian singular
    weighted_cross_tab <- xtabs(data$weights ~ data[[treatment_var]] + data[[response_var]])
    if (any(weighted_cross_tab == 0)) {
        message("Sparse weighted cells for ", response_var, ":")
        print(weighted_cross_tab)
    }

    matched_design <- survey::svydesign(ids = ~1, weights = ~weights, data = data)
    model_formula <- reformulate(treatment_var, response = response_var)
    model <- tryCatch(
        survey::svyolr(model_formula, design = matched_design, method = "logistic"),
        error = function(e) {
            message("svyolr failed for ", response_var, ": ", conditionMessage(e))
            NULL
        }
    )

    if (is.null(model)) {
        return(NULL)
    }

    list(response = response_var, model = model, results = tidy_svyolr(model))
}

matched_models <- Filter(
    Negate(is.null),
    setNames(
        lapply(ordinal_outcomes, fit_matched_ordinal_model, data = matched_data),
        ordinal_outcomes
    )
)

for (outcome_name in names(matched_models)) {
    cat("\n### Matched ATT estimate:", outcome_name, "\n")
    print(summary(matched_models[[outcome_name]]$model))
    print(matched_models[[outcome_name]]$results)
}

matched_comparison_table <- dplyr::bind_rows(lapply(names(matched_models), function(outcome_name) {
    matched_models[[outcome_name]]$results %>% mutate(response = outcome_name)
})) %>%
    dplyr::select(response, term, estimate, std_error, statistic, p_value, odds_ratio)

cat("\n### PSM Coefficient Comparison Across Outcomes\n")
print(matched_comparison_table)
