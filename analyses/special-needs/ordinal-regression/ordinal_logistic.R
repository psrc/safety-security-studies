library(psrc.travelsurvey)
library(dplyr)
library(data.table)
library(MASS)
library(survey)

source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(source_file) || !length(source_file) || is.na(source_file[[1]]) || !nzchar(source_file[[1]])) {
    command_file <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    source_file <- if (length(command_file)) sub("^--file=", "", command_file[[1]]) else NULL
}
script_dir <- if (!is.null(source_file) && length(source_file) && !is.na(source_file[[1]]) && nzchar(source_file[[1]])) {
    dirname(normalizePath(source_file[[1]], winslash = "/", mustWork = FALSE))
} else {
    getwd()
}

# Quarto can run sourced analysis scripts through a temporary R file, so its
# --file argument may not point into this project. Search upward from both the
# source file and working directory to find the repository-owned helper.
project_paths_file <- NULL
for (start_path in unique(c(Sys.getenv("SURVEY_PROJECT_ROOT", unset = ""), script_dir, getwd()))) {
    if (!nzchar(start_path)) next
    start_path <- normalizePath(start_path, winslash = "/", mustWork = FALSE)
    if (file.exists(start_path) && !dir.exists(start_path)) start_path <- dirname(start_path)
    repeat {
        candidate <- file.path(start_path, "R", "project_paths.R")
        if (file.exists(candidate)) {
            project_paths_file <- candidate
            break
        }
        parent_path <- dirname(start_path)
        if (identical(parent_path, start_path)) break
        start_path <- parent_path
    }
    if (!is.null(project_paths_file)) break
}
if (is.null(project_paths_file)) {
    stop("Could not locate R/project_paths.R. Run this analysis from within the safety-security-studies project.")
}
source(project_paths_file)
project_root <- detect_project_root(script_dir)
source(file.path(project_root, "R", "functions", "special_needs_helpers.R"))

ordinal_inputs <- prepare_ordinal_inputs()
data2025 <- ordinal_inputs$data2025
ordinal_model_data <- ordinal_inputs$ordinal_model_data
exp_vars <- ordinal_inputs$exp_vars
ordinal_levels <- ordinal_inputs$ordinal_levels
ordinal_outcomes <- ordinal_inputs$ordinal_outcomes
valid_predictors <- ordinal_inputs$valid_predictors
dropped_predictors <- ordinal_inputs$dropped_predictors

fit_ordinal_model <- function(response_var, data, predictors) {
    model_data <- data %>%
        dplyr::filter(.data[[response_var]] != "Not applicable") %>%
        dplyr::select(all_of(c(response_var, predictors, "sample_segment", "person_weight"))) %>%
        dplyr::filter(complete.cases(.), person_weight > 0) %>%
        droplevels()

    model_formula <- reformulate(predictors, response = response_var)

    unweighted_model <- MASS::polr(
        formula = model_formula,
        data = model_data,
        Hess = TRUE,
        method = "logistic"
    )

    weighted_model <- tryCatch(
        MASS::polr(
            formula = model_formula,
            data = model_data,
            weights = person_weight,
            Hess = TRUE,
            method = "logistic"
        ),
        error = function(e) {
            message(
                "Weighted polr failed for ", response_var,
                " (", conditionMessage(e), "). Falling back to svyolr with ids=~1 (weights-only)."
            )
            NULL
        }
    )

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

    weight_only_design <- survey::svydesign(
        ids = ~1,
        weights = ~person_weight,
        data = model_data
    )

    survey_model <- survey::svyolr(
        formula = model_formula,
        design = survey_design,
        method = "logistic"
    )

    weighted_svy_model <- survey::svyolr(
        formula = model_formula,
        design = weight_only_design,
        method = "logistic"
    )

    tidy_polr <- function(model_object) {
        model_summary <- summary(model_object)
        model_coefficients <- coef(model_summary)
        data.frame(
            term = rownames(model_coefficients),
            estimate = model_coefficients[, "Value"],
            std_error = model_coefficients[, "Std. Error"],
            statistic = model_coefficients[, "t value"],
            p_value = 2 * stats::pnorm(abs(model_coefficients[, "t value"]), lower.tail = FALSE),
            odds_ratio = exp(model_coefficients[, "Value"]),
            row.names = NULL
        )
    }

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

    compute_polr_fit <- function(model_object) {
        null_model <- update(model_object, . ~ 1)
        loglik_full <- as.numeric(stats::logLik(model_object))
        loglik_null <- as.numeric(stats::logLik(null_model))
        lr_df <- attr(stats::logLik(model_object), "df") - attr(stats::logLik(null_model), "df")
        lr_stat <- 2 * (loglik_full - loglik_null)
        lr_p_value <- stats::pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

        data.frame(
            metric = c("likelihood_ratio_test", "aic"),
            value = c(lr_stat, stats::AIC(model_object)),
            df = c(lr_df, NA_real_),
            p_value = c(lr_p_value, NA_real_),
            row.names = NULL
        )
    }

    compute_svyolr_fit <- function(model_object) {
        coefficients <- stats::coef(model_object)
        slope_index <- !grepl("\\|", names(coefficients), fixed = FALSE)
        slope_estimates <- coefficients[slope_index]
        slope_vcov <- as.matrix(stats::vcov(model_object))[slope_index, slope_index, drop = FALSE]

        if (length(slope_estimates) == 0) {
            return(data.frame(
                metric = c("global_wald_test", "aic"),
                value = c(NA_real_, NA_real_),
                df = c(NA_real_, NA_real_),
                p_value = c(NA_real_, NA_real_),
                row.names = NULL
            ))
        }

        wald_df <- length(slope_estimates)
        wald_stat <- tryCatch(
            as.numeric(t(slope_estimates) %*% solve(slope_vcov, slope_estimates)),
            error = function(e) NA_real_
        )
        wald_p_value <- if (is.na(wald_stat)) {
            NA_real_
        } else {
            stats::pchisq(wald_stat, df = wald_df, lower.tail = FALSE)
        }

        data.frame(
            metric = c("global_wald_test", "aic"),
            value = c(wald_stat, NA_real_),
            df = c(wald_df, NA_real_),
            p_value = c(wald_p_value, NA_real_),
            row.names = NULL
        )
    }

    get_model_results <- function(model_object) {
        if (inherits(model_object, "svyolr")) {
            return(tidy_svyolr(model_object))
        }
        tidy_polr(model_object)
    }

    get_model_fit <- function(model_object) {
        if (inherits(model_object, "svyolr")) {
            return(compute_svyolr_fit(model_object))
        }
        compute_polr_fit(model_object)
    }

    compute_wald_test <- function(model_object, term_pattern, label) {
        estimates <- stats::coef(model_object)
        coefficient_names <- names(estimates)
        tested_terms <- grep(term_pattern, coefficient_names, value = TRUE)

        if (length(tested_terms) == 0) {
            return(data.frame(
                predictor = label,
                statistic = NA_real_,
                df = NA_real_,
                p_value = NA_real_,
                row.names = NULL
            ))
        }

        tested_estimates <- estimates[tested_terms]
        tested_vcov <- as.matrix(stats::vcov(model_object))[tested_terms, tested_terms, drop = FALSE]
        wald_statistic <- tryCatch(
            as.numeric(t(tested_estimates) %*% solve(tested_vcov, tested_estimates)),
            error = function(e) NA_real_
        )
        degrees_freedom <- length(tested_terms)

        data.frame(
            predictor = label,
            statistic = wald_statistic,
            df = degrees_freedom,
            p_value = if (is.na(wald_statistic)) NA_real_ else stats::pchisq(
                wald_statistic,
                df = degrees_freedom,
                lower.tail = FALSE
            ),
            row.names = NULL
        )
    }

    compute_adjusted_often_contrasts <- function(model_object, analysis_data) {
        predict_often_probability <- function(new_data) {
            design_matrix <- stats::model.matrix(
                stats::delete.response(stats::terms(model_object)),
                data = new_data
            )
            design_matrix <- design_matrix[, colnames(design_matrix) != "(Intercept)", drop = FALSE]
            slope_coefficients <- stats::coef(model_object)[
                !grepl("\\|", names(stats::coef(model_object)), fixed = FALSE)
            ]
            design_matrix <- design_matrix[, names(slope_coefficients), drop = FALSE]
            linear_predictor <- as.numeric(design_matrix %*% slope_coefficients)
            often_threshold <- model_object$zeta[["Sometimes|Often"]]

            mean(stats::plogis(linear_predictor - often_threshold))
        }

        set_exposure_level <- function(variable, level) {
            new_data <- analysis_data
            new_data[[variable]] <- factor(
                level,
                levels = levels(analysis_data[[variable]])
            )
            new_data
        }

        disability_no <- predict_often_probability(set_exposure_level("disability", "No"))
        disability_yes <- predict_often_probability(set_exposure_level("disability", "Yes"))
        income_low <- predict_often_probability(set_exposure_level("hhincome_bin3", "Less than $50,000"))
        income_middle <- predict_often_probability(set_exposure_level("hhincome_bin3", "$50,000-$99,999"))
        income_high <- predict_often_probability(set_exposure_level("hhincome_bin3", "$100,000 or more"))

        data.frame(
            contrast = c(
                "Disability: Yes minus No",
                "Income: $50,000-$99,999 minus less than $50,000",
                "Income: $100,000 or more minus less than $50,000"
            ),
            reference_probability = c(disability_no, income_low, income_low),
            comparison_probability = c(disability_yes, income_middle, income_high),
            percentage_point_difference = 100 * c(
                disability_yes - disability_no,
                income_middle - income_low,
                income_high - income_low
            ),
            row.names = NULL
        )
    }

    weighted_variant_model <- if (is.null(weighted_model)) weighted_svy_model else weighted_model

    variants <- list(
        unweighted = list(
            model = unweighted_model,
            summary = summary(unweighted_model),
            results = tidy_polr(unweighted_model),
            fit = compute_polr_fit(unweighted_model)
        ),
        weighted = list(
            model = weighted_variant_model,
            summary = summary(weighted_variant_model),
            results = get_model_results(weighted_variant_model),
            fit = get_model_fit(weighted_variant_model)
        ),
        survey_weighted = list(
            model = survey_model,
            summary = summary(survey_model),
            results = tidy_svyolr(survey_model),
            fit = compute_svyolr_fit(survey_model),
            wald_tests = dplyr::bind_rows(
                compute_wald_test(survey_model, "^disability", "disability (global)"),
                compute_wald_test(survey_model, "^hhincome_bin3", "hhincome_bin3 (global)")
            ),
            adjusted_often_contrasts = compute_adjusted_often_contrasts(survey_model, model_data)
        )
    )

    list(
        response = response_var,
        variants = variants
    )
}

ordinal_models <- setNames(
    lapply(ordinal_outcomes, fit_ordinal_model, data = ordinal_model_data, predictors = valid_predictors),
    ordinal_outcomes
)

for (model_name in names(ordinal_models)) {
    cat("\n### ", model_name, "\n", sep = "")

    for (variant_name in names(ordinal_models[[model_name]]$variants)) {
        cat("\n## Variant: ", variant_name, "\n", sep = "")
        print(ordinal_models[[model_name]]$variants[[variant_name]]$summary)
        print(ordinal_models[[model_name]]$variants[[variant_name]]$results)
        print(ordinal_models[[model_name]]$variants[[variant_name]]$fit)
        if (!is.null(ordinal_models[[model_name]]$variants[[variant_name]]$wald_tests)) {
            print(ordinal_models[[model_name]]$variants[[variant_name]]$wald_tests)
        }
        if (!is.null(ordinal_models[[model_name]]$variants[[variant_name]]$adjusted_often_contrasts)) {
            print(ordinal_models[[model_name]]$variants[[variant_name]]$adjusted_often_contrasts)
        }
    }
}

comparison_table <- dplyr::bind_rows(lapply(names(ordinal_models), function(model_name) {
    dplyr::bind_rows(lapply(names(ordinal_models[[model_name]]$variants), function(variant_name) {
        ordinal_models[[model_name]]$variants[[variant_name]]$results %>%
            mutate(response = model_name, variant = variant_name)
    }))
})) %>%
    dplyr::select(response, variant, term, estimate, std_error, statistic, p_value, odds_ratio)

fit_comparison_table <- dplyr::bind_rows(lapply(names(ordinal_models), function(model_name) {
    dplyr::bind_rows(lapply(names(ordinal_models[[model_name]]$variants), function(variant_name) {
        ordinal_models[[model_name]]$variants[[variant_name]]$fit %>%
            mutate(response = model_name, variant = variant_name)
    }))
})) %>%
    dplyr::select(response, variant, metric, value, df, p_value)

cat("\n### Coefficient Comparison Across Variants\n")
print(comparison_table)

cat("\n### Model Fit Comparison Across Variants\n")
print(fit_comparison_table)

wald_test_table <- dplyr::bind_rows(lapply(names(ordinal_models), function(model_name) {
    ordinal_models[[model_name]]$variants$survey_weighted$wald_tests %>%
        mutate(response = model_name, variant = "survey_weighted")
})) %>%
    dplyr::select(response, variant, predictor, statistic, df, p_value)

cat("\n### Global Wald Tests (Survey-Weighted Models)\n")
print(wald_test_table)

adjusted_often_contrast_table <- dplyr::bind_rows(lapply(names(ordinal_models), function(model_name) {
    ordinal_models[[model_name]]$variants$survey_weighted$adjusted_often_contrasts %>%
        mutate(response = model_name, variant = "survey_weighted")
})) %>%
    dplyr::select(
        response,
        variant,
        contrast,
        reference_probability,
        comparison_probability,
        percentage_point_difference
    )

cat("\n### Adjusted Probability Contrasts for Often (Survey-Weighted Models)\n")
print(adjusted_often_contrast_table)
