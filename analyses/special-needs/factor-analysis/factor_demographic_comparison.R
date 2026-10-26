library(tidyverse)
library(survey)

read_csv <- readr::read_csv
read_rds <- readr::read_rds
write_csv <- readr::write_csv
tibble <- tibble::tibble

options(survey.lonely.psu = "adjust")
options(survey.adjust.domain.lonely = TRUE)

demographic_comparisons <- tibble::tibble(
  respondent_dichotomy = c(
    "disability_person",
    "age65plus",
    "low_income",
    "race_poc",
    "zero_vehicles"
  ),
  respondent_dichotomy_levels = list(
    c("Yes", "No"),
    c("Yes", "No"),
    c("Yes", "No"),
    c("Yes", "No"),
    c("Yes", "No")
  )
)

weighted_summary_file <- "factor_demographic_weighted_summary.csv"
test_results_file <- "factor_demographic_tests.csv"
interpretation_summary_file <- "factor_demographic_interpretation.csv"

script_path <- commandArgs(trailingOnly = FALSE) %>%
  stringr::str_subset("^--file=") %>%
  stringr::str_remove("^--file=")

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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

weighted_var <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0

  if (!any(keep)) {
    return(NA_real_)
  }

  x <- x[keep]
  w <- w[keep]
  mean_x <- stats::weighted.mean(x, w)
  sum_w <- sum(w)

  if (sum_w <= 0) {
    return(NA_real_)
  }

  sum(w * (x - mean_x)^2) / sum_w
}

compute_weighted_smd <- function(dt, score_var, respondent_dichotomy, respondent_dichotomy_levels) {
  comparison_groups <- respondent_dichotomy_levels

  if (length(comparison_groups) != 2L) {
    return(NA_real_)
  }

  group_1 <- dt[dt[[respondent_dichotomy]] == comparison_groups[1], ]
  group_2 <- dt[dt[[respondent_dichotomy]] == comparison_groups[2], ]

  if (nrow(group_1) == 0 || nrow(group_2) == 0) {
    return(NA_real_)
  }

  mean_1 <- stats::weighted.mean(group_1[[score_var]], group_1[["person_weight"]])
  mean_2 <- stats::weighted.mean(group_2[[score_var]], group_2[["person_weight"]])

  var_1 <- weighted_var(group_1[[score_var]], group_1[["person_weight"]])
  var_2 <- weighted_var(group_2[[score_var]], group_2[["person_weight"]])
  
  pooled_sd <- sqrt(mean(c(var_1, var_2), na.rm = TRUE))

  if (is.na(pooled_sd) || pooled_sd == 0) {
    return(NA_real_)
  }

  (mean_1 - mean_2) / pooled_sd
}

classify_effect_size <- function(weighted_smd) {
  dplyr::case_when(
    is.na(weighted_smd) ~ NA_character_,
    abs(weighted_smd) < 0.2 ~ "negligible",
    abs(weighted_smd) < 0.5 ~ "small",
    abs(weighted_smd) < 0.8 ~ "moderate",
    TRUE ~ "large"
  )
}

classify_significance <- function(p_value) {
  dplyr::case_when(
    is.na(p_value) ~ NA_character_,
    p_value < 0.001 ~ "very strong evidence",
    p_value < 0.05 ~ "clear evidence",
    TRUE ~ "no clear evidence"
  )
}

if (!file.exists(file.path(output_dir, "factor_scores.rds"))) {
  stop("Expected factor_scores.rds in factor_analysis/outputs. Run factor_model_analysis.R first.")
}

factor_scores <- read_rds(file.path(output_dir, "factor_scores.rds"))
factor_labels <- read_csv(file.path(output_dir, "factor_dimension_labels.csv"), show_col_types = FALSE)

factor_names <- setdiff(
  names(factor_scores),
  c("person_id", "person_weight", "sample_segment", demographic_comparisons$respondent_dichotomy)
)

missing_demographic_vars <- setdiff(demographic_comparisons$respondent_dichotomy, names(factor_scores))

if (length(missing_demographic_vars) > 0L) {
  stop(
    sprintf(
      "Missing demographic variables in factor_scores.rds: %s",
      paste(missing_demographic_vars, collapse = ", ")
    )
  )
}

analyze_demographic_comparison <- function(respondent_dichotomy, respondent_dichotomy_levels) {
  if (length(respondent_dichotomy_levels) != 2L) {
    stop(
      sprintf(
        "`respondent_dichotomy_levels` for `%s` must contain exactly two comparison values.",
        respondent_dichotomy
      )
    )
  }

  available_dichotomy_levels <- factor_scores[[respondent_dichotomy]] %>%
    stats::na.omit() %>%
    unique()

  missing_dichotomy_levels <- setdiff(respondent_dichotomy_levels, available_dichotomy_levels)

  if (length(missing_dichotomy_levels) > 0L) {
    stop(
      sprintf(
        "Requested `%s` levels not found in factor_scores: %s",
        respondent_dichotomy,
        paste(missing_dichotomy_levels, collapse = ", ")
      )
    )
  }

  analysis_data <- factor_scores %>%
    dplyr::filter(.data[[respondent_dichotomy]] %in% respondent_dichotomy_levels, !is.na(person_weight), person_weight > 0, !is.na(sample_segment))

  analysis_data[[respondent_dichotomy]] <- factor(
    analysis_data[[respondent_dichotomy]],
    levels = respondent_dichotomy_levels
  )

  design <- svydesign(
    ids = ~1,
    strata = ~sample_segment,
    weights = ~person_weight,
    data = analysis_data,
    nest = TRUE
  )

  weighted_summary <- purrr::map_dfr(factor_names, function(factor_name) {
    summary_table <- svyby(
      as.formula(paste0("~`", factor_name, "`")),
      as.formula(paste0("~`", respondent_dichotomy, "`")),
      design,
      svymean,
      vartype = c("se", "ci"),
      na.rm = TRUE,
      keep.names = FALSE
    )

    summary_table %>%
      as_tibble() %>%
      rename(respondent_group = 1, mean_score = 2, se = 3, ci_low = 4, ci_high = 5) %>%
      mutate(
        respondent_dichotomy = respondent_dichotomy,
        comparison_group_1 = respondent_dichotomy_levels[1],
        comparison_group_2 = respondent_dichotomy_levels[2],
        factor = factor_name,
        .before = 1
      )
  }) %>%
    left_join(factor_labels, by = "factor")

  test_results <- purrr::map_dfr(factor_names, function(factor_name) {
    test_formula <- as.formula(paste0("`", factor_name, "` ~ `", respondent_dichotomy, "`"))
    test_object <- svyttest(test_formula, design)

    tibble(
      respondent_dichotomy = respondent_dichotomy,
      comparison_group_1 = respondent_dichotomy_levels[1],
      comparison_group_2 = respondent_dichotomy_levels[2],
      factor = factor_name,
      test = "svyttest",
      p_value = unname(test_object$p.value),
      weighted_smd = compute_weighted_smd(
        analysis_data,
        factor_name,
        respondent_dichotomy,
        respondent_dichotomy_levels
      )
    )
  }) %>%
    left_join(factor_labels, by = "factor") %>%
    mutate(
      significance_band = classify_significance(p_value),
      effect_size_band = classify_effect_size(weighted_smd),
      respondent_group_higher = dplyr::case_when(
        is.na(weighted_smd) ~ NA_character_,
        weighted_smd > 0 ~ respondent_dichotomy_levels[1],
        weighted_smd < 0 ~ respondent_dichotomy_levels[2],
        TRUE ~ "Neither"
      )
    )

  interpretation_summary <- test_results %>%
    mutate(
      interpretation = dplyr::case_when(
        is.na(p_value) ~ NA_character_,
        p_value < 0.05 ~ paste0(
          factor_label,
          ": ",
          significance_band,
          " of a ",
          respondent_dichotomy,
          " difference; scores are higher among respondents in the ",
          respondent_group_higher,
          " group with a ",
          effect_size_band,
          " effect (weighted SMD = ",
          round(weighted_smd, 2),
          ")."
        ),
        TRUE ~ paste0(
          factor_label,
          ": ",
          significance_band,
          " of a ",
          respondent_dichotomy,
          " difference; the estimated effect is ",
          effect_size_band,
          " (weighted SMD = ",
          round(weighted_smd, 2),
          ")."
        )
      )
    )

  list(
    weighted_summary = weighted_summary,
    test_results = test_results,
    interpretation_summary = interpretation_summary
  )
}

comparison_results <- purrr::pmap(
  demographic_comparisons,
  analyze_demographic_comparison
)

weighted_summary <- purrr::list_rbind(purrr::map(comparison_results, "weighted_summary"))
test_results <- purrr::list_rbind(purrr::map(comparison_results, "test_results"))
interpretation_summary <- purrr::list_rbind(purrr::map(comparison_results, "interpretation_summary"))

weighted_summary_path <- safe_write_csv(weighted_summary, file.path(output_dir, weighted_summary_file))
test_results_path <- safe_write_csv(test_results, file.path(output_dir, test_results_file))
interpretation_summary_path <- safe_write_csv(interpretation_summary, file.path(output_dir, interpretation_summary_file))

message("Wrote weighted factor summary to: ", weighted_summary_path)
message("Wrote comparison tests to: ", test_results_path)
message("Wrote interpretation summary to: ", interpretation_summary_path)