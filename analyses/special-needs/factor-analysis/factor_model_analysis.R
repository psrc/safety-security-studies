library(tidyverse)
library(psych)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
read_rds <- readr::read_rds
write_csv <- readr::write_csv
write_rds <- readr::write_rds
tibble <- tibble::tibble
as_tibble <- tibble::as_tibble

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c("variable", "anchor_variables", "anchor_match_count", "rule_id")))
}

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

demographic_dichotomy_vars <- c(
  "disability_person",
  "age65plus",
  "low_income",
  "race_poc",
  "zero_vehicles"
)

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

safe_write_rds <- function(object, path) {
  tryCatch(
    {
      write_rds(object, path)
      path
    },
    error = function(e) {
      timestamped_path <- file.path(
        dirname(path),
        paste0(tools::file_path_sans_ext(basename(path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      )
      write_rds(object, timestamped_path)
      timestamped_path
    }
  )
}

orient_factor_solution <- function(loadings_matrix, score_matrix) {
  factor_sign <- apply(loadings_matrix, 2, function(x) sign(x[which.max(abs(x))]))

  list(
    loadings = sweep(loadings_matrix, 2, factor_sign, `*`),
    scores = sweep(score_matrix, 2, factor_sign, `*`),
    factor_sign = factor_sign
  )
}

orient_loading_matrix <- function(loadings_matrix) {
  factor_sign <- apply(loadings_matrix, 2, function(x) {
    dominant_sign <- sign(x[which.max(abs(x))])

    if (is.na(dominant_sign) || dominant_sign == 0) {
      return(1)
    }

    dominant_sign
  })

  sweep(loadings_matrix, 2, factor_sign, `*`)
}

run_correlation_factor_model <- function(correlation_matrix, factor_count, n_obs) {
  captured_warnings <- character()

  fitted_model <- withCallingHandlers(
    psych::fa(
      r = correlation_matrix,
      nfactors = factor_count,
      n.obs = n_obs,
      fm = "minres",
      rotate = "oblimin"
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    model = fitted_model,
    warnings = unique(captured_warnings)
  )
}

run_scoring_factor_model <- function(data, factor_count) {
  captured_warnings <- character()

  fitted_model <- withCallingHandlers(
    psych::fa(
      data,
      nfactors = factor_count,
      fm = "minres",
      rotate = "oblimin",
      cor = "poly"
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    model = fitted_model,
    warnings = unique(captured_warnings)
  )
}

factor_label_rules <- tibble::tibble(
  rule_id = 1:9,
  factor_label = c(
    "Safety Anxiety And Security Worry",
    "Active And Multimodal Travel Orientation",
    "System And Rule-Based Safety Confidence",
    "Risk-Tolerant Travel Confidence",
    "Environmental And Travel Flexibility Attitudes",
    "General Worry And Social Discomfort",
    "Travel Dissatisfaction And Low Safety Confidence",
    "Enforcement And Speed-Safety Norms",
    "Safety-Driven Travel Tradeoffs"
  ),
  factor_description = c(
    "Higher scores indicate more anticipatory anxiety and worry about safety, security, and getting where one needs to go.",
    "Higher scores indicate stronger preference for walking, transit, and mixed-use proximity, alongside lower perceived impracticality of active travel and lower car orientation.",
    "Higher scores indicate lower confidence in transportation systems, traffic rules, and other travelers behaving safely.",
    "Higher scores indicate greater comfort with travel risk, less preference for the safest option, and more confidence mixing with traffic.",
    "Higher scores indicate a diffuse mix of environmental skepticism, lower telework orientation, less walking relaxation, and greater concern about urgent travel safety.",
    "Higher scores indicate more generalized worry, lower life satisfaction, and greater discomfort around other people while traveling.",
    "Higher scores indicate more stressful and less satisfying daily travel, along with lower perceived travel and system safety.",
    "Higher scores indicate stronger support for enforcement, speed management, and other visible traffic-safety norms.",
    "Higher scores indicate that safety concerns more strongly reshape mode choice, routing, and willingness to absorb extra cost or inconvenience."
  ),
  anchor_variables = list(
    c("travel_anxiety", "worry_destination", "worry_security", "urgent_worry", "travel_worry", "traffic_worry"),
    c("walk_trips", "active_transport_practical", "transit_travel", "own_car", "dest_proximity"),
    c("drive_safely", "system_safe", "traffic_rules", "overall_safety", "signal_confidence", "traffic_deaths"),
    c("risk_taking", "safe_option", "bike_comfort_cars", "enjoy_travel", "safety_ratings"),
    c("environment_emphasis", "telework_importance", "walk_recreation_relax", "urgent_worry", "safety_ratings"),
    c("worry", "satisfied", "physical_close", "safety_ratings", "own_car"),
    c("travel_satisfaction", "travel_stressful", "overall_safety", "satisfied", "system_safe"),
    c("traffic_enforcement", "safe_clothes", "speed_limits", "drive_safely", "bike_comfort_cars"),
    c("safety_choices_modes", "safety_choices", "safety_choices_costs", "worry_security")
  )
)

label_factor_dimensions <- function(top_loadings) {
  factor_list <- unique(top_loadings$factor)

  purrr::map_dfr(factor_list, function(factor_name) {
    top_vars <- top_loadings %>%
      dplyr::filter(factor == factor_name) %>%
      dplyr::pull(variable)

    candidate_scores <- factor_label_rules %>%
      mutate(anchor_match_count = purrr::map_int(anchor_variables, ~ sum(top_vars %in% .x))) %>%
      arrange(desc(anchor_match_count), rule_id)

    best_rule <- candidate_scores %>% slice(1)
    top_loading_signature <- paste(top_vars, collapse = ", ")

    tibble(
      factor = factor_name,
      factor_label = ifelse(best_rule$anchor_match_count > 0, best_rule$factor_label, paste("Factor", factor_name)),
      factor_description = ifelse(
        best_rule$anchor_match_count > 0,
        best_rule$factor_description,
        paste("Exploratory factor defined by:", top_loading_signature)
      ),
      anchor_match_count = best_rule$anchor_match_count,
      top_loading_signature = top_loading_signature
    )
  })
}

selection_table <- read_csv(file.path(output_dir, "curated_core_selection.csv"), show_col_types = FALSE)
prepared_core_data <- read_rds(file.path(output_dir, "core_model_data.rds"))

missing_demographic_vars <- setdiff(demographic_dichotomy_vars, names(prepared_core_data))

if (length(missing_demographic_vars) > 0L) {
  stop(
    sprintf(
      "Missing demographic variables in core_model_data.rds: %s",
      paste(missing_demographic_vars, collapse = ", ")
    )
  )
}

selected_variables <- selection_table %>%
  dplyr::filter(selection_status == "keep") %>%
  pull(variable)

model_data <- prepared_core_data %>%
  dplyr::filter(complete_case_core) %>%
  dplyr::select(any_of(selected_variables))

if (nrow(model_data) == 0) {
  stop("No complete-case rows available for factor analysis.")
}

ordered_factor_data <- model_data %>%
  dplyr::mutate(across(everything(), ~ ordered(.x, levels = sort(unique(.x)))))

polychoric_matrix <- psych::polychoric(ordered_factor_data)
smoothed_rho <- psych::cor.smooth(polychoric_matrix$rho)

baseline_max_factor_count <- 8L

parallel_analysis_result <- psych::fa.parallel(
  smoothed_rho,
  n.obs = nrow(model_data),
  fa = "fa",
  fm = "minres",
  plot = FALSE
)

parallel_analysis_recommended <- suppressWarnings(as.integer(parallel_analysis_result$nfact))

if (length(parallel_analysis_recommended) == 0L || is.na(parallel_analysis_recommended) || parallel_analysis_recommended < 1L) {
  parallel_analysis_recommended <- baseline_max_factor_count
}

max_tested_factor_count <- min(
  ncol(model_data) - 1L,
  max(baseline_max_factor_count, parallel_analysis_recommended)
)

tested_factor_counts <- seq_len(max_tested_factor_count)

vss_result <- psych::VSS(
  smoothed_rho,
  n = max_tested_factor_count,
  rotate = "oblimin",
  fm = "minres",
  plot = FALSE,
  diagonal = FALSE,
  n.obs = nrow(model_data)
)

map_best_factor_count <- which.min(vss_result$map)
vss_complexity_1_best_count <- which.max(vss_result$cfit.1)
vss_complexity_2_best_count <- which.max(vss_result$cfit.2)

fit_factor_model <- function(factor_count) {
  fitted_result <- run_correlation_factor_model(smoothed_rho, factor_count, nrow(model_data))
  fitted_model <- fitted_result$model
  fit_warning <- if (length(fitted_result$warnings) == 0L) {
    NA_character_
  } else {
    paste(fitted_result$warnings, collapse = " | ")
  }

  tibble(
    model_family = "factor",
    model_size = factor_count,
    n_obs = nrow(model_data),
    log_likelihood = NA_real_,
    aic = NA_real_,
    bic = fitted_model$BIC,
    entropy = NA_real_,
    rmsea = fitted_model$RMSEA[1],
    tli = fitted_model$TLI,
    rmsr = fitted_model$rms,
    fit_warning = fit_warning
  )
}

factor_summary <- purrr::map_dfr(tested_factor_counts, fit_factor_model)

factor_retention_diagnostics <- tibble(
  factor_count = tested_factor_counts,
  parallel_actual_fa_eigen = parallel_analysis_result$fa.values[tested_factor_counts],
  parallel_simulated_fa_eigen = parallel_analysis_result$fa.sim[tested_factor_counts],
  parallel_above_simulated = parallel_actual_fa_eigen > parallel_simulated_fa_eigen,
  map = vss_result$map[tested_factor_counts],
  vss_complexity_1 = vss_result$cfit.1[tested_factor_counts],
  vss_complexity_2 = vss_result$cfit.2[tested_factor_counts]
) %>%
  left_join(
    factor_summary %>%
      transmute(
        factor_count = model_size,
        bic,
        rmsea,
        tli,
        rmsr,
        fit_warning
      ),
    by = "factor_count"
  )

best_factor_count <- factor_summary %>%
  slice_min(bic, n = 1, with_ties = FALSE) %>%
  pull(model_size)

factor_retention_summary <- tibble(
  method = c(
    "BIC minimum",
    "Parallel analysis",
    "MAP minimum",
    "VSS complexity 1 maximum",
    "VSS complexity 2 maximum"
  ),
  recommended_factor_count = c(
    best_factor_count,
    parallel_analysis_recommended,
    map_best_factor_count,
    vss_complexity_1_best_count,
    vss_complexity_2_best_count
  ),
  criterion_value = c(
    factor_summary %>% slice_min(bic, n = 1, with_ties = FALSE) %>% pull(bic),
    parallel_analysis_result$fa.values[parallel_analysis_recommended] - parallel_analysis_result$fa.sim[parallel_analysis_recommended],
    min(vss_result$map, na.rm = TRUE),
    max(vss_result$cfit.1, na.rm = TRUE),
    max(vss_result$cfit.2, na.rm = TRUE)
  )
)

candidate_factor_counts <- sort(unique(c(
  best_factor_count,
  parallel_analysis_recommended,
  map_best_factor_count,
  vss_complexity_1_best_count,
  vss_complexity_2_best_count
)))

candidate_factor_top_loadings <- purrr::map_dfr(candidate_factor_counts, function(factor_count) {
  candidate_result <- run_correlation_factor_model(smoothed_rho, factor_count, nrow(model_data))
  candidate_loading_matrix <- orient_loading_matrix(unclass(candidate_result$model$loadings))
  rownames(candidate_loading_matrix) <- colnames(model_data)

  candidate_warning <- if (length(candidate_result$warnings) == 0L) {
    NA_character_
  } else {
    paste(candidate_result$warnings, collapse = " | ")
  }

  as.data.frame(candidate_loading_matrix) %>%
    tibble::rownames_to_column("variable") %>%
    as_tibble() %>%
    pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
    mutate(
      factor_count = factor_count,
      abs_loading = abs(loading),
      fit_warning = candidate_warning
    ) %>%
    group_by(factor_count, factor) %>%
    slice_max(order_by = abs_loading, n = 5, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(factor_count, factor, desc(abs_loading))
})

best_factor_model_result <- run_correlation_factor_model(smoothed_rho, best_factor_count, nrow(model_data))
best_factor_model <- best_factor_model_result$model

best_factor_model_for_scores_result <- run_scoring_factor_model(model_data, best_factor_count)
best_factor_model_for_scores <- best_factor_model_for_scores_result$model

factor_score_result <- psych::factor.scores(
  model_data,
  best_factor_model_for_scores,
  method = "tenBerge"
)

oriented_factor_solution <- orient_factor_solution(
  unclass(best_factor_model$loadings),
  factor_score_result[["scores"]]
)

rownames(oriented_factor_solution[["loadings"]]) <- colnames(model_data)

factor_loadings <- as.data.frame(oriented_factor_solution[["loadings"]]) %>%
  tibble::rownames_to_column("variable") %>%
  as_tibble()

factor_top_loadings <- factor_loadings %>%
  pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
  mutate(abs_loading = abs(loading)) %>%
  group_by(factor) %>%
  slice_max(order_by = abs_loading, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(factor, desc(abs_loading))

factor_labels <- label_factor_dimensions(factor_top_loadings)

factor_orientation <- tibble(
  factor = colnames(oriented_factor_solution[["loadings"]]),
  sign_applied = oriented_factor_solution[["factor_sign"]]
) %>%
  left_join(factor_labels, by = "factor")

factor_scores <- prepared_core_data %>%
  dplyr::filter(complete_case_core) %>%
  dplyr::select(person_id, person_weight, sample_segment, all_of(demographic_dichotomy_vars)) %>%
  bind_cols(as_tibble(oriented_factor_solution[["scores"]]))

factor_model_notes <- tibble(
  note = c(
    "This is an exploratory factor analysis on the curated first-pass Likert block.",
    paste0(
      "Best factor model by BIC used ", best_factor_count,
      " factors among the ", min(tested_factor_counts), "-", max(tested_factor_counts),
      " factor solutions that were tested."
    ),
    paste0(
      "Additional retention diagnostics recommended: parallel analysis = ", parallel_analysis_recommended,
      ", MAP = ", map_best_factor_count,
      ", VSS complexity 1 = ", vss_complexity_1_best_count,
      ", VSS complexity 2 = ", vss_complexity_2_best_count, "."
    ),
    paste0(
      "Candidate factor counts carried forward for loading-structure comparison: ",
      paste(candidate_factor_counts, collapse = ", "), "."
    ),
    "Factor labels are heuristic summaries based on each factor's top loading items."
  )
)

factor_summary_path <- safe_write_csv(factor_summary, file.path(output_dir, "factor_model_summary.csv"))
factor_retention_diagnostics_path <- safe_write_csv(factor_retention_diagnostics, file.path(output_dir, "factor_retention_diagnostics.csv"))
factor_retention_summary_path <- safe_write_csv(factor_retention_summary, file.path(output_dir, "factor_retention_summary.csv"))
factor_loadings_path <- safe_write_csv(factor_loadings, file.path(output_dir, "best_factor_loadings.csv"))
candidate_factor_top_loadings_path <- safe_write_csv(candidate_factor_top_loadings, file.path(output_dir, "candidate_factor_top_loadings.csv"))
factor_scores_csv_path <- safe_write_csv(factor_scores, file.path(output_dir, "factor_scores.csv"))
factor_scores_rds_path <- safe_write_rds(factor_scores, file.path(output_dir, "factor_scores.rds"))
factor_orientation_path <- safe_write_csv(factor_orientation, file.path(output_dir, "factor_orientation.csv"))
factor_labels_path <- safe_write_csv(factor_labels, file.path(output_dir, "factor_dimension_labels.csv"))
factor_top_loadings_path <- safe_write_csv(factor_top_loadings %>% left_join(factor_labels, by = "factor"), file.path(output_dir, "factor_top_loadings.csv"))
factor_notes_path <- safe_write_csv(factor_model_notes, file.path(output_dir, "factor_model_notes.csv"))

message("Wrote factor model summary to: ", factor_summary_path)
message("Wrote factor retention diagnostics to: ", factor_retention_diagnostics_path)
message("Wrote factor retention summary to: ", factor_retention_summary_path)
message("Wrote factor loadings to: ", factor_loadings_path)
message("Wrote candidate factor top loadings to: ", candidate_factor_top_loadings_path)
message("Wrote factor scores to: ", factor_scores_csv_path)
message("Wrote factor scores RDS to: ", factor_scores_rds_path)
message("Wrote factor orientation to: ", factor_orientation_path)
message("Wrote factor labels to: ", factor_labels_path)
message("Wrote factor top loadings to: ", factor_top_loadings_path)
message("Wrote factor notes to: ", factor_notes_path)
message("Best factor count: ", best_factor_count)
message("Comparison sample size: ", nrow(model_data))