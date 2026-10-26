library(tidyverse)
library(psych)
library(bit64)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
write_csv <- readr::write_csv
write_rds <- readr::write_rds
tibble <- tibble::tibble
as_tibble <- tibble::as_tibble

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c("variable", "value", "label", "q_id", "index_id", "note_type")))
}

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

safe_quantile <- function(x, prob) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
}

compute_standardized_alpha <- function(correlation_matrix) {
  if (is.null(correlation_matrix) || ncol(correlation_matrix) < 2L) {
    return(NA_real_)
  }

  lower_values <- correlation_matrix[lower.tri(correlation_matrix)]
  lower_values <- lower_values[is.finite(lower_values)]

  if (length(lower_values) == 0L) {
    return(NA_real_)
  }

  item_count <- ncol(correlation_matrix)
  mean_inter_item_correlation <- mean(lower_values)

  (item_count * mean_inter_item_correlation) / (1 + (item_count - 1) * mean_inter_item_correlation)
}

compute_omega_total <- function(correlation_matrix, n_obs) {
  if (is.null(correlation_matrix) || ncol(correlation_matrix) < 3L || is.na(n_obs) || n_obs < 2L) {
    return(list(value = NA_real_, note = "omega_not_available"))
  }

  tryCatch(
    {
      omega_fit <- suppressWarnings(
        psych::omega(
          m = correlation_matrix,
          n.obs = n_obs,
          plot = FALSE,
          warnings = FALSE,
          fm = "minres"
        )
      )

      list(value = as.numeric(omega_fit$omega.tot), note = "omega_total_polychoric")
    },
    error = function(e) {
      list(value = NA_real_, note = paste("omega_error:", conditionMessage(e)))
    }
  )
}

compute_polychoric_matrix <- function(item_data) {
  valid_items <- names(item_data)[purrr::map_lgl(item_data, function(values) {
    non_missing <- values[!is.na(values)]
    length(non_missing) > 1L && dplyr::n_distinct(non_missing) > 1L
  })]

  if (length(valid_items) < 2L) {
    return(list(rho = NULL, item_names = valid_items, note = "fewer_than_two_variable_items"))
  }

  ordered_data <- item_data %>%
    dplyr::select(any_of(valid_items)) %>%
    dplyr::mutate(across(everything(), ~ ordered(.x, levels = sort(unique(.x[!is.na(.x)])))))

  tryCatch(
    {
      polychoric_fit <- suppressWarnings(psych::polychoric(ordered_data))
      list(
        rho = psych::cor.smooth(polychoric_fit$rho),
        item_names = valid_items,
        note = "polychoric"
      )
    },
    error = function(e) {
      list(rho = NULL, item_names = valid_items, note = paste("polychoric_error:", conditionMessage(e)))
    }
  )
}

compute_numeric_alpha <- function(item_data) {
  if (ncol(item_data) < 2L) {
    return(NA_real_)
  }

  tryCatch(
    {
      alpha_fit <- psych::alpha(item_data, check.keys = FALSE, warnings = FALSE)
      as.numeric(alpha_fit$total$raw_alpha)
    },
    error = function(e) NA_real_
  )
}

compute_corrected_item_rest <- function(item_data, item_name) {
  other_items <- setdiff(names(item_data), item_name)

  if (length(other_items) == 0L) {
    return(NA_real_)
  }

  other_matrix <- item_data[, other_items, drop = FALSE]
  other_counts <- rowSums(!is.na(other_matrix))
  other_mean <- rowMeans(other_matrix, na.rm = TRUE)
  other_mean[other_counts == 0L] <- NA_real_

  suppressWarnings(stats::cor(item_data[[item_name]], other_mean, use = "pairwise.complete.obs"))
}

compute_alpha_if_dropped <- function(item_data, item_name) {
  remaining_items <- setdiff(names(item_data), item_name)

  if (length(remaining_items) < 2L) {
    return(NA_real_)
  }

  polychoric_result <- compute_polychoric_matrix(item_data[, remaining_items, drop = FALSE])

  if (!is.null(polychoric_result$rho)) {
    return(compute_standardized_alpha(polychoric_result$rho))
  }

  compute_numeric_alpha(item_data[, remaining_items, drop = FALSE])
}

compute_reliability <- function(item_data) {
  complete_case_n <- sum(stats::complete.cases(item_data))
  polychoric_result <- compute_polychoric_matrix(item_data)

  if (!is.null(polychoric_result$rho)) {
    ordinal_alpha <- compute_standardized_alpha(polychoric_result$rho)
    omega_result <- compute_omega_total(polychoric_result$rho, complete_case_n)

    return(
      list(
        alpha = ordinal_alpha,
        omega_total = omega_result$value,
        reliability_method = ifelse(
          is.na(omega_result$value),
          "ordinal_alpha_polychoric_only",
          "ordinal_alpha_polychoric_and_omega_total_polychoric"
        ),
        reliability_note = paste(polychoric_result$note, omega_result$note, sep = " | "),
        complete_case_n = complete_case_n,
        item_names = polychoric_result$item_names
      )
    )
  }

  list(
    alpha = compute_numeric_alpha(item_data),
    omega_total = NA_real_,
    reliability_method = "numeric_alpha_fallback",
    reliability_note = polychoric_result$note,
    complete_case_n = complete_case_n,
    item_names = names(item_data)
  )
}

index_labels <- tribble(
  ~index_id, ~index_label, ~index_description, ~recommended_future_split,
  "section5_safety_anxiety_security_worry", "Section 5 Safety Anxiety And Security Worry", "Higher scores indicate more safety anxiety, security worry, and travel vigilance.", "A future split could separate general anxiety and destination/security vigilance from mode-specific confidence items that were excluded from the primary index.",
  "section6_core_safety_tradeoffs", "Section 6 Core Safety Tradeoffs", "Higher scores indicate that safety concerns more strongly reshape travel choices, costs, and access.", "A future gated sub-index could evaluate the 6.05 to 6.16 battery after explicitly modeling its structural not-applicable responses.",
  "section7a_system_reform_support", "Section 7A Desire For More Enforcement", "Higher scores indicate greater desire for stronger enforcement and tighter speed management as safety responses.", "If a broader Section 7 summary is needed later, report this enforcement-oriented index separately from Section 7B rather than recombining them into a single composite.",
  "section7b_system_skepticism_anti_fatalism", "Section 7B Acceptance Of Existing System", "Higher scores indicate greater acceptance of the existing safety system, including fatalistic acceptance and confidence that following the rules is sufficient for safety.", "The dropped safe_clothes item can be revisited as a stand-alone anti-victim-blaming indicator, but it should not be folded into this index without stronger coherence evidence."
) %>%
  mutate(
    raw_score_var = paste0(index_id, "_raw"),
    z_score_var = paste0(index_id, "_z")
  )

item_spec <- tribble(
  ~index_id, ~variable, ~inclusion_status, ~direction_rule, ~inclusion_reason, ~direction_rule_source,
  "section5_safety_anxiety_security_worry", "traffic_worry", "include", "keep", "Core direct item about crash worry while traveling.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "harrassment", "include", "reverse", "Personal security item is central after reversing so higher values mean less felt safety from harassment.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "urgent_worry", "include", "keep", "Captures safety-related worry about urgent access.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "walk_recreation_relax", "exclude", "reverse", "Positive walking affect is adjacent to travel comfort but not a direct anxiety or security-worry indicator.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "travel_anxiety", "include", "keep", "Direct anticipatory safety-anxiety item.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "overall_safety", "include", "reverse", "Global day-to-day safety item is central after reversing so higher values mean less felt safety.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "travel_worry", "include", "keep", "Captures worry about companions' safety while traveling.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "drive_safely", "exclude", "reverse", "Neighborhood driver-behavior perception is broader than respondent safety anxiety and security worry.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "bike_comfort_cars", "exclude", "reverse", "Mode-specific bicycle comfort is not universally applicable for a primary all-respondent anxiety index.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "alert_traveler", "include", "keep", "Vigilance while traveling alone fits the security-worry construct.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "drive_worry", "exclude", "keep", "Drivers-only item; excluded from the primary all-respondent index to avoid structural not-applicability.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "signal_confidence", "exclude", "reverse", "Crossing-signal confidence is situational confidence rather than general anxiety or security worry.", "factor_analysis_convention",
  "section5_safety_anxiety_security_worry", "worry_destination", "include", "keep", "Direct destination-safety worry item.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "safety_choices", "include", "keep", "Core general item about safety shaping travel choices.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "safety_choices_costs", "include", "keep", "Core tradeoff item about higher transportation costs from safety-related choices.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "safety_choices_modes", "include", "keep", "Core tradeoff item about avoiding modes because of safety concerns.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "worry_security", "include", "keep", "Core access-constraint item about personal-security worries.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "shortest_path_fear", "exclude", "keep", "Structurally gated shortest-path battery item with a not-applicable category; keep out of the primary all-respondent composite.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "shortest_path_assault", "exclude", "keep", "Structurally gated shortest-path battery item with a not-applicable category; keep out of the primary all-respondent composite.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "traffic_crash_worry", "exclude", "keep", "Context-specific otherwise stem with structural not-applicable responses; reserve for a gated future sub-index.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "traffic_harrassment", "exclude", "keep", "Context-specific otherwise stem with structural not-applicable responses; reserve for a gated future sub-index.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "no_safe_travel", "exclude", "keep", "Context-specific item with structural not-applicable responses; reserve for a gated future sub-index.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "no_harm_travel", "exclude", "keep", "Context-specific item with structural not-applicable responses; reserve for a gated future sub-index.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "feel_safe", "exclude", "keep", "Context-specific otherwise item with structural not-applicable responses and overlapping wording.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "fear_assault", "exclude", "keep", "Context-specific otherwise item with structural not-applicable responses and overlapping wording.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "feel_safe_crash", "exclude", "keep", "Overlapping gated item with structural not-applicable responses.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "fear_assault_2", "exclude", "keep", "Overlapping gated item with structural not-applicable responses.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "avoid_walk_1", "exclude", "keep", "Gated item with structural not-applicable responses and inconsistent ordinal coding.", "factor_analysis_convention",
  "section6_core_safety_tradeoffs", "avoid_walk_2", "exclude", "keep", "Gated item with structural not-applicable responses and inconsistent ordinal coding.", "factor_analysis_convention",
  "section7a_system_reform_support", "traffic_enforcement", "include", "keep", "Agreement indicates more desire for stronger traffic-law enforcement.", "factor_analysis_convention",
  "section7a_system_reform_support", "speed_limits", "include", "keep", "Agreement indicates more desire for stronger speed management through lower limits.", "factor_analysis_convention",
  "section7a_system_reform_support", "disagree_test", "exclude", "keep", "Instructional attention check item; exclude from substantive scoring.", "factor_analysis_convention",
  "section7b_system_skepticism_anti_fatalism", "system_safe", "include", "keep", "Agreement indicates more acceptance that the current transportation system is already generally safe.", "section_index_construct_alignment",
  "section7b_system_skepticism_anti_fatalism", "safe_clothes", "exclude", "reverse", "Drop from the scored indices because it behaved weakly and reads more as a separate anti-victim-blaming item than as part of a coherent Section 7A or 7B composite.", "section_index_construct_alignment",
  "section7b_system_skepticism_anti_fatalism", "traffic_deaths", "include", "keep", "Agreement indicates more fatalistic acceptance that traffic deaths are an inevitable part of travel.", "section_index_construct_alignment",
  "section7b_system_skepticism_anti_fatalism", "traffic_rules", "include", "keep", "Agreement indicates more acceptance that following the rules is sufficient for safety.", "section_index_construct_alignment"
)

variable_descriptions <- read_csv(file.path(project_root, "data", "variable_descriptions.csv"), show_col_types = FALSE)
codebook <- read_csv(file.path(project_root, "data", "value_labels.csv"), show_col_types = FALSE)

if ("question_id" %in% names(variable_descriptions) && !"q_id" %in% names(variable_descriptions)) {
  variable_descriptions <- dplyr::rename(variable_descriptions, q_id = question_id)
}

expected_section_variables <- variable_descriptions %>%
  dplyr::filter(stringr::str_detect(q_id, "^(5|6|7)\\.")) %>%
  dplyr::select(variable, q_id)

missing_spec_variables <- setdiff(expected_section_variables$variable, item_spec$variable)
extra_spec_variables <- setdiff(item_spec$variable, expected_section_variables$variable)

if (length(missing_spec_variables) > 0L || length(extra_spec_variables) > 0L) {
  stop(
    paste(
      "Section item specification does not match variable_descriptions.csv.",
      if (length(missing_spec_variables) > 0L) paste("Missing:", paste(missing_spec_variables, collapse = ", ")) else NULL,
      if (length(extra_spec_variables) > 0L) paste("Extra:", paste(extra_spec_variables, collapse = ", ")) else NULL
    )
  )
}

safety_responses <- load_safety_responses(project_root, source_view = source_view) %>%
  normalize_safety_source() %>%
  filter_completed_responses() %>%
  dplyr::select(-any_of(c(
    "ip_lon", "ip_lat", "work_loc_1", "work_loc_2", "work_loc_3", "work_loc_4", "work_loc_5",
    "home_lon", "home_lat", "work_lat", "work_lon", "work_loc_flag",
    "home_loc_1", "home_loc_2", "home_loc_3", "home_loc_4", "home_loc_5", "home_loc_flag",
    "invite_lat", "invite_lon"
  ))) %>%
  derive_demographic_flags()

candidate_variables <- unique(item_spec$variable)
missing_source_variables <- setdiff(candidate_variables, names(safety_responses))

if (length(missing_source_variables) > 0L) {
  stop(sprintf("Missing candidate section-index variables in source data: %s", paste(missing_source_variables, collapse = ", ")))
}

section_index_data <- safety_responses %>%
  dplyr::select(any_of(c(
    "person_id", "person_weight", "sample_segment", "disability_person",
    "age65plus", "low_income", "race_poc", "zero_vehicles",
    candidate_variables
  )))

for (candidate_variable in candidate_variables) {
  direction_rule <- item_spec %>%
    dplyr::filter(variable == candidate_variable) %>%
    dplyr::slice(1) %>%
    dplyr::pull(direction_rule)

  section_index_data[[candidate_variable]] <- apply_direction_rule(
    section_index_data[[candidate_variable]],
    candidate_variable,
    direction_rule,
    codebook
  )
}

scored_index_results <- purrr::map(index_labels$index_id, function(current_index_id) {
  current_label <- index_labels %>% dplyr::filter(index_id == current_index_id)
  current_spec <- item_spec %>% dplyr::filter(index_id == current_index_id)
  included_items <- current_spec %>% dplyr::filter(inclusion_status == "include") %>% dplyr::pull(variable)
  min_answered_required <- ceiling(length(included_items) / 2)
  item_data <- section_index_data %>% dplyr::select(any_of(included_items))
  answered_item_count <- rowSums(!is.na(item_data))
  raw_score <- ifelse(
    answered_item_count >= min_answered_required,
    rowMeans(item_data, na.rm = TRUE),
    NA_real_
  )
  raw_score[is.nan(raw_score)] <- NA_real_

  raw_mean <- mean(raw_score, na.rm = TRUE)
  raw_sd <- stats::sd(raw_score, na.rm = TRUE)
  z_score <- if (is.finite(raw_sd) && raw_sd > 0) {
    (raw_score - raw_mean) / raw_sd
  } else {
    rep(NA_real_, length(raw_score))
  }

  reliability_result <- compute_reliability(item_data)

  item_diagnostics <- purrr::map_dfr(included_items, function(item_name) {
    tibble(
      index_id = current_index_id,
      index_label = current_label$index_label,
      variable = item_name,
      corrected_item_rest = compute_corrected_item_rest(item_data, item_name),
      alpha_if_dropped = compute_alpha_if_dropped(item_data, item_name),
      item_missing_n = sum(is.na(item_data[[item_name]])),
      item_missing_share = mean(is.na(item_data[[item_name]])),
      item_mean = mean(item_data[[item_name]], na.rm = TRUE),
      item_sd = stats::sd(item_data[[item_name]], na.rm = TRUE),
      unique_nonmissing_values = dplyr::n_distinct(item_data[[item_name]][!is.na(item_data[[item_name]])]),
      reverse_coded = current_spec %>% dplyr::filter(variable == item_name) %>% dplyr::pull(direction_rule) == "reverse"
    )
  }) %>%
    mutate(
      item_fit_band = dplyr::case_when(
        is.na(corrected_item_rest) ~ NA_character_,
        corrected_item_rest < 0.10 ~ "very weak",
        corrected_item_rest < 0.20 ~ "weak",
        corrected_item_rest < 0.30 ~ "modest",
        TRUE ~ "stronger"
      )
    )

  score_summary <- tibble(
    index_id = current_index_id,
    index_label = current_label$index_label,
    index_description = current_label$index_description,
    included_item_count = length(included_items),
    min_answered_required = min_answered_required,
    completion_rule = paste0("Respondent score requires at least ", min_answered_required, " answered items out of ", length(included_items), " included items (ceiling of half the included items)."),
    score_nonmissing_n = sum(!is.na(raw_score)),
    score_missing_n = sum(is.na(raw_score)),
    score_missing_share = mean(is.na(raw_score)),
    mean_answered_items = mean(answered_item_count),
    raw_mean = raw_mean,
    raw_sd = raw_sd,
    raw_min = min(raw_score, na.rm = TRUE),
    raw_p25 = safe_quantile(raw_score, 0.25),
    raw_median = safe_quantile(raw_score, 0.50),
    raw_p75 = safe_quantile(raw_score, 0.75),
    raw_max = max(raw_score, na.rm = TRUE),
    z_mean = mean(z_score, na.rm = TRUE),
    z_sd = stats::sd(z_score, na.rm = TRUE),
    z_min = min(z_score, na.rm = TRUE),
    z_p25 = safe_quantile(z_score, 0.25),
    z_median = safe_quantile(z_score, 0.50),
    z_p75 = safe_quantile(z_score, 0.75),
    z_max = max(z_score, na.rm = TRUE)
  ) %>%
    mutate(across(c(raw_min, raw_max, z_mean, z_sd, z_min, z_max), ~ ifelse(is.infinite(.x), NA_real_, .x)))

  reliability_summary <- tibble(
    index_id = current_index_id,
    index_label = current_label$index_label,
    ordinal_alpha = reliability_result$alpha,
    omega_total = reliability_result$omega_total,
    reliability_method = reliability_result$reliability_method,
    reliability_note = reliability_result$reliability_note,
    reliability_complete_case_n = reliability_result$complete_case_n,
    coherence_assessment = dplyr::case_when(
      is.na(reliability_result$alpha) ~ "not_estimated",
      reliability_result$alpha >= 0.80 ~ "strong",
      reliability_result$alpha >= 0.70 ~ "adequate",
      reliability_result$alpha >= 0.50 ~ "limited",
      TRUE ~ "weak"
    ),
    problematic_item_count = sum(item_diagnostics$corrected_item_rest < 0.15, na.rm = TRUE),
    weakest_item = item_diagnostics %>%
      dplyr::arrange(corrected_item_rest, variable) %>%
      dplyr::slice(1) %>%
      dplyr::pull(variable),
    weakest_item_rest = item_diagnostics %>%
      dplyr::arrange(corrected_item_rest, variable) %>%
      dplyr::slice(1) %>%
      dplyr::pull(corrected_item_rest),
    best_drop_one_item = item_diagnostics %>%
      dplyr::arrange(desc(alpha_if_dropped), variable) %>%
      dplyr::slice(1) %>%
      dplyr::pull(variable),
    best_drop_one_alpha = item_diagnostics %>%
      dplyr::arrange(desc(alpha_if_dropped), variable) %>%
      dplyr::slice(1) %>%
      dplyr::pull(alpha_if_dropped),
    trimming_performed = "No",
    trimming_note = dplyr::case_when(
      is.na(reliability_result$alpha) ~ "Internal consistency could not be estimated cleanly.",
      reliability_result$alpha < 0.50 ~ "No one-item trim yields an acceptable composite; retain the required primary section index, report weak coherence, and use the recommended future split note.",
      TRUE ~ "Primary section indices retain the theory-selected items; excluded candidates are documented separately."
    )
  )

  score_columns <- tibble(
    person_id = section_index_data$person_id,
    person_weight = section_index_data$person_weight,
    sample_segment = section_index_data$sample_segment,
    disability_person = section_index_data$disability_person,
    age65plus = section_index_data$age65plus,
    low_income = section_index_data$low_income,
    race_poc = section_index_data$race_poc,
    zero_vehicles = section_index_data$zero_vehicles
  ) %>%
    mutate(
      !!current_label$raw_score_var := raw_score,
      !!current_label$z_score_var := z_score,
      !!paste0(current_index_id, "_answered_items") := answered_item_count,
      !!paste0(current_index_id, "_min_answered_required") := min_answered_required
    )

  list(
    score_columns = score_columns,
    score_summary = score_summary,
    reliability_summary = reliability_summary,
    item_diagnostics = item_diagnostics
  )
})

score_base <- scored_index_results[[1]]$score_columns %>%
  dplyr::select(person_id, person_weight, sample_segment, disability_person, age65plus, low_income, race_poc, zero_vehicles)

section_index_scores <- purrr::reduce(
  purrr::map(scored_index_results, ~ .x$score_columns %>% dplyr::select(-person_weight, -sample_segment, -disability_person, -age65plus, -low_income, -race_poc, -zero_vehicles)),
  dplyr::left_join,
  by = "person_id"
) %>%
  dplyr::right_join(score_base, by = "person_id") %>%
  dplyr::relocate(person_id, person_weight, sample_segment, disability_person, age65plus, low_income, race_poc, zero_vehicles)

score_summary <- purrr::list_rbind(purrr::map(scored_index_results, "score_summary"))
reliability_summary <- purrr::list_rbind(purrr::map(scored_index_results, "reliability_summary"))
item_diagnostics <- purrr::list_rbind(purrr::map(scored_index_results, "item_diagnostics")) %>%
  left_join(variable_descriptions, by = "variable")

index_thresholds <- score_summary %>%
  dplyr::select(index_id, included_item_count, min_answered_required, completion_rule)

section_index_item_membership <- item_spec %>%
  left_join(variable_descriptions, by = "variable") %>%
  left_join(index_labels, by = "index_id") %>%
  left_join(index_thresholds, by = "index_id") %>%
  mutate(
    reverse_coded = direction_rule == "reverse",
    included_in_primary_index = inclusion_status == "include"
  ) %>%
  dplyr::select(
    index_id,
    index_label,
    variable,
    q_id,
    description,
    inclusion_status,
    included_in_primary_index,
    reverse_coded,
    direction_rule,
    direction_rule_source,
    inclusion_reason,
    included_item_count,
    min_answered_required,
    completion_rule
  )

section_index_notes <- index_labels %>%
  transmute(index_id, index_label, note_type = "recommended_future_split", note = recommended_future_split) %>%
  bind_rows(
    score_summary %>%
      transmute(index_id, index_label, note_type = "completion_rule", note = completion_rule)
  ) %>%
  bind_rows(
    reliability_summary %>%
      transmute(index_id, index_label, note_type = "reliability_method", note = paste(reliability_method, reliability_note, sep = " | "))
  ) %>%
  bind_rows(
    reliability_summary %>%
      transmute(index_id, index_label, note_type = "coherence_assessment", note = paste(coherence_assessment, trimming_note, sep = " | "))
  )

labels_path <- safe_write_csv(index_labels, file.path(output_dir, "section_index_labels.csv"))
membership_path <- safe_write_csv(section_index_item_membership, file.path(output_dir, "section_index_item_membership.csv"))
notes_path <- safe_write_csv(section_index_notes, file.path(output_dir, "section_index_notes.csv"))
score_summary_path <- safe_write_csv(score_summary, file.path(output_dir, "section_index_score_summary.csv"))
item_diagnostics_path <- safe_write_csv(item_diagnostics, file.path(output_dir, "section_index_item_diagnostics.csv"))
reliability_path <- safe_write_csv(reliability_summary, file.path(output_dir, "section_index_reliability_summary.csv"))
scores_csv_path <- safe_write_csv(section_index_scores, file.path(output_dir, "section_index_scores.csv"))
scores_rds_path <- safe_write_rds(section_index_scores, file.path(output_dir, "section_index_scores.rds"))

message("Wrote section index labels to: ", labels_path)
message("Wrote section index item membership to: ", membership_path)
message("Wrote section index notes to: ", notes_path)
message("Wrote section index score summary to: ", score_summary_path)
message("Wrote section index item diagnostics to: ", item_diagnostics_path)
message("Wrote section index reliability summary to: ", reliability_path)
message("Wrote section index scores to: ", scores_csv_path)
message("Wrote section index scores RDS to: ", scores_rds_path)

print(
  score_summary %>%
    left_join(reliability_summary, by = c("index_id", "index_label")) %>%
    dplyr::select(
      index_id,
      index_label,
      included_item_count,
      min_answered_required,
      score_nonmissing_n,
      score_missing_share,
      ordinal_alpha,
      omega_total,
      coherence_assessment,
      weakest_item,
      weakest_item_rest
    )
)
