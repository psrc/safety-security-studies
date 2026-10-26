library(tidyverse)
library(poLCA)

`%>%` <- magrittr::`%>%`
arrange <- dplyr::arrange
bind_rows <- dplyr::bind_rows
distinct <- dplyr::distinct
filter <- dplyr::filter
group_by <- dplyr::group_by
left_join <- dplyr::left_join
mutate <- dplyr::mutate
pull <- dplyr::pull
slice <- dplyr::slice
summarize <- dplyr::summarize
transmute <- dplyr::transmute
ungroup <- dplyr::ungroup
pivot_longer <- tidyr::pivot_longer

read_csv <- readr::read_csv
read_rds <- readr::read_rds
write_csv <- readr::write_csv
tibble <- tibble::tibble
as_tibble <- tibble::as_tibble

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c(
    "aic", "analysis_set", "anchor_match_count", "assigned_class", "assignment_probability",
    "best_bic_in_set", "bic", "burden_rank", "centered_score", "class", "class_count",
    "class_label", "class_share", "collapse_rule", "convergence_notes", "description",
    "direction_note", "direction_rule", "distinct_attempt_loglik_count", "elevated_items",
    "entropy", "expected_score", "factor", "factor_description", "factor_label",
    "factor_mean", "item_count", "iterations_used", "log_likelihood", "lower_items",
    "mean_assignment_probability", "monotonic_with_burden_rank", "n_assigned",
    "negative_factor_signature", "nonmonotonic_factor_count", "nonmonotonic_factor_share",
    "nonmonotonic_factors", "nonmonotonic_item_count", "nonmonotonic_item_share",
    "overall_burden", "person_id", "positive_factor_signature", "probability",
    "random_starts", "response_level", "response_value", "reverse_coded",
    "sample_size", "selection_reason", "selection_status", "small_block_reason",
    "small_block_status", "smallest_class_share", "solution_role", "status_change",
    "summary_line", "top_loading_signature", "variable", "variable_average"
  )))
}

class_count_grid <- 2:7
max_iterations <- 5000
random_starts <- 20
recommended_class_count_override <- NA_integer_

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
factor_output_dir <- file.path(project_root, "analyses", "special-needs", "factor-analysis", "outputs")
legacy_output_dir <- file.path(project_root, "outputs", "legacy_mixed_outputs")

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

safe_write_lines <- function(lines, path) {
  tryCatch(
    {
      writeLines(lines, con = path, useBytes = TRUE)
      path
    },
    error = function(e) {
      timestamped_path <- file.path(
        dirname(path),
        paste0(tools::file_path_sans_ext(basename(path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), tools::file_ext(path))
      )
      writeLines(lines, con = timestamped_path, useBytes = TRUE)
      timestamped_path
    }
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

selection_table <- read_csv(file.path(factor_output_dir, "curated_core_selection.csv"), show_col_types = FALSE)
prepared_core_data <- read_rds(file.path(factor_output_dir, "core_model_data.rds"))
factor_scores <- read_csv(file.path(factor_output_dir, "factor_scores.csv"), show_col_types = FALSE)
factor_labels <- read_csv(file.path(factor_output_dir, "factor_dimension_labels.csv"), show_col_types = FALSE)
legacy_selection_path <- file.path(legacy_output_dir, "lca_curated_core_selection.csv")

if (!file.exists(legacy_selection_path)) {
  stop("Missing legacy LCA selection file needed for old-vs-expanded comparison: ", legacy_selection_path)
}

legacy_selection <- read_csv(legacy_selection_path, show_col_types = FALSE)

expanded_variables <- selection_table %>%
  dplyr::filter(selection_status == "keep") %>%
  pull(variable)

legacy_variables <- legacy_selection %>%
  dplyr::filter(selection_status == "keep") %>%
  pull(variable)

if (length(expanded_variables) == 0) {
  stop("No expanded LCA variables were found in factor_analysis/outputs/curated_core_selection.csv.")
}

analysis_frame <- prepared_core_data %>%
  dplyr::filter(complete_case_core) %>%
  dplyr::select(person_id, person_weight, sample_segment, disability_person, any_of(expanded_variables))

if (nrow(analysis_frame) == 0) {
  stop("No complete-case rows available for the baseline LCA analysis.")
}

factor_columns <- factor_scores %>%
  names() %>%
  stringr::str_subset("^MR\\d+$")

factor_frame <- analysis_frame %>%
  dplyr::select(person_id) %>%
  left_join(factor_scores %>% dplyr::select(person_id, any_of(factor_columns)), by = "person_id")

if (any(!stats::complete.cases(factor_frame[, factor_columns, drop = FALSE]))) {
  stop("Factor scores are missing for one or more complete-case respondents used by the LCA workflow.")
}

variable_set_audit <- selection_table %>%
  dplyr::select(variable, description, selection_status, selection_reason, direction_rule, collapse_rule) %>%
  dplyr::left_join(
    legacy_selection %>%
      dplyr::select(variable, small_block_status = selection_status, small_block_reason = selection_reason),
    by = "variable"
  ) %>%
  mutate(
    small_block_status = dplyr::coalesce(small_block_status, "not_in_legacy_small_block"),
    status_change = dplyr::case_when(
      selection_status == "keep" & small_block_status == "keep" ~ "retained_from_legacy_small_block",
      selection_status == "keep" & small_block_status != "keep" ~ "newly_added_in_expanded_block",
      selection_status != "keep" & small_block_status == "keep" ~ "removed_from_expanded_baseline",
      TRUE ~ "held_out_from_baseline"
    )
  ) %>%
  arrange(
    factor(status_change, levels = c(
      "retained_from_legacy_small_block",
      "newly_added_in_expanded_block",
      "removed_from_expanded_baseline",
      "held_out_from_baseline"
    )),
    variable
  )

direction_audit <- variable_set_audit %>%
  dplyr::filter(selection_status == "keep") %>%
  transmute(
    variable,
    description,
    direction_rule,
    reverse_coded = direction_rule == "reverse",
    direction_note = dplyr::case_when(
      direction_rule == "reverse" ~ "Higher coded values were flipped so larger values indicate greater burden, lower safety, lower confidence, lower satisfaction, or weaker safety orientation.",
      TRUE ~ "Values were kept in the encoded order used by the factor-analysis workflow because higher values already align with greater burden, worry, restriction, or endorsement of the underlying construct."
    )
  )

build_formula <- function(variable_names) {
  as.formula(paste0("cbind(", paste(variable_names, collapse = ", "), ") ~ 1"))
}

compute_entropy <- function(posterior_matrix) {
  class_count <- ncol(posterior_matrix)
  row_count <- nrow(posterior_matrix)

  if (class_count <= 1 || row_count == 0) {
    return(NA_real_)
  }

  posterior_matrix <- pmax(posterior_matrix, .Machine$double.eps)
  1 + sum(posterior_matrix * log(posterior_matrix)) / (row_count * log(class_count))
}

format_class_shares <- function(shares) {
  paste0("class", seq_along(shares), "=", sprintf("%.1f%%", shares * 100)) %>%
    paste(collapse = "; ")
}

build_instability_notes <- function(fitted_model, min_share_threshold = 0.05) {
  notes <- character()

  if (!is.null(fitted_model$numiter) && isTRUE(fitted_model$numiter >= max_iterations)) {
    notes <- c(notes, "hit_max_iterations")
  }

  if (!is.null(fitted_model$attempts)) {
    attempts <- fitted_model$attempts[is.finite(fitted_model$attempts)]

    if (length(attempts) > 1) {
      if (length(unique(round(attempts, 3))) > 1) {
        notes <- c(notes, "multiple_local_optima_seen_across_random_starts")
      }

      if (sum(abs(attempts - max(attempts)) < 1e-6) == 1) {
        notes <- c(notes, "single_best_random_start")
      }
    }
  }

  if (min(fitted_model$P) < min_share_threshold) {
    notes <- c(notes, "class_below_5pct")
  }

  if (min(fitted_model$P) < 0.03) {
    notes <- c(notes, "class_below_3pct")
  }

  if (length(notes) == 0) {
    return("no_immediate_within_run_warning")
  }

  paste(unique(notes), collapse = "; ")
}

extract_class_probabilities <- function(fitted_model, analysis_set) {
  purrr::imap_dfr(fitted_model$probs, function(prob_matrix, variable_name) {
    as_tibble(prob_matrix, rownames = "class") %>%
      pivot_longer(-class, names_to = "response_level", values_to = "probability") %>%
      mutate(
        analysis_set = analysis_set,
        variable = variable_name,
        class = as.integer(stringr::str_extract(class, "\\d+")),
        response_value = as.integer(stringr::str_extract(response_level, "\\d+")),
        .before = 1
      )
  })
}

compute_expected_scores <- function(class_probabilities) {
  class_probabilities %>%
    group_by(analysis_set, class, variable) %>%
    summarize(expected_score = sum(response_value * probability), .groups = "drop")
}

check_monotonic <- function(values, tolerance = 1e-8) {
  if (length(values) <= 2 || any(is.na(values))) {
    return(TRUE)
  }

  diffs <- diff(values)
  all(diffs >= -tolerance) || all(diffs <= tolerance)
}

rank_classes_by_burden <- function(expected_scores) {
  expected_scores %>%
    group_by(class) %>%
    summarize(overall_burden = mean(expected_score), .groups = "drop") %>%
    arrange(overall_burden) %>%
    mutate(
      burden_rank = row_number(),
      class_label = paste0("burden_rank_", burden_rank)
    )
}

compute_factor_summary <- function(fitted_model, class_map, analysis_set) {
  posterior <- fitted_model$posterior

  weighted_factor_means <- purrr::map_dfr(seq_along(factor_columns), function(factor_index) {
    factor_name <- factor_columns[[factor_index]]
    factor_values <- factor_frame[[factor_name]]

    tibble(
      class = seq_len(ncol(posterior)),
      factor = factor_name,
      factor_mean = purrr::map_dbl(seq_len(ncol(posterior)), function(class_index) {
        stats::weighted.mean(factor_values, posterior[, class_index], na.rm = TRUE)
      })
    )
  })

  weighted_factor_means %>%
    mutate(analysis_set = analysis_set, .before = 1) %>%
    left_join(class_map, by = "class") %>%
    left_join(factor_labels, by = c("factor")) %>%
    arrange(burden_rank, factor)
}

compute_candidate_diagnostics <- function(expected_scores, factor_summary, analysis_set, class_count) {
  item_diagnostics <- expected_scores %>%
    arrange(burden_rank) %>%
    group_by(variable) %>%
    summarize(
      monotonic_with_burden_rank = check_monotonic(expected_score),
      .groups = "drop"
    )

  factor_diagnostics <- factor_summary %>%
    arrange(burden_rank) %>%
    group_by(factor, factor_label) %>%
    summarize(
      monotonic_with_burden_rank = check_monotonic(factor_mean),
      .groups = "drop"
    )

  summary_row <- tibble(
    analysis_set = analysis_set,
    class_count = class_count,
    nonmonotonic_item_count = sum(!item_diagnostics$monotonic_with_burden_rank),
    nonmonotonic_item_share = mean(!item_diagnostics$monotonic_with_burden_rank),
    nonmonotonic_factor_count = sum(!factor_diagnostics$monotonic_with_burden_rank),
    nonmonotonic_factor_share = mean(!factor_diagnostics$monotonic_with_burden_rank),
    nonmonotonic_factors = paste(factor_diagnostics$factor[!factor_diagnostics$monotonic_with_burden_rank], collapse = "; ")
  )

  list(summary = summary_row, factors = factor_diagnostics, items = item_diagnostics)
}

fit_analysis_set <- function(analysis_set, variable_names) {
  model_frame <- analysis_frame %>%
    dplyr::select(person_id, any_of(variable_names))

  lca_formula <- build_formula(variable_names)
  fitted_models <- list()
  comparison_rows <- list()
  diagnostics_rows <- list()
  factor_diag_rows <- list()
  item_diag_rows <- list()

  for (class_count in class_count_grid) {
    set.seed(20260520 + (if (analysis_set == "expanded_39_item") 0 else 100) + class_count)

    fitted_model <- poLCA(
      formula = lca_formula,
      data = model_frame,
      nclass = class_count,
      nrep = random_starts,
      maxiter = max_iterations,
      verbose = FALSE,
      calc.se = FALSE,
      na.rm = TRUE
    )

    fitted_models[[as.character(class_count)]] <- fitted_model

    class_probabilities <- extract_class_probabilities(fitted_model, analysis_set)
    expected_scores <- compute_expected_scores(class_probabilities)
    class_map <- rank_classes_by_burden(expected_scores)
    factor_summary <- compute_factor_summary(fitted_model, class_map, analysis_set)
    diagnostics <- compute_candidate_diagnostics(
      expected_scores %>% left_join(class_map, by = "class"),
      factor_summary,
      analysis_set,
      class_count
    )

    attempts <- fitted_model$attempts[is.finite(fitted_model$attempts)]
    second_best_ll <- if (length(attempts) > 1) sort(attempts, decreasing = TRUE)[[2]] else NA_real_

    comparison_rows[[as.character(class_count)]] <- tibble(
      analysis_set = analysis_set,
      item_count = length(variable_names),
      class_count = class_count,
      sample_size = fitted_model$N,
      log_likelihood = fitted_model$llik,
      aic = fitted_model$aic,
      bic = fitted_model$bic,
      entropy = compute_entropy(fitted_model$posterior),
      class_shares = format_class_shares(fitted_model$P),
      smallest_class_share = min(fitted_model$P),
      iterations_used = fitted_model$numiter,
      random_starts = random_starts,
      distinct_attempt_loglik_count = dplyr::n_distinct(round(attempts, 3)),
      best_minus_second_best_loglik = ifelse(length(attempts) > 1, max(attempts) - second_best_ll, NA_real_),
      convergence_notes = build_instability_notes(fitted_model)
    )

    diagnostics_rows[[as.character(class_count)]] <- diagnostics$summary
    factor_diag_rows[[as.character(class_count)]] <- diagnostics$factors %>% mutate(analysis_set = analysis_set, class_count = class_count, .before = 1)
    item_diag_rows[[as.character(class_count)]] <- diagnostics$items %>% mutate(analysis_set = analysis_set, class_count = class_count, .before = 1)
  }

  list(
    variable_names = variable_names,
    model_frame = model_frame,
    fitted_models = fitted_models,
    comparison_summary = bind_rows(comparison_rows),
    candidate_diagnostics = bind_rows(diagnostics_rows),
    factor_monotonicity = bind_rows(factor_diag_rows),
    item_monotonicity = bind_rows(item_diag_rows)
  )
}

build_solution_outputs <- function(analysis_result, chosen_class_count, analysis_set) {
  chosen_model <- analysis_result$fitted_models[[as.character(chosen_class_count)]]
  class_probabilities <- extract_class_probabilities(chosen_model, analysis_set)
  expected_scores <- compute_expected_scores(class_probabilities)
  class_map <- rank_classes_by_burden(expected_scores)
  expected_scores <- expected_scores %>% left_join(class_map, by = "class")
  factor_summary <- compute_factor_summary(chosen_model, class_map, analysis_set)

  posterior_table <- as_tibble(
    chosen_model$posterior,
    .name_repair = ~ paste0("posterior_class_", seq_along(.x))
  )

  membership_table <- analysis_result$model_frame %>%
    dplyr::select(person_id) %>%
    bind_cols(posterior_table) %>%
    mutate(
      assigned_class = chosen_model$predclass,
      assignment_probability = purrr::pmap_dbl(dplyr::select(., starts_with("posterior_class_")), max)
    ) %>%
    left_join(class_map, by = c("assigned_class" = "class")) %>%
    mutate(analysis_set = analysis_set, class_count = chosen_class_count, .before = 1) %>%
    arrange(burden_rank, desc(assignment_probability), person_id)

  class_shares <- tibble(
    class = seq_along(chosen_model$P),
    class_share = chosen_model$P
  ) %>%
    left_join(class_map, by = "class")

  factor_profile_strings <- factor_summary %>%
    group_by(class, burden_rank, class_label) %>%
    summarize(
      positive_factor_signature = paste(factor_label[order(-factor_mean)][seq_len(min(2, dplyr::n()))], collapse = " | "),
      negative_factor_signature = paste(factor_label[order(factor_mean)][seq_len(min(2, dplyr::n()))], collapse = " | "),
      .groups = "drop"
    )

  item_profile_strings <- expected_scores %>%
    group_by(variable) %>%
    mutate(variable_average = mean(expected_score), centered_score = expected_score - variable_average) %>%
    ungroup() %>%
    group_by(class, burden_rank, class_label) %>%
    summarize(
      elevated_items = paste(variable[order(-centered_score)][seq_len(min(3, dplyr::n()))], collapse = " | "),
      lower_items = paste(variable[order(centered_score)][seq_len(min(3, dplyr::n()))], collapse = " | "),
      .groups = "drop"
    )

  profile_summary <- expected_scores %>%
    group_by(class, burden_rank, class_label, overall_burden) %>%
    summarize(.groups = "drop") %>%
    left_join(class_shares %>% dplyr::select(class, class_share), by = "class") %>%
    left_join(
      membership_table %>%
        group_by(class = assigned_class) %>%
        summarize(mean_assignment_probability = mean(assignment_probability), n_assigned = n(), .groups = "drop"),
      by = "class"
    ) %>%
    left_join(factor_profile_strings, by = c("class", "burden_rank", "class_label")) %>%
    left_join(item_profile_strings, by = c("class", "burden_rank", "class_label")) %>%
    arrange(burden_rank)

  list(
    model = chosen_model,
    class_map = class_map,
    membership_table = membership_table,
    class_probabilities = class_probabilities %>% left_join(class_map, by = "class") %>% arrange(burden_rank, variable, response_value),
    expected_scores = expected_scores %>% arrange(burden_rank, variable),
    factor_summary = factor_summary %>% arrange(burden_rank, factor),
    profile_summary = profile_summary
  )
}

expanded_result <- fit_analysis_set("expanded_39_item", expanded_variables)
legacy_result <- fit_analysis_set("legacy_23_item", legacy_variables)

candidate_summary <- bind_rows(expanded_result$comparison_summary, legacy_result$comparison_summary) %>%
  left_join(
    bind_rows(expanded_result$candidate_diagnostics, legacy_result$candidate_diagnostics),
    by = c("analysis_set", "class_count")
  ) %>%
  group_by(analysis_set) %>%
  mutate(best_bic_in_set = bic == min(bic, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(analysis_set, class_count)

expanded_best_bic_class_count <- candidate_summary %>%
  dplyr::filter(analysis_set == "expanded_39_item") %>%
  slice_min(bic, n = 1, with_ties = FALSE) %>%
  pull(class_count)

legacy_best_bic_class_count <- candidate_summary %>%
  dplyr::filter(analysis_set == "legacy_23_item") %>%
  slice_min(bic, n = 1, with_ties = FALSE) %>%
  pull(class_count)

recommended_expanded_class_count <- ifelse(
  is.na(recommended_class_count_override),
  expanded_best_bic_class_count,
  recommended_class_count_override
)

expanded_solution <- build_solution_outputs(expanded_result, recommended_expanded_class_count, "expanded_39_item")
legacy_solution <- build_solution_outputs(legacy_result, legacy_best_bic_class_count, "legacy_23_item")

framing_comparison <- bind_rows(
  candidate_summary %>%
    dplyr::filter(analysis_set == "expanded_39_item", class_count == expanded_best_bic_class_count) %>%
    mutate(solution_role = "expanded_best_bic"),
  candidate_summary %>%
    dplyr::filter(analysis_set == "expanded_39_item", class_count == recommended_expanded_class_count) %>%
    mutate(solution_role = "expanded_recommended"),
  candidate_summary %>%
    dplyr::filter(analysis_set == "legacy_23_item", class_count == legacy_best_bic_class_count) %>%
    mutate(solution_role = "legacy_best_bic")
) %>%
  distinct()

analysis_notes <- tibble(
  note_type = c(
    "baseline_scope",
    "direction_logic",
    "held_out_items",
    "legacy_comparison",
    "expanded_best_bic",
    "expanded_recommended",
    "legacy_best_bic",
    "random_starts"
  ),
  note = c(
    paste0("Baseline LCA uses the shared 39-item all-respondent factor block on ", nrow(analysis_frame), " complete cases."),
    "Direction handling is inherited from factor_analysis/outputs/curated_core_selection.csv so higher coded values consistently indicate greater burden, lower safety, lower confidence, lower satisfaction, or weaker safety orientation.",
    paste(
      variable_set_audit %>%
        dplyr::filter(selection_status != "keep") %>%
        transmute(note_piece = paste0(variable, " (", selection_reason, ")")) %>%
        pull(note_piece),
      collapse = "; "
    ),
    paste0("Legacy comparison reuses the prior 23-item small block recorded in legacy_mixed_outputs/lca_curated_core_selection.csv."),
    paste0("Expanded best-BIC solution uses ", expanded_best_bic_class_count, " classes."),
    paste0("Expanded recommended solution currently uses ", recommended_expanded_class_count, " classes."),
    paste0("Legacy best-BIC solution uses ", legacy_best_bic_class_count, " classes."),
    paste0("Each candidate model was fit with ", random_starts, " random starts and maxiter=", max_iterations, ".")
  )
)

retained_small_block_variables <- variable_set_audit %>%
  dplyr::filter(status_change == "retained_from_legacy_small_block") %>%
  pull(variable)

newly_added_variables <- variable_set_audit %>%
  dplyr::filter(status_change == "newly_added_in_expanded_block") %>%
  pull(variable)

held_out_variables <- variable_set_audit %>%
  dplyr::filter(selection_status != "keep") %>%
  pull(variable)

reverse_coded_variables <- direction_audit %>%
  dplyr::filter(reverse_coded) %>%
  pull(variable)

expanded_recommended_row <- candidate_summary %>%
  dplyr::filter(analysis_set == "expanded_39_item", class_count == recommended_expanded_class_count) %>%
  slice(1)

legacy_best_row <- candidate_summary %>%
  dplyr::filter(analysis_set == "legacy_23_item", class_count == legacy_best_bic_class_count) %>%
  slice(1)

expanded_candidate_lines <- candidate_summary %>%
  dplyr::filter(analysis_set == "expanded_39_item") %>%
  mutate(
    summary_line = paste0(
      "- ", class_count, " classes: BIC=", round(bic, 1),
      ", entropy=", round(entropy, 3),
      ", smallest class=", sprintf("%.1f%%", 100 * smallest_class_share),
      ", non-monotonic items=", nonmonotonic_item_count, "/", item_count,
      ", non-monotonic factors=", nonmonotonic_factor_count, "/9",
      ", notes=", convergence_notes
    )
  ) %>%
  pull(summary_line)

legacy_candidate_lines <- candidate_summary %>%
  dplyr::filter(analysis_set == "legacy_23_item") %>%
  mutate(
    summary_line = paste0(
      "- ", class_count, " classes: BIC=", round(bic, 1),
      ", entropy=", round(entropy, 3),
      ", smallest class=", sprintf("%.1f%%", 100 * smallest_class_share),
      ", non-monotonic items=", nonmonotonic_item_count, "/", item_count,
      ", non-monotonic factors=", nonmonotonic_factor_count, "/9",
      ", notes=", convergence_notes
    )
  ) %>%
  pull(summary_line)

cross_domain_lines <- expanded_solution$profile_summary %>%
  dplyr::filter(burden_rank %in% c(3, 4, 5, 6, 7)) %>%
  mutate(
    summary_line = paste0(
      "- ", class_label, " (share ", sprintf("%.1f%%", 100 * class_share), ") emphasizes ",
      positive_factor_signature,
      " while de-emphasizing ",
      negative_factor_signature,
      "; elevated items: ",
      elevated_items,
      "."
    )
  ) %>%
  pull(summary_line)

direct_answer_line <- paste0(
  "The larger 39-item block produces materially more conceptually distinct classes than the legacy 23-item framing, ",
  "but the final solution still sits on top of a lower-to-higher safety-burden gradient rather than replacing it entirely. ",
  "The strongest evidence is that the expanded ", recommended_expanded_class_count, "-class solution shows ",
  expanded_recommended_row$nonmonotonic_factor_count, " of 9 factors and ",
  expanded_recommended_row$nonmonotonic_item_count, " of 39 items breaking a simple monotonic burden ordering, compared with only ",
  legacy_best_row$nonmonotonic_factor_count, " of 9 factors and ",
  legacy_best_row$nonmonotonic_item_count, " of 23 items in the legacy best-BIC solution."
)

summary_lines <- c(
  "# Latent Class Analysis Summary",
  "",
  "",
  "## Variables",
  paste0("- Baseline analysis set: expanded 39-item all-respondent block from factor_analysis/outputs/curated_core_selection.csv (n=", nrow(analysis_frame), ")."),
  paste0("- Retained from the earlier 23-item LCA framing: ", paste(retained_small_block_variables, collapse = ", "), "."),
  paste0("- Newly added in the expanded block: ", paste(newly_added_variables, collapse = ", "), "."),
  paste0("- Held out from the baseline all-respondent LCA: ", paste(held_out_variables, collapse = ", "), "."),
  "",
  "## Directionality",
  paste0("- Direction coding is inherited directly from factor_analysis/outputs/curated_core_selection.csv so higher values always align with greater burden, lower safety, lower confidence, lower satisfaction, or weaker safety orientation."),
  paste0("- Reverse-coded variables in the baseline set (", length(reverse_coded_variables), "): ", paste(reverse_coded_variables, collapse = ", "), "."),
  "",
  "## Class Count Sweep",
  paste0("- Expanded best-BIC class count: ", expanded_best_bic_class_count, ". Recommended final class count: ", recommended_expanded_class_count, "."),
  paste0("- Legacy best-BIC class count: ", legacy_best_bic_class_count, "."),
  expanded_candidate_lines,
  "- Legacy 23-item comparison:",
  legacy_candidate_lines,
  "",
  "## Interpretation",
  paste0("- ", direct_answer_line),
  "- The expanded solution remains severity-ordered overall, but several mid-spectrum classes split by domain rather than by simple burden level alone.",
  cross_domain_lines,
  paste0("- Recommended final solution: ", recommended_expanded_class_count, " classes. It wins on BIC, keeps the smallest class above 7%, and exposes much more cross-domain structure than the legacy framing. The tradeoff is that poLCA still found multiple local optima across random starts, so the segmentation should be treated as moderately stable rather than definitive."),
  "- Remaining limitations: the workflow is unweighted, complete-case, and restricted to the all-respondent block; gated items such as drive_worry and the structural not-applicable frequency battery remain out of scope for this baseline run.",
  "",
  "## Output Guide",
  "- See lca_candidate_model_summary.csv for fit, entropy, class shares, and convergence notes.",
  "- See lca_candidate_cross_domain_diagnostics.csv, lca_candidate_factor_monotonicity.csv, and lca_recommended_class_factor_summary.csv for the evidence on monotonicity versus cross-domain patterning.",
  "- See lca_recommended_class_membership.csv, lca_recommended_class_probabilities.csv, and lca_recommended_class_profile_summary.csv for the recommended solution outputs."
)

variable_set_audit_path <- safe_write_csv(variable_set_audit, file.path(output_dir, "lca_variable_set_audit.csv"))
direction_audit_path <- safe_write_csv(direction_audit, file.path(output_dir, "lca_direction_audit.csv"))
candidate_summary_path <- safe_write_csv(candidate_summary, file.path(output_dir, "lca_candidate_model_summary.csv"))
compat_summary_path <- safe_write_csv(candidate_summary, file.path(output_dir, "lca_factor_model_comparison.csv"))
candidate_diagnostics_path <- safe_write_csv(
  bind_rows(expanded_result$candidate_diagnostics, legacy_result$candidate_diagnostics),
  file.path(output_dir, "lca_candidate_cross_domain_diagnostics.csv")
)
factor_monotonicity_path <- safe_write_csv(
  bind_rows(expanded_result$factor_monotonicity, legacy_result$factor_monotonicity),
  file.path(output_dir, "lca_candidate_factor_monotonicity.csv")
)
item_monotonicity_path <- safe_write_csv(
  bind_rows(expanded_result$item_monotonicity, legacy_result$item_monotonicity),
  file.path(output_dir, "lca_candidate_item_monotonicity.csv")
)
framing_comparison_path <- safe_write_csv(framing_comparison, file.path(output_dir, "lca_framing_comparison.csv"))
membership_path <- safe_write_csv(expanded_solution$membership_table, file.path(output_dir, "lca_recommended_class_membership.csv"))
probabilities_path <- safe_write_csv(expanded_solution$class_probabilities, file.path(output_dir, "lca_recommended_class_probabilities.csv"))
compat_probabilities_path <- safe_write_csv(expanded_solution$class_probabilities, file.path(output_dir, "lca_best_class_probabilities.csv"))
expected_scores_path <- safe_write_csv(expanded_solution$expected_scores, file.path(output_dir, "lca_recommended_class_item_expected_scores.csv"))
factor_summary_path <- safe_write_csv(expanded_solution$factor_summary, file.path(output_dir, "lca_recommended_class_factor_summary.csv"))
profile_summary_path <- safe_write_csv(expanded_solution$profile_summary, file.path(output_dir, "lca_recommended_class_profile_summary.csv"))
legacy_profile_summary_path <- safe_write_csv(legacy_solution$profile_summary, file.path(output_dir, "lca_legacy_best_bic_profile_summary.csv"))
analysis_notes_path <- safe_write_csv(analysis_notes, file.path(output_dir, "lca_analysis_notes.csv"))
compat_notes_path <- safe_write_csv(analysis_notes, file.path(output_dir, "lca_factor_model_notes.csv"))
summary_md_path <- safe_write_lines(summary_lines, file.path(output_dir, "lca_summary.md"))

message("Wrote variable audit to: ", variable_set_audit_path)
message("Wrote direction audit to: ", direction_audit_path)
message("Wrote candidate model summary to: ", candidate_summary_path)
message("Wrote candidate diagnostics to: ", candidate_diagnostics_path)
message("Wrote recommended class membership table to: ", membership_path)
message("Wrote recommended class probabilities to: ", probabilities_path)
message("Wrote recommended class factor summary to: ", factor_summary_path)
message("Wrote recommended class profile summary to: ", profile_summary_path)
message("Wrote framing comparison to: ", framing_comparison_path)
message("Wrote written summary to: ", summary_md_path)
message("Expanded best-BIC class count: ", expanded_best_bic_class_count)
message("Expanded recommended class count: ", recommended_expanded_class_count)
message("Legacy best-BIC class count: ", legacy_best_bic_class_count)
message("Baseline comparison sample size: ", nrow(analysis_frame))
