library(tidyverse)
library(psych)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
read_rds <- readr::read_rds
write_csv <- readr::write_csv
tibble <- tibble::tibble
as_tibble <- tibble::as_tibble

script_path <- commandArgs(trailingOnly = FALSE) %>%
  stringr::str_subset("^--file=") %>%
  stringr::str_remove("^--file=")

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

output_dir <- file.path(script_dir, "outputs")

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

extract_factor_metric <- function(metric_object, factor_names) {
  factor_count <- length(factor_names)

  if (is.null(metric_object)) {
    return(stats::setNames(rep(NA_real_, factor_count), factor_names))
  }

  metric_matrix <- suppressWarnings(as.matrix(metric_object))

  metric_values <- if (length(dim(metric_matrix)) == 2L && all(dim(metric_matrix) == factor_count)) {
    diag(metric_matrix)
  } else if (length(metric_object) >= factor_count) {
    as.numeric(metric_object)[seq_len(factor_count)]
  } else {
    c(as.numeric(metric_object), rep(NA_real_, factor_count - length(metric_object)))
  }

  stats::setNames(as.numeric(metric_values), factor_names)
}

selection_table <- read_csv(file.path(output_dir, "curated_core_selection.csv"), show_col_types = FALSE)
prepared_core_data <- read_rds(file.path(output_dir, "core_model_data.rds"))
factor_summary <- read_csv(file.path(output_dir, "factor_model_summary.csv"), show_col_types = FALSE)
factor_labels <- read_csv(file.path(output_dir, "factor_dimension_labels.csv"), show_col_types = FALSE)

selected_variables <- selection_table %>%
  dplyr::filter(selection_status == "keep") %>%
  dplyr::pull(variable)

model_data <- prepared_core_data %>%
  dplyr::filter(complete_case_core) %>%
  dplyr::select(any_of(selected_variables))

if (nrow(model_data) == 0) {
  stop("No complete-case rows available for factor measurement diagnostics.")
}

best_factor_count <- factor_summary %>%
  dplyr::slice_min(bic, n = 1, with_ties = FALSE) %>%
  dplyr::pull(model_size)

ordered_factor_data <- model_data %>%
  dplyr::mutate(across(everything(), ~ ordered(.x, levels = sort(unique(.x)))))

polychoric_matrix <- psych::polychoric(ordered_factor_data)
smoothed_rho <- psych::cor.smooth(polychoric_matrix$rho)

best_factor_model <- psych::fa(
  r = smoothed_rho,
  nfactors = best_factor_count,
  n.obs = nrow(model_data),
  fm = "minres",
  rotate = "oblimin"
)

best_factor_model_for_scores <- psych::fa(
  model_data,
  nfactors = best_factor_count,
  fm = "minres",
  rotate = "oblimin",
  cor = "poly"
)

loadings_matrix <- unclass(best_factor_model$loadings)
rownames(loadings_matrix) <- colnames(model_data)
factor_names <- colnames(loadings_matrix)

abs_loadings_long <- as.data.frame(loadings_matrix) %>%
  tibble::rownames_to_column("variable") %>%
  as_tibble() %>%
  pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
  mutate(abs_loading = abs(loading))

variable_loading_summary <- abs_loadings_long %>%
  arrange(variable, desc(abs_loading), factor) %>%
  group_by(variable) %>%
  summarise(
    primary_factor = dplyr::nth(factor, 1),
    primary_loading = dplyr::nth(loading, 1),
    primary_abs_loading = dplyr::nth(abs_loading, 1),
    secondary_factor = dplyr::nth(factor, 2),
    secondary_abs_loading = dplyr::nth(abs_loading, 2),
    loading_gap = primary_abs_loading - secondary_abs_loading,
    .groups = "drop"
  )

factor_strength_summary <- abs_loadings_long %>%
  group_by(factor) %>%
  summarise(
    item_count = n(),
    salient_items_30 = sum(abs_loading >= 0.30, na.rm = TRUE),
    salient_items_40 = sum(abs_loading >= 0.40, na.rm = TRUE),
    top_abs_loading = max(abs_loading, na.rm = TRUE),
    mean_abs_loading_all_items = mean(abs_loading, na.rm = TRUE),
    ss_loading = sum(loading^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prop_common_variance = ss_loading / sum(ss_loading, na.rm = TRUE))

primary_item_summary <- variable_loading_summary %>%
  group_by(primary_factor) %>%
  summarise(
    primary_item_count = dplyr::n(),
    mean_primary_abs_loading = mean(primary_abs_loading, na.rm = TRUE),
    median_primary_abs_loading = stats::median(primary_abs_loading, na.rm = TRUE),
    mean_secondary_abs_loading = mean(secondary_abs_loading, na.rm = TRUE),
    mean_loading_gap = mean(loading_gap, na.rm = TRUE),
    min_loading_gap = min(loading_gap, na.rm = TRUE),
    items_with_gap_20 = sum(loading_gap >= 0.20, na.rm = TRUE),
    items_with_gap_10 = sum(loading_gap >= 0.10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(factor = primary_factor)

score_validity <- extract_factor_metric(best_factor_model_for_scores$valid, factor_names)
score_multiple_r2 <- extract_factor_metric(best_factor_model_for_scores$R2.scores, factor_names)

factor_measurement_summary <- tibble(
  factor = factor_names,
  score_validity = unname(score_validity[factor_names]),
  score_multiple_r2 = unname(score_multiple_r2[factor_names])
) %>%
  left_join(factor_labels, by = "factor") %>%
  left_join(factor_strength_summary, by = "factor") %>%
  left_join(primary_item_summary, by = "factor") %>%
  mutate(
    rank_ss_loading = min_rank(desc(ss_loading)),
    rank_primary_loading = min_rank(desc(mean_primary_abs_loading)),
    rank_loading_gap = min_rank(desc(mean_loading_gap)),
    rank_score_validity = min_rank(desc(score_validity)),
    average_rank = rowMeans(across(c(rank_ss_loading, rank_primary_loading, rank_loading_gap, rank_score_validity)), na.rm = TRUE),
    measurement_cleanliness_rank = min_rank(average_rank)
  ) %>%
  arrange(measurement_cleanliness_rank, desc(score_validity), desc(mean_primary_abs_loading))

factor_measurement_notes <- tibble(
  note = c(
    "This summary ranks factors using a simple average of ranks for factor strength, primary loading strength, simple-structure separation, and score validity.",
    "Higher score_validity and higher score_multiple_r2 indicate better recovery of the latent factor in the estimated scores.",
    "Higher mean_loading_gap indicates cleaner separation between an item's primary factor and its next-strongest factor.",
    "This is a diagnostic ranking for interpretation support, not a formal hypothesis test."
  )
)

factor_item_assignment_path <- safe_write_csv(
  variable_loading_summary %>% arrange(desc(primary_abs_loading), desc(loading_gap)),
  file.path(output_dir, "factor_measurement_item_assignments.csv")
)

factor_measurement_summary_path <- safe_write_csv(
  factor_measurement_summary,
  file.path(output_dir, "factor_measurement_summary.csv")
)

factor_measurement_notes_path <- safe_write_csv(
  factor_measurement_notes,
  file.path(output_dir, "factor_measurement_notes.csv")
)

message("Wrote factor measurement summary to: ", factor_measurement_summary_path)
message("Wrote factor item assignments to: ", factor_item_assignment_path)
message("Wrote factor measurement notes to: ", factor_measurement_notes_path)
print(
  factor_measurement_summary %>%
    dplyr::select(
      measurement_cleanliness_rank,
      factor,
      factor_label,
      score_validity,
      score_multiple_r2,
      ss_loading,
      mean_primary_abs_loading,
      mean_loading_gap,
      primary_item_count
    )
)