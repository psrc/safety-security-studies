library(tidyverse)
library(psych)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
read_rds <- readr::read_rds
write_csv <- readr::write_csv
as_tibble <- tibble::as_tibble

parse_solution_sizes <- function(args) {
  parsed_sizes <- suppressWarnings(as.integer(args))
  parsed_sizes <- parsed_sizes[!is.na(parsed_sizes) & parsed_sizes > 0]

  if (length(parsed_sizes) == 0L) {
    return(c(4L, 5L, 9L))
  }

  sort(unique(parsed_sizes))
}

script_path <- commandArgs(trailingOnly = FALSE) %>%
  stringr::str_subset("^--file=") %>%
  stringr::str_remove("^--file=")

solution_sizes <- commandArgs(trailingOnly = TRUE) %>%
  parse_solution_sizes()

output_suffix <- paste(solution_sizes, collapse = "_")

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

selection_table <- read_csv(file.path(output_dir, "curated_core_selection.csv"), show_col_types = FALSE)
prepared_core_data <- read_rds(file.path(output_dir, "core_model_data.rds"))

selected_variables <- selection_table %>%
  dplyr::filter(selection_status == "keep") %>%
  dplyr::pull(variable)

model_data <- prepared_core_data %>%
  dplyr::filter(complete_case_core) %>%
  dplyr::select(any_of(selected_variables))

if (nrow(model_data) == 0) {
  stop("No complete-case rows available for factor solution comparison.")
}

ordered_factor_data <- model_data %>%
  dplyr::mutate(across(everything(), ~ ordered(.x, levels = sort(unique(.x)))))

polychoric_matrix <- psych::polychoric(ordered_factor_data)
smoothed_rho <- psych::cor.smooth(polychoric_matrix$rho)

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

run_factor_model <- function(factor_count) {
  captured_warnings <- character()

  fitted_model <- withCallingHandlers(
    psych::fa(
      r = smoothed_rho,
      nfactors = factor_count,
      n.obs = nrow(model_data),
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

solution_results <- purrr::map(solution_sizes, run_factor_model)
names(solution_results) <- as.character(solution_sizes)

comparison_summary <- purrr::map_dfr(solution_sizes, function(factor_count) {
  fitted_result <- solution_results[[as.character(factor_count)]]
  fitted_model <- fitted_result$model
  fit_warning <- if (length(fitted_result$warnings) == 0L) {
    NA_character_
  } else {
    paste(fitted_result$warnings, collapse = " | ")
  }

  tibble(
    factor_count = factor_count,
    n_obs = nrow(model_data),
    item_count = ncol(model_data),
    bic = fitted_model$BIC,
    rmsea = fitted_model$RMSEA[[1]],
    tli = fitted_model$TLI,
    rmsr = fitted_model$rms,
    fit_warning = fit_warning
  )
}) %>%
  arrange(factor_count)

comparison_top_loadings <- purrr::map_dfr(solution_sizes, function(factor_count) {
  fitted_result <- solution_results[[as.character(factor_count)]]
  fitted_model <- fitted_result$model
  fit_warning <- if (length(fitted_result$warnings) == 0L) {
    NA_character_
  } else {
    paste(fitted_result$warnings, collapse = " | ")
  }

  loading_matrix <- orient_loading_matrix(unclass(fitted_model$loadings))
  rownames(loading_matrix) <- colnames(model_data)

  as.data.frame(loading_matrix) %>%
    tibble::rownames_to_column("variable") %>%
    as_tibble() %>%
    pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
    mutate(
      factor_count = factor_count,
      abs_loading = abs(loading),
      fit_warning = fit_warning
    ) %>%
    group_by(factor_count, factor) %>%
    slice_max(order_by = abs_loading, n = 5, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(factor_count, factor, desc(abs_loading))
})

comparison_output_path <- file.path(output_dir, paste0("factor_solution_comparison_", output_suffix, ".csv"))
top_loadings_output_path <- file.path(output_dir, paste0("factor_solution_top_loadings_", output_suffix, ".csv"))

write_csv(comparison_summary, comparison_output_path)
write_csv(comparison_top_loadings, top_loadings_output_path)

message("Wrote factor solution comparison to: ", comparison_output_path)
message("Wrote factor solution top loadings to: ", top_loadings_output_path)
