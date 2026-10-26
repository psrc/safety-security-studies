library(tidyverse)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c(
    "direction_rule", "disability_person", "label", "person_weight",
    "primary_abs_loading", "primary_factor", "response", "response_pct",
    "response_share", "response_value", "value", "variable"
  )))
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

details_path <- file.path(project_root, "outputs", "disability_response_sss_significant_details.csv")
assignments_path <- file.path(output_dir, "factor_measurement_item_assignments.csv")
core_data_path <- file.path(output_dir, "core_model_data.csv")
selection_path <- file.path(output_dir, "curated_core_selection.csv")
value_labels_path <- file.path(project_root, "data", "value_labels.csv")

if (!file.exists(details_path)) {
  stop("Missing disability_response_sss_significant_details.csv in outputs/.")
}

if (!file.exists(assignments_path)) {
  stop(
    "Missing factor_measurement_item_assignments.csv in factor_analysis/outputs. Run factor_measurement_summary.R first."
  )
}

if (!requireNamespace("paletteer", quietly = TRUE)) {
  stop("Missing required package `paletteer`. Install it to use the PrettyCols::Beach palette.")
}

selected_items <- read_csv(assignments_path, show_col_types = FALSE) %>%
  filter(primary_factor %in% c("MR1", "MR4")) %>%
  arrange(primary_factor, desc(primary_abs_loading), variable) %>%
  distinct(variable, .keep_all = TRUE)

if (nrow(selected_items) == 0) {
  stop("No MR1 or MR4 variables were found in factor_measurement_item_assignments.csv.")
}

selected_variables <- selected_items %>%
  pull(variable)

detail_plot_data <- read_csv(details_path, show_col_types = FALSE) %>%
  filter(variable %in% selected_variables) %>%
  pivot_longer(
    cols = c(No, Yes),
    names_to = "disability_person",
    values_to = "response_pct"
  ) %>%
  mutate(
    response_share = readr::parse_number(response_pct) / 100
  ) %>%
  filter(!is.na(response_share), is.finite(response_share)) %>%
  group_by(variable, disability_person) %>%
  mutate(response_share = response_share / sum(response_share, na.rm = TRUE)) %>%
  ungroup()

missing_variables <- setdiff(selected_variables, unique(detail_plot_data$variable))

build_missing_plot_data <- function(variable_names) {
  if (length(variable_names) == 0) {
    return(tibble())
  }

  required_paths <- c(core_data_path, selection_path, value_labels_path)

  if (!all(file.exists(required_paths))) {
    stop(
      "Missing one or more supporting files needed to backfill non-significant MR1/MR4 items: core_model_data.csv, curated_core_selection.csv, or data/value_labels.csv."
    )
  }

  selection_metadata <- read_csv(selection_path, show_col_types = FALSE) %>%
    filter(variable %in% variable_names) %>%
    select(variable, direction_rule)

  label_lookup <- read_csv(value_labels_path, show_col_types = FALSE) %>%
    filter(variable %in% variable_names, !is.na(label), nzchar(label)) %>%
    mutate(value = suppressWarnings(as.integer(value))) %>%
    filter(!is.na(value)) %>%
    inner_join(selection_metadata, by = "variable") %>%
    group_by(variable) %>%
    mutate(
      response_value = if_else(
        direction_rule == "reverse",
        max(value, na.rm = TRUE) + min(value, na.rm = TRUE) - value,
        value
      )
    ) %>%
    ungroup() %>%
    select(variable, response_value, response = label)

  core_plot_data <- read_csv(core_data_path, show_col_types = FALSE) %>%
    filter(disability_person %in% c("Yes", "No")) %>%
    select(disability_person, person_weight, all_of(variable_names)) %>%
    pivot_longer(
      cols = all_of(variable_names),
      names_to = "variable",
      values_to = "response_value"
    ) %>%
    mutate(response_value = suppressWarnings(as.integer(response_value))) %>%
    filter(!is.na(response_value), is.finite(response_value), !is.na(person_weight), person_weight > 0) %>%
    inner_join(label_lookup, by = c("variable", "response_value")) %>%
    group_by(variable, disability_person, response) %>%
    summarise(weighted_count = sum(person_weight), .groups = "drop_last") %>%
    mutate(response_share = weighted_count / sum(weighted_count)) %>%
    ungroup() %>%
    select(variable, disability_person, response, response_share)

  core_plot_data
}

missing_plot_data <- build_missing_plot_data(missing_variables)

plot_data <- bind_rows(
  detail_plot_data %>% select(variable, disability_person, response, response_share),
  missing_plot_data
)

if (nrow(plot_data) == 0) {
  stop("No disability response percentages were available for MR1 or MR4 variables.")
}

response_levels <- read_csv(value_labels_path, show_col_types = FALSE) %>%
  filter(variable %in% selected_variables, !is.na(label), nzchar(label)) %>%
  mutate(value = suppressWarnings(as.integer(value))) %>%
  filter(!is.na(value)) %>%
  distinct(label, value) %>%
  arrange(value, label) %>%
  pull(label)

if (length(response_levels) == 0) {
  response_levels <- plot_data %>% distinct(response) %>% pull(response)
}

beach_palette <- as.character(paletteer::paletteer_d("PrettyCols::Beach"))

if (length(beach_palette) < length(response_levels)) {
  response_palette <- grDevices::colorRampPalette(beach_palette)(length(response_levels))
} else {
  response_palette <- beach_palette[seq_along(response_levels)]
}

names(response_palette) <- response_levels

plot_data <- plot_data %>%
  mutate(
    disability_person = factor(
      disability_person,
      levels = c("Yes", "No"),
      labels = c("With disability", "Without")
    ),
    variable = factor(variable, levels = selected_variables),
    response = factor(response, levels = response_levels)
  )

missing_after_backfill <- setdiff(selected_variables, unique(as.character(plot_data$variable)))

if (length(missing_after_backfill) > 0) {
  warning(
    "The following MR1/MR4 variables still have no response rows after backfilling: ",
    paste(missing_after_backfill, collapse = ", ")
  )
}

facet_columns <- dplyr::case_when(
  length(selected_variables) <= 4 ~ 2L,
  length(selected_variables) <= 9 ~ 3L,
  TRUE ~ 4L
)

stacked_plot <- ggplot(plot_data, aes(x = disability_person, y = response_share, fill = response)) +
  geom_col(width = 0.76, color = "white", linewidth = 0.25) +
  facet_wrap(vars(variable), ncol = facet_columns, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(
    values = response_palette,
    name = "Response",
    guide = guide_legend(title.position = "top", nrow = max(1, ceiling(length(response_levels) / 6)), byrow = TRUE)
  ) +
  labs(
    title = "Disability response distributions for MR1 and MR4 items",
    subtitle = "Each facet shows the response mix for respondents with and without a disability.",
    x = NULL,
    y = "Share of responses"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title.align = 0.5,
    plot.title.position = "plot"
  )

png_path <- file.path(output_dir, "disability_response_mr1_mr4_stacked_facets.png")
pdf_path <- file.path(output_dir, "disability_response_mr1_mr4_stacked_facets.pdf")

plot_height <- max(7, ceiling(length(selected_variables) / facet_columns) * 3.2 + 1.8)
plot_width <- max(10, facet_columns * 3.8)

ggsave(png_path, stacked_plot, width = plot_width, height = plot_height, dpi = 320, bg = "white")
ggsave(pdf_path, stacked_plot, width = plot_width, height = plot_height, device = grDevices::cairo_pdf, bg = "white")

message("Wrote disability response stacked facet plot to: ", png_path)
message("Wrote disability response stacked facet plot PDF to: ", pdf_path)
