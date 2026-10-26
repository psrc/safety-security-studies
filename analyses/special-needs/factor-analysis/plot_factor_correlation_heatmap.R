library(tidyverse)
library(psych)
library(grid)
library(gtable)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
read_rds <- readr::read_rds

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c(
    "abs_correlation", "abs_loading", "col_label", "correlation", "description", "factor", "factor_display",
    "loading", "row_label", "selection_status", "variable", "variable_col", "variable_label", "variable_row"
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

output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_paths <- c(
  file.path(output_dir, "curated_core_selection.csv"),
  file.path(output_dir, "core_model_data.rds"),
  file.path(output_dir, "best_factor_loadings.csv"),
  file.path(output_dir, "factor_dimension_labels.csv")
)

missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0) {
  stop(
    "Missing required factor-analysis outputs. Run prepare_core_data.R and factor_model_analysis.R first:\n",
    paste(missing_paths, collapse = "\n")
  )
}

selection_table <- read_csv(file.path(output_dir, "curated_core_selection.csv"), show_col_types = FALSE)
prepared_core_data <- read_rds(file.path(output_dir, "core_model_data.rds"))
factor_loadings <- read_csv(file.path(output_dir, "best_factor_loadings.csv"), show_col_types = FALSE)
factor_labels <- read_csv(file.path(output_dir, "factor_dimension_labels.csv"), show_col_types = FALSE)

selected_variables <- selection_table %>%
  filter(selection_status == "keep") %>%
  pull(variable)

model_data <- prepared_core_data %>%
  filter(complete_case_core) %>%
  dplyr::select(any_of(selected_variables))

if (nrow(model_data) == 0) {
  stop("No complete-case rows available for the correlation heatmap.")
}

factor_columns <- factor_labels %>%
  pull(factor) %>%
  intersect(names(factor_loadings))

if (length(factor_columns) == 0) {
  stop("No factor loading columns were found in best_factor_loadings.csv.")
}

ordered_factor_data <- model_data %>%
  mutate(across(everything(), ~ ordered(.x, levels = sort(unique(.x)))))

polychoric_matrix <- psych::polychoric(ordered_factor_data)
dimnames(polychoric_matrix$rho) <- list(colnames(model_data), colnames(model_data))
smoothed_rho <- psych::cor.smooth(polychoric_matrix$rho)
dimnames(smoothed_rho) <- list(colnames(model_data), colnames(model_data))

variable_metadata <- selection_table %>%
  filter(variable %in% selected_variables) %>%
  transmute(
    variable,
    description = dplyr::coalesce(description, variable),
    row_label = variable,
    col_label = variable
  )

factor_display_map <- factor_labels %>%
  filter(factor %in% factor_columns) %>%
  transmute(factor, factor_display = dplyr::coalesce(factor_label, factor))

best_factor_count <- length(factor_columns)

dominant_factor_order <- factor_loadings %>%
  select(variable, any_of(factor_columns)) %>%
  pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
  mutate(abs_loading = abs(loading)) %>%
  group_by(variable) %>%
  slice_max(order_by = abs_loading, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(factor = factor(factor, levels = factor_columns)) %>%
  arrange(factor, desc(abs_loading), variable)

variable_order <- dominant_factor_order %>%
  pull(variable)

if (length(variable_order) != length(selected_variables)) {
  stop("Unable to build a complete variable order from the saved factor loadings.")
}

row_label_lookup <- variable_metadata %>%
  with(setNames(row_label, variable))

col_label_lookup <- variable_metadata %>%
  with(setNames(col_label, variable))

row_label_order <- unname(row_label_lookup[variable_order])
col_label_order <- unname(col_label_lookup[variable_order])
heatmap_y_levels <- rev(row_label_order)

correlation_long <- as.data.frame(smoothed_rho) %>%
  tibble::rownames_to_column("variable_row") %>%
  as_tibble() %>%
  pivot_longer(-variable_row, names_to = "variable_col", values_to = "correlation") %>%
  mutate(
    abs_correlation = abs(correlation),
    abs_correlation = dplyr::if_else(variable_row == variable_col, NA_real_, abs_correlation),
    variable_row = factor(variable_row, levels = variable_order),
    variable_col = factor(variable_col, levels = variable_order),
    variable_row = factor(unname(row_label_lookup[as.character(variable_row)]), levels = heatmap_y_levels),
    variable_col = factor(unname(col_label_lookup[as.character(variable_col)]), levels = col_label_order)
  )

max_abs_correlation <- correlation_long %>%
  summarise(max_abs_correlation = max(abs_correlation, na.rm = TRUE)) %>%
  pull(max_abs_correlation)

loading_long <- factor_loadings %>%
  select(variable, any_of(factor_columns)) %>%
  pivot_longer(-variable, names_to = "factor", values_to = "loading") %>%
  mutate(abs_loading = abs(loading)) %>%
  left_join(factor_display_map, by = "factor") %>%
  mutate(
    factor = factor(factor, levels = factor_columns),
    factor_display = factor(
      factor_display,
      levels = factor_display_map %>% arrange(match(factor, factor_columns)) %>% pull(factor_display)
    ),
    variable_label = factor(unname(row_label_lookup[variable]), levels = heatmap_y_levels)
  )

factor_palette <- c(
  "Safety Anxiety And Security Worry" = "#1F4E79",
  "System And Driver Safety Confidence" = "#D97757",
  "System And Rule-Based Safety Confidence" = "#D97757",
  "Enforcement And Safety Norms" = "#5C6B73",
  "Enforcement And Speed-Safety Norms" = "#5C6B73",
  "Safety-Driven Travel Tradeoffs" = "#7AA37A"
)

missing_palette_levels <- setdiff(levels(loading_long$factor_display), names(factor_palette))

if (length(missing_palette_levels) > 0) {
  fallback_palette <- grDevices::hcl.colors(
    n = length(levels(loading_long$factor_display)),
    palette = "Blue-Red 3"
  )
  names(fallback_palette) <- levels(loading_long$factor_display)
  factor_palette <- fallback_palette
}

heatmap_plot <- ggplot(correlation_long, aes(x = variable_col, y = variable_row, fill = abs_correlation)) +
  geom_tile(color = "white", linewidth = 0.25) +
  coord_fixed() +
  scale_fill_gradientn(
    colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
    limits = c(0, max_abs_correlation),
    na.value = NA,
    name = "Absolute\npolychoric\ncorrelation"
  ) +
  labs(
    title = "Safety factor-analysis item correlations and loading structure",
    subtitle = paste0(
      "Heatmap uses absolute complete-case smoothed polychoric correlations, omits diagonal self-correlations, and scales to the strongest non-diagonal relationship. ",
      "Edge bars stack absolute loadings from the current ", best_factor_count, "-factor solution."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7.5),
    axis.text.y = element_text(size = 8),
    plot.margin = margin(10, 8, 10, 18),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

loading_plot <- ggplot(loading_long, aes(x = abs_loading, y = variable_label, fill = factor_display)) +
  geom_col(width = 0.86, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = factor_palette, name = "Factor") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.04)),
    position = "top"
  ) +
  labs(x = "Absolute loading", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(10, 10, 10, 0),
    legend.position = "bottom"
  )

heatmap_grob <- ggplotGrob(heatmap_plot)
loading_grob <- ggplotGrob(loading_plot)

shared_heights <- grid::unit.pmax(heatmap_grob$heights, loading_grob$heights)
heatmap_grob$heights <- shared_heights
loading_grob$heights <- shared_heights

combined_grob <- cbind(heatmap_grob, loading_grob, size = "first")

save_combined_plot <- function(path, width, height, dpi = 300) {
  extension <- tools::file_ext(path)

  if (tolower(extension) == "png") {
    grDevices::png(path, width = width, height = height, units = "in", res = dpi)
  } else if (tolower(extension) == "pdf") {
    grDevices::pdf(path, width = width, height = height, useDingbats = FALSE)
  } else {
    stop("Unsupported output extension: ", extension)
  }

  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(combined_grob)
}

plot_height <- max(12, length(selected_variables) * 0.55 + 3.5)
plot_width <- 21

png_path <- file.path(output_dir, "factor_correlation_heatmap.png")
pdf_path <- file.path(output_dir, "factor_correlation_heatmap.pdf")

save_combined_plot(png_path, width = plot_width, height = plot_height)
save_combined_plot(pdf_path, width = plot_width, height = plot_height)

message("Wrote factor correlation heatmap to: ", png_path)
message("Wrote factor correlation heatmap PDF to: ", pdf_path)