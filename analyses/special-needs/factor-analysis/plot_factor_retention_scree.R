library(tidyverse)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c(
    "eigenvalue", "factor_count", "parallel_above_simulated", "series"
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

input_path <- file.path(output_dir, "factor_retention_diagnostics.csv")

if (!file.exists(input_path)) {
  stop(
    "Missing factor_retention_diagnostics.csv in factor_analysis/outputs. Run factor_model_analysis.R first."
  )
}

diagnostic_data <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(
    !is.na(factor_count),
    !is.na(parallel_actual_fa_eigen),
    !is.na(parallel_simulated_fa_eigen)
  ) %>%
  arrange(factor_count)

if (nrow(diagnostic_data) == 0) {
  stop("No scree-plot rows were available in factor_retention_diagnostics.csv.")
}

scree_plot_data <- diagnostic_data %>%
  select(factor_count, parallel_actual_fa_eigen, parallel_simulated_fa_eigen) %>%
  pivot_longer(
    cols = c(parallel_actual_fa_eigen, parallel_simulated_fa_eigen),
    names_to = "series",
    values_to = "eigenvalue"
  ) %>%
  mutate(
    series = factor(
      series,
      levels = c("parallel_actual_fa_eigen", "parallel_simulated_fa_eigen"),
      labels = c("Observed FA eigenvalue", "Parallel-analysis reference")
    )
  )

parallel_recommended_count <- diagnostic_data %>%
  filter(parallel_above_simulated) %>%
  summarise(recommended = max(factor_count, na.rm = TRUE)) %>%
  pull(recommended)

series_colors <- c(
  "Observed FA eigenvalue" = "#2c7fb8",
  "Parallel-analysis reference" = "#d95f0e"
)

scree_plot <- ggplot(scree_plot_data, aes(x = factor_count, y = eigenvalue, color = series)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.6) +
  geom_vline(
    xintercept = parallel_recommended_count,
    color = "grey45",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = parallel_recommended_count,
    y = max(scree_plot_data$eigenvalue, na.rm = TRUE),
    label = paste0("Parallel analysis: ", parallel_recommended_count),
    vjust = -0.7,
    hjust = 1,
    color = "grey30",
    size = 3.4
  ) +
  scale_x_continuous(
    breaks = diagnostic_data$factor_count,
    minor_breaks = NULL
  ) +
  scale_color_manual(values = series_colors, name = NULL) +
  labs(
    title = "Scree plot for 1-9 factor models",
    subtitle = "Observed factor-analysis eigenvalues compared with the parallel-analysis reference.",
    x = "Number of factors",
    y = "Eigenvalue"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title.position = "plot"
  )

png_path <- file.path(output_dir, "factor_retention_scree_plot.png")
pdf_path <- file.path(output_dir, "factor_retention_scree_plot.pdf")

ggsave(png_path, scree_plot, width = 8.5, height = 5.5, dpi = 320, bg = "white")
ggsave(pdf_path, scree_plot, width = 8.5, height = 5.5, device = grDevices::cairo_pdf, bg = "white")

message("Wrote factor retention scree plot to: ", png_path)
message("Wrote factor retention scree plot PDF to: ", pdf_path)