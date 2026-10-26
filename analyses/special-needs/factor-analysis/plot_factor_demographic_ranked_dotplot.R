library(tidyverse)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c(
    "factor_label", "neg_log10_p", "p_value", "respondent_dichotomy", "respondent_order",
    "significant", "weighted_smd"
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

input_path <- file.path(output_dir, "factor_demographic_tests.csv")

if (!file.exists(input_path)) {
  stop(
    "Missing factor_demographic_tests.csv in factor_analysis/outputs. Run factor_demographic_comparison.R first."
  )
}

plot_data <- read_csv(input_path, show_col_types = FALSE) %>%
  mutate(
    p_value = dplyr::case_when(
      is.na(p_value) | !is.finite(p_value) ~ NA_real_,
      p_value <= 0 ~ .Machine$double.xmin,
      p_value > 1 ~ 1,
      TRUE ~ p_value
    ),
    neg_log10_p = -log10(p_value),
    significant = p_value < 0.05,
    weighted_smd = dplyr::if_else(is.finite(weighted_smd), weighted_smd, NA_real_),
    factor_label = dplyr::coalesce(factor_label, factor)
  ) %>%
  filter(
    !is.na(respondent_dichotomy),
    !is.na(factor_label),
    !is.na(weighted_smd),
    !is.na(neg_log10_p),
    is.finite(weighted_smd),
    is.finite(neg_log10_p)
  )

if (nrow(plot_data) == 0) {
  stop("No finite observations were available to plot from factor_demographic_tests.csv.")
}

respondent_order <- plot_data %>%
  group_by(respondent_dichotomy) %>%
  summarise(max_abs_weighted_smd = max(abs(weighted_smd), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_abs_weighted_smd), respondent_dichotomy) %>%
  pull(respondent_dichotomy)

plot_data <- plot_data %>%
  mutate(
    # Order groups by the strongest absolute weighted SMD seen in any factor so the
    # highest-signal respondent dichotomies stay at the top across all panels.
    respondent_dichotomy = factor(respondent_dichotomy, levels = rev(respondent_order)),
    factor_label = forcats::fct_inorder(factor_label)
  )

significant_points <- plot_data %>%
  filter(significant)

nonsignificant_points <- plot_data %>%
  filter(!significant)

max_abs_smd <- max(abs(plot_data$weighted_smd), na.rm = TRUE)
x_limit <- max(0.25, ceiling(max_abs_smd * 4) / 4)

p_value_breaks <- pretty(plot_data$neg_log10_p, n = 4)
p_value_breaks <- p_value_breaks[p_value_breaks >= 0]

if (length(p_value_breaks) == 0) {
  p_value_breaks <- c(0, plot_data$neg_log10_p[[1]])
}

preferred_p_breaks <- c(0, -log10(0.05), 2, 3, 5)
p_value_breaks <- preferred_p_breaks[preferred_p_breaks <= max(plot_data$neg_log10_p, na.rm = TRUE)]

if (length(p_value_breaks) < 2) {
  p_value_breaks <- unique(c(0, max(plot_data$neg_log10_p, na.rm = TRUE)))
}

color_ramp <- c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")

break_labels <- function(x) {
  formatted_p <- 10^(-x)
  dplyr::case_when(
    formatted_p < 0.001 ~ formatC(formatted_p, format = "e", digits = 1),
    TRUE ~ formatC(formatted_p, format = "f", digits = 3)
  )
}

ranked_dot_plot <- ggplot(plot_data, aes(x = weighted_smd, y = respondent_dichotomy)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.5, linetype = "dashed") +
  geom_point(
    data = nonsignificant_points,
    shape = 21,
    fill = NA,
    size = 4.1,
    color = "grey20",
    stroke = 0.45,
    alpha = 0.95
  ) +
  geom_point(
    data = significant_points,
    aes(fill = neg_log10_p),
    shape = 21,
    color = "grey20",
    size = 4.1,
    stroke = 0.45,
    alpha = 0.95
  ) +
  facet_wrap(vars(factor_label), ncol = 1) +
  scale_x_continuous(
    limits = c(-x_limit, x_limit),
    breaks = scales::pretty_breaks(n = 5),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_fill_gradientn(
    name = "p-value\n(darker = lower)",
    colours = color_ramp,
    breaks = p_value_breaks,
    labels = break_labels,
    guide = guide_colorbar(
      order = 1,
      title.position = "top",
      barwidth = grid::unit(7, "cm"),
      barheight = grid::unit(0.45, "cm")
    )
  ) +
  labs(
    title = "Weighted factor-score differences across Special Needs populations",
    x = "Weighted standardized mean difference",
    y = "Respondent dichotomy"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing.y = grid::unit(0.55, "lines"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title.align = 0.5,
    axis.text.y = element_text(face = "bold"),
    plot.title.position = "plot"
  )

png_path <- file.path(output_dir, "factor_demographic_ranked_dotplot.png")
pdf_path <- file.path(output_dir, "factor_demographic_ranked_dotplot.pdf")

factor_count <- dplyr::n_distinct(plot_data$factor_label)
respondent_count <- length(levels(plot_data$respondent_dichotomy))

# Scale output height by both the number of stacked factor panels and the rows
# inside each panel so labels and points do not collide as the model expands.
plot_height <- max(7, factor_count * 1.25 + respondent_count * 0.55 + 2.8)
plot_width <- 8.5

ggsave(png_path, ranked_dot_plot, width = plot_width, height = plot_height, dpi = 320, bg = "white")
ggsave(pdf_path, ranked_dot_plot, width = plot_width, height = plot_height, device = grDevices::cairo_pdf, bg = "white")

message("Wrote ranked demographic dot plot to: ", png_path)
message("Wrote ranked demographic dot plot PDF to: ", pdf_path)