library(data.table)
library(ggplot2)
library(psrc.travelsurvey)
library(psrcelmer)
library(scales)

script_path <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", grep("^--file=", script_path, value = TRUE))

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

project_paths_file <- file.path(script_dir, "..", "..", "..", "R", "project_paths.R")
if (!file.exists(project_paths_file)) project_paths_file <- file.path(getwd(), "R", "project_paths.R")
source(project_paths_file)
project_root <- detect_project_root(script_dir)

source(file.path(project_root, "R", "functions", "section_index_functions.R"))

normalize_label_text <- function(values) {
  normalized <- tolower(trimws(as.character(values)))
  normalized <- gsub("[[:space:]]+", " ", normalized)
  normalized[normalized %chin% c("", "na", "n/a")] <- NA_character_
  normalized
}

decode_response_label <- function(values, variable_name, codebook) {
  label_map <- codebook[variable == variable_name, .(label, value)]
  label_map[, value := suppressWarnings(as.integer(value))]
  value_lookup <- stats::setNames(label_map$label, as.character(label_map$value))

  raw_values <- trimws(as.character(values))
  raw_values[raw_values %chin% c("", "NA", "N/A")] <- NA_character_

  numeric_mask <- !is.na(raw_values) & grepl("^-?[0-9]+$", raw_values)
  decoded <- raw_values
  decoded[numeric_mask] <- unname(value_lookup[raw_values[numeric_mask]])
  decoded
}

categorize_agreement <- function(labels) {
  normalized <- normalize_label_text(labels)

  valid <- !is.na(normalized) & !normalized %chin% c(
    "not applicable", "prefer not to say", "don't know", "dont know", "missing response"
  )
  positive <- valid & normalized %chin% c("agree", "strongly agree")

  data.table(valid = valid, positive = positive)
}

categorize_frequency <- function(labels) {
  normalized <- normalize_label_text(labels)

  valid <- !is.na(normalized) & !normalized %chin% c(
    "not applicable", "prefer not to say", "don't know", "dont know", "missing response"
  )
  positive <- valid & normalized %chin% c("often", "sometimes")

  data.table(valid = valid, positive = positive)
}

compute_weighted_share <- function(weights, valid, positive) {
  keep <- !is.na(weights) & weights > 0 & valid

  if (!any(keep)) {
    return(NA_real_)
  }

  100 * sum(weights[keep] * as.numeric(positive[keep]), na.rm = TRUE) / sum(weights[keep], na.rm = TRUE)
}

format_facet_label <- function(question_label, width = 26, min_lines = 1L) {
  if (is.na(question_label) || trimws(question_label) == "") {
    return("(missing question label)")
  }

  wrapped_lines <- strwrap(question_label, width = width)

  if (length(wrapped_lines) < min_lines) {
    missing_lines <- min_lines - length(wrapped_lines)
    pad_top <- floor(missing_lines / 2)
    pad_bottom <- ceiling(missing_lines / 2)
    wrapped_lines <- c(rep(" ", pad_top), wrapped_lines, rep(" ", pad_bottom))
  }

  paste(wrapped_lines, collapse = "\n")
}

summarize_single_question <- function(dt, variable_name, group_var, group_levels, response_type) {
  decoded <- decode_response_label(dt[[variable_name]], variable_name, value_labels)
  classified <- if (identical(response_type, "agreement")) {
    categorize_agreement(decoded)
  } else {
    categorize_frequency(decoded)
  }

  working <- data.table(
    person_weight = dt[["person_weight"]],
    group_value = trimws(as.character(dt[[group_var]])),
    valid = classified[["valid"]],
    positive = classified[["positive"]]
  )

  rows <- list(
    data.table(
      row_group = "overall",
      weighted_pct = compute_weighted_share(working$person_weight, working$valid, working$positive)
    )
  )

  for (lvl in group_levels) {
    group_mask <- working$group_value == lvl
    rows[[length(rows) + 1L]] <- data.table(
      row_group = lvl,
      weighted_pct = compute_weighted_share(
        working$person_weight[group_mask],
        working$valid[group_mask],
        working$positive[group_mask]
      )
    )
  }

  rbindlist(rows, fill = TRUE)
}

summarize_paired_question <- function(dt, variable_a, variable_b, group_var, group_levels) {
  decoded_a <- decode_response_label(dt[[variable_a]], variable_a, value_labels)
  decoded_b <- decode_response_label(dt[[variable_b]], variable_b, value_labels)
  classified_a <- categorize_frequency(decoded_a)
  classified_b <- categorize_frequency(decoded_b)

  valid_pair <- classified_a$valid | classified_b$valid
  positive_pair <- (classified_a$positive & classified_a$valid) | (classified_b$positive & classified_b$valid)

  working <- data.table(
    person_weight = dt[["person_weight"]],
    group_value = trimws(as.character(dt[[group_var]])),
    valid = valid_pair,
    positive = positive_pair
  )

  rows <- list(
    data.table(
      row_group = "overall",
      weighted_pct = compute_weighted_share(working$person_weight, working$valid, working$positive)
    )
  )

  for (lvl in group_levels) {
    group_mask <- working$group_value == lvl
    rows[[length(rows) + 1L]] <- data.table(
      row_group = lvl,
      weighted_pct = compute_weighted_share(
        working$person_weight[group_mask],
        working$valid[group_mask],
        working$positive[group_mask]
      )
    )
  }

  rbindlist(rows, fill = TRUE)
}

build_faceted_plot <- function(plot_dt, ncol = 1L) {
  palette <- c(
    overall = "#6b6b6b",
    group_yes = "#58b2ad",
    group_no = "#9bc65f"
  )

  ggplot(plot_dt, aes(x = weighted_pct, y = row_label, fill = fill_group)) +
    geom_col(width = 0.62, color = NA) +
    geom_text(
      aes(label = ifelse(is.na(weighted_pct), "NA", sprintf("%.0f%%", weighted_pct))),
      hjust = -0.1,
      size = 4.6
    ) +
    facet_wrap(~facet_label, ncol = ncol) +
    scale_fill_manual(values = palette, guide = "none") +
    scale_x_continuous(
      limits = c(0, 105),
      breaks = c(0, 50, 100),
      labels = label_number(suffix = "%", accuracy = 1)
    ) +
    labs(x = "Weighted %", y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(
        face = "bold",
        size = 11,
        color = "white",
        hjust = 0.5,
        vjust = 0.5,
        lineheight = 1.0,
        margin = margin(2, 8, 2, 8)
      ),
      strip.background = element_rect(fill = "#630460", color = "#630460"),
      strip.clip = "on",
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(angle = 30, hjust = 1),
      plot.margin = margin(12, 24, 12, 12)
    )
}

resolve_question_spec <- function(question_ids, variable_descriptions) {
  spec <- copy(variable_descriptions[question_id %chin% question_ids, .(question_id, variable, description)])

  if (nrow(spec) == 0L) {
    stop("No requested question IDs were found in variable descriptions.")
  }

  missing_qids <- setdiff(question_ids, spec$question_id)
  if (length(missing_qids) > 0L) {
    stop(sprintf("Question IDs missing from variable descriptions: %s", paste(missing_qids, collapse = ", ")))
  }

  spec[, question_id := factor(question_id, levels = question_ids)]
  setorder(spec, question_id)
  spec
}

build_plot_data_unpaired <- function(
    dt,
    question_spec,
    group_var,
    group_levels,
    row_labels,
    response_type,
    facet_wrap_width = 26,
    min_facet_lines = 1L
) {
  plot_dt <- rbindlist(lapply(seq_len(nrow(question_spec)), function(i) {
    question_id <- as.character(question_spec$question_id[i])
    variable_name <- question_spec$variable[i]
    question_text <- question_spec$description[i]

    summary_dt <- summarize_single_question(
      dt = dt,
      variable_name = variable_name,
      group_var = group_var,
      group_levels = group_levels,
      response_type = response_type
    )

    summary_dt[, `:=`(
      question_id = question_id,
      variable = variable_name,
      facet_label = format_facet_label(question_text, width = facet_wrap_width, min_lines = min_facet_lines)
    )]
    summary_dt
  }), fill = TRUE)[
    , `:=`(
      row_label = factor(row_labels[row_group], levels = rev(unname(row_labels))),
      fill_group = fcase(
        row_group == "overall", "overall",
        row_group == group_levels[1], "group_yes",
        row_group == group_levels[2], "group_no",
        default = "overall"
      )
    )
  ]

  ordered_facet_labels <- vapply(
    question_spec$description,
    function(x) format_facet_label(x, width = facet_wrap_width, min_lines = min_facet_lines),
    FUN.VALUE = character(1)
  )
  plot_dt[, facet_label := factor(facet_label, levels = ordered_facet_labels)]

  plot_dt
}

build_plot_data_pair <- function(
    dt,
    variable_a,
    variable_b,
    pair_label,
    group_var,
    group_levels,
    row_labels,
    facet_wrap_width = 26,
    min_facet_lines = 1L
) {
  summarize_paired_question(
    dt = dt,
    variable_a = variable_a,
    variable_b = variable_b,
    group_var = group_var,
    group_levels = group_levels
  )[
    , `:=`(
      variable = sprintf("%s_or_%s", variable_a, variable_b),
      facet_label = format_facet_label(pair_label, width = facet_wrap_width, min_lines = min_facet_lines),
      row_label = factor(row_labels[row_group], levels = rev(unname(row_labels))),
      fill_group = fcase(
        row_group == "overall", "overall",
        row_group == group_levels[1], "group_yes",
        row_group == group_levels[2], "group_no",
        default = "overall"
      )
    )
  ]
}

build_special_needs_composition_plots <- function(
    output_dir = "outputs/section_composition",
    save_plots = TRUE,
    width = 14,
    height = 7.5,
    dpi = 300
) {
  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)

  variable_descriptions <- psrcelmer::get_query("SELECT * FROM safety_survey.variable_descriptions_2025") |> setDT()
  value_labels <<- psrcelmer::get_query("SELECT variable, label, value FROM safety_survey.value_labels_2025") |> setDT()

  if ("q_id" %in% names(variable_descriptions) && !"question_id" %in% names(variable_descriptions)) {
    setnames(variable_descriptions, "q_id", "question_id")
  }

  responses <- psrc.travelsurvey::get_psrc_sss("*") |> setDT()
  responses <- normalize_safety_source(responses)
  responses <- filter_completed_responses(responses)
  responses <- derive_demographic_flags(responses)
  responses <- responses[!is.na(person_weight) & person_weight > 0]

  dichotomy_spec <- data.table(
    dichotomy_var = c("disability_person", "age65plus", "low_income", "race_poc", "zero_vehicles"),
    dichotomy_label = c(
      "Disability",
      "Age 65+",
      "Low income",
      "Person of color",
      "Zero vehicles"
    ),
    group_yes_label = c(
      "Has disability",
      "Age 65+",
      "Low income",
      "POC household",
      "No vehicles"
    ),
    group_no_label = c(
      "No disability",
      "Under 65",
      "Not low income",
      "Not POC household",
      "Has 1+ vehicle"
    )
  )

  anxiety_qids <- c("5.13", "5.05", "5.10", "5.03", "5.01", "5.07")
  tradeoff_qids <- c("6.01", "6.02", "6.03", "6.04")
  paired_qids <- data.table(
    question_a = c("6.15", "6.05", "6.11", "6.13", "6.09", "6.07"),
    question_b = c("6.16", "6.06", "6.12", "6.14", "6.10", "6.08"),
    pair_label = c(
      "Avoid walking/rolling or bicycling for trips",
      "Choose a longer route",
      "Choose a more expensive means of travel",
      "Drive to a destination instead",
      "Skip travel altogether",
      "Use a different means of travel"
    )
  )

  anxiety_spec <- resolve_question_spec(anxiety_qids, variable_descriptions)
  tradeoff_spec <- resolve_question_spec(tradeoff_qids, variable_descriptions)
  paired_spec <- rbindlist(lapply(seq_len(nrow(paired_qids)), function(i) {
    q_pair <- c(paired_qids$question_a[i], paired_qids$question_b[i])
    pair_rows <- resolve_question_spec(q_pair, variable_descriptions)
    data.table(
      question_a = q_pair[1],
      question_b = q_pair[2],
      variable_a = pair_rows$variable[1],
      variable_b = pair_rows$variable[2],
      pair_label = paired_qids$pair_label[i]
    )
  }), fill = TRUE)

  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  all_plot_objects <- list()

  for (i in seq_len(nrow(dichotomy_spec))) {
    current_var <- dichotomy_spec$dichotomy_var[i]
    group_levels <- c("Yes", "No")

    if (!current_var %in% names(responses)) {
      message(sprintf("Skipping %s: variable not found.", current_var))
      next
    }

    current_data <- copy(responses[trimws(as.character(get(current_var))) %chin% group_levels])

    if (nrow(current_data) == 0L) {
      message(sprintf("Skipping %s: no Yes/No responses.", current_var))
      next
    }

    row_labels <- c(
      overall = "Overall",
      Yes = dichotomy_spec$group_yes_label[i],
      No = dichotomy_spec$group_no_label[i]
    )

    anxiety_plot_dt <- build_plot_data_unpaired(
      dt = current_data,
      question_spec = anxiety_spec,
      group_var = current_var,
      group_levels = group_levels,
      row_labels = row_labels,
      response_type = "agreement",
      facet_wrap_width = 18,
      min_facet_lines = 5L
    )

    anxiety_plot <- build_faceted_plot(
      anxiety_plot_dt,
      ncol = 6
    )

    tradeoff_plot_dt <- build_plot_data_unpaired(
      dt = current_data,
      question_spec = tradeoff_spec,
      group_var = current_var,
      group_levels = group_levels,
      row_labels = row_labels,
      response_type = "agreement",
      facet_wrap_width = 20,
      min_facet_lines = 4L
    )

    tradeoff_plot <- build_faceted_plot(
      tradeoff_plot_dt,
      ncol = 4
    )

    paired_plot_dt <- rbindlist(lapply(seq_len(nrow(paired_spec)), function(j) {
      pair_dt <- build_plot_data_pair(
        dt = current_data,
        variable_a = paired_spec$variable_a[j],
        variable_b = paired_spec$variable_b[j],
        pair_label = paired_spec$pair_label[j],
        group_var = current_var,
        group_levels = group_levels,
        row_labels = row_labels,
        facet_wrap_width = 17,
        min_facet_lines = 5L
      )

      pair_dt[, pair_id := sprintf("%s_or_%s", paired_spec$question_a[j], paired_spec$question_b[j])]
      pair_dt
    }), fill = TRUE)

    pair_levels <- paired_spec[, sprintf("%s_or_%s", question_a, question_b)]
    paired_plot_dt[, pair_id := factor(pair_id, levels = pair_levels)]
    paired_plot_dt[, facet_label := factor(
      facet_label,
      levels = vapply(
        paired_spec$pair_label,
        function(x) format_facet_label(x, width = 17, min_lines = 5L),
        FUN.VALUE = character(1)
      )
    )]
    setorder(paired_plot_dt, pair_id)

    paired_plot <- build_faceted_plot(
      paired_plot_dt,
      ncol = 6
    )

    all_plot_objects[[current_var]] <- list(
      anxiety = anxiety_plot,
      tradeoffs = tradeoff_plot,
      paired = paired_plot
    )

    if (save_plots) {
      ggsave(
        filename = file.path(output_dir, sprintf("%s_section5_anxiety.png", current_var)),
        plot = anxiety_plot,
        width = width,
        height = height,
        dpi = dpi
      )

      ggsave(
        filename = file.path(output_dir, sprintf("%s_section6_tradeoffs.png", current_var)),
        plot = tradeoff_plot,
        width = width,
        height = height,
        dpi = dpi
      )

      ggsave(
        filename = file.path(output_dir, sprintf("%s_section6_paired_tradeoffs.png", current_var)),
        plot = paired_plot,
        width = width * 1.35,
        height = height,
        dpi = dpi
      )
    }
  }

  all_plot_objects
}

export_special_needs_plots_pdf <- function(
    plots,
    output_file = "outputs/section_composition/section_composition_plots_16x9.pdf",
    width = 16,
    height = 9
) {
  if (!is.list(plots) || length(plots) == 0L) {
    stop("`plots` must be a non-empty list returned by build_special_needs_composition_plots().")
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  grDevices::pdf(file = output_file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  page_count <- 0L

  for (group_name in names(plots)) {
    group_plots <- plots[[group_name]]
    if (!is.list(group_plots) || length(group_plots) == 0L) {
      next
    }

    for (plot_name in names(group_plots)) {
      plot_obj <- group_plots[[plot_name]]
      if (inherits(plot_obj, "ggplot")) {
        print(plot_obj)
        page_count <- page_count + 1L
      }
    }
  }

  if (page_count == 0L) {
    stop("No ggplot objects were found in `plots`; no PDF pages were written.")
  }

  invisible(output_file)
}

# Example:
# plots <- build_special_needs_composition_plots()
# export_special_needs_plots_pdf(plots)
