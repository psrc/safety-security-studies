library(data.table)
library(dplyr)
library(stringr)
library(survey)
library(tibble)

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
source(file.path(project_root, "R", "data-prep", "core_prep_helpers.R"))

respondent_dichotomy <- "disability_person"
respondent_dichotomy_levels <- c("Yes", "No")
output_dir <- script_dir
codebook_path <- file.path(project_root, "data", "value_labels.csv")

paired_domains <- tibble::tribble(
  ~domain_id, ~domain_label, ~safety_var, ~security_var,
  "longer_route", "Take a longer route", "shortest_path_fear", "shortest_path_assault",
  "different_mode", "Choose a different mode", "traffic_crash_worry", "traffic_harrassment",
  "no_travel", "Do not travel at all", "no_safe_travel", "no_harm_travel",
  "more_expensive_mode", "Choose a more expensive mode", "feel_safe", "fear_assault",
  "drive_instead", "Drive instead of a preferred mode", "feel_safe_crash", "fear_assault_2",
  "avoid_active_modes", "Avoid walking or biking", "avoid_walk_1", "avoid_walk_2"
)

normalize_label_text <- function(values) {
  normalized <- stringr::str_squish(stringr::str_to_lower(as.character(values)))
  normalized[normalized %in% c("", "na", "n/a")] <- NA_character_
  normalized
}

decode_response_label <- function(values, variable_name, codebook) {
  label_map <- codebook[codebook$variable == variable_name, c("label", "value"), drop = FALSE]
  label_map$value <- suppressWarnings(as.integer(label_map$value))
  value_lookup <- stats::setNames(label_map$label, as.character(label_map$value))

  raw_values <- trimws(as.character(values))
  raw_values[raw_values %in% c("", "NA", "N/A")] <- NA_character_

  numeric_mask <- !is.na(raw_values) & grepl("^-?\\d+$", raw_values)
  decoded <- raw_values
  decoded[numeric_mask] <- unname(value_lookup[raw_values[numeric_mask]])
  decoded
}

classify_concern_response <- function(labels) {
  normalized <- normalize_label_text(labels)

  response_bucket <- dplyr::case_when(
    is.na(normalized) ~ "Missing/other",
    normalized == "often" ~ "Often",
    normalized == "sometimes" ~ "Sometimes",
    normalized %in% c("seldom/never", "seldom", "never") ~ "Seldom/never",
    normalized == "not applicable" ~ "Not applicable",
    normalized %in% c("prefer not to say", "don't know", "dont know", "missing response") ~ "Missing/other",
    TRUE ~ "Missing/other"
  )

  concern_score <- dplyr::case_when(
    response_bucket == "Seldom/never" ~ 1,
    response_bucket == "Sometimes" ~ 2,
    response_bucket == "Often" ~ 3,
    TRUE ~ NA_real_
  )

  data.table(
    response_bucket = factor(
      response_bucket,
      levels = c("Often", "Sometimes", "Seldom/never", "Not applicable", "Missing/other")
    ),
    concern_score = concern_score,
    applicable = response_bucket %in% c("Often", "Sometimes", "Seldom/never"),
    structural_not_applicable = response_bucket == "Not applicable"
  )
}

ensure_parent_dir <- function(path) {
  dir_path <- dirname(path)

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

write_output <- function(dt, file_name) {
  file_path <- file.path(output_dir, file_name)
  ensure_parent_dir(file_path)

  out_dt <- as.data.table(dt)
  factor_cols <- names(out_dt)[vapply(out_dt, is.factor, logical(1))]

  for (col_name in factor_cols) {
    out_dt[[col_name]] <- as.character(out_dt[[col_name]])
  }

  fwrite(out_dt, file_path)
  file_path
}

weighted_var <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0

  if (!any(keep)) {
    return(NA_real_)
  }

  x <- x[keep]
  w <- w[keep]
  mean_x <- stats::weighted.mean(x, w)
  sum_w <- sum(w)

  if (sum_w <= 0) {
    return(NA_real_)
  }

  sum(w * (x - mean_x) ^ 2) / sum_w
}

compute_weighted_smd <- function(dt, value_col, group_col = respondent_dichotomy, group_levels = respondent_dichotomy_levels) {
  group_1 <- dt[get(group_col) == group_levels[1]]
  group_2 <- dt[get(group_col) == group_levels[2]]

  if (nrow(group_1) == 0L || nrow(group_2) == 0L) {
    return(NA_real_)
  }

  mean_1 <- stats::weighted.mean(group_1[[value_col]], group_1[["person_weight"]], na.rm = TRUE)
  mean_2 <- stats::weighted.mean(group_2[[value_col]], group_2[["person_weight"]], na.rm = TRUE)
  var_1 <- weighted_var(group_1[[value_col]], group_1[["person_weight"]])
  var_2 <- weighted_var(group_2[[value_col]], group_2[["person_weight"]])
  pooled_sd <- sqrt(mean(c(var_1, var_2), na.rm = TRUE))

  if (is.na(pooled_sd) || pooled_sd == 0) {
    return(NA_real_)
  }

  (mean_1 - mean_2) / pooled_sd
}

compute_weighted_cramers_v <- function(design, formula_text) {
  contingency_table <- tryCatch(
    svytable(as.formula(formula_text), design),
    error = function(e) NULL
  )

  if (is.null(contingency_table)) {
    return(NA_real_)
  }

  contingency_matrix <- as.matrix(contingency_table)

  if (length(dim(contingency_matrix)) != 2L) {
    return(NA_real_)
  }

  keep_rows <- rowSums(contingency_matrix, na.rm = TRUE) > 0
  keep_cols <- colSums(contingency_matrix, na.rm = TRUE) > 0
  contingency_matrix <- contingency_matrix[keep_rows, keep_cols, drop = FALSE]

  if (min(dim(contingency_matrix)) < 2L) {
    return(NA_real_)
  }

  chi_sq <- tryCatch(
    suppressWarnings(stats::chisq.test(contingency_matrix, correct = FALSE)),
    error = function(e) NULL
  )

  if (is.null(chi_sq)) {
    return(NA_real_)
  }

  total_weight <- sum(contingency_matrix)
  min_dim <- min(dim(contingency_matrix) - 1)

  if (is.na(total_weight) || total_weight <= 0 || min_dim <= 0) {
    return(NA_real_)
  }

  as.numeric(sqrt(unname(chi_sq$statistic) / (total_weight * min_dim)))
}

safe_svyttest <- function(dt, outcome_col) {
  dt <- dt[!is.na(get(outcome_col))]

  if (nrow(dt) == 0L || uniqueN(dt[[respondent_dichotomy]]) < 2L) {
    return(data.table(
      outcome = outcome_col,
      test = "svyttest",
      p_value = NA_real_,
      effect_size_type = "weighted_smd",
      effect_size = NA_real_,
      note = "Insufficient non-missing data for both groups."
    ))
  }

  design <- svydesign(
    ids = ~1,
    strata = ~sample_segment,
    weights = ~person_weight,
    data = dt,
    nest = TRUE
  )

  test_object <- tryCatch(
    svyttest(as.formula(sprintf("`%s` ~ `%s`", outcome_col, respondent_dichotomy)), design),
    error = function(e) e
  )

  if (inherits(test_object, "error")) {
    return(data.table(
      outcome = outcome_col,
      test = "svyttest",
      p_value = NA_real_,
      effect_size_type = "weighted_smd",
      effect_size = NA_real_,
      note = test_object$message
    ))
  }

  data.table(
    outcome = outcome_col,
    test = "svyttest",
    p_value = unname(test_object$p.value),
    effect_size_type = "weighted_smd",
    effect_size = compute_weighted_smd(dt, outcome_col),
    note = NA_character_
  )
}

safe_svychisq <- function(dt, outcome_col) {
  dt <- dt[!is.na(get(outcome_col))]

  if (nrow(dt) == 0L || uniqueN(dt[[respondent_dichotomy]]) < 2L || uniqueN(dt[[outcome_col]]) < 2L) {
    return(data.table(
      outcome = outcome_col,
      test = "svychisq",
      p_value = NA_real_,
      effect_size_type = "cramers_v",
      effect_size = NA_real_,
      note = "Insufficient non-missing data for both groups or response levels."
    ))
  }

  design <- svydesign(
    ids = ~1,
    strata = ~sample_segment,
    weights = ~person_weight,
    data = dt,
    nest = TRUE
  )

  test_object <- tryCatch(
    suppressWarnings(svychisq(as.formula(sprintf("~ `%s` + `%s`", respondent_dichotomy, outcome_col)), design, statistic = "F")),
    error = function(e) e
  )

  if (inherits(test_object, "error")) {
    return(data.table(
      outcome = outcome_col,
      test = "svychisq",
      p_value = NA_real_,
      effect_size_type = "cramers_v",
      effect_size = NA_real_,
      note = test_object$message
    ))
  }

  data.table(
    outcome = outcome_col,
    test = "svychisq",
    p_value = unname(test_object$p.value),
    effect_size_type = "cramers_v",
    effect_size = compute_weighted_cramers_v(design, sprintf("~ `%s` + `%s`", respondent_dichotomy, outcome_col)),
    note = NA_character_
  )
}

build_analysis_data <- function() {
  safety_responses <- load_safety_responses(project_root) %>%
    normalize_safety_source() %>%
    filter_completed_responses() %>%
    derive_demographic_flags()

  if (!respondent_dichotomy %in% names(safety_responses)) {
    stop(sprintf("`%s` was not found in the safety survey responses.", respondent_dichotomy))
  }

  required_columns <- unique(c(
    respondent_dichotomy,
    "person_weight",
    "sample_segment",
    unlist(paired_domains[, c("safety_var", "security_var")], use.names = FALSE)
  ))

  missing_columns <- setdiff(required_columns, names(safety_responses))

  if (length(missing_columns) > 0L) {
    stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")))
  }

  analysis_data <- as.data.table(safety_responses)[
    trimws(as.character(get(respondent_dichotomy))) %in% respondent_dichotomy_levels &
      !is.na(person_weight) &
      person_weight > 0 &
      !is.na(sample_segment)
  ]

  analysis_data[, respondent_row_id := .I]
  analysis_data[, (respondent_dichotomy) := factor(trimws(as.character(get(respondent_dichotomy))), levels = respondent_dichotomy_levels)]

  analysis_data
}

build_long_responses <- function(analysis_data, codebook) {
  long_tables <- lapply(seq_len(nrow(paired_domains)), function(i) {
    domain_row <- paired_domains[i, ]

    rbindlist(list(
      data.table(
        respondent_row_id = analysis_data[["respondent_row_id"]],
        person_weight = analysis_data[["person_weight"]],
        sample_segment = analysis_data[["sample_segment"]],
        disability_person = analysis_data[[respondent_dichotomy]],
        domain_id = domain_row[["domain_id"]],
        domain_label = domain_row[["domain_label"]],
        concern_type = "safety",
        variable = domain_row[["safety_var"]],
        response_label = decode_response_label(analysis_data[[domain_row[["safety_var"]]]], domain_row[["safety_var"]], codebook)
      ),
      data.table(
        respondent_row_id = analysis_data[["respondent_row_id"]],
        person_weight = analysis_data[["person_weight"]],
        sample_segment = analysis_data[["sample_segment"]],
        disability_person = analysis_data[[respondent_dichotomy]],
        domain_id = domain_row[["domain_id"]],
        domain_label = domain_row[["domain_label"]],
        concern_type = "security",
        variable = domain_row[["security_var"]],
        response_label = decode_response_label(analysis_data[[domain_row[["security_var"]]]], domain_row[["security_var"]], codebook)
      )
    ), fill = TRUE)
  })

  long_dt <- rbindlist(long_tables, fill = TRUE)
  long_dt <- cbind(long_dt, classify_concern_response(long_dt[["response_label"]]))
  long_dt[, concern_type := factor(concern_type, levels = c("safety", "security"))]
  long_dt
}

summarize_item_responses <- function(long_dt) {
  out <- long_dt[
    ,
    .(weighted_n = sum(person_weight, na.rm = TRUE)),
    by = .(domain_id, domain_label, concern_type, variable, disability_person, response_bucket)
  ]

  out[, weighted_prop := weighted_n / sum(weighted_n), by = .(domain_id, concern_type, disability_person)]
  setorder(out, domain_id, concern_type, disability_person, response_bucket)
  out
}

build_domain_wide <- function(long_dt) {
  unique_cols <- c("respondent_row_id", "domain_id", "domain_label", "person_weight", "sample_segment", respondent_dichotomy)

  score_wide <- dcast(
    long_dt[, c(unique_cols, "concern_type", "concern_score"), with = FALSE],
    respondent_row_id + domain_id + domain_label + person_weight + sample_segment + disability_person ~ concern_type,
    value.var = "concern_score"
  )

  applicability_wide <- dcast(
    long_dt[, c(unique_cols, "concern_type", "applicable", "structural_not_applicable"), with = FALSE],
    respondent_row_id + domain_id + domain_label + person_weight + sample_segment + disability_person ~ concern_type,
    value.var = c("applicable", "structural_not_applicable")
  )

  domain_wide <- merge(score_wide, applicability_wide, by = unique_cols, all = TRUE)
  domain_wide[, safety_applicable := fifelse(isTRUE(applicable_safety), 1, 0)]
  domain_wide[, security_applicable := fifelse(isTRUE(applicable_security), 1, 0)]
  domain_wide[, either_applicable := fifelse(safety_applicable == 1 | security_applicable == 1, 1, 0)]
  domain_wide[, both_applicable := fifelse(safety_applicable == 1 & security_applicable == 1, 1, 0)]
  domain_wide
}

summarize_domain_applicability <- function(domain_wide) {
  out <- domain_wide[
    ,
    .(
      weighted_n = sum(person_weight, na.rm = TRUE),
      safety_applicable_prop = stats::weighted.mean(safety_applicable, person_weight, na.rm = TRUE),
      security_applicable_prop = stats::weighted.mean(security_applicable, person_weight, na.rm = TRUE),
      either_applicable_prop = stats::weighted.mean(either_applicable, person_weight, na.rm = TRUE),
      both_applicable_prop = stats::weighted.mean(both_applicable, person_weight, na.rm = TRUE),
      safety_not_applicable_prop = stats::weighted.mean(as.numeric(structural_not_applicable_safety), person_weight, na.rm = TRUE),
      security_not_applicable_prop = stats::weighted.mean(as.numeric(structural_not_applicable_security), person_weight, na.rm = TRUE)
    ),
    by = .(domain_id, domain_label, disability_person)
  ]

  setorder(out, domain_id, disability_person)
  out
}

run_domain_applicability_tests <- function(domain_wide) {
  binary_outcomes <- c("safety_applicable", "security_applicable", "either_applicable", "both_applicable")

  rbindlist(lapply(split(domain_wide, by = "domain_id", keep.by = TRUE), function(domain_dt) {
    rbindlist(lapply(binary_outcomes, function(outcome_col) {
      result <- safe_svyttest(domain_dt, outcome_col)
      result[, `:=`(domain_id = domain_dt[["domain_id"]][1], domain_label = domain_dt[["domain_label"]][1])]
      result
    }), fill = TRUE)
  }), fill = TRUE)
}

summarize_domain_mean_scores <- function(long_dt) {
  applicable_dt <- long_dt[applicable == TRUE & !is.na(concern_score)]

  summary_dt <- applicable_dt[
    ,
    .(
      weighted_n = sum(person_weight, na.rm = TRUE),
      mean_score = stats::weighted.mean(concern_score, person_weight, na.rm = TRUE)
    ),
    by = .(domain_id, domain_label, concern_type, disability_person)
  ]

  gap_dt <- dcast(
    summary_dt,
    domain_id + domain_label + concern_type ~ disability_person,
    value.var = "mean_score"
  )

  if (all(respondent_dichotomy_levels %in% names(gap_dt))) {
    gap_dt[, disability_gap := get(respondent_dichotomy_levels[1]) - get(respondent_dichotomy_levels[2])]
  } else {
    gap_dt[, disability_gap := NA_real_]
  }

  did_dt <- dcast(
    gap_dt,
    domain_id + domain_label ~ concern_type,
    value.var = "disability_gap"
  )

  if (all(c("safety", "security") %in% names(did_dt))) {
    did_dt[, safety_minus_security_gap := safety - security]
  } else {
    did_dt[, safety_minus_security_gap := NA_real_]
  }

  list(summary = summary_dt, gaps = gap_dt, gap_diff = did_dt)
}

run_domain_mean_tests <- function(long_dt) {
  applicable_dt <- long_dt[applicable == TRUE & !is.na(concern_score)]

  rbindlist(lapply(split(applicable_dt, by = c("domain_id", "concern_type"), keep.by = TRUE), function(dt) {
    result <- safe_svyttest(dt, "concern_score")
    result[, `:=`(
      domain_id = dt[["domain_id"]][1],
      domain_label = dt[["domain_label"]][1],
      concern_type = as.character(dt[["concern_type"]][1])
    )]
    result
  }), fill = TRUE)
}

summarize_paired_contrasts <- function(domain_wide) {
  both_dt <- domain_wide[both_applicable == 1 & !is.na(safety) & !is.na(security)]

  if (nrow(both_dt) == 0L) {
    empty_summary <- data.table(
      domain_id = character(),
      domain_label = character(),
      disability_person = character(),
      contrast_category = character(),
      weighted_n = numeric(),
      weighted_prop = numeric(),
      mean_difference = numeric()
    )
    empty_tests <- data.table(
      outcome = character(),
      test = character(),
      p_value = numeric(),
      effect_size_type = character(),
      effect_size = numeric(),
      note = character(),
      domain_id = character(),
      domain_label = character()
    )
    return(list(summary = empty_summary, tests = empty_tests))
  }

  both_dt[, contrast_difference := safety - security]
  both_dt[, contrast_category := fifelse(
    contrast_difference > 0,
    "Safety higher",
    fifelse(contrast_difference < 0, "Security higher", "Equal")
  )]

  summary_dt <- both_dt[
    ,
    .(
      weighted_n = sum(person_weight, na.rm = TRUE),
      mean_difference = stats::weighted.mean(contrast_difference, person_weight, na.rm = TRUE)
    ),
    by = .(domain_id, domain_label, disability_person, contrast_category)
  ]
  summary_dt[, weighted_prop := weighted_n / sum(weighted_n), by = .(domain_id, disability_person)]
  setorder(summary_dt, domain_id, disability_person, contrast_category)

  test_dt <- rbindlist(lapply(split(both_dt, by = "domain_id", keep.by = TRUE), function(dt) {
    result <- safe_svychisq(dt, "contrast_category")
    result[, `:=`(domain_id = dt[["domain_id"]][1], domain_label = dt[["domain_label"]][1])]
    result
  }), fill = TRUE)

  list(summary = summary_dt, tests = test_dt)
}

summarize_summary_scores <- function(domain_wide) {
  score_dt <- domain_wide[
    ,
    .(
      person_weight = person_weight[1],
      sample_segment = sample_segment[1],
      disability_person = disability_person[1],
      safety_domains_answered = sum(!is.na(safety)),
      security_domains_answered = sum(!is.na(security)),
      safety_score = if (all(is.na(safety))) NA_real_ else mean(safety, na.rm = TRUE),
      security_score = if (all(is.na(security))) NA_real_ else mean(security, na.rm = TRUE)
    ),
    by = .(respondent_row_id)
  ]

  weighted_summary <- score_dt[
    ,
    .(
      weighted_n = sum(person_weight, na.rm = TRUE),
      safety_score_mean = stats::weighted.mean(safety_score, person_weight, na.rm = TRUE),
      security_score_mean = stats::weighted.mean(security_score, person_weight, na.rm = TRUE),
      safety_domains_answered_mean = stats::weighted.mean(safety_domains_answered, person_weight, na.rm = TRUE),
      security_domains_answered_mean = stats::weighted.mean(security_domains_answered, person_weight, na.rm = TRUE)
    ),
    by = .(disability_person)
  ]

  test_dt <- rbindlist(list(
    safe_svyttest(score_dt, "safety_score"),
    safe_svyttest(score_dt, "security_score")
  ), fill = TRUE)

  list(scores = score_dt, summary = weighted_summary, tests = test_dt)
}

run_stacked_models <- function(long_dt) {
  model_data <- copy(long_dt[applicable == TRUE & !is.na(concern_score)])

  if (nrow(model_data) == 0L) {
    empty <- data.table(term = character(), estimate = numeric(), std_error = numeric(), statistic = numeric(), p_value = numeric())
    return(list(overall = empty, by_domain = empty))
  }

  model_data[, disability_person := factor(disability_person, levels = c("No", "Yes"))]
  model_data[, concern_type := factor(concern_type, levels = c("safety", "security"))]
  model_data[, domain_label := factor(domain_label, levels = unique(paired_domains$domain_label))]

  overall_design <- svydesign(
    ids = ~1,
    strata = ~sample_segment,
    weights = ~person_weight,
    data = model_data,
    nest = TRUE
  )

  overall_model <- tryCatch(
    svyglm(concern_score ~ disability_person * concern_type + domain_label, design = overall_design),
    error = function(e) e
  )

  overall_out <- if (inherits(overall_model, "error")) {
    data.table(term = "model_error", estimate = NA_real_, std_error = NA_real_, statistic = NA_real_, p_value = NA_real_, note = overall_model$message)
  } else {
    coef_table <- as.data.frame(summary(overall_model)$coefficients)
    data.table(
      term = rownames(coef_table),
      estimate = coef_table[[1]],
      std_error = coef_table[[2]],
      statistic = coef_table[[3]],
      p_value = coef_table[[4]],
      note = NA_character_
    )
  }

  by_domain_out <- rbindlist(lapply(split(model_data, by = "domain_id", keep.by = TRUE), function(dt) {
    if (uniqueN(dt[["concern_type"]]) < 2L || uniqueN(dt[["disability_person"]]) < 2L) {
      return(data.table(
        domain_id = dt[["domain_id"]][1],
        domain_label = dt[["domain_label"]][1],
        term = "model_error",
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        note = "Insufficient variation to fit domain interaction model."
      ))
    }

    domain_design <- svydesign(
      ids = ~1,
      strata = ~sample_segment,
      weights = ~person_weight,
      data = dt,
      nest = TRUE
    )

    domain_model <- tryCatch(
      svyglm(concern_score ~ disability_person * concern_type, design = domain_design),
      error = function(e) e
    )

    if (inherits(domain_model, "error")) {
      return(data.table(
        domain_id = dt[["domain_id"]][1],
        domain_label = dt[["domain_label"]][1],
        term = "model_error",
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        note = domain_model$message
      ))
    }

    coef_table <- as.data.frame(summary(domain_model)$coefficients)
    data.table(
      domain_id = dt[["domain_id"]][1],
      domain_label = dt[["domain_label"]][1],
      term = rownames(coef_table),
      estimate = coef_table[[1]],
      std_error = coef_table[[2]],
      statistic = coef_table[[3]],
      p_value = coef_table[[4]],
      note = NA_character_
    )
  }), fill = TRUE)

  list(overall = overall_out, by_domain = by_domain_out)
}

codebook <- fread(codebook_path)
analysis_data <- build_analysis_data()
long_dt <- build_long_responses(analysis_data, codebook)
domain_wide <- build_domain_wide(long_dt)

item_response_summary <- summarize_item_responses(long_dt)
domain_applicability_summary <- summarize_domain_applicability(domain_wide)
domain_applicability_tests <- run_domain_applicability_tests(domain_wide)
domain_mean_scores <- summarize_domain_mean_scores(long_dt)
domain_mean_tests <- run_domain_mean_tests(long_dt)
paired_contrasts <- summarize_paired_contrasts(domain_wide)
summary_scores <- summarize_summary_scores(domain_wide)
stacked_models <- run_stacked_models(long_dt)

item_response_path <- write_output(item_response_summary, "paired_tradeoff_item_response_summary.csv")
domain_applicability_summary_path <- write_output(domain_applicability_summary, "paired_tradeoff_domain_applicability_summary.csv")
domain_applicability_tests_path <- write_output(domain_applicability_tests, "paired_tradeoff_domain_applicability_tests.csv")
domain_mean_summary_path <- write_output(domain_mean_scores$summary, "paired_tradeoff_domain_mean_scores.csv")
domain_mean_gaps_path <- write_output(domain_mean_scores$gaps, "paired_tradeoff_domain_mean_gaps.csv")
domain_gap_diff_path <- write_output(domain_mean_scores$gap_diff, "paired_tradeoff_domain_gap_diff.csv")
domain_mean_tests_path <- write_output(domain_mean_tests, "paired_tradeoff_domain_mean_tests.csv")
paired_contrast_summary_path <- write_output(paired_contrasts$summary, "paired_tradeoff_paired_contrast_summary.csv")
paired_contrast_tests_path <- write_output(paired_contrasts$tests, "paired_tradeoff_paired_contrast_tests.csv")
summary_scores_path <- write_output(summary_scores$summary, "paired_tradeoff_summary_score_summary.csv")
summary_score_tests_path <- write_output(summary_scores$tests, "paired_tradeoff_summary_score_tests.csv")
stacked_model_overall_path <- write_output(stacked_models$overall, "paired_tradeoff_stacked_model_overall.csv")
stacked_model_by_domain_path <- write_output(stacked_models$by_domain, "paired_tradeoff_stacked_model_by_domain.csv")

cat("Paired tradeoff disability analysis completed.\n")
cat("Respondents analyzed: ", nrow(analysis_data), "\n", sep = "")
cat("Outputs:\n")
cat(" - ", item_response_path, "\n", sep = "")
cat(" - ", domain_applicability_summary_path, "\n", sep = "")
cat(" - ", domain_applicability_tests_path, "\n", sep = "")
cat(" - ", domain_mean_summary_path, "\n", sep = "")
cat(" - ", domain_mean_gaps_path, "\n", sep = "")
cat(" - ", domain_gap_diff_path, "\n", sep = "")
cat(" - ", domain_mean_tests_path, "\n", sep = "")
cat(" - ", paired_contrast_summary_path, "\n", sep = "")
cat(" - ", paired_contrast_tests_path, "\n", sep = "")
cat(" - ", summary_scores_path, "\n", sep = "")
cat(" - ", summary_score_tests_path, "\n", sep = "")
cat(" - ", stacked_model_overall_path, "\n", sep = "")
cat(" - ", stacked_model_by_domain_path, "\n", sep = "")
