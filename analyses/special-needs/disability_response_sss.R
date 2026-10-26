library(data.table)
library(psrc.travelsurvey)
library(psrcelmer)
library(survey)

script_path <- sub("--file=", "", commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))])

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

project_paths_file <- file.path(script_dir, "..", "..", "R", "project_paths.R")
if (!file.exists(project_paths_file)) project_paths_file <- file.path(getwd(), "R", "project_paths.R")
source(project_paths_file)
project_root <- detect_project_root(script_dir)
output_dir <- file.path(project_root, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

alpha <- 0.05
respondent_dichotomy <- "disability_person"
respondent_dichotomy_levels <- c("Yes", "No")

test_results_file <- file.path(output_dir, "disability_response_sss_test_results.csv")
significant_details_file <- file.path(output_dir, "disability_response_sss_significant_details.csv")
skipped_tests_file <- file.path(output_dir, "disability_response_sss_skipped_variables.csv")

No <- Yes <- abs_diff_pct_points <- count <- diff_pct_points <- mean_moe <- note <- p_value <- prop <- response <- test <- variable <- NULL

response_vars <- c(
  "enjoy_travel",
  "cost_consideration",
  "worry",
  "dest_proximity",
  "satisfied",
  "safety_ratings",
  "environment_emphasis",
  "own_car",
  "risk_taking",
  "travel_satisfaction",
  "safe_option",
  "travel_stressful",
  "telework_importance",
  "transit_travel",
  "drive",
  "travel_methods_drove",
  "travel_methods_rideshare",
  "travel_methods_carshare",
  "travel_methods_motorcycle",
  "travel_methods_transit",
  "travel_methods_bicycle",
  "travel_methods_e_scooter",
  "travel_methods_walk",
  "travel_methods_none",
  "drive_relax",
  "small_car",
  "walk_trips",
  "physical_close",
  "comfortable_test",
  "active_transport_practical",
  "safe_car_size",
  "learn_bike",
  "want_learn_bike",
  "can_bike",
  "bike_confidence",
  "res_with_lanes",
  "bike_trails",
  "want_bike",
  "employ",
  "employment",
  "work_days",
  "telework",
  "telework_freq",
  "commute_freq",
  "office_type",
  "work_loc_1",
  "work_loc_2",
  "work_loc_3",
  "work_loc_4",
  "work_loc_5",
  "commute_mode",
  "commute_mode_other",
  "transit_freq",
  "bike_freq",
  "walk_freq",
  "tnc_freq",
  "motorcycle_freq",
  "car_freq",
  "caregiver",
  "walk_comfort",
  "walk_comfort_traffic",
  "walk_comfort_infra",
  "walk_comfort_comfort",
  "walk_comfort_security",
  "walk_comfort_maintenance",
  "walk_comfort_perception",
  "walk_comfort_far",
  "walk_comfort_dist",
  "walk_comfort_dislike",
  "walk_comfort_other",
  "bike_comfort",
  "bike_comfort_traffic",
  "bike_comfort_infra",
  "bike_comfort_comfort",
  "bike_comfort_security",
  "bike_comfort_maintenance",
  "bike_comfort_perception",
  "bike_comfort_far",
  "bike_comfort_distance",
  "bike_comfort_own",
  "bike_comfort_dislike",
  "bike_comfort_other",
  "transit_comfort",
  "transit_comfort_traffic",
  "transit_comfort_infra",
  "transit_comfort_schedule",
  "transit_comfort_comfort",
  "transit_comfort_comfort_bus",
  "transit_comfort_security",
  "transit_comfort_perception",
  "transit_comfort_mismatch",
  "transit_comfort_available",
  "transit_comfort_challenge",
  "transit_comfort_dislike",
  "transit_comfort_other",
  "neighborhood_trips",
  "walk_recreation",
  "walk_recreation_comfort",
  "bike_recreation",
  "bike_recreation_comfort",
  "traffic_worry",
  "harrassment",
  "urgent_worry",
  "walk_recreation_relax",
  "travel_anxiety",
  "overall_safety",
  "travel_worry",
  "drive_safely",
  "bike_comfort_cars",
  "alert_traveler",
  "drive_worry",
  "signal_confidence",
  "worry_destination",
  "safety_choices",
  "safety_choices_costs",
  "safety_choices_modes",
  "worry_security",
  "shortest_path_fear",
  "shortest_path_assault",
  "traffic_crash_worry",
  "traffic_harrassment",
  "no_safe_travel",
  "no_harm_travel",
  "feel_safe",
  "fear_assault",
  "feel_safe_crash",
  "fear_assault_2",
  "avoid_walk_1",
  "avoid_walk_2",
  "system_safe",
  "safe_clothes",
  "traffic_enforcement",
  "speed_limits",
  "disagree_test",
  "traffic_deaths",
  "traffic_rules",
  "crash_participant",
  "crash_witness",
  "crash_near_miss",
  "security_concern",
  "no_experience",
  "no_share",
  "experience_followup",
  "crash_when",
  "crash_mode",
  "crash_travelling_other",
  "crash_time",
  "crash_time_other",
  "crash_conditions_clear",
  "crash_conditions_cloud",
  "crash_conditions_rain",
  "crash_conditions_snow",
  "crash_conditions_sleet",
  "crash_conditions_fog",
  "crash_conditions_unknown",
  "crash_conditions_other",
  "crash_conditions_unsure",
  "crash_conditions_pnta",
  "crash_involved",
  "crash_number_people",
  "crash_other_mode",
  "crash_other_tnc",
  "crash_other_motorcycle",
  "crash_other_car",
  "crash_other_bike",
  "crash_other_escooter",
  "crash_other_walk",
  "crash_other_other",
  "crash_other_unsure",
  "crash_other_pnta",
  "crash_severity",
  "crash_worry_self",
  "crash_worry_others",
  "crash_change_travel",
  "crash_change_time",
  "crash_travel_less",
  "crash_none",
  "crash_share_other",
  "crash_share_sentiment",
  "near_miss_followup",
  "near_miss_when",
  "near_miss_mode",
  "near_miss_time",
  "near_miss_clear",
  "near_miss_cloudy",
  "near_miss_rain",
  "near_miss_snow",
  "near_miss_sleet",
  "near_miss_fog",
  "near_miss_unknown",
  "near_miss_other",
  "near_miss_unsure",
  "near_miss_pnta",
  "near_miss_other_party",
  "near_miss_num_people",
  "near_miss_care",
  "near_miss_tnc",
  "near_miss_motorcylce",
  "near_miss_driving",
  "near_miss_bike",
  "near_miss_scooter",
  "near_miss_walking",
  "near_miss_mode_other_party",
  "near_miss_mode_unsure",
  "near_miss_mode_pnta",
  "near_miss_worry_self",
  "near_miss_worry_others",
  "near_miss_change_travel",
  "near_miss_change_time",
  "near_miss_travel_less",
  "near_miss_none",
  "witness_followup",
  "witness_when",
  "witness_num_people",
  "witness_mode_driving",
  "witness_mode_tnc",
  "witness_mode_motorcycle",
  "witness_mode_transit",
  "witness_mode_bike",
  "witness_mode_scooter",
  "witness_mode_walk",
  "witness_mode_other",
  "witness_mode_unsure",
  "witness_mode_pnta",
  "witness_time",
  "witness_clear",
  "witness_cloud",
  "witness_rain",
  "witness_snow",
  "witness_sleet",
  "witness_fog",
  "witness_unsure",
  "witness_severity",
  "witness_worry_self",
  "witness_worry_others",
  "witness_change_travel",
  "witness_change_time",
  "witness_travel_less",
  "witness_none",
  "personal_security_followup",
  "security_smoking",
  "security_music",
  "security_yelling",
  "security_touching",
  "security_comments",
  "security_intimidation",
  "security_erratic",
  "security_approaching",
  "security_lewd",
  "security_violence",
  "security_property",
  "security_other",
  "mobility_aides",
  "hhsize",
  "numchildren",
  "vehicle_count",
  "hhincome_detailed",
  "participate",
  "witness_other",
  "non_res_streets",
  "work_mode"
)

column_sql <- paste(
  "SELECT c.name",
  "FROM sys.columns AS c",
  "INNER JOIN sys.objects AS o",
  "ON c.object_id = o.object_id",
  "WHERE o.type = 'V'",
  "AND o.name = 'v_responses_2025'",
  "ORDER BY c.column_id;"
)

available_columns <- psrcelmer::get_query(column_sql)[["name"]]

if (!respondent_dichotomy %in% available_columns) {
  stop(sprintf("`%s` was not found in safety_survey.v_responses_2025.", respondent_dichotomy))
}

available_vars <- intersect(response_vars, available_columns)
missing_vars <- setdiff(response_vars, available_vars)

if (length(available_vars) == 0L) {
  stop("None of the requested response variables were found in safety_survey.v_responses_2025.")
}

if (length(missing_vars) > 0L) {
  message(
    "Skipping variables not found in safety_survey.v_responses_2025: ",
    paste(missing_vars, collapse = ", ")
  )
}

sss_data <- as.data.table(
  get_psrc_sss(unique(c(respondent_dichotomy, available_vars)))
)

sss_data[, (respondent_dichotomy) := trimws(as.character(get(respondent_dichotomy)))]

available_dichotomy_levels <- sort(unique(sss_data[[respondent_dichotomy]]))
available_dichotomy_levels <- available_dichotomy_levels[
  !is.na(available_dichotomy_levels) & nzchar(available_dichotomy_levels)
]

if (all(respondent_dichotomy_levels %in% available_dichotomy_levels)) {
  respondent_dichotomy_levels <- respondent_dichotomy_levels
} else if (length(available_dichotomy_levels) == 2L) {
  respondent_dichotomy_levels <- available_dichotomy_levels
  message(
    sprintf(
      "Using detected `%s` levels: %s",
      respondent_dichotomy,
      paste(respondent_dichotomy_levels, collapse = " vs ")
    )
  )
} else {
  stop(
    sprintf(
      "`%s` must have exactly two non-missing levels. Found %s.",
      respondent_dichotomy,
      length(available_dichotomy_levels)
    )
  )
}

sss_data <- sss_data[
  get(respondent_dichotomy) %in% respondent_dichotomy_levels &
    !is.na(person_weight) &
    person_weight > 0 &
    !is.na(sample_segment)
]

sss_data[, (respondent_dichotomy) := factor(get(respondent_dichotomy), levels = respondent_dichotomy_levels)]

format_prop <- function(x) {
  sprintf("%.1f%%", 100 * x)
}

format_num <- function(x, digits = 2L) {
  ifelse(is.na(x), NA_character_, format(round(x, digits), nsmall = digits, trim = TRUE))
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

compute_weighted_smd <- function(dt, var_name) {
  comparison_groups <- respondent_dichotomy_levels

  if (length(comparison_groups) != 2L) {
    return(NA_real_)
  }

  group_1 <- dt[get(respondent_dichotomy) == comparison_groups[1]]
  group_2 <- dt[get(respondent_dichotomy) == comparison_groups[2]]

  if (nrow(group_1) == 0L || nrow(group_2) == 0L) {
    return(NA_real_)
  }

  mean_1 <- stats::weighted.mean(group_1[[var_name]], group_1[["person_weight"]])
  mean_2 <- stats::weighted.mean(group_2[[var_name]], group_2[["person_weight"]])
  var_1 <- weighted_var(group_1[[var_name]], group_1[["person_weight"]])
  var_2 <- weighted_var(group_2[[var_name]], group_2[["person_weight"]])
  pooled_sd <- sqrt(mean(c(var_1, var_2), na.rm = TRUE))

  if (is.na(pooled_sd) || pooled_sd == 0) {
    return(NA_real_)
  }

  (mean_1 - mean_2) / pooled_sd
}

compute_weighted_cramers_v <- function(design, var_name) {
  contingency_formula <- as.formula(sprintf("~ `%s` + `%s`", respondent_dichotomy, var_name))
  contingency_table <- tryCatch(
    svytable(contingency_formula, design),
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

prep_response_data <- function(dt, var_name) {
  values <- dt[[var_name]]
  keep <- !is.na(values)

  if (is.character(values) || is.factor(values)) {
    keep <- keep & trimws(as.character(values)) != "Prefer not to say"
  }

  dt[keep]
}

ensure_parent_dir <- function(file_path) {
  dir_path <- dirname(file_path)

  if (!identical(dir_path, ".") && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

normalize_for_csv <- function(dt) {
  out <- copy(as.data.table(dt))
  factor_cols <- names(out)[vapply(out, is.factor, logical(1))]

  for (col_name in factor_cols) {
    out[[col_name]] <- as.character(out[[col_name]])
  }

  out
}

safe_fwrite <- function(dt, file_path) {
  ensure_parent_dir(file_path)
  out_dt <- normalize_for_csv(dt)

  write_result <- tryCatch(
    {
      fwrite(out_dt, file_path)
      file_path
    },
    error = function(e) {
      fallback_path <- file.path(
        dirname(file_path),
        paste0(tools::file_path_sans_ext(basename(file_path)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      )
      fwrite(out_dt, fallback_path)
      message(
        "Could not write to ", file_path, ": ", e$message,
        " Writing to ", fallback_path, " instead."
      )
      fallback_path
    }
  )

  write_result
}

format_categorical_detail <- function(detail_dt, var_name, effect_size = NA_real_, effect_size_type = NA_character_) {
  response_col <- setdiff(
    names(detail_dt),
    c(respondent_dichotomy, "count", "prop", "prop_moe", "est", "est_moe")
  )

  if (length(response_col) != 1L) {
    return(detail_dt)
  }

  detail_copy <- copy(detail_dt)
  setnames(detail_copy, response_col, "response")

  wide_dt <- dcast(
    detail_copy[, c("response", respondent_dichotomy, "prop"), with = FALSE],
    as.formula(sprintf("response ~ `%s`", respondent_dichotomy)),
    value.var = "prop"
  )

  comparison_groups <- respondent_dichotomy_levels[respondent_dichotomy_levels %in% names(wide_dt)]

  if (length(comparison_groups) == 2L) {
    wide_dt[["diff_pct_points"]] <- 100 * (
      wide_dt[[comparison_groups[1]]] - wide_dt[[comparison_groups[2]]]
    )
    wide_dt[["abs_diff_pct_points"]] <- abs(wide_dt[["diff_pct_points"]])
    wide_dt <- wide_dt[order(wide_dt[["abs_diff_pct_points"]], decreasing = TRUE, na.last = TRUE)]
    wide_dt[["abs_diff_pct_points"]] <- NULL
  }

  for (group_name in respondent_dichotomy_levels) {
    if (group_name %in% names(wide_dt)) {
      wide_dt[[group_name]] <- format_prop(wide_dt[[group_name]])
    }
  }

  if ("diff_pct_points" %in% names(wide_dt)) {
    wide_dt[["diff_pct_points"]] <- sprintf("%.1f", wide_dt[["diff_pct_points"]])
  }

  wide_dt[["variable"]] <- var_name
  wide_dt[["effect_size_type"]] <- effect_size_type
  wide_dt[["effect_size"]] <- format_num(effect_size, digits = 3L)
  setcolorder(
    wide_dt,
    c("variable", "effect_size_type", "effect_size", "response", setdiff(names(wide_dt), c("variable", "effect_size_type", "effect_size", "response")))
  )
  wide_dt[]
}

format_numeric_detail <- function(detail_dt, var_name, effect_size = NA_real_, effect_size_type = NA_character_) {
  out <- copy(detail_dt)[, c(respondent_dichotomy, "count", "mean", "mean_moe", "median"), with = FALSE]
  out[["variable"]] <- var_name
  out[["effect_size_type"]] <- effect_size_type
  out[["effect_size"]] <- format_num(effect_size, digits = 3L)
  out[["mean"]] <- format_num(out[["mean"]])
  out[["mean_moe"]] <- format_num(out[["mean_moe"]])
  out[["median"]] <- format_num(out[["median"]])
  setcolorder(out, c("variable", "effect_size_type", "effect_size", respondent_dichotomy, "count", "mean", "mean_moe", "median"))
  out[]
}

analyze_response <- function(var_name, dt) {
  test_data <- prep_response_data(dt, var_name)
  response_values <- test_data[[var_name]]

  test_row <- data.table(
    variable = var_name,
    test = NA_character_,
    p_value = NA_real_,
    effect_size_type = NA_character_,
    effect_size = NA_real_,
    n = nrow(test_data),
    levels = uniqueN(response_values),
    note = NA_character_
  )

  if (nrow(test_data) == 0L) {
    test_row[["note"]] <- sprintf(
      "No `%s` responses remained after filtering.",
      respondent_dichotomy
    )
    return(list(test = test_row, detail = NULL))
  }

  if (uniqueN(test_data[[respondent_dichotomy]]) < 2L) {
    test_row[["note"]] <- sprintf(
      "Only one `%s` group remained after filtering.",
      respondent_dichotomy
    )
    return(list(test = test_row, detail = NULL))
  }

  if (test_row[["levels"]] < 2L) {
    test_row[["note"]] <- "Response has fewer than two non-missing values."
    return(list(test = test_row, detail = NULL))
  }

  if ((is.character(response_values) || is.factor(response_values)) && uniqueN(response_values) > 25L) {
    test_row[["note"]] <- "Skipped high-cardinality text response."
    return(list(test = test_row, detail = NULL))
  }

  design <- svydesign(
    ids = ~1,
    strata = ~sample_segment,
    weights = ~person_weight,
    data = test_data,
    nest = TRUE
  )

  if (is.numeric(response_values) || is.integer(response_values)) {
    test_formula <- as.formula(sprintf("`%s` ~ `%s`", var_name, respondent_dichotomy))
    test_object <- tryCatch(
      svyttest(test_formula, design),
      error = function(e) e
    )

    test_row[["test"]] <- "svyttest"

    if (inherits(test_object, "error")) {
      test_row[["note"]] <- test_object$message
      return(list(test = test_row, detail = NULL))
    }

    effect_size <- compute_weighted_smd(test_data, var_name)
    test_row[["effect_size_type"]] <- "weighted_smd"
    test_row[["effect_size"]] <- effect_size

    detail_dt <- psrc_sss_stat(
      test_data,
      group_vars = respondent_dichotomy,
      stat_var = var_name,
      incl_na = FALSE
    )

    test_row[["p_value"]] <- unname(test_object$p.value)

    return(list(
      test = test_row,
      detail = format_numeric_detail(
        as.data.table(detail_dt),
        var_name,
        effect_size = effect_size,
        effect_size_type = test_row[["effect_size_type"]]
      )
    ))
  }

  test_formula <- as.formula(sprintf("~ `%s` + `%s`", respondent_dichotomy, var_name))
  test_object <- tryCatch(
    suppressWarnings(svychisq(test_formula, design, statistic = "F")),
    error = function(e) e
  )

  test_row[["test"]] <- "svychisq"

  if (inherits(test_object, "error")) {
    test_row[["note"]] <- test_object$message
    return(list(test = test_row, detail = NULL))
  }

  effect_size <- compute_weighted_cramers_v(design, var_name)
  test_row[["effect_size_type"]] <- "cramers_v"
  test_row[["effect_size"]] <- effect_size

  detail_dt <- psrc_sss_stat(
    test_data,
    group_vars = c(respondent_dichotomy, var_name),
    incl_na = FALSE
  )

  test_row[["p_value"]] <- unname(test_object$p.value)

  list(
    test = test_row,
    detail = format_categorical_detail(
      as.data.table(detail_dt),
      var_name,
      effect_size = effect_size,
      effect_size_type = test_row[["effect_size_type"]]
    )
  )
}

analysis_list <- lapply(available_vars, analyze_response, dt = sss_data)
test_results <- rbindlist(lapply(analysis_list, `[[`, "test"), fill = TRUE)
detail_lookup <- setNames(lapply(analysis_list, `[[`, "detail"), available_vars)

completed_tests <- test_results[!is.na(p_value)]
significant_tests <- completed_tests[p_value < alpha][order(p_value)]
skipped_tests <- test_results[is.na(p_value)]

ensure_parent_dir(test_results_file)
ensure_parent_dir(significant_details_file)
ensure_parent_dir(skipped_tests_file)

test_results_path <- safe_fwrite(test_results, test_results_file)
skipped_tests_path <- safe_fwrite(skipped_tests, skipped_tests_file)

significant_detail_tables <- lapply(significant_tests[["variable"]], function(var_name) {
  detail_lookup[[var_name]]
})
significant_detail_tables <- significant_detail_tables[!vapply(significant_detail_tables, is.null, logical(1))]

if (length(significant_detail_tables) > 0L) {
  significant_details <- rbindlist(lapply(significant_detail_tables, normalize_for_csv), fill = TRUE)
  significant_details_path <- safe_fwrite(significant_details, significant_details_file)
} else {
  significant_details_path <- safe_fwrite(data.table(variable = character()), significant_details_file)
}

cat(
  "Tested ", nrow(completed_tests), " of ", length(available_vars),
  " available variables at alpha = ", alpha, ".\n",
  sep = ""
)

cat(
  "Saved full test results to ", test_results_path,
  ", significant weighted details to ", significant_details_path,
  ", and skipped variables to ", skipped_tests_path, ".\n",
  sep = ""
)

if (nrow(skipped_tests) > 0L) {
  cat("Skipped ", nrow(skipped_tests), " variables with insufficient variation or unsupported text responses.\n", sep = "")
}

if (nrow(significant_tests) == 0L) {
  cat("No statistically significant differences were found.\n")
} else {
  cat(
    "\nSignificant differences in 2025 safety survey responses by ",
    respondent_dichotomy,
    " (",
    respondent_dichotomy_levels[1],
    " vs ",
    respondent_dichotomy_levels[2],
    ")\n",
    sep = ""
  )
  print(significant_tests[, .(variable, test, p_value, n)])

  cat("\nWeighted summaries for significant variables\n")
  for (var_name in significant_tests[["variable"]]) {
    cat("\n", var_name, " (p = ", signif(significant_tests[variable == var_name, p_value][1], 4), ")\n", sep = "")
    print(detail_lookup[[var_name]])
  }
}
