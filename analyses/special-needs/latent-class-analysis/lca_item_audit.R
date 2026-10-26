library(tidyverse)

`%>%` <- magrittr::`%>%`
str_detect <- stringr::str_detect
tibble <- tibble::tibble

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c("hh_id", "complete", "variable", "label", "safety_responses")))
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
config_values <- config::get(file = file.path(project_root, "R", "data-prep", "config.yml"))
resolve_project_path <- function(path_value) {
  if (grepl("^[A-Za-z]:|^/", path_value)) {
    return(path_value)
  }

  file.path(project_root, path_value)
}

codebook <- read_csv(resolve_project_path(config_values$codebook), show_col_types = FALSE)
variable_list <- read_csv(resolve_project_path(config_values$variable_list), show_col_types = FALSE)
source_view <- "Elmer.safety_survey.v_responses_2025"

normalize_safety_source <- function(.data) {
  if ("hh_id" %in% names(.data) && !"hhid" %in% names(.data)) {
    names(.data)[names(.data) == "hh_id"] <- "hhid"
  }

  .data
}

filter_completed_responses <- function(.data) {
  if (!"complete" %in% names(.data)) {
    return(.data)
  }

  .data[as.character(.data[["complete"]]) %in% c("2", "Complete", "complete"), , drop = FALSE]
}

if (file.exists(file.path(project_root, "safety_responses.RDS"))) {
  safety_responses <- readRDS(file.path(project_root, "safety_responses.RDS"))
} else {
  library(psrcelmer)

  safety_responses <- get_query(paste("SELECT * FROM", source_view)) %>%
    normalize_safety_source() %>%
    filter_completed_responses() %>%
    select(-any_of(c(
      "ip_lon", "ip_lat", "work_loc_1", "work_loc_2", "work_loc_3", "work_loc_4", "work_loc_5",
      "home_lon", "home_lat", "work_lat", "work_lon", "work_loc_flag",
      "home_loc_1", "home_loc_2", "home_loc_3", "home_loc_4", "home_loc_5", "home_loc_flag",
      "invite_lat", "invite_lon"
    )))
}

output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

core_blocks <- list(
  general_attitudes = c(
    "enjoy_travel",
    "cost_consideration",
    "worry",
    "satisfied",
    "travel_satisfaction",
    "safe_option",
    "travel_stressful",
    "transit_travel"
  ),
  emotional_safety_burden = c(
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
    "signal_confidence",
    "worry_destination"
  ),
  practical_safety_burden = c(
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
    "avoid_walk_2"
  ),
  safe_systems_beliefs = c(
    "system_safe",
    "safe_clothes",
    "traffic_enforcement",
    "speed_limits",
    "traffic_deaths",
    "traffic_rules"
  )
)

conditional_blocks <- list(
  driving_specific = c(
    "drive_relax",
    "small_car",
    "safe_car_size"
  ),
  cycling_specific = c(
    "bike_confidence",
    "non_res_streets",
    "res_with_lanes",
    "bike_trails",
    "want_bike"
  ),
  access_and_recreation = c(
    "walk_comfort",
    "bike_comfort",
    "transit_comfort",
    "walk_recreation_comfort",
    "bike_recreation_comfort"
  )
)

excluded_variables <- c(
  "time_start",
  "date_complete",
  "complete",
  "ip_lon",
  "ip_lat",
  "hhid",
  "person_id",
  "platform",
  "comfortable_test",
  "disagree_test",
  "same_answer_prop",
  "same_answer_count",
  "test_q_flag",
  "time_to_complete",
  "time_flag",
  "rsg_rec",
  "incentive_complete",
  "commute_fix_flag",
  "participate",
  "work_mode"
)

excluded_prefixes <- c(
  "home_loc_",
  "work_loc_",
  "home_",
  "work_",
  "invite_"
)

profiling_variables <- c(
  "dest_proximity",
  "safety_ratings",
  "environment_emphasis",
  "own_car",
  "risk_taking",
  "telework_importance",
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
  "walk_trips",
  "physical_close",
  "active_transport_practical",
  "learn_bike",
  "want_learn_bike",
  "can_bike",
  "employ",
  "employment",
  "work_days",
  "telework",
  "telework_freq",
  "commute_freq",
  "office_type",
  "commute_mode",
  "commute_mode_other",
  "transit_freq",
  "bike_freq",
  "walk_freq",
  "tnc_freq",
  "motorcycle_freq",
  "car_freq",
  "caregiver",
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
  "bike_recreation",
  "drive_safely",
  "bike_comfort_cars",
  "signal_confidence",
  "crash_participant",
  "crash_witness",
  "crash_near_miss",
  "security_concern",
  "no_experience",
  "no_share",
  "experience_followup",
  "near_miss_followup",
  "witness_followup",
  "personal_security_followup",
  "age",
  "gender",
  "hhsize",
  "hh_children",
  "numchildren",
  "vehicle_count",
  "hhincome_detailed",
  "disability_person",
  "mobility_aides"
)

profiling_prefixes <- c(
  "crash_",
  "near_miss_",
  "witness_",
  "security_",
  "ethnicity_",
  "race_"
)

overlap_flags <- list(
  general_worry_cluster = c("worry", "traffic_worry", "travel_anxiety", "worry_destination"),
  feel_safe_pair = c("feel_safe", "feel_safe_crash"),
  assault_pair = c("fear_assault", "fear_assault_2"),
  practical_burden_cluster = c(
    "shortest_path_fear",
    "traffic_crash_worry",
    "no_safe_travel",
    "feel_safe",
    "avoid_walk_1"
  )
)

direction_review_variables <- c(
  "enjoy_travel",
  "travel_satisfaction",
  "harrassment",
  "drive_safely",
  "bike_comfort_cars",
  "signal_confidence",
  "system_safe",
  "traffic_rules",
  "overall_safety",
  "feel_safe",
  "feel_safe_crash"
)

normalize_logic <- function(logic_value) {
  cleaned_value <- dplyr::na_if(logic_value, "NA")
  cleaned_value <- dplyr::na_if(cleaned_value, "NULL")
  dplyr::na_if(cleaned_value, "")
}

has_prefix_match <- function(variable_name, prefixes) {
  any(startsWith(variable_name, prefixes))
}

find_block <- function(variable_name, block_list) {
  matched_block <- purrr::keep(names(block_list), ~ variable_name %in% block_list[[.x]])

  if (length(matched_block) == 0) {
    return(NA_character_)
  }

  matched_block[[1]]
}

find_overlap_flags <- function(variable_name) {
  matched_flags <- purrr::keep(names(overlap_flags), ~ variable_name %in% overlap_flags[[.x]])

  if (variable_name %in% direction_review_variables) {
    matched_flags <- c(matched_flags, "direction_review")
  }

  if (length(matched_flags) == 0) {
    return(NA_character_)
  }

  paste(matched_flags, collapse = ";")
}

classify_variable <- function(variable_name) {
  if (variable_name %in% excluded_variables || has_prefix_match(variable_name, excluded_prefixes) || str_detect(variable_name, "(_specify|_sentiment)$")) {
    return(list(role = "exclude", role_source = "exclude_rule"))
  }

  if (!is.na(find_block(variable_name, core_blocks))) {
    return(list(role = "core_indicator", role_source = "manual_core"))
  }

  if (!is.na(find_block(variable_name, conditional_blocks))) {
    return(list(role = "conditional_indicator", role_source = "manual_conditional"))
  }

  if (variable_name %in% profiling_variables || has_prefix_match(variable_name, profiling_prefixes)) {
    return(list(role = "profiling_variable", role_source = "profiling_rule"))
  }

  list(role = "profiling_variable", role_source = "default_profile")
}

infer_response_type <- function(variable_name, observed_values) {
  variable_labels <- codebook %>%
    filter(variable == variable_name) %>%
    pull(label) %>%
    unique()

  if (length(variable_labels) == 0) {
    if (is.numeric(observed_values)) {
      return("numeric_or_integer")
    }

    return("unlabeled")
  }

  lowercase_labels <- stringr::str_to_lower(variable_labels)

  if (all(c("strongly disagree", "disagree", "agree", "strongly agree") %in% lowercase_labels)) {
    return("ordinal_likert")
  }

  if (all(c("seldom/never", "sometimes", "often") %in% lowercase_labels)) {
    return("ordinal_frequency")
  }

  if (all(c("yes", "no") %in% lowercase_labels)) {
    return("binary")
  }

  if (length(variable_labels) <= 2) {
    return("binary_or_two_level")
  }

  "categorical_labeled"
}

summarize_variable <- function(variable_name, variable_logic) {
  cleaned_logic <- normalize_logic(variable_logic)

  if (!variable_name %in% names(safety_responses)) {
    return(tibble(
      variable = variable_name,
      present_in_data = FALSE,
      non_missing_n = NA_integer_,
      missing_n = NA_integer_,
      missing_pct = NA_real_,
      distinct_non_missing_n = NA_integer_,
      min_category_count = NA_integer_,
      max_category_share = NA_real_,
      low_variation_flag = NA,
      sparse_flag = NA,
      response_type = NA_character_,
      skip_logic = cleaned_logic,
      structural_skip_flag = !is.na(cleaned_logic)
    ))
  }

  observed_values <- safety_responses[[variable_name]]
  non_missing_values <- observed_values[!is.na(observed_values)]
  non_missing_n <- length(non_missing_values)
  missing_n <- sum(is.na(observed_values))
  category_counts <- table(non_missing_values)
  distinct_non_missing_n <- dplyr::n_distinct(non_missing_values)
  min_category_count <- if (length(category_counts) == 0) NA_integer_ else as.integer(min(category_counts))
  max_category_share <- if (length(category_counts) == 0) NA_real_ else as.numeric(max(category_counts) / non_missing_n)
  sparse_threshold <- max(5L, floor(non_missing_n * 0.01))

  tibble(
    variable = variable_name,
    present_in_data = TRUE,
    non_missing_n = non_missing_n,
    missing_n = missing_n,
    missing_pct = missing_n / length(observed_values),
    distinct_non_missing_n = distinct_non_missing_n,
    min_category_count = min_category_count,
    max_category_share = max_category_share,
    low_variation_flag = distinct_non_missing_n <= 1 || (!is.na(max_category_share) && max_category_share >= 0.95),
    sparse_flag = !is.na(min_category_count) && min_category_count <= sparse_threshold,
    response_type = infer_response_type(variable_name, non_missing_values),
    skip_logic = cleaned_logic,
    structural_skip_flag = !is.na(cleaned_logic)
  )
}

recommend_action <- function(model_role, present_in_data, low_variation_flag, sparse_flag) {
  if (!isTRUE(present_in_data)) {
    return("missing_from_dataset")
  }

  if (model_role == "exclude") {
    return("exclude")
  }

  if (model_role == "profiling_variable") {
    return("profile_only")
  }

  if (model_role == "conditional_indicator") {
    return("hold_for_subgroup_model")
  }

  if (isTRUE(low_variation_flag)) {
    return("drop_or_recode")
  }

  if (isTRUE(sparse_flag)) {
    return("collapse_or_review")
  }

  "keep_first_pass"
}

variable_inventory <- variable_list %>%
  distinct(variable, description, logic) %>%
  mutate(
    classification = purrr::map(variable, classify_variable),
    model_role = purrr::map_chr(classification, "role"),
    role_source = purrr::map_chr(classification, "role_source"),
    model_block = case_when(
      model_role == "core_indicator" ~ purrr::map_chr(variable, find_block, block_list = core_blocks),
      model_role == "conditional_indicator" ~ purrr::map_chr(variable, find_block, block_list = conditional_blocks),
      TRUE ~ NA_character_
    ),
    review_flags = purrr::map_chr(variable, find_overlap_flags),
    logic = normalize_logic(logic)
  ) %>%
  select(-classification)

audit_table <- variable_inventory %>%
  left_join(
    purrr::map_dfr(seq_len(nrow(variable_inventory)), function(row_number) {
      summarize_variable(variable_inventory$variable[[row_number]], variable_inventory$logic[[row_number]])
    }),
    by = c("variable", "logic" = "skip_logic")
  ) %>%
  mutate(
    recommended_action = purrr::pmap_chr(
      list(model_role, present_in_data, low_variation_flag, sparse_flag),
      recommend_action
    )
  ) %>%
  arrange(factor(model_role, levels = c("core_indicator", "conditional_indicator", "profiling_variable", "exclude")), variable)

write_csv(variable_inventory, file.path(output_dir, "lca_variable_inventory.csv"))
write_csv(audit_table, file.path(output_dir, "lca_item_audit.csv"))
write_csv(
  audit_table %>% filter(model_role == "core_indicator"),
  file.path(output_dir, "lca_first_pass_core_candidates.csv")
)

message("Wrote variable inventory to: ", file.path(output_dir, "lca_variable_inventory.csv"))
message("Wrote item audit to: ", file.path(output_dir, "lca_item_audit.csv"))
message("Core indicator count: ", sum(audit_table$model_role == "core_indicator"))
message("Conditional indicator count: ", sum(audit_table$model_role == "conditional_indicator"))
