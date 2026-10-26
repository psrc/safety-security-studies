library(tidyverse)
library(bit64)

`%>%` <- magrittr::`%>%`
read_csv <- readr::read_csv
write_csv <- readr::write_csv
write_rds <- readr::write_rds
tibble <- tibble::tibble

if (getRversion() >= "2.15.1") {
  invisible(utils::globalVariables(c("variable", "value", "label", "hhsize", "hhincome_detailed", "hhsize_int", "min_income", "income_threshold")))
}

# Determine project root directory
# Scripts are in analyses/special-needs/factor-analysis/, so go up 3 levels
script_dir <- tryCatch({
  dirname(normalizePath(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))] %>% 
    sub("--file=", "", .), winslash = "/"))
}, error = function(e) getwd())

project_paths_file <- file.path(script_dir, "..", "..", "..", "R", "project_paths.R")
if (!file.exists(project_paths_file)) project_paths_file <- file.path(getwd(), "R", "project_paths.R")
source(project_paths_file)
project_root <- detect_project_root(script_dir)

# Fallback if the above doesn't work

survey_data_prep_dir <- file.path(project_root, "R", "data-prep")
survey_data_dir <- file.path(project_root, "data")
output_dir <- file.path(script_dir, "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source_view <- "Elmer.safety_survey.v_responses_2025"

# Source core data preparation helpers
source(file.path(survey_data_prep_dir, "core_prep_helpers.R"))

safety_responses <- load_safety_responses(project_root, source_view = source_view)

safety_responses <- safety_responses %>%
  normalize_safety_source() %>%
  filter_completed_responses() %>%
  dplyr::select(-any_of(c(
    "ip_lon", "ip_lat", "work_loc_1", "work_loc_2", "work_loc_3", "work_loc_4", "work_loc_5",
    "home_lon", "home_lat", "work_lat", "work_lon", "work_loc_flag",
    "home_loc_1", "home_loc_2", "home_loc_3", "home_loc_4", "home_loc_5", "home_loc_flag",
    "invite_lat", "invite_lon"
  ))) %>%
  derive_demographic_flags()

codebook <- read_csv(file.path(survey_data_dir, "value_labels.csv"), show_col_types = FALSE)
variable_descriptions <- read_csv(file.path(survey_data_dir, "variable_description.csv"), show_col_types = FALSE)

curated_selection <- tribble(
  ~variable,                     ~selection_status, ~selection_reason,                                                                                                           ~direction_rule, ~collapse_rule,
  "enjoy_travel",               "keep",           "Add a global travel-affect item and reverse it so higher scores indicate less enjoyment.",                              "reverse",      "none",
  "cost_consideration",         "keep",           "Add a broad day-to-day travel constraint item that is fully observed and ordinal.",                                       "keep",         "none",
  "worry",                      "keep",           "Keep one broad predisposition item for generalized worry.",                                                              "keep",         "none",
  "dest_proximity",             "keep",           "Add neighborhood-access preference because it is fully observed and may define a built-environment attitude dimension.",   "keep",         "none",
  "satisfied",                  "keep",           "Add general life satisfaction and reverse it so higher scores indicate lower satisfaction.",                               "reverse",      "none",
  "safety_ratings",             "keep",           "Add a direct safety-orientation item about paying attention to vehicle safety ratings.",                                  "keep",         "none",
  "environment_emphasis",       "keep",           "Add the environment-versus-travel tradeoff attitude item as a distinct worldview measure.",                                "keep",         "none",
  "own_car",                    "keep",           "Add car-ownership orientation because it is fully observed and likely informative for broader travel attitudes.",          "keep",         "none",
  "risk_taking",                "keep",           "Add a direct risk-tolerance item that may separate from safety worry.",                                                  "keep",         "none",
  "travel_satisfaction",        "keep",           "Add daily travel satisfaction and reverse it so higher scores indicate lower travel satisfaction.",                       "reverse",      "none",
  "safe_option",                "keep",           "Add the general preference for safe choices and reverse it so higher scores indicate less safety-oriented behavior.",    "reverse",      "none",
  "travel_stressful",           "keep",           "Keep a direct day-to-day travel burden item.",                                                                           "keep",         "none",
  "telework_importance",        "keep",           "Add telework importance because it is fully observed and may reflect safety-driven flexibility preferences.",             "keep",         "none",
  "transit_travel",             "keep",           "Add personal transit affinity because it is fully observed and may define a mode-orientation factor.",                   "keep",         "none",
  "walk_trips",                 "keep",           "Add an active-travel orientation item that is fully observed across the sample.",                                         "keep",         "none",
  "physical_close",             "keep",           "Add discomfort with close proximity to strangers because it is conceptually adjacent to personal security burden.",      "keep",         "none",
  "active_transport_practical", "keep",           "Add the perceived practicality barrier for walking and bicycling.",                                                      "keep",         "none",
  "traffic_worry",              "keep",           "Core day-to-day crash worry item.",                                                                                      "keep",         "none",
  "harrassment",                "keep",           "Core personal security item; reverse so higher means more burden.",                                                      "reverse",      "none",
  "urgent_worry",               "keep",           "Captures safety constraints on urgent access.",                                                                          "keep",         "none",
  "walk_recreation_relax",      "keep",           "Add a positive walking/rolling affect item and reverse it so higher scores indicate less relaxation.",                  "reverse",      "none",
  "travel_anxiety",             "keep",           "Captures anticipatory safety anxiety.",                                                                                  "keep",         "none",
  "overall_safety",             "keep",           "Global perceived travel safety; reverse so higher means less safety.",                                                   "reverse",      "none",
  "travel_worry",               "keep",           "Captures concern for companions' safety.",                                                                               "keep",         "none",
  "drive_safely",               "keep",           "Neighborhood driving safety perception; reverse to align direction.",                                                    "reverse",      "none",
  "bike_comfort_cars",          "keep",           "Traffic-exposure comfort item; reverse to align direction.",                                                             "reverse",      "none",
  "alert_traveler",             "keep",           "Captures security vigilance while traveling alone.",                                                                     "keep",         "none",
  "drive_worry",                "drop",           "Keep out of the all-respondent factor block because it is only asked of drivers and would force avoidable complete-case loss.", "keep",      "none",
  "signal_confidence",          "keep",           "Pedestrian safety confidence item; reverse to align direction.",                                                         "reverse",      "none",
  "worry_destination",          "keep",           "Destination safety worry item distinct from global anxiety.",                                                            "keep",         "none",
  "safety_choices",             "keep",           "Core general item for safety shaping travel choices.",                                                                   "keep",         "none",
  "safety_choices_costs",       "keep",           "Captures safety burden through added cost.",                                                                             "keep",         "none",
  "safety_choices_modes",       "keep",           "Captures safety burden through avoided modes.",                                                                          "keep",         "none",
  "worry_security",             "keep",           "Captures practical restriction from personal security concerns.",                                                        "keep",         "none",
  "shortest_path_fear",         "drop",           "Hold for later because the ordinal scale mixes with a structural not-applicable category.",                             "keep",         "none",
  "shortest_path_assault",      "drop",           "Hold for later because the ordinal scale mixes with a structural not-applicable category.",                             "keep",         "none",
  "traffic_crash_worry",        "drop",           "Hold for later because the item stem is context-specific and not-applicable responses complicate baseline comparison.", "keep",         "none",
  "traffic_harrassment",        "drop",           "Hold for later because the item stem is context-specific and not-applicable responses complicate baseline comparison.", "keep",         "none",
  "no_safe_travel",             "drop",           "Hold for later because the item uses a context-specific frequency scale with not-applicable responses.",                "keep",         "none",
  "no_harm_travel",             "drop",           "Hold for later because the item uses a context-specific frequency scale with not-applicable responses.",                "keep",         "none",
  "feel_safe",                  "drop",           "Hold for later because it is part of a context-dependent frequency battery and overlaps with other crash-avoidance items.", "keep",      "none",
  "fear_assault",               "drop",           "Hold for later because it is part of a context-dependent frequency battery.",                                             "keep",         "none",
  "feel_safe_crash",            "drop",           "Drop from this baseline because it overlaps with feel_safe and sits in the same context-dependent battery.",            "keep",         "none",
  "fear_assault_2",             "drop",           "Drop from this baseline because it overlaps with fear_assault and sits in the same context-dependent battery.",         "keep",         "none",
  "avoid_walk_1",               "drop",           "Hold for later because it is part of a context-dependent frequency battery.",                                             "keep",         "none",
  "avoid_walk_2",               "drop",           "Hold for later because it is part of a context-dependent frequency battery.",                                             "keep",         "none",
  "system_safe",                "keep",           "Safe-systems belief item; reverse so higher means less perceived system safety.",                                       "reverse",      "none",
  "safe_clothes",               "keep",           "Retain as a safety-belief item for comparison.",                                                                        "keep",         "none",
  "traffic_enforcement",        "keep",           "Retain as a safety-belief and policy-orientation item.",                                                                "keep",         "none",
  "speed_limits",               "keep",           "Retain as a direct road-safety concern item.",                                                                          "keep",         "none",
  "disagree_test",              "drop",           "Exclude the instructional attention-check item from the factor model.",                                                 "keep",         "none",
  "traffic_deaths",             "keep",           "Retain as a fatalism and safety-belief item.",                                                                          "keep",         "none",
  "traffic_rules",              "keep",           "Traffic-rule confidence item; reverse to align direction.",                                                             "reverse",      "none"
)

selection_with_metadata <- curated_selection %>%
  left_join(variable_descriptions, by = "variable") %>%
  relocate(variable, description, logic, .before = selection_status)

selected_variables <- selection_with_metadata %>%
  filter(selection_status == "keep") %>%
  pull(variable)

prepared_core_data <- safety_responses %>%
  dplyr::select(any_of(c(
    "person_id", "person_weight", "sample_segment", "disability_person",
    "age65plus", "low_income", "race_poc", "zero_vehicles",
    selected_variables
  )))

for (selected_variable in selected_variables) {
  direction_rule <- selection_with_metadata %>%
    filter(variable == selected_variable) %>%
    pull(direction_rule)

  prepared_core_data[[selected_variable]] <- apply_direction_rule(
    prepared_core_data[[selected_variable]],
    selected_variable,
    direction_rule,
    codebook
  )
}

prepared_core_data <- prepared_core_data %>%
  mutate(complete_case_core = complete.cases(across(any_of(selected_variables))))

preprocessing_summary <- tibble(
  total_rows = nrow(prepared_core_data),
  complete_case_rows = sum(prepared_core_data$complete_case_core),
  selected_variable_count = length(selected_variables),
  reversed_variable_count = sum(selection_with_metadata$selection_status == "keep" & selection_with_metadata$direction_rule == "reverse"),
  collapsed_variable_count = sum(selection_with_metadata$selection_status == "keep" & selection_with_metadata$collapse_rule != "none")
)

write_csv(selection_with_metadata, file.path(output_dir, "curated_core_selection.csv"))
write_csv(prepared_core_data, file.path(output_dir, "core_model_data.csv"))
write_rds(prepared_core_data, file.path(output_dir, "core_model_data.rds"))
write_csv(preprocessing_summary, file.path(output_dir, "core_preprocessing_summary.csv"))

message("Wrote curated selection to: ", file.path(output_dir, "curated_core_selection.csv"))
message("Wrote model-ready core data to: ", file.path(output_dir, "core_model_data.rds"))
message("Selected variables: ", length(selected_variables))
message("Complete-case rows for comparison: ", sum(prepared_core_data$complete_case_core))
