# Shared recodes and preparation for the special-needs analysis scripts.

recode_special_needs_covariates <- function(data) {
  dplyr::mutate(
    data,
    age_bin5 = factor(
      dplyr::case_when(
        age < 18 ~ "Under 18 Years",
        age < 25 ~ "18-24 Years",
        age < 45 ~ "25-44 Years",
        age < 65 ~ "45-64 Years",
        age >= 65 ~ "65 years or older",
        TRUE ~ NA_character_
      ),
      levels = c("Under 18 Years", "18-24 Years", "25-44 Years", "45-64 Years", "65 years or older"),
      ordered = TRUE
    ),
    hhincome_bin3 = factor(
      dplyr::case_when(
        hhincome_detailed %in% c("Under $10,000", "$10,000-$24,999", "$25,000-$34,999", "$35,000-$49,999") ~ "Less than $50,000",
        hhincome_detailed %in% c("$50,000-$74,999", "$75,000-$99,999") ~ "$50,000-$99,999",
        hhincome_detailed %in% c("$100,000-$149,999", "$150,000-$199,999", "$200,000-$249,999", "$250,000 or more") ~ "$100,000 or more",
        TRUE ~ NA_character_
      ),
      levels = c("Less than $50,000", "$50,000-$99,999", "$100,000 or more")
    ),
    gender_simple = factor(dplyr::case_when(
      grepl("^Man", gender) ~ "Man",
      grepl("^Woman", gender) ~ "Woman",
      gender %in% c("Missing Response", "Prefer not to answer", "Another gender identity") ~ NA_character_
    )),
    neighborhood_trips = factor(dplyr::if_else(
      grepl("don't know", neighborhood_trips), NA_character_, neighborhood_trips
    ))
  )
}

predictor_is_valid <- function(x) {
  dplyr::n_distinct(x, na.rm = TRUE) >= 2
}

ordinal_source_variables <- function() {
  c(
    "disability_person", "age", "gender", "numchildren", "hhincome_detailed", "employ",
    "vehicle_count", "work_mode", "rgs_class", "sample_segment", "person_weight",
    "no_safe_travel", "no_harm_travel", "neighborhood_trips"
  )
}

prepare_ordinal_inputs <- function(data = psrc.travelsurvey::get_psrc_sss(ordinal_source_variables())) {
  ordinal_levels <- c("Seldom/never", "Sometimes", "Often")
  ordinal_outcomes <- c("no_safe_travel", "no_harm_travel")
  exp_vars <- c("disability", "age_bin5", "hhincome_bin3", "gender_simple", "neighborhood_trips")

  data2025 <- dplyr::mutate(
    data,
      disability = factor(disability_person, levels = c("No", "Yes")),
      employed = factor(dplyr::case_when(
        employ == "Yes" ~ "Yes",
        employ == "No" ~ "No",
        TRUE ~ NA_character_
      ), levels = c("No", "Yes")),
      has_children = factor(dplyr::case_when(
        numchildren == "0 Children" | is.na(numchildren) ~ "No",
        TRUE ~ "Yes"
      ), levels = c("No", "Yes")),
      no_vehicle = factor(dplyr::case_when(
        vehicle_count == "0 (no vehicles in my household)" ~ "Yes",
        is.na(vehicle_count) | vehicle_count == "Missing Response" ~ NA_character_,
        TRUE ~ "No"
      ), levels = c("No", "Yes")),
      urbanicity = dplyr::case_when(
        rgs_class == "Metro" ~ 5,
        rgs_class == "Core" ~ 4,
        rgs_class == "HCT" ~ 3,
        rgs_class == "CitiesTowns" ~ 2,
        rgs_class == "UU" ~ 1,
        rgs_class == "Rural" ~ 0
      ),
      sample_segment = factor(sample_segment),
      person_weight = suppressWarnings(as.numeric(gsub(",", "", person_weight))),
      no_safe_travel = factor(no_safe_travel, levels = ordinal_levels, ordered = TRUE),
      no_harm_travel = factor(no_harm_travel, levels = ordinal_levels, ordered = TRUE)
    )
  data2025 <- recode_special_needs_covariates(data2025)

  ordinal_complete_data <- dplyr::select(
    data2025,
    dplyr::all_of(c(ordinal_outcomes, exp_vars, "sample_segment", "person_weight"))
  )
  ordinal_model_data <- dplyr::filter(
    ordinal_complete_data,
    stats::complete.cases(ordinal_complete_data),
    person_weight > 0
  )

  valid_predictors <- exp_vars[vapply(
    dplyr::select(ordinal_model_data, dplyr::all_of(exp_vars)),
    predictor_is_valid,
    logical(1)
  )]
  dropped_predictors <- setdiff(exp_vars, valid_predictors)

  if (length(valid_predictors) == 0L) {
    stop("No explanatory variables with at least two observed values remain after filtering.")
  }
  if (length(dropped_predictors) > 0L) {
    message("Dropped predictors with <2 observed values: ", paste(dropped_predictors, collapse = ", "))
  }

  list(
    data2025 = data2025,
    ordinal_model_data = ordinal_model_data,
    exp_vars = exp_vars,
    ordinal_levels = ordinal_levels,
    ordinal_outcomes = ordinal_outcomes,
    valid_predictors = valid_predictors,
    dropped_predictors = dropped_predictors
  )
}
