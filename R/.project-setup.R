# Project Setup and Configuration
# This file should be sourced by all analysis scripts to ensure consistent paths
# and access to shared functions.

# Load the side-effect-free root finder from this setup script's own location.
setup_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(setup_file) || !length(setup_file) || is.na(setup_file[[1]]) || !nzchar(setup_file[[1]])) {
  setup_path <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  setup_file <- if (length(setup_path)) sub("^--file=", "", setup_path[[1]]) else NULL
}
if (!is.null(setup_file) && length(setup_file) && !is.na(setup_file[[1]]) && nzchar(setup_file[[1]])) {
  setup_file <- normalizePath(setup_file[[1]], winslash = "/", mustWork = FALSE)
}

# During Quarto renders, --file can point to a temporary runner instead of
# this project. Search the source and working-directory ancestors for the
# shared path helper.
project_paths_file <- NULL
for (start_path in unique(c(
  Sys.getenv("SURVEY_PROJECT_ROOT", unset = ""),
  if (!is.null(setup_file) && length(setup_file)) dirname(setup_file) else "",
  getwd()
))) {
  if (!nzchar(start_path)) next
  start_path <- normalizePath(start_path, winslash = "/", mustWork = FALSE)
  if (file.exists(start_path) && !dir.exists(start_path)) start_path <- dirname(start_path)
  repeat {
    candidate <- file.path(start_path, "R", "project_paths.R")
    if (file.exists(candidate)) {
      project_paths_file <- candidate
      break
    }
    parent_path <- dirname(start_path)
    if (identical(parent_path, start_path)) break
    start_path <- parent_path
  }
  if (!is.null(project_paths_file)) break
}
if (is.null(project_paths_file)) {
  stop("Could not locate R/project_paths.R. Set SURVEY_PROJECT_ROOT or run within the safety-security-studies project.")
}
source(project_paths_file)

# Initialize project paths (execute on load)
if (!exists("survey_project_root")) {
  survey_project_root <<- detect_project_root(if (length(setup_file) > 0L && !is.na(setup_file[[1]])) dirname(setup_file[[1]]) else getwd())
  project_root <<- survey_project_root
  
  # Assign standard paths
  survey_data_prep_dir <<- file.path(survey_project_root, "R", "data-prep")
  survey_functions_dir <<- file.path(survey_project_root, "R", "functions")
  survey_data_dir <<- file.path(survey_project_root, "data")
  survey_analyses_dir <<- file.path(survey_project_root, "analyses", "special-needs")
  survey_outputs_dir <<- file.path(survey_project_root, "outputs")
  
  # Message for debugging (commented out for production)
  # message("Survey project root: ", survey_project_root)
}

# Source core data preparation helpers
if (!exists("load_safety_responses")) {
  source(file.path(survey_data_prep_dir, "core_prep_helpers.R"))
}

# Source analysis functions
if (!exists("plot_single")) {
  source(file.path(survey_functions_dir, "functions.R"))
}

# Load configuration if it exists
if (!exists("config") && file.exists(file.path(survey_data_prep_dir, "config.yml"))) {
  tryCatch({
    config <<- config::get(file = file.path(survey_data_prep_dir, "config.yml"))
  }, error = function(e) {
    message("Note: Could not load config.yml - config package may not be installed")
  })
}
