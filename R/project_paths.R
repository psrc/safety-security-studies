# Project path helpers. This file has no side effects when sourced.

detect_project_root <- function(start_path = getwd()) {
  is_project_root <- function(path) {
    file.exists(file.path(path, "safety-security-studies.Rproj")) ||
      (dir.exists(file.path(path, "R", "data-prep")) &&
         dir.exists(file.path(path, "analyses", "special-needs")))
  }

  starts <- c(Sys.getenv("SURVEY_PROJECT_ROOT", unset = NA_character_), start_path, getwd())
  starts <- starts[!is.na(starts) & nzchar(starts)]
  starts <- unique(normalizePath(starts, winslash = "/", mustWork = FALSE))

  candidates <- character()
  for (start in starts) {
    if (file.exists(start) && !dir.exists(start)) start <- dirname(start)
    while (dir.exists(start)) {
      candidates <- c(candidates, start)
      parent <- dirname(start)
      if (identical(parent, start)) break
      start <- parent
    }
  }

  candidates <- unique(candidates)
  for (candidate in candidates) {
    if (is_project_root(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    "Could not locate project root. Set SURVEY_PROJECT_ROOT or run the analysis " ,
    "from within the safety-security-studies project."
  )
}
