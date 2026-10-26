library(tidyverse)
library(psych)

# Helpers
`%>%` <- magrittr::`%>%`

# Load data
script_path <- commandArgs(trailingOnly = FALSE) %>%
  stringr::str_subset("^--file=") %>%
  stringr::str_remove("^--file=")

script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(script_path[[1]]))
} else {
  getwd()
}

output_dir <- file.path(script_dir, "outputs")

data <- readRDS(file.path(output_dir, "core_model_data.rds"))
selection <- read.csv(file.path(output_dir, "curated_core_selection.csv"))
variables <- selection$variable[selection$selection_status == "keep"]

# Subset data to variables of interest
model_data <- data %>% select(all_of(variables))

# Factor sizes to test
factor_counts <- c(4, 5, 9)

fit_metrics_list <- list()
top_loadings_list <- list()

for (n in factor_counts) {
  message(paste("Fitting", n, "factor model..."))
  
  # Run polychoric factor analysis (minres, oblimin)
  # Using polychoric correlation for ordinal data
  fit <- psych::fa(model_data, nfactors = n, fm = "minres", rotate = "oblimin", cor = "poly")
  
  # 1. Fit metrics
  fit_metrics_list[[as.character(n)]] <- tibble(
    n_factors = n,
    BIC = fit$BIC,
    RMSEA = fit$RMSEA[1],
    TLI = fit$TLI,
    RMSR = fit$rms
  )
  
  # 2. Top loadings
  loadings <- as.data.frame(unclass(fit$loadings))
  loadings$variable <- rownames(loadings)
  
  long_loadings <- loadings %>%
    pivot_longer(cols = starts_with("MR"), names_to = "factor", values_to = "loading")
  
  # Orient loadings (standardize sign based on max absolute loading)
  long_loadings <- long_loadings %>%
    group_by(factor) %>%
    mutate(max_abs_loading = loading[which.max(abs(loading))],
           loading = loading * sign(max_abs_loading)) %>%
    ungroup()
  
  top_5 <- long_loadings %>%
    group_by(factor) %>%
    arrange(desc(abs(loading))) %>%
    slice_head(n = 5) %>%
    mutate(n_factors = n) %>%
    ungroup()
  
  top_loadings_list[[as.character(n)]] <- top_5
}

# Combine results
comparison_df <- bind_rows(fit_metrics_list)
top_loadings_df <- bind_rows(top_loadings_list)

# Write outputs
write.csv(comparison_df, "outputs/factor_solution_comparison_4_5_9.csv", row.names = FALSE)
write.csv(top_loadings_df, "outputs/factor_solution_top_loadings_4_5_9.csv", row.names = FALSE)

# Print comparison for summary
print(comparison_df)

# Print top loadings for analysis
# (Optional: just print unique factors for each model)
for (n in factor_counts) {
    cat(paste("\n--- Top Loadings for", n, "Factors ---\n"))
    print(top_loadings_list[[as.character(n)]] %>% select(factor, variable, loading) %>% group_by(factor) %>% slice_head(n=5))
}
