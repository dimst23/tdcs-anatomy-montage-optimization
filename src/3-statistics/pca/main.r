#!/usr/bin/env Rscript
# ==============================================================================
# PCA Analysis of GLM Residuals
# Purpose: Principal Component Analysis of anatomical sensitivity patterns
#
# Usage:
#   Rscript main.r <JSON_SETTINGS_PATH> <BASE_DIR> [PARCEL_AVERAGES_PATH]
#
# Arguments:
#   JSON_SETTINGS_PATH    - Path to JSON configuration file
#   BASE_DIR              - Base directory for data paths
#   PARCEL_AVERAGES_PATH  - (Optional) Path to parcel averages CSV file for correlation analysis
#
# Example:
#   Rscript main.r config.json /path/to/base
#   Rscript main.r config.json /path/to/base data/parcel_stats_total.csv
# ==============================================================================

# Source module dependencies
# Determine script directory (works with both Rscript and source())
get_script_dir <- function() {
  # Try commandArgs (works with Rscript)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("--file=", "", file_arg[1])
    return(dirname(normalizePath(script_path)))
  }

  # Try sys.frame (works when sourced)
  tryCatch(
    {
      ofile <- sys.frame(1)$ofile
      if (!is.null(ofile)) {
        return(dirname(normalizePath(ofile)))
      }
    },
    error = function(e) {}
  )

  # Fallback to current working directory
  return(getwd())
}

script_dir <- get_script_dir()
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))
source(file.path(script_dir, "plots.r"))
source(file.path(script_dir, "calculations.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))
# Source GLM functions for extract_bvalues
source(file.path(dirname(script_dir), "glm", "functions.r"))

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 2,
  max_args = 3,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR", "PARCEL_AVERAGES_PATH")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR
PARCEL_AVERAGES_PATH <- if (length(cli_args) >= 3) cli_args$PARCEL_AVERAGES_PATH else NULL

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)
subjects <- read_subjects(config$paths$subjects)

# Setup output directories
output_paths <- setup_figure_directories(config$figure$save_path)

# ==============================================================================
# Load Demographics Data
# ==============================================================================

cat("Loading demographics data...\n")

# TODO: Make demographics file path configurable
demographics_path <- file.path(BASE_DIR, config$full_settings$glm$paths$age_file_path)
demographics <- load_demographics_data(demographics_path, subjects)

# ==============================================================================
# Load GLM Results
# ==============================================================================

cat("Loading GLM results...\n")

# Construct path from config
DATA_BASE_PATH <- file.path(config$paths$glm_results, "glm", "per_subject")

glm_results <- list()
for (hemi in config$hemispheres) {
  for (sim_type in config$simulation_types) {
    data_file_name <- paste(
      hemi, config$settings_id, config$space_type, config$network$atlas,
      sim_type, config$ind_variable,
      sep = "."
    )

    sim_data <- jsonlite::read_json(file.path(DATA_BASE_PATH, data_file_name))
    sim_name <- config$simulation_type_names[[sim_type]]
    glm_results[[hemi]][[sim_name]] <- sim_data
  }
}

# Extract b-values
cat("Extracting b-values...\n")
df_bvalues <- extract_bvalues(glm_results, config$hemispheres, subjects)

# Clean up memory
rm(glm_results)
gc()

# ==============================================================================
# Run PCA Analysis
# ==============================================================================

cat("Running PCA analysis...\n")

pca_analysis <- run_pca_analysis_pipeline(
  df_bvalues,
  demographics$age_df,
  demographics$gender_df,
  subjects
)

# ==============================================================================
# Save PCA Results
# ==============================================================================

cat("Saving PCA results...\n")

save_pca_results(
  pca_analysis$pca_result,
  output_paths$csv,
  config$settings_id,
  n_components = 3
)

# ==============================================================================
# Create Network Grouping Metadata
# ==============================================================================

cat("Creating network grouping metadata...\n")

parcel_meta <- df_bvalues %>%
  distinct(parcel, network)

parcel_meta_clean <- create_network_grouping(parcel_meta)

# ==============================================================================
# Generate PCA Biplots
# ==============================================================================

cat("Generating PCA biplots...\n")

pc_combinations <- list(c(1, 2), c(1, 3), c(2, 3))

generate_pca_biplot_series(
  pca_analysis$pca_result,
  parcel_meta_clean,
  pc_combinations,
  output_paths,
  config$settings_id,
  width = 8,
  height = 6,
  title_base = "PCA of Anatomical Sensitivity Patterns Across Montages (Grouped Networks)"
)

# ==============================================================================
# Correlation Analysis
# ==============================================================================

if (!is.null(PARCEL_AVERAGES_PATH)) {
  cat("Loading parcel averages for correlation analysis...\n")

  parcel_averages <- load_parcel_averages(PARCEL_AVERAGES_PATH)

  # Run correlation analysis
  cat("Calculating correlations...\n")

  cor_results <- run_pca_correlation_analysis(
    pca_analysis$pca_result,
    parcel_averages,
    parcel_meta,
    pc_names = c("PC1", "PC2", "PC3")
  )

  # Save correlation results
  for (pc_name in c("PC1", "PC2", "PC3")) {
    pc_results <- cor_results %>% filter(pc == pc_name)
    output_file <- file.path(
      output_paths$csv,
      paste(config$settings_id, "pca", tolower(pc_name), "correlations.csv", sep = ".")
    )
    write.csv(pc_results, output_file, row.names = FALSE)
  }
} else {
  cat("\nWarning: PARCEL_AVERAGES_PATH not provided. Skipping correlation analysis.\n")
  cat("To include correlation analysis, provide the parcel averages file path as the third argument.\n")
}

cat("\nPCA analysis complete!\n")
cat("Results saved to:", output_paths$csv, "\n")
