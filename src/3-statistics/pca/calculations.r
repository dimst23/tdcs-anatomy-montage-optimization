#!/usr/bin/env Rscript
# ==============================================================================
# PCA Analysis - Calculation Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))

#' Run complete PCA analysis pipeline
#'
#' @param df_bvalues Data frame with b-values from GLM
#' @param age_df Data frame with subject ages
#' @param gender_df Data frame with subject genders
#' @param subjects Vector of subject IDs
#' @return List containing PCA results and derived data frames
run_pca_analysis_pipeline <- function(df_bvalues, age_df, gender_df, subjects) {
  # Merge demographic data
  df_full <- df_bvalues %>%
    left_join(age_df, by = "subject_id") %>%
    left_join(gender_df, by = "subject_id")

  # Calculate residuals controlling for demographics
  cat("Calculating demographic residuals...\n")
  residual_summary <- calculate_demographic_residuals(df_full)

  # Prepare PCA input
  cat("Preparing PCA input matrix...\n")
  pca_input <- prepare_pca_input(residual_summary, metric = "resid_mean")

  # Run PCA
  cat("Running PCA...\n")
  pca_result <- run_pca(pca_input, center = TRUE, scale = TRUE)

  # Print variance explained
  cat("\nVariance explained by first 5 components:\n")
  variance_explained <- summary(pca_result)$importance[2, 1:min(5, ncol(pca_result$x))]
  print(round(variance_explained * 100, 2))

  return(list(
    pca_result = pca_result,
    residual_summary = residual_summary,
    df_full = df_full,
    pca_input = pca_input
  ))
}

#' Load demographic data from TSV file
#'
#' @param csv_path Path to demographics TSV file
#' @param subjects Vector of subject IDs
#' @return List with age_df and gender_df
#'
#' TODO: Make the demographics path configurable through settings
load_demographics_data <- function(csv_path, subjects) {
  if (!file.exists(csv_path)) {
    stop("Demographics file not found at: ", csv_path, call. = FALSE)
  }

  csv_data <- read.csv(csv_path, sep = "\t", header = TRUE)
  csv_data <- csv_data[-1, ] # Remove first row

  # Extract age data
  age_data <- numeric(length(subjects))
  for (i in seq_along(subjects)) {
    age_data[i] <- as.numeric(
      csv_data[csv_data$src_subject_id == subjects[i], "interview_age"]
    ) / 12
  }

  age_df <- data.frame(
    subject_id = subjects,
    age_group = age_groups(age_data),
    age_num = age_data
  )

  # Extract gender data
  gender_data <- character(length(subjects))
  for (i in seq_along(subjects)) {
    gender_data[i] <- csv_data[csv_data$src_subject_id == subjects[i], "sex"]
  }

  gender_df <- data.frame(
    subject_id = subjects,
    gender = gender_data
  )

  return(list(age_df = age_df, gender_df = gender_df))
}

#' Load parcel average data for correlation analysis
#'
#' @param parcel_averages_path Path to combined parcel averages file
#' @return Data frame with parcel averages in wide format (one row per parcel)
load_parcel_averages <- function(parcel_averages_path) {
  if (!file.exists(parcel_averages_path)) {
    stop("Parcel averages file not found: ", parcel_averages_path, call. = FALSE)
  }

  parcel_averages_long <- read_csv(parcel_averages_path, show_col_types = FALSE)

  # Check if data is in long format (has 'variable' column)
  if ("variable" %in% colnames(parcel_averages_long)) {
    # Pivot to wide format: one row per parcel, columns for each variable
    parcel_averages <- parcel_averages_long %>%
      select(parcel, variable, mean) %>%
      pivot_wider(names_from = variable, values_from = mean)
  } else {
    # Already in wide format
    parcel_averages <- parcel_averages_long
  }

  return(parcel_averages)
}

#' Run PCA correlation analysis with morphological variables
#'
#' @param pca_result PCA result object
#' @param parcel_averages Data frame with parcel-level morphological data
#' @param parcel_meta Data frame with parcel metadata
#' @param pc_names Vector of PC names to analyze
#' @return Data frame with correlation results
run_pca_correlation_analysis <- function(pca_result, parcel_averages,
                                         parcel_meta, pc_names = c("PC1", "PC2", "PC3")) {
  # Prepare PCA scores with metadata
  pca_scores <- as.data.frame(pca_result$x) %>%
    rownames_to_column("parcel") %>%
    left_join(parcel_meta, by = "parcel")

  # Merge with parcel averages
  merged_data <- pca_scores %>%
    select(parcel, all_of(pc_names)) %>%
    inner_join(parcel_averages, by = "parcel")

  # Calculate correlations
  cat("\nCalculating correlations between PCA components and morphological variables...\n")
  cor_results <- calculate_pca_correlations(
    pca_scores, parcel_averages, pc_names,
    method = "spearman"
  )

  # Print summary
  for (pc_name in pc_names) {
    pc_results <- cor_results %>% filter(pc == pc_name)
    cat("\n", pc_name, ":\n", sep = "")
    cat(
      "Significant correlations (p_adj < 0.05):",
      sum(pc_results$p_adj < 0.05), "/", nrow(pc_results), "\n"
    )
  }

  return(cor_results)
}
