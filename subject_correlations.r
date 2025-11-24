#!/usr/bin/env Rscript
# ==============================================================================
# Script: subject_correlations.r
# Purpose: Calculate Spearman correlations between PCA components and
#          morphological variables across parcels
# ==============================================================================

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
})

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

# Check if no arguments provided
if (length(args) == 0) {
  cat("\n")
  cat("Missing required arguments\n\n")
  cat("Usage:\n")
  cat("  Rscript subject_correlations.r --pca <pca_file> --input <input_file> --type <1|2> [OPTIONS]\n\n")
  cat("Required Arguments:\n")
  cat("  --pca            : Path to PCA CSV file (parcel, PC1, PC2, PC3)\n")
  cat("  --input          : Path to input data CSV file\n")
  cat("  --type           : Data type (1=population, 2=subject)\n\n")
  cat("Optional Arguments:\n")
  cat("  --output         : Path to output CSV file\n")
  cat("  --no-correction  : Disable FDR multiple testing correction (enabled by default)\n\n")
  cat("Examples:\n")
  cat("  Rscript subject_correlations.r --pca loadings.csv --input population.csv --type 1\n")
  cat("  Rscript subject_correlations.r --pca loadings.csv --input subject.csv --type 2 --output results.csv\n")
  cat("  Rscript subject_correlations.r --pca loadings.csv --input data.csv --type 1 --no-correction\n\n")
  quit(status = 0)
}

# Parse command-line arguments
arg_list <- list()
flags <- character(0)
i <- 1
while (i <= length(args)) {
  if (grepl("^--", args[i])) {
    arg_name <- sub("^--", "", args[i])
    # Check if next argument exists and is not another flag
    if (i < length(args) && !grepl("^--", args[i + 1])) {
      arg_list[[arg_name]] <- args[i + 1]
      i <- i + 2
    } else {
      # This is a flag (no value)
      flags <- c(flags, arg_name)
      i <- i + 1
    }
  } else {
    i <- i + 1
  }
}

# Validate required arguments
required_args <- c("pca", "input", "type")
missing_args <- setdiff(required_args, names(arg_list))
if (length(missing_args) > 0) {
  stop(
    "Error: Missing required arguments: --",
    paste(missing_args, collapse = ", --")
  )
}

# Set variables from arguments
pca_file <- arg_list$pca
input_file <- arg_list$input
data_type_choice <- arg_list$type
output_file <- ifelse(is.null(arg_list$output), "", arg_list$output)

# Set correction flag (default is TRUE, disable with --no-correction)
apply_correction <- !("no-correction" %in% flags)

# Validate data type
if (!(data_type_choice %in% c("1", "2"))) {
  stop("Error: --type must be 1 (population) or 2 (subject)")
}

use_population_data <- (data_type_choice == "1")

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("  Spearman Correlation Analysis: PCA Components vs Variables\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
if (apply_correction) {
  cat("  FDR correction: ENABLED\n")
} else {
  cat("  FDR correction: DISABLED\n")
}
cat("\n")

# ==============================================================================
# Data Loading
# ==============================================================================

cat("\n[1/5] Loading data files...\n")

# Load PCA data
if (!file.exists(pca_file)) {
  stop("Error: PCA file not found at: ", pca_file)
}
pca_data <- read_csv(pca_file, show_col_types = FALSE)
cat("  [OK] Loaded PCA data: ", nrow(pca_data), " parcels\n")

# Load input data
if (!file.exists(input_file)) {
  stop("Error: Input file not found at: ", input_file)
}
input_data <- read_csv(input_file, show_col_types = FALSE)
cat("  [OK] Loaded input data: ", nrow(input_data), " rows\n")

# ==============================================================================
# Data Type Detection
# ==============================================================================

# Detect actual data format
has_variable_col <- "variable" %in% colnames(input_data)
has_mean_col <- "mean" %in% colnames(input_data)
looks_like_population <- has_variable_col && has_mean_col
looks_like_subject <- !has_variable_col && !has_mean_col

# Check for type mismatch
if (use_population_data && looks_like_subject) {
  cat("\n")
  cat("  [!] ERROR: Data format mismatch detected!\n")
  cat("  You specified --type 1 (population data)\n")
  cat("  But the data appears to be in SUBJECT format (wide format with variable columns)\n")
  cat("\n")
  cat("  Expected population format: parcel, variable, mean, std, ...\n")
  cat("  Found subject format: parcel, var1, var2, var3, ...\n")
  cat("\n")
  cat("  Solution: Use --type 2 instead\n")
  cat("  Example: Rscript subject_correlations.r --pca ", pca_file, " --input ", input_file, " --type 2\n")
  cat("\n")
  stop("Data type mismatch. Please use --type 2 for subject data.")
}

if (!use_population_data && looks_like_population) {
  cat("\n")
  cat("  [!] ERROR: Data format mismatch detected!\n")
  cat("  You specified --type 2 (subject data)\n")
  cat("  But the data appears to be in POPULATION format (long format with 'variable' and 'mean' columns)\n")
  cat("\n")
  cat("  Expected subject format: parcel, var1, var2, var3, ...\n")
  cat("  Found population format: parcel, variable, mean, std, ...\n")
  cat("\n")
  cat("  Solution: Use --type 1 instead\n")
  cat("  Example: Rscript subject_correlations.r --pca ", pca_file, " --input ", input_file, " --type 1\n")
  cat("\n")
  stop("Data type mismatch. Please use --type 1 for population data.")
}

# ==============================================================================
# Data Validation
# ==============================================================================

cat("\n[2/5] Validating data structure...\n")

# Check PCA columns
required_pca_cols <- c("parcel", "PC1", "PC2", "PC3")
if (!all(required_pca_cols %in% colnames(pca_data))) {
  stop(
    "Error: PCA data must contain columns: ",
    paste(required_pca_cols, collapse = ", ")
  )
}

# Check input columns based on data type
if (use_population_data) {
  required_input_cols <- c("parcel", "variable", "mean")
  if (!all(required_input_cols %in% colnames(input_data))) {
    stop(
      "Error: Population data must contain columns: ",
      paste(required_input_cols, collapse = ", ")
    )
  }

  # Get unique variables from population data
  unique_variables <- unique(input_data$variable)
  cat("  [OK] Found", length(unique_variables), "unique variables:\n")
  for (var in unique_variables) {
    cat("    -", var, "\n")
  }
} else {
  # For individual subject data, check that parcel column exists
  if (!("parcel" %in% colnames(input_data))) {
    stop("Error: Subject data must contain 'parcel' column")
  }

  # Get variable names from column names (excluding 'parcel')
  unique_variables <- setdiff(colnames(input_data), "parcel")
  cat("  [OK] Found", length(unique_variables), "unique variables:\n")
  for (var in unique_variables) {
    cat("    -", var, "\n")
  }
}

# Get PC columns (in case there are more than PC1, PC2, PC3)
pc_columns <- colnames(pca_data)[grep("^PC\\d+$", colnames(pca_data))]
cat("  [OK] Found", length(pc_columns), "PC components:\n")
cat("    -", paste(pc_columns, collapse = ", "), "\n")

# ==============================================================================
# Data Preparation
# ==============================================================================

cat("\n[3/5] Preparing data for correlation analysis...\n")

# Prepare input data based on type
if (use_population_data) {
  # Wide format (one row per parcel, one column per variable)
  input_wide <- input_data %>%
    select(parcel, variable, mean) %>%
    pivot_wider(
      names_from = variable,
      values_from = mean,
      names_prefix = "var_"
    )

  cat("  [OK] Reshaped population data: ", nrow(input_wide), " parcels\n")
} else {
  # For individual subject data, already in wide format
  # Just need to rename variable columns to add "var_" prefix
  input_wide <- input_data %>%
    rename_with(
      ~ paste0("var_", .x),
      .cols = -parcel
    )

  cat("  [OK] Processed subject data: ", nrow(input_wide), " parcels\n")
}

# Merge PCA and input data by parcel
merged_data <- inner_join(pca_data, input_wide, by = "parcel")

cat("  [OK] Merged datasets: ", nrow(merged_data), " parcels matched\n")

# Check for parcels that didn't match
pca_only <- setdiff(pca_data$parcel, input_wide$parcel)
input_only <- setdiff(input_wide$parcel, pca_data$parcel)

if (length(pca_only) > 0) {
  cat(
    "  [!] Warning:", length(pca_only),
    "parcels in PCA data not found in input data\n"
  )
}
if (length(input_only) > 0) {
  cat(
    "  [!] Warning:", length(input_only),
    "parcels in input data not found in PCA data\n"
  )
}

# ==============================================================================
# Correlation Analysis
# ==============================================================================

cat("\n[4/5] Calculating Spearman correlations...\n\n")

# Initialize results data frame
correlation_results <- tibble()

# Calculate correlations for each PC and each variable
for (pc in pc_columns) {
  for (var in unique_variables) {
    var_col <- paste0("var_", var)

    if (var_col %in% colnames(merged_data)) {
      # Remove rows with NA values for this specific pair
      valid_data <- merged_data %>%
        filter(!is.na(.data[[pc]]) & !is.na(.data[[var_col]]))

      if (nrow(valid_data) >= 3) { # Need at least 3 points for correlation
        # Calculate Spearman correlation
        cor_test <- cor.test(
          valid_data[[pc]],
          valid_data[[var_col]],
          method = "spearman",
          exact = FALSE
        )

        # Store results
        correlation_results <- bind_rows(
          correlation_results,
          tibble(
            pc_component = pc,
            variable = var,
            rho = cor_test$estimate,
            p_value = cor_test$p.value,
            n_parcels = nrow(valid_data)
          )
        )
      }
    }
  }
}

# ==============================================================================
# Multiple Testing Correction
# ==============================================================================

cat("\n[5/5] Multiple testing correction...\n")

# Apply FDR correction if requested
if (apply_correction) {
  cat("  Applying FDR correction (Benjamini-Hochberg)\n\n")
  correlation_results <- correlation_results %>%
    mutate(
      p_adj = p.adjust(p_value, method = "fdr"),
      significance = case_when(
        p_adj < 0.001 ~ "***",
        p_adj < 0.01 ~ "**",
        p_adj < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
} else {
  cat("  Skipping correction (--no-correction flag used)\n\n")
  correlation_results <- correlation_results %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ ""
      )
    )
}

# ==============================================================================
# Display Results
# ==============================================================================

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
if (use_population_data) {
  cat("  CORRELATION RESULTS (Normative Population Data)\n")
} else {
  cat("  CORRELATION RESULTS (Individual Subject Data)\n")
}
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# Print results grouped by PC component
for (pc in pc_columns) {
  cat("\n", pc, ":\n")
  cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

  pc_results <- correlation_results %>%
    filter(pc_component == pc) %>%
    arrange(desc(abs(rho)))

  for (i in seq_len(nrow(pc_results))) {
    row <- pc_results[i, ]
    if (apply_correction) {
      cat(sprintf(
        "  %-30s  rho = %7.4f  p = %.4e  p_adj = %.4e  %s\n",
        row$variable,
        row$rho,
        row$p_value,
        row$p_adj,
        row$significance
      ))
    } else {
      cat(sprintf(
        "  %-30s  rho = %7.4f  p = %.4e  %s\n",
        row$variable,
        row$rho,
        row$p_value,
        row$significance
      ))
    }
  }
}

cat("\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n")
if (apply_correction) {
  cat("Significance codes: *** p_adj<0.001, ** p_adj<0.01, * p_adj<0.05 (FDR corrected)\n")
} else {
  cat("Significance codes: *** p<0.001, ** p<0.01, * p<0.05 (uncorrected)\n")
}
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# ==============================================================================
# Save Results
# ==============================================================================

if (output_file != "") {
  cat("\nSaving results to:", output_file, "\n")
  write_csv(correlation_results, output_file)
  cat("  [OK] Results saved successfully\n")
}

# ==============================================================================
# Summary Statistics
# ==============================================================================

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("  SUMMARY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("Total correlations calculated:", nrow(correlation_results), "\n")
if (apply_correction) {
  cat(
    "Significant correlations (p_adj < 0.05):",
    sum(correlation_results$p_adj < 0.05), "\n"
  )
  cat(
    "Highly significant correlations (p_adj < 0.001):",
    sum(correlation_results$p_adj < 0.001), "\n\n"
  )
} else {
  cat(
    "Significant correlations (p < 0.05):",
    sum(correlation_results$p_value < 0.05), "\n"
  )
  cat(
    "Highly significant correlations (p < 0.001):",
    sum(correlation_results$p_value < 0.001), "\n\n"
  )
}

cat("Strongest correlations:\n")
top_cors <- correlation_results %>%
  arrange(desc(abs(rho))) %>%
  head(5)

for (i in seq_len(nrow(top_cors))) {
  row <- top_cors[i, ]
  cat(sprintf(
    "  %s vs %-25s  rho = %7.4f  %s\n",
    row$pc_component,
    row$variable,
    row$rho,
    row$significance
  ))
}

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("Analysis complete!\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
