#!/usr/bin/env Rscript
# ==============================================================================
# Population Statistics
# Purpose: Calculate population-level morphological statistics by demographics
#
# Usage:
#   Rscript population_stats.r <JSON_SETTINGS_PATH> <BASE_DIR> <HEMISPHERE> <CSV_SAVE_PATH>
#
# Arguments:
#   JSON_SETTINGS_PATH - Path to JSON configuration file
#   BASE_DIR           - Base directory for data paths
#   HEMISPHERE         - Hemisphere to analyze (lh or rh)
#   CSV_SAVE_PATH      - Output directory for CSV files
# ==============================================================================

# Source common dependencies
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
stats_dir <- dirname(script_dir)

source(file.path(stats_dir, "common", "packages.r"))
source(file.path(stats_dir, "common", "functions.r"))
source(file.path(stats_dir, "common", "settings_parser.r"))

# Additional packages
suppressPackageStartupMessages({
  library(freesurferformats)
  library(lme4)
  library(car)
  library(reshape2)
})

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 4,
  max_args = 4,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR", "HEMISPHERE", "CSV_SAVE_PATH")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR
HEMISPHERE <- cli_args$HEMISPHERE
CSV_SAVE_PATH <- cli_args$CSV_SAVE_PATH

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)

# Validate hemisphere
validate_hemisphere(HEMISPHERE, config$hemispheres)

# Load subjects
subjects <- read_subjects(config$paths$subjects)

# ==============================================================================
# Load Demographics
# ==============================================================================

cat("Loading demographics data...\n")

demographics_path <- file.path(BASE_DIR, config$full_settings$glm$paths$age_file_path)
csv_data <- read.csv(demographics_path, sep = "\t", header = TRUE)
csv_data <- csv_data[-1, ]

# Extract age data
age_data <- numeric(length(subjects))
for (i in seq_along(subjects)) {
  age_data[i] <- as.numeric(csv_data[csv_data$src_subject_id == subjects[i], "interview_age"]) / 12
}

# Extract gender data
gender_data <- character(length(subjects))
for (i in seq_along(subjects)) {
  gender_data[i] <- csv_data[csv_data$src_subject_id == subjects[i], "sex"]
}

age_group <- as.character(age_groups(age_data))
gender <- gender_data

# ==============================================================================
# Load Subject Data
# ==============================================================================

cat("Reading subject data....\n")

n_subjects <- length(subjects)
regressor_list <- list()
var_list <- list()

for (subject_index in 1:n_subjects) {
  subject <- paste0(subjects[subject_index], "_V1_MR")
  fs_path <- file.path(config$paths$data, subject, "fs")

  subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")

  # Load regressors
  for (regressor in config$glm$regressors) {
    if (regressor == "age") next

    regressor_name <- paste(HEMISPHERE, config$space_type, regressor, sep = ".")
    regressor_data <- read.fs.curv(file.path(fs_path, regressor_name))

    if (subject_index == 1) {
      n_vertices <- length(regressor_data)
      regressor_matrix <- matrix(NA, nrow = n_vertices, ncol = n_subjects)
      regressor_list <- c(regressor_list, setNames(list(regressor_matrix), regressor))
    }
    regressor_list[[regressor]][, subject_index] <- regressor_data
  }

  # Load independent variables for all simulation types
  for (sim_type in config$simulation_types) {
    sim_path <- file.path(config$paths$data, subject, "sim", sim_type, subj_type)
    ind_var_name <- paste("E_magn", sim_type, sep = "_")

    variable_name <- paste(HEMISPHERE, paste0(subject, "_TDCS_1_scalar.central"), config$ind_variable, sep = ".")
    variable_data <- read.fs.curv(file.path(sim_path, variable_name))

    if (subject_index == 1) {
      variable_matrix <- matrix(NA, nrow = n_vertices, ncol = n_subjects)
      var_list <- c(var_list, setNames(list(variable_matrix), ind_var_name))
    }
    var_list[[ind_var_name]][, subject_index] <- variable_data
  }
}

glm_data_list <- c(regressor_list, var_list)

# ==============================================================================
# Load Network Annotations
# ==============================================================================

annot_file <- file.path(config$network$atlas_path, paste(HEMISPHERE, config$network$atlas, "annot", sep = "."))
ANNOTATION <- read.fs.annot(annot_file)
annot_labels <- unique(ANNOTATION$label_names)

# Precompute network node selections
valid_nets <- setdiff(annot_labels, "Background+FreeSurfer_Defined_Medial_Wall")
selected_nodes_list <- lapply(valid_nets, function(net_name) ANNOTATION$label_names == net_name)
names(selected_nodes_list) <- valid_nets

net_name_replacement_pattern <- paste0("\\d+Networks_")
clean_parcel_names <- str_remove(names(selected_nodes_list), net_name_replacement_pattern)

# ==============================================================================
# Calculate Per-Subject Parcel Statistics
# ==============================================================================

cat("Calculating per-subject parcel statistics...\n")

n_vars <- length(glm_data_list)
n_parcels <- length(selected_nodes_list)
n_rows_intermediate <- n_subjects * n_vars * n_parcels

# Pre-allocate vectors
subject_vec <- character(n_rows_intermediate)
age_group_vec <- character(n_rows_intermediate)
gender_vec <- character(n_rows_intermediate)
parcel_vec <- character(n_rows_intermediate)
variable_vec <- character(n_rows_intermediate)
subject_mean_vec <- numeric(n_rows_intermediate)

row_idx <- 1
for (var_name in names(glm_data_list)) {
  var_matrix <- glm_data_list[[var_name]]

  for (p_idx in seq_along(selected_nodes_list)) {
    parcel_vertices <- selected_nodes_list[[p_idx]]
    parcel_data <- var_matrix[parcel_vertices, , drop = FALSE]
    subject_means <- colMeans(parcel_data, na.rm = TRUE)

    idx_range <- row_idx:(row_idx + n_subjects - 1)
    subject_vec[idx_range] <- subjects
    age_group_vec[idx_range] <- age_group
    gender_vec[idx_range] <- gender
    parcel_vec[idx_range] <- clean_parcel_names[p_idx]
    variable_vec[idx_range] <- var_name
    subject_mean_vec[idx_range] <- subject_means

    row_idx <- row_idx + n_subjects
  }
}

# Create subject-level data frame
subject_level_data <- tibble(
  subject = subject_vec,
  age_group = age_group_vec,
  gender = gender_vec,
  parcel = parcel_vec,
  variable = variable_vec,
  value = subject_mean_vec
) %>%
  mutate(variable = case_when(
    variable == "area" ~ "cortical_surface_area",
    variable == "sulc" ~ "sulcal_depth",
    variable == "thickness" ~ "cortical_thickness",
    TRUE ~ variable
  ))

# ==============================================================================
# Aggregate and Save Statistics
# ==============================================================================

cat("Aggregating statistics by demographics...\n")

if (!dir.exists(CSV_SAVE_PATH)) {
  dir.create(CSV_SAVE_PATH, recursive = TRUE)
}

# Helper functions
calc_summary_stats <- function(data, group_vars) {
  data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      mean = mean(.data$value, na.rm = TRUE),
      std = sd(.data$value, na.rm = TRUE),
      median = median(.data$value, na.rm = TRUE),
      iqr = IQR(.data$value, na.rm = TRUE),
      mean_abs = mean(abs(.data$value), na.rm = TRUE),
      std_abs = sd(abs(.data$value), na.rm = TRUE),
      median_abs = median(abs(.data$value), na.rm = TRUE),
      iqr_abs = IQR(abs(.data$value), na.rm = TRUE),
      n_subjects = n(),
      .groups = "drop"
    ) %>%
    filter(!str_starts(.data$variable, "E_magn"))
}

round_summary_cols <- function(data, digits = 5) {
  summary_cols <- c("mean", "std", "median", "iqr", "mean_abs", "std_abs", "median_abs", "iqr_abs")
  data %>%
    mutate(across(all_of(summary_cols), ~ round(.x, digits)))
}

build_filename <- function(hemisphere, space_type, atlas, ..., n_subjects) {
  parts <- list(hemisphere, space_type, atlas, ...)
  parts <- c(parts, paste0("n_", n_subjects), "parcel_stats.csv")
  paste(parts, collapse = ".")
}

# Aggregate data at different levels
aggregated_data <- calc_summary_stats(subject_level_data, c("age_group", "gender", "parcel", "variable"))
aggregated_data_all_gender <- calc_summary_stats(subject_level_data, c("gender", "parcel", "variable"))
aggregated_data_all <- calc_summary_stats(subject_level_data, c("parcel", "variable"))

# Save by gender
for (gn in unique(aggregated_data_all_gender$gender)) {
  n_subs <- unique(aggregated_data_all_gender$n_subjects[aggregated_data_all_gender$gender == gn])
  gd <- aggregated_data_all_gender %>%
    filter(gender == gn) %>%
    round_summary_cols() %>%
    select(parcel, variable, mean, std, median, iqr, mean_abs, std_abs, median_abs, iqr_abs, n_subjects)

  filename <- build_filename(HEMISPHERE, config$space_type, config$network$atlas, paste0("gender_", gn), n_subjects = n_subs)
  write.csv(gd, file.path(CSV_SAVE_PATH, filename), row.names = FALSE)
}

# Save total
aggregated_data_all_rounded <- round_summary_cols(aggregated_data_all)
filename <- build_filename(HEMISPHERE, config$space_type, config$network$atlas, n_subjects = unique(aggregated_data_all_rounded$n_subjects))
filename <- sub("parcel_stats.csv", "parcel_stats_total.csv", filename)
write.csv(aggregated_data_all_rounded, file.path(CSV_SAVE_PATH, filename), row.names = FALSE)

# Save by age and gender
demographic_groups <- aggregated_data %>%
  distinct(age_group, gender, n_subjects) %>%
  arrange(age_group, gender)

for (i in seq_len(nrow(demographic_groups))) {
  curr_age <- demographic_groups$age_group[i]
  curr_gender <- demographic_groups$gender[i]
  n_subjects_group <- demographic_groups$n_subjects[i]

  group_data <- aggregated_data %>%
    filter(age_group == curr_age, gender == curr_gender) %>%
    round_summary_cols() %>%
    select(parcel, variable, mean, std, median, iqr, mean_abs, std_abs, median_abs, iqr_abs, n_subjects)

  filename <- build_filename(
    HEMISPHERE, config$space_type, config$network$atlas,
    paste0("age_", curr_age), paste0("gender_", curr_gender),
    n_subjects = n_subjects_group
  )

  write.csv(group_data, file.path(CSV_SAVE_PATH, filename), row.names = FALSE)
  cat(sprintf("Saved: %s (n=%d subjects)\n", filename, n_subjects_group))
}

cat("\nPopulation statistics complete!\n")
cat("Results saved to:", CSV_SAVE_PATH, "\n")
