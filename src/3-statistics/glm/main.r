#!/usr/bin/env Rscript
# ==============================================================================
# Per-Subject GLM Analysis
# Purpose: Run GLM analysis per subject per network parcel
#          Supports both basic and full (with interactions) analysis modes
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
source(file.path(script_dir, "calculations.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 4,
  max_args = 5,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR", "HEMISPHERE", "SIMULATION_TYPE", "ANALYSIS_MODE")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR
HEMISPHERE <- cli_args$HEMISPHERE
SIMULATION_TYPE <- cli_args$SIMULATION_TYPE

# Analysis mode: "per_parcel" or "total" (default: "per_parcel" for backward compatibility)
ANALYSIS_MODE <- if (!is.null(cli_args$ANALYSIS_MODE)) {
  cli_args$ANALYSIS_MODE
} else {
  "per_parcel"
}

if (!ANALYSIS_MODE %in% c("per_parcel", "total")) {
  stop("ANALYSIS_MODE must be either 'per_parcel' or 'total'", call. = FALSE)
}

cat("Running GLM analysis in", ANALYSIS_MODE, "mode\n")

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)

# Validate inputs
validate_hemisphere(HEMISPHERE, config$hemispheres)
validate_simulation_type(SIMULATION_TYPE, config$simulation_types)

# Load subjects
subjects <- read_subjects(config$paths$subjects)

# Validate field scale factor for total mode
if (ANALYSIS_MODE == "total") {
  if (is.null(config$field_scale_factor)) {
    stop("Field scale factor must be defined in settings for total mode analysis.", call. = FALSE)
  }
}

# ==============================================================================
# Check Output Path
# ==============================================================================

glm_save_path <- file.path(
  config$paths$glm_results, "glm", "per_subject",
  if (ANALYSIS_MODE == "total") "total" else ""
)
if (!dir.exists(glm_save_path)) {
  dir.create(glm_save_path, recursive = TRUE)
}

save_file_name_parts <- c(
  HEMISPHERE, config$settings_id, config$space_type, config$network$atlas,
  SIMULATION_TYPE, config$ind_variable
)

if (ANALYSIS_MODE == "total") {
  scale_factor_name <- paste0("SF-", round(config$field_scale_factor, 2))
  save_file_name_parts <- c(save_file_name_parts, scale_factor_name)
}

save_file_name <- paste(c(save_file_name_parts, "json"), collapse = ".")

final_save_path <- file.path(glm_save_path, save_file_name)
if (file.exists(final_save_path)) {
  cat(save_file_name, "\n")
  stop("Output file exists from a previous run.", call. = FALSE)
}

# ==============================================================================
# Load Subject Data
# ==============================================================================

cat("Reading subject data....\n")

if (ANALYSIS_MODE == "total") {
  # Use helper function for total mode
  glm_data_list <- load_glm_subject_data(
    subjects, config, HEMISPHERE, SIMULATION_TYPE
  )
} else {
  # Inline data loading for per_parcel mode
  n_subjects <- length(subjects)
  regressor_list <- list()

  for (subject_index in 1:n_subjects) {
    subject <- paste0(subjects[subject_index], "_V1_MR")
    fs_path <- file.path(config$paths$data, subject, "fs")

    subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
    sim_path <- file.path(config$paths$data, subject, "sim", SIMULATION_TYPE, subj_type)

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

    # Load independent variable
    variable_name <- paste(HEMISPHERE, paste0(subject, "_TDCS_1_scalar.central"), config$ind_variable, sep = ".")
    variable_data <- read.fs.curv(file.path(sim_path, variable_name))

    if (subject_index == 1) {
      variable_matrix <- matrix(NA, nrow = n_vertices, ncol = n_subjects)
    }
    variable_matrix[, subject_index] <- variable_data
  }

  glm_data_list <- c(regressor_list, setNames(list(variable_matrix), config$ind_variable))
}

# ==============================================================================
# Load Network Annotation
# ==============================================================================

cat("Loading network annotations....\n")

annot_file <- file.path(config$network$atlas_path, paste(HEMISPHERE, config$network$atlas, "annot", sep = "."))
ANNOTATION <- read.fs.annot(annot_file)

# ==============================================================================
# Run GLM Analysis
# ==============================================================================

if (ANALYSIS_MODE == "total") {
  cat("Running GLM calculations (multi-core)....\n")

  analysis_results <- run_parallel_glm_analysis(
    glm_data_list,
    config,
    ANNOTATION,
    HEMISPHERE,
    length(subjects),
    use_parallel = TRUE
  )

  # Combine parallel results
  glm_results <- combine_parallel_glm_results(analysis_results$results)

  # ==============================================================================
  # Rescale Coefficients
  # ==============================================================================

  cat("Rescaling coefficients to original scale....\n")

  tic("Rescaling b-values")
  glm_results <- rescale_glm_coefficients(
    glm_results,
    glm_data_list,
    analysis_results$valid_net_ids,
    config$glm$regressors,
    length(subjects)
  )
  toc()
} else {
  cat("Running GLM calculations (single core)....\n")

  annot_ids <- seq_along(ANNOTATION$label_names)
  annot_labels <- unique(ANNOTATION$label_names)
  n_subjects <- length(subjects)

  glm_results <- list()

  # Precompute network node selections
  valid_nets <- setdiff(annot_labels, "Background+FreeSurfer_Defined_Medial_Wall")
  selected_nodes_list <- lapply(valid_nets, function(net_name) ANNOTATION$label_names == net_name)
  names(selected_nodes_list) <- valid_nets

  for (subject_index in seq_len(n_subjects)) {
    for (net_name in names(selected_nodes_list)) {
      selected_nodes <- selected_nodes_list[[net_name]]

      input_data <- lapply(glm_data_list, `[`, annot_ids[selected_nodes], subject_index)
      glm_df <- data.frame(input_data)

      if (config$glm$scale_vars) {
        scaled_data <- scale(glm_df)
        glm_df <- data.frame(scaled_data)
        glm_df[is.na(glm_df)] <- 0
      }

      glm_equation <- paste(config$ind_variable, "~", paste(config$glm$regressors, collapse = " + "))
      temp_glm <- glm(glm_equation, data = glm_df)

      glm_summary <- summary(temp_glm)
      coef_names <- rownames(glm_summary$coefficients)

      for (regressor in config$glm$regressors) {
        if (subject_index == 1) {
          glm_results[[regressor]][[net_name]][["t"]] <- numeric(n_subjects)
          glm_results[[regressor]][[net_name]][["p"]] <- numeric(n_subjects) + 1
          glm_results[[regressor]][[net_name]][["b"]] <- numeric(n_subjects)
        }

        if (regressor %in% coef_names) {
          glm_results[[regressor]][[net_name]][["t"]][subject_index] <- glm_summary$coefficients[regressor, "t value"]
          glm_results[[regressor]][[net_name]][["p"]][subject_index] <- glm_summary$coefficients[regressor, "Pr(>|t|)"]
          glm_results[[regressor]][[net_name]][["b"]][subject_index] <- glm_summary$coefficients[regressor, "Estimate"]
        }
      }
    }
  }
}

# ==============================================================================
# Save Results
# ==============================================================================

cat("Saving output files....\n")

if (ANALYSIS_MODE == "total") {
  glm_results[["subjects"]] <- subjects
}

jsonlite::write_json(glm_results, final_save_path, pretty = TRUE)

cat("Results saved to:", final_save_path, "\n")
cat("Analysis complete!\n")

gc()
