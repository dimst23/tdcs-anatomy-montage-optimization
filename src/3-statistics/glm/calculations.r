#!/usr/bin/env Rscript
# ==============================================================================
# GLM Analysis - Calculation Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))

#' Load subject data for GLM analysis
#'
#' @param subjects Vector of subject IDs
#' @param config Configuration list from settings parser
#' @param hemisphere Hemisphere to load
#' @param simulation_type Simulation type to load
#' @return List with regressor data and independent variable data
load_glm_subject_data <- function(subjects, config, hemisphere, simulation_type) {
  n_subjects <- length(subjects)
  regressors <- config$glm$regressors
  ind_variable <- config$ind_variable

  regressor_list <- list()

  for (subject_index in 1:n_subjects) {
    subject <- paste0(subjects[subject_index], "_V1_MR")
    fs_path <- file.path(config$paths$data, subject, "fs")

    subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
    sim_path <- file.path(config$paths$data, subject, "sim", simulation_type, subj_type)

    # Load regressors
    for (regressor in regressors) {
      if (regressor == "age") next

      regressor_name <- paste(hemisphere, config$space_type, regressor, sep = ".")
      regressor_data <- read.fs.curv(file.path(fs_path, regressor_name))

      if (subject_index == 1) {
        n_vertices <- length(regressor_data)
        regressor_matrix <- matrix(NA, nrow = n_vertices, ncol = n_subjects)
        regressor_list <- c(regressor_list, setNames(list(regressor_matrix), regressor))
      }
      regressor_list[[regressor]][, subject_index] <- regressor_data
    }

    # Load independent variable
    variable_name <- paste(hemisphere, paste0(subject, "_TDCS_1_scalar.central"),
      ind_variable,
      sep = "."
    )
    variable_data <- read.fs.curv(file.path(sim_path, variable_name))

    if (!is.null(config$field_scale_factor)) {
      variable_data <- variable_data * config$field_scale_factor
    }

    if (subject_index == 1) {
      variable_matrix <- matrix(NA, nrow = n_vertices, ncol = n_subjects)
    }
    variable_matrix[, subject_index] <- variable_data
  }

  glm_data_list <- c(regressor_list, setNames(list(variable_matrix), ind_variable))
  return(glm_data_list)
}

#' Run GLM analysis across all subjects in parallel
#'
#' @param glm_data_list List of data matrices
#' @param config Configuration list
#' @param annotation Annotation object
#' @param hemisphere Hemisphere being analyzed ("lh" or "rh")
#' @param n_subjects Number of subjects
#' @param use_parallel Use parallel processing (default: TRUE)
#' @param n_workers Number of workers for parallel processing
#' @return List of GLM results per subject
run_parallel_glm_analysis <- function(glm_data_list, config, annotation,
                                      hemisphere, n_subjects,
                                      use_parallel = TRUE,
                                      n_workers = NULL) {
  # Prepare network information
  annot_ids <- 1:length(annotation$label_names)
  valid_nets_bool <- annotation$label_names != "Background+FreeSurfer_Defined_Medial_Wall"
  valid_nets <- annotation$label_names[valid_nets_bool]
  valid_net_ids <- annot_ids[valid_nets_bool]

  # Extract network components
  # NOTE: This assumes a specific naming pattern - TODO: make configurable
  name_pattern <- paste0("\\d+Networks_", toupper(hemisphere), "_")
  split_name_nets <- extract_network_info(valid_nets, name_pattern)
  valid_coarse_nets <- as.factor(split_name_nets$Network)
  valid_parcels_nets <- as.factor(split_name_nets$Parcel)

  if (use_parallel) {
    if (is.null(n_workers)) {
      n_workers <- max(1, parallel::detectCores() - 2)
    }

    plan(multicore, workers = n_workers)

    # Set limit to 20 GiB to accommodate large datasets
    options(future.globals.maxSize = 20 * 1024^3)

    cat("Calculating GLM per subject - Parallel (", n_workers, " workers)\n")
    tic("Parallel GLM calculation")

    results_list <- future_lapply(1:n_subjects, function(subject_index) {
      temp_glm <- calculate_subject_glm(
        subject_index, glm_data_list, config$glm$regressors,
        valid_net_ids, valid_coarse_nets, valid_parcels_nets,
        config$ind_variable, config$glm$interactions, config$glm$extra_terms
      )
      extract_glm_coefficients(temp_glm)
    }, future.globals = TRUE)

    plan(sequential)
    gc()
    toc()
  } else {
    cat("Calculating GLM per subject - Sequential\n")
    tic("Sequential GLM calculation")

    results_list <- lapply(1:n_subjects, function(subject_index) {
      temp_glm <- calculate_subject_glm(
        subject_index, glm_data_list, config$glm$regressors,
        valid_net_ids, valid_coarse_nets, valid_parcels_nets,
        config$ind_variable, config$glm$interactions, config$glm$extra_terms
      )
      extract_glm_coefficients(temp_glm)
    })

    toc()
  }

  return(list(
    results = results_list,
    valid_net_ids = valid_net_ids
  ))
}

#' Combine parallel GLM results into single structure
#'
#' @param results_list List of per-subject results
#' @return Combined GLM results structure
combine_parallel_glm_results <- function(results_list) {
  glm_results <- reduce(results_list, function(acc, res) {
    imap(res, function(coef_data, coef_name) {
      imap(coef_data, function(values, reg) {
        c(acc[[coef_name]][[reg]], values)
      })
    })
  })

  return(glm_results)
}
