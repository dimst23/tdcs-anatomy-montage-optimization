#!/usr/bin/env Rscript
# ==============================================================================
# Common Settings Parser
# Purpose: Parse JSON configuration files and extract common settings
# ==============================================================================

#' Parse command-line arguments for settings file and base directory
#'
#' @param min_args Minimum number of required arguments (default: 1)
#' @param max_args Maximum number of allowed arguments (default: NULL for no limit)
#' @param arg_names Names of expected arguments for help message
#' @return List containing parsed arguments
parse_cli_args <- function(min_args = 1, max_args = NULL, arg_names = c("JSON_SETTINGS_PATH")) {
  cli_args <- commandArgs(trailingOnly = TRUE)

  if (length(cli_args) < min_args) {
    cat("\nError: Insufficient arguments provided.\n")
    cat("Expected arguments:", paste(arg_names, collapse = ", "), "\n")
    cat("Received:", length(cli_args), "arguments\n\n")
    stop("Please provide the required arguments.", call. = FALSE)
  }

  if (!is.null(max_args) && length(cli_args) > max_args) {
    cat("\nError: Too many arguments provided.\n")
    cat("Expected arguments:", paste(arg_names, collapse = ", "), "\n")
    cat("Received:", length(cli_args), "arguments\n\n")
    stop("Please provide only the required arguments.", call. = FALSE)
  }

  # Create named list of arguments
  result <- as.list(cli_args)
  names(result) <- arg_names[1:length(cli_args)]

  return(result)
}

#' Parse JSON settings file and extract common configuration
#'
#' @param json_path Path to JSON settings file
#' @param base_dir Base directory for constructing file paths
#' @return List containing all parsed settings
parse_settings <- function(json_path, base_dir = NULL) {
  if (!file.exists(json_path)) {
    stop("Settings file not found at: ", json_path, call. = FALSE)
  }

  settings <- jsonlite::read_json(json_path)

  # Extract simulation settings
  # Handle both old and new JSON field structures
  # Old: "field": { "E.magn": "Magnitude" }, "field_scale_factor": 2
  # New: "field": { "type": { "E.magn": "Magnitude" }, "scale_factor": 2 }

  field_config <- settings$simulation$field

  # Detect which structure is being used
  if (!is.null(field_config$type)) {
    # New structure
    field_name <- field_config$type
    field_scale_factor <- field_config$scale_factor
    field_direction <- field_config$direction
  } else {
    # Old structure
    field_name <- field_config
    field_scale_factor <- settings$simulation$field_scale_factor
    field_direction <- NULL
  }

  # Validate required field_scale_factor
  if (is.null(field_scale_factor)) {
    stop("field_scale_factor is missing in settings. ",
      "Add 'field_scale_factor' at simulation level or 'scale_factor' under field.",
      call. = FALSE
    )
  }

  config <- list(
    # Basic identifiers
    settings_id = ifelse(is.null(settings$id), "00000000", settings$id),

    # Simulation parameters
    ind_variable = names(field_name),
    field_direction = field_direction,
    field_name = field_name,
    space_type = settings$simulation$space_type,
    field_scale_factor = field_scale_factor,

    # Hemisphere and simulation types
    hemispheres = names(settings$simulation$hemisphere_name_map),
    hemisphere_names = settings$simulation$hemisphere_name_map,
    simulation_types = names(settings$simulation$simulation_types),
    simulation_type_names = settings$simulation$simulation_types,
    simulations_to_exclude = unlist(settings$simulation$exclusion),

    # General settings
    cortex_only = settings$general$cortex_only,

    # Store full settings for custom access
    full_settings = settings
  )

  # Construct paths if base_dir is provided
  if (!is.null(base_dir)) {
    config$paths <- list(
      data = file.path(base_dir, settings$simulation$paths$data),
      subjects = file.path(base_dir, settings$simulation$paths$subjects),
      temp = file.path(base_dir, settings$figure$paths$temp),
      fsaverage_template = settings$general$paths$fsaverage_template
    )

    # GLM-specific paths
    if (!is.null(settings$glm)) {
      config$glm <- list(
        regressors = names(settings$glm$regressors),
        interactions = settings$glm$interactions,
        extra_terms = settings$glm$extra_terms,
        scale_vars = settings$glm$within$scale_vars,
        terms = settings$glm$within$terms
      )
      config$paths$glm_results <- file.path(base_dir, settings$glm$within$paths$results)
    }

    # Network-specific settings
    if (!is.null(settings$network)) {
      config$network <- list(
        atlas = settings$network$atlas,
        atlas_path = file.path(base_dir, settings$network$paths$atlas),
        mappings_path = file.path(base_dir, settings$network$paths$mappings),
        combine_nets = settings$network$combine_same_nets,
        networks_to_plot = settings$network$networks_to_plot_dist,
        annotation_surface = settings$network$annotation_surface
      )
    }

    # Figure settings
    if (!is.null(settings$figure)) {
      config$figure <- list(
        save_path = file.path(base_dir, settings$figure$paths$save),
        width = settings$figure$dimensions$width,
        height = settings$figure$dimensions$width / settings$figure$dimensions$aspect,
        greyscale = settings$figure$greyscale,
        with_annotations = settings$figure$with_brain_annotations
      )

      if (!is.null(settings$figure$paths$renders)) {
        config$figure$renders_path <- file.path(base_dir, settings$figure$paths$renders)
      }
    }
  }

  return(config)
}

#' Setup figure output directories
#'
#' @param figure_save_path Base path for saving figures
#' @return List of paths for different figure formats
setup_figure_directories <- function(figure_save_path) {
  if (!dir.exists(figure_save_path)) {
    stop("Figure saving directory does not exist: ", figure_save_path, call. = FALSE)
  }

  paths <- list(
    pdf = file.path(figure_save_path, "pdf"),
    eps = file.path(figure_save_path, "eps"),
    png = file.path(figure_save_path, "png"),
    rds = file.path(figure_save_path, "rds"),
    csv = file.path(figure_save_path, "csv")
  )

  # Create directories
  for (path in paths) {
    dir.create(path, recursive = FALSE, showWarnings = FALSE)
  }

  return(paths)
}

#' Read subject list from file
#'
#' @param subject_path Path to subjects file
#' @return Character vector of subject IDs
read_subjects <- function(subject_path) {
  if (!file.exists(subject_path)) {
    stop("Subject file not found at: ", subject_path, call. = FALSE)
  }

  subjects <- readLines(subject_path)

  if (length(subjects) == 0) {
    stop("No subjects found in file: ", subject_path, call. = FALSE)
  }

  return(subjects)
}

#' Validate hemisphere parameter
#'
#' @param hemisphere Hemisphere to validate
#' @param valid_hemispheres Vector of valid hemisphere names
validate_hemisphere <- function(hemisphere, valid_hemispheres) {
  if (!(hemisphere %in% valid_hemispheres)) {
    stop("Invalid hemisphere: ", hemisphere,
      ". Valid options: ", paste(valid_hemispheres, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate simulation type parameter
#'
#' @param sim_type Simulation type to validate
#' @param valid_types Vector of valid simulation types
validate_simulation_type <- function(sim_type, valid_types) {
  if (!(sim_type %in% valid_types)) {
    stop("Invalid simulation type: ", sim_type,
      ". Valid options: ", paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Load cortex node IDs from FreeSurfer label file
#'
#' @param fsaverage_path Path to fsaverage template
#' @param hemisphere Hemisphere ("lh" or "rh")
#' @return Vector of cortex node IDs (1-indexed)
get_cortex_node_ids <- function(fsaverage_path, hemisphere) {
  label_file <- file.path(
    fsaverage_path, "label",
    paste(hemisphere, "cortex.label", sep = ".")
  )

  if (!file.exists(label_file)) {
    stop("Cortex label file not found: ", label_file, call. = FALSE)
  }

  cortex_ids <- read.fs.label(label_file)

  # Convert to 1-indexed if needed
  if (min(cortex_ids) == 0) {
    cortex_ids <- cortex_ids + 1
  }

  return(cortex_ids)
}
