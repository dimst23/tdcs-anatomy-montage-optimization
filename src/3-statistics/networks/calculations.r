#!/usr/bin/env Rscript
# ==============================================================================
# Network Analysis - Calculation Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))

#' Load and process subject field data for network analysis
#'
#' @param subjects Vector of subject IDs
#' @param config Configuration list from settings parser
#' @param simulation Simulation type
#' @param hemisphere Hemisphere to process
#' @param annotation Annotation object for this hemisphere
#' @param network_name_map Network name mapping
#' @return List of network statistics per subject and simulation
load_network_subject_data <- function(subjects, config, simulation, hemisphere,
                                      annotation, network_name_map) {
  n_subjects <- length(subjects)
  region_stat_list <- list()
  region_labels <- unique(annotation$label_names)

  for (i in seq_along(subjects)) {
    subject <- paste0(subjects[i], "_V1_MR")

    subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
    sim_path <- file.path(config$paths$data, subject, "sim", simulation, subj_type)

    variable_name <- paste(
      hemisphere,
      paste0(subject, "_TDCS_1_scalar.central"),
      names(config$field_name),
      sep = "."
    )

    subject_data <- read.fs.curv(file.path(sim_path, variable_name)) *
      config$field_scale_factor

    # Calculate statistics per network
    stats <- calculate_network_stats(
      subject_data, annotation, region_labels,
      network_name_map, names(config$field_name)
    )

    # Accumulate results
    if (i == 1) {
      for (net_name in names(stats)) {
        region_stat_list[[net_name]] <- list(
          mean = numeric(n_subjects),
          std = numeric(n_subjects)
        )
      }
    }

    for (net_name in names(stats)) {
      region_stat_list[[net_name]]$mean[i] <- stats[[net_name]]$mean
      region_stat_list[[net_name]]$std[i] <- stats[[net_name]]$std
    }
  }

  return(region_stat_list)
}

#' Load and process subject field data for node-level network analysis
#'
#' @param subjects Vector of subject IDs
#' @param config Configuration list from settings parser
#' @param simulation Simulation type
#' @param hemisphere Hemisphere to process
#' @param annotation Annotation object for this hemisphere
#' @param node_list List of nodes per network
#' @param network_name_map Network name mapping
#' @return List of node-level network statistics per subject and simulation
load_network_node_subject_data <- function(subjects, config, simulation, hemisphere,
                                           annotation, node_list, network_name_map) {
  n_subjects <- length(subjects)
  region_stat_list <- list()
  region_labels <- unique(annotation$label_names)

  for (i in seq_along(subjects)) {
    subject <- paste0(subjects[i], "_V1_MR")

    subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
    sim_path <- file.path(config$paths$data, subject, "sim", simulation, subj_type)

    variable_name <- paste(
      hemisphere,
      paste0(subject, "_TDCS_1_scalar.central"),
      names(config$field_name),
      sep = "."
    )

    subject_data <- read.fs.curv(file.path(sim_path, variable_name)) *
      config$field_scale_factor

    # Calculate statistics per network node
    stats <- calculate_node_stats(
      subject_data, annotation, region_labels,
      node_list, network_name_map, hemisphere
    )

    # Accumulate results
    if (i == 1) {
      for (net_name in names(stats)) {
        region_stat_list[[net_name]] <- list()
        for (node in names(stats[[net_name]])) {
          region_stat_list[[net_name]][[node]] <- list(
            mean = numeric(n_subjects),
            std = numeric(n_subjects)
          )
        }
      }
    }

    for (net_name in names(stats)) {
      for (node in names(stats[[net_name]])) {
        idx <- length(region_stat_list[[net_name]][[node]]$mean)
        region_stat_list[[net_name]][[node]]$mean <-
          c(region_stat_list[[net_name]][[node]]$mean, stats[[net_name]][[node]]$mean)
        region_stat_list[[net_name]][[node]]$std <-
          c(region_stat_list[[net_name]][[node]]$std, stats[[net_name]][[node]]$std)
      }
    }
  }

  return(region_stat_list)
}

#' Run statistical comparisons on network data
#'
#' @param data_df_hist Individual subject data frame
#' @param group_by Grouping variables
#' @return List with anova and wilcox test results
run_network_statistical_tests <- function(data_df_hist,
                                          group_by = c("Hemisphere", "Network")) {
  # Check if there are at least 2 montages for comparison
  n_montages <- length(unique(data_df_hist$Montage))

  if (n_montages < 2) {
    warning("Less than 2 montages found. Skipping statistical tests.")
    return(list(
      anova = data.frame(),
      wilcox = data.frame()
    ))
  }

  # Run Kruskal-Wallis test with error handling
  anova_data <- tryCatch(
    {
      compare_means(
        Mean ~ Montage,
        data_df_hist,
        group.by = group_by,
        method = "kruskal.test",
        p.adjust.method = "fdr"
      )
    },
    error = function(e) {
      warning("Kruskal-Wallis test failed: ", e$message, "\n")
      data.frame()
    }
  )

  # Run Wilcoxon test with error handling
  wilcox_data <- tryCatch(
    {
      compare_means(
        Mean ~ Montage,
        data_df_hist,
        group.by = group_by,
        method = "wilcox.test",
        p.adjust.method = "fdr"
      )
    },
    error = function(e) {
      warning("Wilcoxon test failed: ", e$message, "\n")
      data.frame()
    }
  )

  return(list(anova = anova_data, wilcox = wilcox_data))
}
