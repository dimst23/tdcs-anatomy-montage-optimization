#!/usr/bin/env Rscript
# ==============================================================================
# Network Analysis - Utility Functions
# ==============================================================================

# Source common functions
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "functions.r"))

#' Load network mapping configuration
#'
#' @param base_dir Base directory path
#' @param mappings_path Relative path to mappings directory
#' @param atlas Atlas name
#' @param combine_nets Whether to use combined network mapping
#' @return List with network name mappings
load_network_mapping <- function(base_dir, mappings_path, atlas, combine_nets = FALSE) {
  mappings_file <- file.path(base_dir, mappings_path, paste(atlas, "json", sep = "."))

  if (!file.exists(mappings_file)) {
    stop("Network mapping file not found: ", mappings_file, call. = FALSE)
  }

  mapping_settings <- jsonlite::read_json(mappings_file)
  network_name_map <- mapping_settings$network_names_map

  if (combine_nets) {
    combined_map <- mapping_settings$network_names_map_combined
    if (!is.null(combined_map)) {
      network_name_map <- combined_map
    } else {
      warning("Combined map requested but not available in atlas. Using default mapping.")
    }
  }

  return(network_name_map)
}

#' Create node list from annotation data
#'
#' @param annotation FreeSurfer annotation object
#' @param network_selection Vector of networks to include
#' @param network_name_map Mapping of network names
#' @return List of nodes organized by network
create_node_list <- function(annotation, network_selection, network_name_map) {
  node_list <- list()
  region_labels <- unique(annotation$label_names)

  for (region_label in region_labels) {
    components <- get_network_components(region_label)

    if (components$net %in% names(network_selection)) {
      if (is.null(node_list[[components$net]])) {
        node_list[[components$net]] <- c()
      }

      if (!is.null(components$comp)) {
        node_list[[components$net]] <- c(node_list[[components$net]], components$comp)
      }
    }
  }

  # Remove duplicates
  node_list <- lapply(node_list, unique)

  return(node_list)
}

#' Calculate network statistics from subject data
#'
#' @param subject_data Vector of field values for one subject
#' @param annotation Annotation object
#' @param region_labels Vector of region label names
#' @param network_name_map Network name mapping
#' @param field_name Field type being analyzed
#' @param net_normal_component Whether to include net normal component
#' @return List of statistics per network
calculate_network_stats <- function(subject_data, annotation, region_labels,
                                    network_name_map, field_name = "E.magn",
                                    net_normal_component = TRUE) {
  stats_list <- list()

  for (net_name in names(network_name_map)) {
    region_mask <- numeric(length(annotation$label_names))

    for (region_label in region_labels) {
      if (grepl(net_name, region_label)) {
        region_mask[annotation$label_names == region_label] <- 1
      }
    }

    region_mask_bool <- region_mask == 1
    mapped_net_name <- network_name_map[[net_name]]
    region_data <- subject_data[region_mask_bool]

    if (field_name == "E.normal" && !net_normal_component) {
      # Separate excitation and inhibition
      stats_list[[mapped_net_name]][["excitation"]] <- list(
        mean = mean(region_data[region_data >= 0]),
        std = sd(region_data[region_data >= 0])
      )

      stats_list[[mapped_net_name]][["inhibition"]] <- list(
        mean = mean(abs(region_data[region_data < 0])),
        std = sd(abs(region_data[region_data < 0]))
      )
    } else {
      stats_list[[mapped_net_name]] <- list(
        mean = mean(region_data),
        std = sd(region_data)
      )
    }
  }

  return(stats_list)
}

#' Calculate node-specific network statistics
#'
#' @param subject_data Field data for one subject
#' @param annotation Annotation object
#' @param region_labels Region label names
#' @param node_list List of nodes per network
#' @param network_name_map Network name mapping
#' @param hemisphere Current hemisphere
#' @return List of statistics per network and node
calculate_node_stats <- function(subject_data, annotation, region_labels,
                                 node_list, network_name_map, hemisphere) {
  stats_list <- list()

  for (net_name in names(node_list)) {
    for (node in node_list[[net_name]]) {
      region_mask <- numeric(length(annotation$label_names))

      for (region_label in region_labels) {
        match_name <- paste(toupper(hemisphere), net_name, node, sep = "_")
        if (grepl(match_name, region_label)) {
          region_mask[annotation$label_names == region_label] <- 1
        }
      }

      region_mask_bool <- region_mask == 1
      mapped_net_name <- network_name_map[[net_name]]
      region_data <- subject_data[region_mask_bool]

      if (is.null(stats_list[[mapped_net_name]][[node]])) {
        stats_list[[mapped_net_name]][[node]] <- list(mean = c(), std = c())
      }

      stats_list[[mapped_net_name]][[node]][["mean"]] <-
        c(stats_list[[mapped_net_name]][[node]][["mean"]], mean(region_data))
      stats_list[[mapped_net_name]][[node]][["std"]] <-
        c(stats_list[[mapped_net_name]][[node]][["std"]], sd(region_data))
    }
  }

  return(stats_list)
}

#' Create a grob function for network renders
#'
#' @param net_name Network name
#' @param hemi Hemisphere
#' @param atlas Atlas name
#' @param annotation_surface Surface type for annotation
#' @param network_renders_path Path to network render images
#' @param network_name_map Network name mapping
#' @param height Height parameter for grob
#' @return Grid raster grob object
create_network_grob <- function(net_name, hemi, atlas, annotation_surface,
                                network_renders_path, network_name_map,
                                height = 0.225) {
  # Backward mapping of the name (match old implementation)
  name_index <- which(grepl(net_name, network_name_map))

  if (length(name_index) == 0) {
    warning("Network name not found in mapping: ", net_name)
    return(NULL)
  }

  orig_net_name <- names(network_name_map)[[name_index]]

  network_render_file <- file.path(
    network_renders_path, atlas,
    paste(hemi, annotation_surface, orig_net_name, "png", sep = ".")
  )

  if (!file.exists(network_render_file)) {
    warning("Network render file not found: ", network_render_file)
    return(NULL)
  }

  png_grob <- readPNG(network_render_file)
  rasterGrob(png_grob, interpolate = TRUE, height = height)
}
