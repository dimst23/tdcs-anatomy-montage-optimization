#!/usr/bin/env Rscript
# ==============================================================================
# Network Analysis
# Purpose: Analyze electric field distribution across functional brain networks
#          Supports both network-level and node-level analysis modes
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

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 3,
  max_args = 5,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR", "SUBJECT_PATH", "ANALYSIS_MODE", "RUN_STATS")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR
SUBJECT_PATH <- cli_args$SUBJECT_PATH

# Analysis mode: "network" or "nodes" (default: "network" for backward compatibility)
ANALYSIS_MODE <- if (!is.null(cli_args$ANALYSIS_MODE)) {
  cli_args$ANALYSIS_MODE
} else {
  "network"
}

if (!ANALYSIS_MODE %in% c("network", "nodes")) {
  stop("ANALYSIS_MODE must be either 'network' or 'nodes'", call. = FALSE)
}

# Statistical tests flag: "true" or "false" (default: "true")
RUN_STATS <- if (!is.null(cli_args$RUN_STATS)) {
  tolower(cli_args$RUN_STATS) %in% c("true", "1", "yes")
} else {
  TRUE
}

cat("Running network analysis in", ANALYSIS_MODE, "mode\n")
if (!RUN_STATS) {
  cat("Statistical tests: DISABLED\n")
}

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)
subjects <- read_subjects(SUBJECT_PATH)

# Setup output directories
output_paths <- setup_figure_directories(config$figure$save_path)

# Load network mapping
network_name_map <- load_network_mapping(
  BASE_DIR,
  config$full_settings$network$paths$mappings,
  config$network$atlas,
  config$network$combine_nets
)

# For nodes mode, get network selection
if (ANALYSIS_MODE == "nodes") {
  NET_SELECTION <- config$full_settings$network$networks_to_plot_dist
  NET_SELECTION_NAMES <- network_name_map[unlist(NET_SELECTION)]
}

# ==============================================================================
# Load Subject Data and Calculate Statistics
# ==============================================================================

cat("Loading subject data and calculating", ANALYSIS_MODE, "statistics...\n")

n_subjects <- length(subjects)
data_per_hemi <- list()
node_list <- list()

for (hemi in names(config$hemisphere_names)) {
  annot_file <- file.path(config$network$atlas_path, paste(hemi, config$network$atlas, "annot", sep = "."))
  annotation <- read.fs.annot(annot_file)

  # Build node list for nodes mode
  if (ANALYSIS_MODE == "nodes") {
    region_labels <- unique(annotation$label_names)
    for (region_label in region_labels) {
      components <- get_network_components(region_label)
      if (!is.null(components$comp) && components$net %in% names(NET_SELECTION_NAMES)) {
        node_list[[tolower(components$hemi)]][[components$net]] <-
          c(node_list[[tolower(components$hemi)]][[components$net]], components$comp)
      }
    }
    node_list[[hemi]] <- lapply(node_list[[hemi]], unique)
  }

  # Load data for each simulation type
  data_per_montage <- list()
  for (simulation in config$simulation_types) {
    if (ANALYSIS_MODE == "network") {
      # Network-level analysis
      region_stat_list <- load_network_subject_data(
        subjects,
        config,
        simulation,
        hemi,
        annotation,
        network_name_map
      )
    } else {
      # Node-level analysis
      region_stat_list <- list()

      for (i in seq_along(subjects)) {
        subject <- paste0(subjects[i], "_V1_MR")
        subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
        sim_path <- file.path(config$paths$data, subject, "sim", simulation, subj_type)

        variable_name <- paste(hemi, paste0(subject, "_TDCS_1_scalar.central"),
          names(config$field_name),
          sep = "."
        )
        subject_data <- read.fs.curv(file.path(sim_path, variable_name)) * config$field_scale_factor

        # Calculate statistics for each network node
        region_labels <- unique(annotation$label_names)
        for (net_name in names(NET_SELECTION_NAMES)) {
          for (node in node_list[[hemi]][[net_name]]) {
            region_mask <- numeric(length(annotation$label_names))
            for (region_label in region_labels) {
              match_name <- paste(toupper(hemi), net_name, node, sep = "_")
              if (grepl(match_name, region_label)) {
                region_mask[annotation$label_names == region_label] <- 1
              }
            }

            region_mask_bool <- region_mask == 1
            mapped_net_name <- network_name_map[[net_name]]

            if (i == 1) {
              region_stat_list[[mapped_net_name]][[node]] <- list(
                mean = numeric(n_subjects),
                std = numeric(n_subjects)
              )
            }

            region_data <- subject_data[region_mask_bool]
            region_stat_list[[mapped_net_name]][[node]][["mean"]][i] <- mean(region_data)
            region_stat_list[[mapped_net_name]][[node]][["std"]][i] <- sd(region_data)
          }
        }
      }
    }

    data_per_montage[[config$simulation_type_names[[simulation]]]] <- region_stat_list

    if (ANALYSIS_MODE == "network" && (as.integer(which(config$simulation_types == simulation)) %% 2) == 0) {
      cat("  Processed simulation", simulation, "for", hemi, "\n")
    }
  }

  data_per_hemi[[hemi]] <- data_per_montage
}

# ==============================================================================
# Prepare Plot Data
# ==============================================================================

cat("Preparing plot data...\n")

if (ANALYSIS_MODE == "network") {
  plot_data <- prepare_network_plot_data(
    data_per_hemi,
    config$hemisphere_names,
    config$simulation_type_names,
    n_subjects
  )
} else {
  plot_data <- prepare_network_node_plot_data(
    data_per_hemi,
    config$hemisphere_names,
    config$simulation_type_names,
    n_subjects
  )
}

# ==============================================================================
# Run Statistical Tests
# ==============================================================================

if (RUN_STATS) {
  cat("Running statistical tests...\n")

  if (ANALYSIS_MODE == "network") {
    stat_tests <- run_network_statistical_tests(
      plot_data$individual,
      group_by = c("Hemisphere", "Network")
    )

    # Additional test for hemispheric differences
    wilcox_data_hemi <- tryCatch(
      {
        compare_means(
          Mean ~ Hemisphere,
          plot_data$individual,
          group.by = c("Network"),
          method = "wilcox.test",
          p.adjust.method = "fdr"
        )
      },
      error = function(e) {
        warning("Hemispheric comparison failed: ", e$message)
        NULL
      }
    )
  } else {
    stat_tests <- run_network_statistical_tests(
      plot_data$individual,
      group_by = c("Nodes", "Hemisphere", "Network")
    )
    wilcox_data_hemi <- NULL
  }
} else {
  cat("Skipping statistical tests (disabled by CLI argument)\n")
  stat_tests <- list(anova = data.frame(), wilcox = data.frame())
  wilcox_data_hemi <- NULL
}

# ==============================================================================
# Create Visualization
# ==============================================================================

cat("Creating visualization...\n")

y_label <- "Electric field mean ± SD [V/m]"
field_threshold <- if (names(config$field_name) == "E.magn") 0.15 else NULL

if (ANALYSIS_MODE == "network") {
  plt <- create_network_barplot(
    plot_data$summary,
    y_label = y_label,
    title = paste0("Electric field (", names(config$field_name), ")"),
    greyscale = config$figure$greyscale,
    field_threshold = field_threshold
  )
} else {
  plt <- create_network_node_barplot(
    plot_data$summary,
    y_label = y_label,
    title = paste0("Electric field within the network nodes (", names(config$field_name), ")"),
    greyscale = config$figure$greyscale
  )
}

# Add field-specific scales
if (names(config$field_name) == "E.magn") {
  if (ANALYSIS_MODE == "network") {
    plt <- plt + scale_y_continuous(
      limits = c(0, 0.35),
      breaks = c(0, 0.1, 0.15, 0.2, 0.3)
    )
    annotation_offset <- 0.3
  } else {
    plt <- plt + scale_y_continuous(
      limits = c(0, 0.42),
      breaks = c(0, 0.1, 0.15, 0.2, 0.3, 0.4)
    ) +
      geom_hline(yintercept = field_threshold, color = "grey25", linetype = "dashed")
    annotation_offset <- 0.35
  }
} else {
  if (ANALYSIS_MODE == "network") {
    field_threshold_calc <- quantile(plot_data$summary$Mean, 0.99) / 2
    plt <- plt + scale_y_continuous(limits = c(-0.04, 0.05), breaks = c(-0.04, -0.02, 0.00, 0.02, 0.04))
    annotation_offset <- 0.04
  } else {
    plt <- plt + scale_y_continuous(
      limits = c(-0.12, 0.17),
      breaks = c(-0.1, -0.05, 0, 0.05, 0.1, 0.15)
    )
    annotation_offset <- 0.14
  }
}

# Add brain annotations if enabled
if (config$figure$with_annotations) {
  cat("Adding brain annotations...\n")

  if (ANALYSIS_MODE == "network") {
    # Get networks for first montage across both hemispheres (not unique)
    # This maintains the structure: N networks x 2 hemispheres = 2N rows
    first_montage <- config$simulation_type_names[[1]]
    label_imgs_data <- plot_data$summary %>%
      filter(Montage == first_montage) %>%
      select(Hemisphere, Network)

    label_imgs_nets <- label_imgs_data$Network
    image_df <- data.frame(
      Hemisphere = label_imgs_data$Hemisphere,
      Network = label_imgs_nets,
      Montage = rep(first_montage, times = length(label_imgs_nets)),
      Mean = rep(0, times = length(label_imgs_nets)),
      SD = rep(0, times = length(label_imgs_nets))
    )

    for (hemi_key in names(config$hemisphere_names)) {
      hemi_data <- subset(image_df, Hemisphere == config$hemisphere_names[[hemi_key]])

      # Add annotations for each network using the approach from old implementation
      plt <- plt + geom_custom(
        aes(data = Network, y = annotation_offset),
        data = hemi_data,
        grob_fun = function(x) {
          create_network_grob(
            x, hemi_key, config$network$atlas,
            config$network$annotation_surface,
            config$figure$renders_path,
            network_name_map, 0.225
          )
        }
      )
    }
  } else {
    # Add network render annotations for nodes
    # Helper function to create grob closure
    create_grob_fn <- function(rg) {
      force(rg)
      function(x) rg
    }

    for (hemi in names(config$hemisphere_names)) {
      for (simulation in config$simulation_types) {
        for (net_name in names(NET_SELECTION_NAMES)) {
          for (node in node_list[[hemi]][[net_name]]) {
            df <- data.frame(
              x = node,
              y = annotation_offset,
              Network = NET_SELECTION_NAMES[[net_name]],
              Montage = config$simulation_type_names[[simulation]],
              Hemisphere = config$hemisphere_names[[hemi]]
            )

            network_render_file <- file.path(
              config$figure$renders_path,
              config$network$atlas,
              paste(hemi, config$network$annotation_surface, net_name, node, "png", sep = ".")
            )

            if (file.exists(network_render_file)) {
              img_fl <- image_read(network_render_file)
              img_resized <- image_resize(img_fl, "75%")
              png_grob <- as.raster(img_resized)
              rg <- rasterGrob(png_grob, interpolate = TRUE, height = 0.225)

              plt <- plt + geom_custom(
                aes(data = Network, y = y, x = x),
                data = df,
                grob_fun = create_grob_fn(rg)
              )
            }
          }
        }
      }
    }
  }
}

# ==============================================================================
# Save Outputs
# ==============================================================================

cat("Saving outputs...\n")

if (ANALYSIS_MODE == "network") {
  figure_name <- paste(
    config$settings_id, names(config$field_name), config$space_type,
    config$network$atlas, "all_networks_across_subjects",
    sep = "."
  )
} else {
  figure_name <- paste(
    config$settings_id, names(config$field_name), config$space_type,
    config$network$atlas, paste(NET_SELECTION, collapse = "."),
    "all_networks_nodes",
    sep = "."
  )
}

save_plot_multiple_formats(plt, figure_name, output_paths, config$figure$width, config$figure$height)

# Save statistical results (only if stats were run and not empty)
if (RUN_STATS) {
  if (nrow(stat_tests$anova) > 0) {
    stat_tests$anova$Hemisphere <- str_replace(stat_tests$anova$Hemisphere, " Hemisphere", "")
    write.csv(stat_tests$anova, file.path(output_paths$csv, paste(figure_name, "kruskal", "csv", sep = ".")),
      row.names = FALSE, quote = FALSE
    )
    cat("Saved Kruskal-Wallis test results\n")
  } else {
    cat("Skipped Kruskal-Wallis test (insufficient data)\n")
  }

  if (nrow(stat_tests$wilcox) > 0) {
    stat_tests$wilcox$Hemisphere <- str_replace(stat_tests$wilcox$Hemisphere, " Hemisphere", "")
    write.csv(stat_tests$wilcox, file.path(output_paths$csv, paste(figure_name, "wilcox", "csv", sep = ".")),
      row.names = FALSE, quote = FALSE
    )
    cat("Saved Wilcoxon test results\n")
  } else {
    cat("Skipped Wilcoxon test (insufficient data)\n")
  }

  # Save hemispheric comparison for network mode
  if (ANALYSIS_MODE == "network" && !is.null(wilcox_data_hemi)) {
    wilcox_data_hemi$group1 <- str_replace(wilcox_data_hemi$group1, " Hemisphere", "")
    wilcox_data_hemi$group2 <- str_replace(wilcox_data_hemi$group2, " Hemisphere", "")

    write.csv(wilcox_data_hemi, file.path(output_paths$csv, paste(figure_name, "hemispheres", "wilcox", "csv", sep = ".")),
      row.names = FALSE, quote = FALSE
    )
    cat("Saved hemispheric comparison results\n")
  }
} else {
  cat("Skipped saving statistical tests (disabled by CLI argument)\n")
}

cat("\nNetwork", ANALYSIS_MODE, "analysis complete!\n")
