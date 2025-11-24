#!/usr/bin/env Rscript
# ==============================================================================
# GLM Feature Importance Analysis
# Purpose: Calculate and visualize feature importance across electrode montages
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
source(file.path(dirname(script_dir), "glm", "packages.r"))
source(file.path(dirname(script_dir), "glm", "functions.r"))
source(file.path(dirname(script_dir), "glm", "plots.r"))
source(file.path(dirname(script_dir), "common", "settings_parser.r"))

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 2,
  max_args = 2,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)
subjects <- read_subjects(config$paths$subjects)

# Setup output directories
output_paths <- setup_figure_directories(config$figure$save_path)

# Figure dimensions
FIGURE_WIDTH <- 8 # inches
FIGURE_HEIGHT <- 6 # inches

# ==============================================================================
# Load GLM Results
# ==============================================================================

cat("Loading GLM results...\n")

# Construct path from config
DATA_BASE_PATH <- file.path(config$paths$glm_results, "glm", "per_subject", "total")

glm_results <- list()
for (hemi in config$hemispheres) {
  for (sim_type in config$simulation_types) {
    # Construct file name including hemisphere
    scale_factor_name <- paste0("SF-", round(config$field_scale_factor, 2))
    data_file_name <- paste(
      hemi, config$settings_id, config$space_type, config$network$atlas,
      sim_type, config$ind_variable, scale_factor_name, "json",
      sep = "."
    )

    sim_data <- jsonlite::read_json(file.path(DATA_BASE_PATH, data_file_name))
    sim_name <- config$simulation_type_names[[sim_type]]
    glm_results[[hemi]][[sim_name]] <- sim_data
  }
}

# ==============================================================================
# Calculate Feature Importance
# ==============================================================================

cat("Calculating feature importance...\n")

df_importance <- calculate_feature_importance(
  glm_results,
  config$hemispheres,
  config$glm$terms,
  config$simulation_type_names
)

# ==============================================================================
# Prepare Plot Data
# ==============================================================================

cat("Preparing visualization data...\n")

plot_data <- prepare_feature_importance_plot_data(df_importance)

# ==============================================================================
# Create Visualizations
# ==============================================================================

cat("Creating visualizations...\n")

# Get feature name mapping
feature_name_map <- get_feature_name_mapping()

# Create annotations heatmap
annotations_plot <- create_feature_importance_heatmap(
  plot_data$df_long_z,
  plot_data$montage_order,
  feature_name_map
)

# Create montage metadata for clustering
montage_meta <- create_montage_metadata(plot_data$montage_order)
anno_colors <- get_default_annotation_colors()

# Prepare matrices for hierarchical clustering
z_mat <- plot_data$df_z %>%
  select(feature, montage, z_value) %>%
  pivot_wider(names_from = montage, values_from = z_value) %>%
  column_to_rownames("feature") %>%
  as.matrix()

z_mat_global <- plot_data$df_long_z %>%
  mutate(
    feature_char = as.character(feature),
    feature_label = ifelse(
      feature_char %in% names(feature_name_map),
      feature_name_map[feature_char],
      feature_char
    )
  ) %>%
  select(feature_label, montage, z_value) %>%
  pivot_wider(names_from = montage, values_from = z_value) %>%
  column_to_rownames("feature_label") %>%
  as.matrix()

# Create hierarchical clustering heatmap
el_heatmap <- create_hierarchical_heatmap(
  z_mat_global,
  montage_meta,
  anno_colors,
  title = "Clustered Heatmap with Electrode Shape and Region"
)

# ==============================================================================
# Save Figures
# ==============================================================================

cat("Saving figures...\n")

save_plots <- list(heatmap = el_heatmap, annotations = annotations_plot)

for (plt_name in names(save_plots)) {
  plt <- save_plots[[plt_name]]
  figure_name <- paste(config$settings_id, "glm_analysis", config$space_type, plt_name, sep = ".")

  # Check if it's a ggplot object or pheatmap
  if (inherits(plt, "gg") || inherits(plt, "ggplot")) {
    save_plot_multiple_formats(plt, figure_name, output_paths, FIGURE_WIDTH, FIGURE_HEIGHT)
  } else {
    save_heatmap_multiple_formats(plt, figure_name, output_paths, FIGURE_WIDTH, FIGURE_HEIGHT)
  }

  cat("  Saved:", figure_name, "\n")
}

cat("\nFeature importance analysis complete!\n")
