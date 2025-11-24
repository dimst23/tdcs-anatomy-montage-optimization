#!/usr/bin/env Rscript
# ==============================================================================
# Spatial Location Analysis
# Purpose: Calculate center of mass, FWHM, and spatial distribution of E-fields
#
# Usage:
#   Rscript spatial_location.r <JSON_SETTINGS_PATH> <BASE_DIR> <EEG_LOCATIONS_PATH> <MNI_EEG_COORDS_FILE_PATH>
#
# Arguments:
#   JSON_SETTINGS_PATH       - Path to JSON configuration file
#   BASE_DIR                 - Base directory for data paths
#   EEG_LOCATIONS_PATH       - Path to subject-specific EEG locations directory
#   MNI_EEG_COORDS_FILE_PATH - Path to MNI EEG coordinates CSV file
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
  library(ggplot2)
  library(ggh4x)
  library(ggpubr)
  library(purrr)
  library(latex2exp)
})

# ==============================================================================
# Parse Command-Line Arguments
# ==============================================================================

cli_args <- parse_cli_args(
  min_args = 4,
  max_args = 4,
  arg_names = c("JSON_SETTINGS_PATH", "BASE_DIR", "EEG_LOCATIONS_PATH", "MNI_EEG_COORDS_FILE_PATH")
)

JSON_SETTINGS_PATH <- cli_args$JSON_SETTINGS_PATH
BASE_DIR <- cli_args$BASE_DIR
EEG_LOCATIONS_PATH <- cli_args$EEG_LOCATIONS_PATH
MNI_EEG_COORDS_FILE_PATH <- cli_args$MNI_EEG_COORDS_FILE_PATH

# ==============================================================================
# Load Configuration
# ==============================================================================

config <- parse_settings(JSON_SETTINGS_PATH, BASE_DIR)
subjects <- read_subjects(config$paths$subjects)

# Setup output directories
output_paths <- setup_figure_directories(config$figure$save_path)

# Check if FWHM calculation should be enabled
ENABLE_FWHM <- ifelse(
  is.null(config$full_settings$general$enable_fwhm),
  FALSE,
  config$full_settings$general$enable_fwhm
)
ENABLE_FWHM <- TRUE

# ==============================================================================
# Helper Functions
# ==============================================================================

center_of_mass_max <- function(efield_df, cutoff_val) {
  threshold <- cutoff_val * max(efield_df$value)
  efield_thresh <- efield_df[efield_df$value >= threshold, ]

  com_x <- sum(efield_thresh$x * efield_thresh$value) / sum(efield_thresh$value)
  com_y <- sum(efield_thresh$y * efield_thresh$value) / sum(efield_thresh$value)
  com_z <- sum(efield_thresh$z * efield_thresh$value) / sum(efield_thresh$value)

  list(x = com_x, y = com_y, z = com_z)
}

calculate_fwhm_3d <- function(efield_df, max_threshold = 0.99) {
  max_val <- quantile(efield_df$value, max_threshold, na.rm = TRUE)
  half_max <- max_val / 2
  above_half_max <- efield_df[efield_df$value >= half_max, ]

  if (nrow(above_half_max) < 2) {
    return(list(fwhm_volume = 0, fwhm_diameter = 0, n_vertices = 0))
  }

  if (nrow(above_half_max) >= 4) {
    tryCatch(
      {
        hull_indices <- chull(above_half_max[, c("x", "y")])
        hull_points <- above_half_max[hull_indices, ]
        fwhm_area <- 0.5 * abs(sum(hull_points$x * c(hull_points$y[-1], hull_points$y[1]) -
          c(hull_points$x[-1], hull_points$x[1]) * hull_points$y))
        distances <- dist(above_half_max[, c("x", "y", "z")])
        fwhm_diameter <- max(distances)

        list(
          fwhm_area = fwhm_area, fwhm_diameter = fwhm_diameter,
          n_vertices = nrow(above_half_max), max_val_99th = max_val
        )
      },
      error = function(e) {
        x_range <- diff(range(above_half_max$x))
        y_range <- diff(range(above_half_max$y))
        z_range <- diff(range(above_half_max$z))
        fwhm_diameter <- sqrt(x_range^2 + y_range^2 + z_range^2)

        list(
          fwhm_area = x_range * y_range, fwhm_diameter = fwhm_diameter,
          n_vertices = nrow(above_half_max), max_val_99th = max_val
        )
      }
    )
  } else {
    x_range <- diff(range(above_half_max$x))
    y_range <- diff(range(above_half_max$y))
    z_range <- diff(range(above_half_max$z))
    fwhm_diameter <- sqrt(x_range^2 + y_range^2 + z_range^2)

    list(
      fwhm_area = x_range * y_range, fwhm_diameter = fwhm_diameter,
      n_vertices = nrow(above_half_max), max_val_99th = max_val
    )
  }
}

project_azimuthal <- function(x, y, z) {
  r <- sqrt(x^2 + y^2 + z^2)
  theta <- atan2(y, x)
  phi <- acos(z / r)
  x2d <- phi * cos(theta)
  y2d <- phi * sin(theta)
  data.frame(x2d = x2d, y2d = y2d)
}

# ==============================================================================
# Load Cortex Node IDs
# ==============================================================================

cat("Loading cortex node IDs...\n")

rh_cortex_node_ids <- get_cortex_node_ids(config$paths$fsaverage_template, "rh")
lh_cortex_node_ids <- get_cortex_node_ids(config$paths$fsaverage_template, "lh")
cortex_only_ids <- list(rh = rh_cortex_node_ids, lh = lh_cortex_node_ids)

# ==============================================================================
# Calculate Center of Mass and FWHM
# ==============================================================================

cat("Calculating spatial metrics...\n")

threshold_fraction <- 0.99
center_of_mass <- list()

if (ENABLE_FWHM) {
  fwhm_results <- list()
  cat("FWHM calculation is ENABLED.\n")
} else {
  cat("FWHM calculation is DISABLED.\n")
}

for (simulation in config$simulation_types) {
  sim_name <- config$simulation_type_names[[simulation]]

  for (i in seq_along(subjects)) {
    subject <- paste0(subjects[i], "_V1_MR")

    # Load data for both hemispheres
    hemi_data <- list()
    for (hemi in names(config$hemisphere_names)) {
      surf_path <- file.path(config$paths$fsaverage_template, "surf", paste0(hemi, ".pial"))
      surface <- read.fs.surface(surf_path)

      subj_type <- paste(gsub("\\.", "_", config$space_type), "overlays", sep = "_")
      sim_path <- file.path(config$paths$data, subject, "sim", simulation, subj_type)
      field_name <- paste(hemi, paste0(subject, "_TDCS_1_scalar.central"),
        names(config$field_name),
        sep = "."
      )
      values <- read.fs.curv(file.path(sim_path, field_name))

      nodes_to_keep <- cortex_only_ids[[hemi]]
      hemi_data[[hemi]] <- data.frame(
        x = surface$vertices[nodes_to_keep, 1],
        y = surface$vertices[nodes_to_keep, 2],
        z = surface$vertices[nodes_to_keep, 3],
        value = values[nodes_to_keep] * config$field_scale_factor
      )
    }

    # Combine both hemispheres
    combined <- do.call(rbind, hemi_data)

    # Calculate center of mass
    com <- center_of_mass_max(combined, threshold_fraction)
    center_of_mass[[sim_name]]$x[i] <- com$x
    center_of_mass[[sim_name]]$y[i] <- com$y
    center_of_mass[[sim_name]]$z[i] <- com$z

    # Find maximum location
    max_loc <- which.max(combined$value)
    center_of_mass[[sim_name]]$x_max[i] <- combined[max_loc, 1]
    center_of_mass[[sim_name]]$y_max[i] <- combined[max_loc, 2]
    center_of_mass[[sim_name]]$z_max[i] <- combined[max_loc, 3]
    center_of_mass[[sim_name]]$max_val[i] <- combined[max_loc, 4]

    # Calculate FWHM metrics
    if (ENABLE_FWHM) {
      fwhm_metrics <- calculate_fwhm_3d(combined, max_threshold = 0.99)
      fwhm_results[[sim_name]]$fwhm_area[i] <- fwhm_metrics$fwhm_area
      fwhm_results[[sim_name]]$fwhm_diameter[i] <- fwhm_metrics$fwhm_diameter
      fwhm_results[[sim_name]]$n_vertices_fwhm[i] <- fwhm_metrics$n_vertices
      fwhm_results[[sim_name]]$max_val_99th[i] <- fwhm_metrics$max_val_99th
    }

    # Print progress
    if (i %% 10 == 0) {
      cat("  Processed", i, "subjects for", sim_name, "\n")
    }
  }
}

# Convert results to data frames
df_com <- map_dfr(center_of_mass, ~ as.data.frame(.x), .id = "Montage")

# ==============================================================================
# FWHM Analysis and Visualization
# ==============================================================================

if (ENABLE_FWHM) {
  cat("\nAnalyzing FWHM results...\n")

  df_fwhm <- map_dfr(fwhm_results, ~ as.data.frame(.x), .id = "Montage")
  total_cortex_vertices <- length(rh_cortex_node_ids) + length(lh_cortex_node_ids)

  # Define montages to include
  montages_to_include <- c("P3-P4", "P3-Fp2", "F3-P4", "F3-Fp2")

  # Calculate summary statistics
  fwhm_summary <- df_fwhm %>%
    group_by(Montage) %>%
    summarise(
      mean_fwhm_area = mean(fwhm_area, na.rm = TRUE),
      sd_fwhm_area = sd(fwhm_area, na.rm = TRUE),
      mean_fwhm_diameter = mean(fwhm_diameter, na.rm = TRUE),
      sd_fwhm_diameter = sd(fwhm_diameter, na.rm = TRUE),
      mean_n_vertices = mean(n_vertices_fwhm, na.rm = TRUE),
      mean_vertex_fraction = mean(n_vertices_fwhm, na.rm = TRUE) / total_cortex_vertices,
      mean_max_val_99th = mean(max_val_99th, na.rm = TRUE),
      n_subjects = n(),
      .groups = "drop"
    ) %>%
    mutate(
      electrode_shape = str_sub(Montage, 1, 1),
      electrode_pair = str_sub(Montage, 2),
      shape_label = case_when(
        electrode_shape == "R" ~ "Rectangular",
        electrode_shape == "C" ~ "Circular",
        TRUE ~ electrode_shape
      )
    )

  cat("\n=== FWHM Analysis Summary ===\n")
  print(fwhm_summary %>%
    select(Montage, shape_label, mean_fwhm_area, mean_fwhm_diameter, mean_n_vertices))

  # Statistical comparison
  fwhm_comparison <- df_fwhm %>%
    mutate(
      electrode_shape = str_sub(Montage, 1, 1),
      electrode_pair = str_sub(Montage, 2),
      shape_label = case_when(
        electrode_shape == "R" ~ "Rectangular",
        electrode_shape == "C" ~ "Circular",
        TRUE ~ electrode_shape
      )
    ) %>%
    filter(electrode_pair %in% montages_to_include)

  area_test <- wilcox.test(fwhm_area ~ shape_label, data = fwhm_comparison)
  diameter_test <- wilcox.test(fwhm_diameter ~ shape_label, data = fwhm_comparison)

  cat("\n=== Statistical Comparison (Wilcoxon Rank Sum Test) ===\n")
  cat("FWHM Area - Rectangular vs Circular: p =", format(area_test$p.value, digits = 4), "\n")
  cat("FWHM Diameter - Rectangular vs Circular: p =", format(diameter_test$p.value, digits = 4), "\n")

  # Save FWHM results
  fwhm_save_path <- file.path(output_paths$rds, paste(config$settings_id, "fwhm_results.rds", sep = "."))
  saveRDS(list(
    fwhm_data = df_fwhm,
    fwhm_summary = fwhm_summary,
    statistical_tests = list(area_test = area_test, diameter_test = diameter_test)
  ), fwhm_save_path)

  cat("FWHM results saved to:", fwhm_save_path, "\n")

  # Create FWHM visualization
  fwhm_plot_data <- df_fwhm %>%
    mutate(
      electrode_shape = str_sub(Montage, 1, 1),
      electrode_pair = str_sub(Montage, 2),
      shape_label = case_when(
        electrode_shape == "R" ~ "Rectangular",
        electrode_shape == "C" ~ "Circular",
        TRUE ~ electrode_shape
      )
    ) %>%
    filter(electrode_pair %in% montages_to_include) %>%
    select(Montage, shape_label, electrode_pair, fwhm_area, fwhm_diameter, n_vertices_fwhm) %>%
    pivot_longer(
      cols = c(fwhm_area, fwhm_diameter),
      names_to = "metric", values_to = "value"
    ) %>%
    mutate(
      metric_label = case_when(
        metric == "fwhm_area" ~ "FWHM Area (mm$^2$)",
        metric == "fwhm_diameter" ~ "FWHM Diameter (mm)"
      )
    )

  fwhm_boxplot <- ggplot(fwhm_plot_data, aes(x = shape_label, y = value, fill = shape_label)) +
    geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 0.5) +
    facet_wrap(~metric_label, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = c("Rectangular" = "#E74C3C", "Circular" = "#3498DB")) +
    labs(
      title = "Full Width at Half Maximum (FWHM) Comparison",
      subtitle = "Spatial spread of electric field at 50% of 99th percentile maximum",
      x = "Electrode Shape",
      y = "FWHM Value",
      fill = "Electrode Shape"
    ) +
    theme_pubclean() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "none"
    ) +
    stat_compare_means(method = "wilcox.test", label = "p.signif", label.y.npc = 0.9, size = 4)

  # Save FWHM plot
  fwhm_figure_name <- paste(config$settings_id, "fwhm_comparison", config$space_type, sep = ".")
  ggsave(file.path(output_paths$png, paste(fwhm_figure_name, "png", sep = ".")), fwhm_boxplot,
    width = config$figure$width, height = config$figure$height * 0.8, device = "png", dpi = 600
  )
  ggsave(file.path(output_paths$pdf, paste(fwhm_figure_name, "pdf", sep = ".")), fwhm_boxplot,
    width = config$figure$width, height = config$figure$height * 0.8, device = cairo_pdf
  )
}

# ==============================================================================
# Create Spatial Density Plots
# ==============================================================================

cat("\nCreating spatial distribution visualizations...\n")

elec <- read.csv(MNI_EEG_COORDS_FILE_PATH,
  header = FALSE,
  col.names = c("Type", "x", "y", "z", "Electrode")
)

# Project electrodes and CoM data
ele_df <- elec %>%
  mutate(projection = project_azimuthal(x, y, z)) %>%
  mutate(xe_proj = projection$x2d, ye_proj = projection$y2d) %>%
  select(-projection)

proj_com_df <- df_com %>%
  mutate(projection = project_azimuthal(x, y, z)) %>%
  mutate(xp_proj = projection$x2d, yp_proj = projection$y2d) %>%
  select(-projection)

# Prepare electrode coordinates
ele_coords <- ele_df %>%
  select(Electrode, x, y, z) %>%
  rename_with(.fn = ~ paste0(., "e"), .cols = c(x, y, z)) %>%
  rename_with(tolower)

# Load subject-specific electrode positions
electrodes <- unique(str_split(paste(config$simulation_type_names, collapse = "-"), "-")[[1]])
electrode_xyz <- map_dfr(subjects, function(subject) {
  csv_path <- file.path(
    EEG_LOCATIONS_PATH, paste0(subject, "_V1_MR"),
    "EEG10-10_UI_Jurak_2007.csv"
  )

  if (file.exists(csv_path)) {
    read.csv(csv_path, header = FALSE, col.names = c("Type", "x", "y", "z", "Electrode")) %>%
      filter(Electrode %in% electrodes) %>%
      mutate(Subject = subject)
  } else {
    NULL
  }
}) %>%
  select(Subject, Electrode, x, y, z) %>%
  arrange(Electrode, Subject) %>%
  rename_with(tolower)

# Prepare montage metadata
montage_info <- proj_com_df %>%
  distinct(Montage) %>%
  mutate(
    electrode_shape = str_sub(Montage, 1, 1),
    Electrodes = str_sub(Montage, 2),
    anode = str_split(Electrodes, "-", simplify = TRUE)[, 1],
    cathode = str_split(Electrodes, "-", simplify = TRUE)[, 2]
  ) %>%
  rename_with(tolower) %>%
  select(Montage = montage, electrode_shape, anode, cathode) %>%
  pivot_longer(cols = c(anode, cathode), names_to = "electrode_pos", values_to = "electrode") %>%
  left_join(electrode_xyz %>% group_by(electrode) %>%
    summarise(xe = mean(x), ye = mean(y), ze = mean(z)), by = "electrode") %>%
  distinct(Montage, electrode, xe, ye, ze)

# Prepare data for plotting
proj_com_df_long <- proj_com_df %>%
  pivot_longer(cols = c(y_max, z_max), names_to = "axis", values_to = "coord_y") %>%
  mutate(projection = case_when(axis == "y_max" ~ "Axial", axis == "z_max" ~ "Coronal")) %>%
  rename(coord_x = x_max)

# Load surface for background (using fsaverage5 for reduced resolution)
fsaverage5_path <- gsub("fsaverage$", "fsaverage5", config$paths$fsaverage_template)
red_srf_lh <- read.fs.surface(file.path(fsaverage5_path, "surf", "lh.pial"))
red_srf_rh <- read.fs.surface(file.path(fsaverage5_path, "surf", "rh.pial"))
red_srf <- data.frame(
  x = c(red_srf_lh$vertices[, 1], red_srf_rh$vertices[, 1]),
  y = c(red_srf_lh$vertices[, 2], red_srf_rh$vertices[, 2]),
  z = c(red_srf_lh$vertices[, 3], red_srf_rh$vertices[, 3])
)

proj_red_srf <- red_srf %>%
  pivot_longer(cols = c(y, z), names_to = "axis", values_to = "coord_y") %>%
  mutate(projection = case_when(axis == "y" ~ "Axial", axis == "z" ~ "Coronal")) %>%
  rename(coord_x = x)

proj_montage_info <- montage_info %>%
  pivot_longer(cols = c(ye, ze), names_to = "axis", values_to = "coord_y") %>%
  mutate(projection = case_when(axis == "ye" ~ "Axial", axis == "ze" ~ "Coronal")) %>%
  rename(coord_x = xe)

# Create spatial density plot
plt <- ggplot(proj_com_df_long) +
  geom_path(data = proj_red_srf, aes(x = coord_x, y = coord_y), color = "grey75", alpha = 0.25) +
  geom_point(
    data = proj_montage_info, aes(x = coord_x, y = coord_y),
    shape = 18, size = 3, color = "black"
  ) +
  geom_point(aes(x = coord_x, y = coord_y), shape = 4, size = 1, color = "grey25") +
  geom_text(
    data = proj_montage_info, aes(x = coord_x, y = coord_y, label = electrode),
    vjust = -1, color = "black", size = 3
  ) +
  facet_grid(rows = vars(projection), cols = vars(Montage), switch = "y") +
  scale_y_continuous(limits = c(-110, 100)) +
  labs(
    title = "Spatial Distribution of Maximum Field (Magnitude)",
    x = "X MNI [mm]",
    y = "Z/Y MNI [mm]"
  ) +
  coord_fixed() +
  theme_pubclean() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right",
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.placement = "outside"
  )

# Save spatial density plot
figure_name <- paste(config$settings_id, "global_max", config$space_type, "density_xz", sep = ".")
ggsave(file.path(output_paths$png, paste(figure_name, "png", sep = ".")), plt,
  width = config$figure$width, height = config$figure$height, device = "png", dpi = 600
)
ggsave(file.path(output_paths$pdf, paste(figure_name, "pdf", sep = ".")), plt,
  width = config$figure$width, height = config$figure$height, device = cairo_pdf
)
ggsave(file.path(output_paths$eps, paste(figure_name, "eps", sep = ".")), plt,
  width = config$figure$width, height = config$figure$height, device = cairo_ps
)

cat("\nSpatial location analysis complete!\n")
cat("Results saved to:", config$figure$save_path, "\n")
