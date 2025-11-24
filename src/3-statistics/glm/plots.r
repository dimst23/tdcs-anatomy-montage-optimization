#!/usr/bin/env Rscript
# ==============================================================================
# GLM Analysis - Plotting Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))

#' Create feature importance heatmap with annotations
#'
#' @param df_long_z Long format data frame with z-scores
#' @param montage_order Ordered vector of montage names
#' @param feature_name_map Named vector mapping feature names to display names
#' @return ggplot object
create_feature_importance_heatmap <- function(df_long_z, montage_order, feature_name_map) {
  # Apply feature name mapping
  df_long_z <- df_long_z %>%
    mutate(
      feature_char = as.character(feature),
      feature_label = ifelse(
        feature_char %in% names(feature_name_map),
        feature_name_map[feature_char],
        feature_char
      )
    ) %>%
    select(-feature_char)

  # Determine label colors based on z-value intensity
  df_long_z <- df_long_z %>%
    mutate(label_color = ifelse(z_value > 3, "#f7f7f7", "black"))

  # Create heatmap
  plt <- ggplot(df_long_z, aes(
    x = montage, y = fct_reorder(feature_label, `Feature Sum`),
    fill = z_value
  )) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(z_value, 2), color = label_color), size = 3) +
    scale_fill_gradient(low = "white", high = "black", name = "Z-score") +
    scale_color_identity() +
    labs(
      title = "Feature Importance per Electrode Montage (global)",
      x = "Montage",
      y = "Anatomical Feature or Interaction"
    ) +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plt)
}

#' Create hierarchical clustering heatmap with montage annotations
#'
#' @param z_mat Matrix of z-scores (features x montages)
#' @param montage_meta Data frame with montage metadata (shape, region, symmetry)
#' @param anno_colors List of color palettes for annotations
#' @param title Plot title
#' @return pheatmap object
create_hierarchical_heatmap <- function(z_mat, montage_meta, anno_colors,
                                        title = "Hierarchical Clustering of Feature x Montage Z-Scores") {
  # Convert montage metadata to row names format
  anno_col <- montage_meta %>%
    column_to_rownames("montage")

  # Create heatmap
  heatmap_obj <- pheatmap(
    z_mat,
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    scale = "none",
    display_numbers = TRUE,
    fontsize_number = 8,
    annotation_col = anno_col,
    annotation_colors = anno_colors,
    color = colorRampPalette(c("white", "indianred"))(100),
    main = title,
    silent = TRUE
  )

  return(heatmap_obj)
}

#' Create montage metadata for clustering annotations
#'
#' @param montage_names Vector of montage names
#' @return Data frame with shape, region, and symmetry columns
create_montage_metadata <- function(montage_names) {
  montage_meta <- tibble(montage = montage_names) %>%
    mutate(
      shape = ifelse(str_starts(montage, "r"), "rectangular", "circular"),
      region = case_when(
        str_detect(montage, "F3") ~ "frontal",
        str_detect(montage, "P3") ~ "parietal",
        TRUE ~ "other"
      ),
      symmetry = case_when(
        str_detect(montage, "F4") ~ "symmetrical",
        str_detect(montage, "P4") ~ "symmetrical",
        str_detect(montage, "Fp2") ~ "asymmetrical",
        TRUE ~ "other"
      )
    )

  return(montage_meta)
}

#' Get default annotation colors for montage metadata
#'
#' @return List of color palettes
get_default_annotation_colors <- function() {
  anno_colors <- list(
    shape = c(rectangular = "#1f77b4", circular = "#ff7f0e"),
    region = c(frontal = "#66c2a5", parietal = "#8da0cb", other = "#cccccc"),
    symmetry = c(symmetrical = "#66c2a5", asymmetrical = "#fc8d62", other = "#cccccc")
  )

  return(anno_colors)
}

#' Get default feature name mapping
#'
#' @return Named vector of feature display names
get_feature_name_mapping <- function() {
  feature_name_map <- c(
    "sulc" = "Sulcal Depth",
    "pial_lgi" = "Local Gyrification Index (LGI)",
    "csf_thickness" = "CSF Thickness",
    "skull_thickness" = "Skull Thickness",
    "skin_thickness" = "Skin Thickness",
    "area" = "Cortical Surface Area (CSA)",
    "thickness" = "Cortical Thickness (CTh.)",
    "thickness:area" = "CTh. x CSA",
    "thickness:sulc" = "CTh. X Sulcal Depth",
    "thickness:pial_lgi" = "CTh. x LGI",
    "thickness:csf_thickness" = "CTh. x CSF Thickness",
    "thickness:pial_lgi:sulc" = "CTh. x LGI x Sulcal Depth",
    "skull_thickness:skin_thickness" = "Skull x Skin Thickness",
    "csf_thickness:skull_thickness" = "CSF x Skull Thickness",
    "pial_lgi:sulc" = "LGI x Sulcal Depth"
  )

  return(feature_name_map)
}

#' Prepare feature importance data for visualization
#'
#' @param df_feature_importance Data frame with feature, montage, and score
#' @return List with df_long, df_long_z (ungrouped z-scores), df_z (grouped abs z-scores), montage_order
prepare_feature_importance_plot_data <- function(df_feature_importance) {
  # Pivot to wide format
  df_wide <- df_feature_importance %>%
    pivot_wider(names_from = montage, values_from = score)

  # Add row-wise Feature Sum
  df_wide <- df_wide %>%
    mutate(`Feature Sum` = rowSums(select(., where(is.numeric)), na.rm = TRUE))

  # Add column-wise Setup Sum
  setup_sum <- df_wide %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
    mutate(feature = "Setup Sum")

  # Bind to final table
  df_final <- bind_rows(df_wide, setup_sum) %>%
    relocate(feature)

  # Extract montage order (lowest to highest Setup Sum)
  montage_order <- df_final %>%
    filter(feature == "Setup Sum") %>%
    select(where(is.numeric), -`Feature Sum`) %>%
    pivot_longer(cols = everything(), names_to = "montage", values_to = "setup_sum") %>%
    arrange(setup_sum) %>%
    pull(montage)

  # Order rows by Feature Sum
  df_ordered <- df_final %>%
    filter(feature != "Setup Sum") %>%
    arrange(`Feature Sum`)

  # Append Setup Sum row
  setup_sum_row <- df_final %>%
    filter(feature == "Setup Sum")

  # Reorder columns
  df_ordered <- bind_rows(df_ordered, setup_sum_row) %>%
    select(feature, all_of(montage_order), `Feature Sum`)

  # Convert to long format for plotting
  df_long <- df_ordered %>%
    filter(feature != "Setup Sum") %>%
    pivot_longer(cols = -c(feature, `Feature Sum`), names_to = "montage", values_to = "value")

  # Calculate z-scores (ungrouped, for annotations heatmap)
  df_long_z <- df_long %>%
    mutate(z_value = scale(value)[, 1]) %>%
    mutate(
      montage = factor(montage, levels = montage_order),
      feature = fct_reorder(feature, `Feature Sum`)
    )

  # Calculate z-scores (grouped by feature, absolute, for hierarchical clustering)
  df_z <- df_long %>%
    group_by(feature) %>%
    mutate(z_value = abs(scale(value)[, 1])) %>%
    ungroup() %>%
    mutate(
      montage = factor(montage, levels = montage_order),
      feature = fct_reorder(feature, `Feature Sum`)
    )

  return(list(
    df_long = df_long,
    df_long_z = df_long_z,
    df_z = df_z,
    montage_order = montage_order
  ))
}
