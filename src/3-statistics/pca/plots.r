#!/usr/bin/env Rscript
# ==============================================================================
# PCA Analysis - Plotting Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))

#' Create PCA biplot with network grouping
#'
#' @param pca_result prcomp PCA result object
#' @param parcel_meta_clean Data frame with parcel metadata and grouping
#' @param comp_1 First PC component number (default: 1)
#' @param comp_2 Second PC component number (default: 2)
#' @param title Plot title
#' @param network_shapes Vector of shape values for networks
#' @return ggplot object
create_pca_biplot <- function(pca_result, parcel_meta_clean, comp_1 = 1, comp_2 = 2,
                              title = "PCA of Anatomical Sensitivity Patterns",
                              network_shapes = c(16, 17, 15, 3, 7, 8, 18, 4, 14)) {
  variance_1 <- round(summary(pca_result)$importance[2, comp_1] * 100, 1)
  variance_2 <- round(summary(pca_result)$importance[2, comp_2] * 100, 1)

  plt <- autoplot(
    pca_result,
    x = comp_1,
    y = comp_2,
    data = parcel_meta_clean,
    colour = "Networks",
    shape = "Networks",
    size = 3,
    label = FALSE,
    loadings = TRUE,
    loadings.label = TRUE,
    loadings.label.size = 3,
    loadings.label.hjust = 1.125,
    frame = TRUE,
    frame.type = "t",
    frame.alpha = 0.05,
    frame.level = 0.95
  ) +
    labs(
      title = title,
      x = paste0("Principal Component ", comp_1, " (", variance_1, "%)"),
      y = paste0("Principal Component ", comp_2, " (", variance_2, "%)")
    ) +
    scale_shape_manual(values = network_shapes) +
    facet_wrap(~network_grouping, ncol = 2) +
    theme_pubclean() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5)
    )

  return(plt)
}

#' Create scree plot showing variance explained
#'
#' @param pca_result prcomp PCA result object
#' @param n_components Number of components to show (default: 10)
#' @return ggplot object
create_scree_plot <- function(pca_result, n_components = 10) {
  variance_df <- data.frame(
    PC = 1:min(n_components, ncol(pca_result$x)),
    variance = (pca_result$sdev^2 / sum(pca_result$sdev^2))[1:min(n_components, ncol(pca_result$x))]
  )

  plt <- ggplot(variance_df, aes(x = PC, y = variance)) +
    geom_col(fill = "steelblue") +
    geom_line(color = "darkred", size = 1) +
    geom_point(color = "darkred", size = 2) +
    scale_x_continuous(breaks = 1:n_components) +
    labs(
      title = "Scree Plot: Variance Explained by Principal Components",
      x = "Principal Component",
      y = "Proportion of Variance Explained"
    ) +
    theme_pubclean() +
    theme(plot.title = element_text(hjust = 0.5))

  return(plt)
}

#' Create loadings heatmap
#'
#' @param pca_result prcomp PCA result object
#' @param n_components Number of components to show (default: 5)
#' @return pheatmap object
create_loadings_heatmap <- function(pca_result, n_components = 5) {
  loadings_mat <- pca_result$rotation[, 1:min(n_components, ncol(pca_result$rotation))]

  heatmap_obj <- pheatmap(
    loadings_mat,
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    scale = "none",
    display_numbers = TRUE,
    fontsize_number = 8,
    color = colorRampPalette(c("blue", "white", "red"))(100),
    main = "PCA Loadings: Montage Contributions to Principal Components",
    angle_col = 0
  )

  return(heatmap_obj)
}

#' Generate multiple PCA biplots for different PC combinations
#'
#' @param pca_result prcomp PCA result object
#' @param parcel_meta_clean Data frame with parcel metadata
#' @param pc_combinations List of PC pairs (e.g., list(c(1,2), c(1,3), c(2,3)))
#' @param output_paths List of output directory paths
#' @param settings_id Settings identifier for filenames
#' @param width Figure width
#' @param height Figure height
#' @param title_base Base title for plots
generate_pca_biplot_series <- function(pca_result, parcel_meta_clean,
                                       pc_combinations = list(c(1, 2), c(1, 3), c(2, 3)),
                                       output_paths, settings_id,
                                       width = 8, height = 6,
                                       title_base = "PCA of Anatomical Sensitivity Patterns") {
  network_shapes <- c(16, 17, 15, 3, 7, 8, 18, 4, 14)

  for (pc_pair in pc_combinations) {
    comp_1 <- pc_pair[1]
    comp_2 <- pc_pair[2]

    plt <- create_pca_biplot(
      pca_result, parcel_meta_clean, comp_1, comp_2,
      title = title_base, network_shapes = network_shapes
    )

    figure_name <- paste(
      settings_id, "per_parcel_groupped_residual_mean-pca",
      paste0("PC", comp_1, "_PC", comp_2), "subs",
      sep = "."
    )

    save_plot_multiple_formats(plt, figure_name, output_paths, width, height)

    cat("Saved PCA biplot: PC", comp_1, "vs PC", comp_2, "\n")
  }

  invisible(NULL)
}

#' Create correlation heatmap between PCA components and variables
#'
#' @param cor_results Data frame with correlation results
#' @param pc_names Vector of PC names to include
#' @return ggplot object
create_pca_correlation_heatmap <- function(cor_results, pc_names = c("PC1", "PC2", "PC3")) {
  cor_plot_data <- cor_results %>%
    filter(pc %in% pc_names) %>%
    mutate(
      significance_label = case_when(
        p_adj < 0.001 ~ "***",
        p_adj < 0.01 ~ "**",
        p_adj < 0.05 ~ "*",
        TRUE ~ ""
      )
    )

  plt <- ggplot(cor_plot_data, aes(x = pc, y = variable, fill = estimate)) +
    geom_tile(color = "white") +
    geom_text(aes(label = significance_label), size = 5, vjust = 0.8) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-1, 1),
      name = "Spearman's <U+03C1>"
    ) +
    labs(
      title = "Correlations: PCA Components vs. Morphological Variables",
      x = "Principal Component",
      y = "Variable"
    ) +
    theme_pubclean() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0),
      axis.text.y = element_text(size = 9)
    )

  return(plt)
}
