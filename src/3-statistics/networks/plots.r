#!/usr/bin/env Rscript
# ==============================================================================
# Network Analysis - Plotting Functions
# ==============================================================================

# Source dependencies
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(script_dir, "packages.r"))
source(file.path(script_dir, "functions.r"))

#' Create network bar plot with error bars
#'
#' @param data_df Data frame with network statistics
#' @param y_label Y-axis label
#' @param title Plot title
#' @param greyscale Use greyscale colors
#' @param field_threshold Optional threshold line to plot
#' @return ggplot object
create_network_barplot <- function(data_df, y_label = "Electric field mean +/- SD [V/m]",
                                   title = NULL, greyscale = FALSE,
                                   field_threshold = NULL) {
  plt <- ggplot(data_df, aes(x = Network, y = Mean, fill = Montage)) +
    geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.8),
      width = 0.25
    ) +
    facet_wrap2(vars(Hemisphere),
      scales = "free", nrow = 2,
      strip = strip_split(c("right"))
    ) +
    labs(y = y_label, x = "Functional Networks") +
    theme_pubclean()

  if (greyscale) {
    plt <- plt + scale_fill_grey(start = 0.2, end = 0.8)
  }

  if (!is.null(field_threshold)) {
    plt <- plt + geom_hline(
      yintercept = field_threshold,
      color = "grey25", linetype = "dashed"
    )
  }

  if (!is.null(title)) {
    plt <- plt + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  return(plt)
}

#' Create network node bar plot
#'
#' @param data_df Data frame with node-level network statistics
#' @param y_label Y-axis label
#' @param title Plot title
#' @param greyscale Use greyscale colors
#' @return ggplot object
create_network_node_barplot <- function(data_df, y_label = "Electric field mean +/- SD [V/m]",
                                        title = NULL, greyscale = FALSE) {
  plt <- ggplot(data_df, aes(x = Nodes, y = Mean, fill = Montage)) +
    geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
    geom_errorbar(
      aes(ymin = Mean - SD, ymax = Mean + SD),
      position = position_dodge(width = 0.8),
      width = 0.25
    ) +
    facet_wrap2(vars(Hemisphere, Network),
      scales = "free", nrow = 2,
      strip = strip_split(c("right", "top"))
    ) +
    labs(y = y_label, x = "Network nodes") +
    theme_pubclean()

  if (greyscale) {
    plt <- plt + scale_fill_grey(start = 0.2, end = 0.8)
  }

  if (!is.null(title)) {
    plt <- plt + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  return(plt)
}

#' Add brain annotation images to network plot
#'
#' @param plt Base ggplot object
#' @param image_df Data frame with network annotations
#' @param hemispheres Named vector of hemisphere labels
#' @param network_renders_path Path to network render images
#' @param atlas Atlas name
#' @param annotation_surface Surface type
#' @param network_name_map Network name mapping
#' @param annotation_height Height of annotation images
#' @param y_position Y position for annotations
#' @return Updated ggplot object with annotations
add_network_annotations <- function(plt, image_df, hemispheres,
                                    network_renders_path, atlas, annotation_surface,
                                    network_name_map, annotation_height = 0.225,
                                    y_position = 0.3) {
  for (hemi_key in names(hemispheres)) {
    hemi_data <- subset(image_df, Hemisphere == hemispheres[[hemi_key]])

    # Add annotations using the approach from old implementation
    plt <- plt + geom_custom(
      aes(data = Network, y = y_position),
      data = hemi_data,
      grob_fun = function(x) {
        create_network_grob(
          x, hemi_key, atlas, annotation_surface,
          network_renders_path, network_name_map, annotation_height
        )
      }
    )
  }

  return(plt)
}

#' Prepare network statistics data frame for plotting
#'
#' @param data_per_hemi Nested list of network statistics
#' @param hemispheres Named vector of hemispheres
#' @param simulation_types Named vector of simulation types
#' @param n_subjects Number of subjects
#' @return Data frame with network statistics
prepare_network_plot_data <- function(data_per_hemi, hemispheres,
                                      simulation_types, n_subjects) {
  network_names <- c()
  means <- c()
  means_of_means <- c()
  stds <- c()
  montages <- c()
  hemi_labels <- c()

  for (hemi in names(hemispheres)) {
    for (montage in simulation_types) {
      data <- data_per_hemi[[hemi]][[montage]]
      for (network in unique(names(data))) {
        avg <- mean(data[[network]]$mean)
        std <- sd(data[[network]]$mean)

        network_names <- c(network_names, network)
        means_of_means <- c(means_of_means, avg)
        means <- c(means, data[[network]]$mean)
        stds <- c(stds, std)
        montages <- c(montages, montage)
        hemi_labels <- c(hemi_labels, hemispheres[[hemi]])
      }
    }
  }

  data_df <- data.frame(
    Hemisphere = hemi_labels,
    Network = network_names,
    Montage = montages,
    Mean = means_of_means,
    SD = stds
  )

  data_df_hist <- data.frame(
    Hemisphere = rep(hemi_labels, each = n_subjects),
    Network = rep(network_names, each = n_subjects),
    Montage = rep(montages, each = n_subjects),
    Mean = means
  )

  return(list(summary = data_df, individual = data_df_hist))
}

#' Prepare network node statistics data frame for plotting
#'
#' @param data_per_hemi Nested list of node-level network statistics
#' @param hemispheres Named vector of hemispheres
#' @param simulation_types Named vector of simulation types
#' @param n_subjects Number of subjects
#' @return Data frame with node-level network statistics
prepare_network_node_plot_data <- function(data_per_hemi, hemispheres,
                                           simulation_types, n_subjects) {
  network_names <- c()
  means <- c()
  means_of_means <- c()
  stds <- c()
  montages <- c()
  hemi_labels <- c()
  nodes <- c()

  for (hemi in names(hemispheres)) {
    for (montage in simulation_types) {
      data <- data_per_hemi[[hemi]][[montage]]
      for (network in unique(names(data))) {
        node_data <- data[[network]]
        for (node in names(node_data)) {
          avg <- mean(node_data[[node]]$mean)
          std <- sd(node_data[[node]]$mean)

          network_names <- c(network_names, network)
          means_of_means <- c(means_of_means, avg)
          means <- c(means, node_data[[node]]$mean)
          nodes <- c(nodes, node)
          stds <- c(stds, std)
          montages <- c(montages, montage)
          hemi_labels <- c(hemi_labels, hemispheres[[hemi]])
        }
      }
    }
  }

  data_df <- data.frame(
    Hemisphere = hemi_labels,
    Network = network_names,
    Nodes = nodes,
    Montage = montages,
    Mean = means_of_means,
    SD = stds
  )

  data_df_hist <- data.frame(
    Hemisphere = rep(hemi_labels, each = n_subjects),
    Network = rep(network_names, each = n_subjects),
    Montage = rep(montages, each = n_subjects),
    Nodes = rep(nodes, each = n_subjects),
    Mean = means
  )

  return(list(summary = data_df, individual = data_df_hist))
}
