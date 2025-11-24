#!/usr/bin/env Rscript
# ==============================================================================
# Common Utility Functions
# Purpose: Reusable functions used across multiple analysis scripts
# ==============================================================================

#' Convert t-statistic to z-score
#'
#' @param t_stat T-statistic value
#' @param p_value P-value associated with the t-statistic
#' @return Z-score
tstat_to_zscore <- function(t_stat, p_value) {
  z_score <- qnorm(1 - p_value / 2) * sign(t_stat)
  return(z_score)
}

#' Create age groups from continuous age data
#'
#' @param age_data Vector of ages
#' @param step Age group interval (default: 5 years)
#' @param numerical Return numeric group IDs instead of labels
#' @return Factor of age groups
age_groups <- function(age_data, step = 5, numerical = FALSE) {
  breaks <- seq(min(age_data) - 1, max(age_data) + 1, by = step)
  labels <- paste(head(breaks, -1), tail(breaks, -1), sep = "-")

  groups <- cut(age_data,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )

  if (numerical) {
    groups <- cut(age_data,
      breaks = breaks,
      labels = FALSE,
      include.lowest = TRUE,
      right = TRUE
    )
  }

  return(groups)
}

#' Adjust p-values in emmeans model output
#'
#' @param em_model Emmeans model object
#' @param method Adjustment method (default: "fdr")
#' @return Summary with adjusted p-values
adjust_p <- function(em_model, method = "fdr") {
  sm <- summary(em_model)
  sm$contrasts$p.value <- p.adjust(sm$contrasts$p.value, method = method)
  return(sm)
}

#' Convert p-values to significance symbols
#'
#' @param p_values Vector of p-values
#' @return Character vector of significance symbols
convert_to_r <- function(p_values) {
  symbols <- symnum(p_values,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 1),
    symbols = c("***", "**", "*", "n.s.")
  )
  return(symbols)
}

#' Extract network and parcel information from annotation labels
#'
#' @param net_names Vector of network label names
#' @param prefix Prefix pattern to remove
#' @return Data frame with Network and Parcel columns
extract_network_info <- function(net_names, prefix) {
  cleaned_name <- sub(paste("^", prefix, sep = ""), "", net_names)

  # Extract the network name (first part before the first underscore)
  network <- sub("_.*", "", cleaned_name)

  # Extract the parcel name (everything after the first underscore)
  parcel <- sub("^[^_]+_", "", cleaned_name)

  return(data.frame(Network = network, Parcel = parcel))
}

#' Build GLM equation string from components
#'
#' @param dep_variable Dependent variable name
#' @param ind_variables Vector of independent variable names
#' @param interactions List of interaction terms
#' @param extra_terms Vector of additional terms
#' @return Formula string for GLM
build_glm_equation <- function(dep_variable,
                               ind_variables,
                               interactions = NULL,
                               extra_terms = NULL) {
  ind_var_part <- paste(dep_variable, "~", paste(ind_variables, collapse = " + "))
  equation <- paste(ind_var_part, sep = " + ")

  if (!is.null(interactions)) {
    int_terms <- sapply(interactions, function(x) paste(x, collapse = " * "))
    interactions_str <- paste(int_terms, collapse = " + ")
    equation <- paste(c(equation, interactions_str), collapse = " + ")
  }

  if (!is.null(extra_terms)) {
    extra <- paste(extra_terms, collapse = " * ")
    equation <- paste(c(equation, extra), collapse = " + ")
  }

  return(equation)
}

#' Save ggplot in multiple formats
#'
#' @param plot ggplot object
#' @param filename Base filename (without extension)
#' @param output_paths List of output directory paths
#' @param width Figure width in inches
#' @param height Figure height in inches
#' @param dpi Resolution for PNG (default: 600)
save_plot_multiple_formats <- function(plot, filename, output_paths,
                                       width, height, dpi = 600) {
  # PNG
  if (!is.null(output_paths$png)) {
    ggsave(
      file.path(output_paths$png, paste(filename, "png", sep = ".")),
      plot,
      width = width, height = height, device = "png", dpi = dpi
    )
  }

  # PDF
  if (!is.null(output_paths$pdf)) {
    ggsave(
      file.path(output_paths$pdf, paste(filename, "pdf", sep = ".")),
      plot,
      width = width, height = height, device = cairo_pdf
    )
  }

  # EPS
  if (!is.null(output_paths$eps)) {
    ggsave(
      file.path(output_paths$eps, paste(filename, "eps", sep = ".")),
      plot,
      width = width, height = height, device = cairo_ps
    )
  }

  invisible(NULL)
}

#' Save pheatmap object in multiple formats
#'
#' @param heatmap pheatmap object
#' @param filename Base filename (without extension)
#' @param output_paths List of output directory paths
#' @param width Figure width in inches
#' @param height Figure height in inches
save_heatmap_multiple_formats <- function(heatmap, filename, output_paths,
                                          width, height) {
  # PNG
  if (!is.null(output_paths$png)) {
    png(
      file.path(output_paths$png, paste(filename, "png", sep = ".")),
      width = width, height = height, units = "in", res = 600
    )
    grid::grid.draw(heatmap$gtable)
    dev.off()
  }

  # PDF
  if (!is.null(output_paths$pdf)) {
    cairo_pdf(
      file.path(output_paths$pdf, paste(filename, "pdf", sep = ".")),
      width = width, height = height
    )
    grid::grid.draw(heatmap$gtable)
    dev.off()
  }

  # EPS
  if (!is.null(output_paths$eps)) {
    cairo_ps(
      file.path(output_paths$eps, paste(filename, "eps", sep = ".")),
      width = width, height = height
    )
    grid::grid.draw(heatmap$gtable)
    dev.off()
  }

  invisible(NULL)
}

#' Get network components from full network name
#'
#' @param net_name Full network name string
#' @return List with hemisphere, network, and optionally component
get_network_components <- function(net_name) {
  comps <- strsplit(net_name, "_")[[1]]
  ret_lst <- list(hemi = comps[2], net = comps[3])

  if (length(comps) > 4) {
    ret_lst <- c(ret_lst, list(comp = comps[4]))
  }

  return(ret_lst)
}

#' Calculate quantile with special handling for normal field components
#'
#' @param data Vector of field values
#' @param quantile Quantile to compute (e.g., 0.99)
#' @param field_name Field type name
#' @param net_normal_component Include net normal component
#' @param inhibition_component Focus on inhibition (negative values)
#' @return Quantile value
get_field_quantile <- function(data, quantile, field_name = "E.magn",
                               net_normal_component = TRUE,
                               inhibition_component = TRUE) {
  if (field_name != "Normal") {
    perc <- quantile(data, quantile)
    return(perc)
  }

  if (!net_normal_component) {
    if (inhibition_component) {
      perc <- quantile(abs(data[data < 0]), quantile)
    } else {
      perc <- quantile(data[data >= 0], quantile)
    }
  } else {
    perc <- quantile(data, quantile)
  }

  return(perc)
}
