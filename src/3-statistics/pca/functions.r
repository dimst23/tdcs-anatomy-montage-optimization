#!/usr/bin/env Rscript
# ==============================================================================
# PCA Analysis - Utility Functions
# ==============================================================================

# Source common functions
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "functions.r"))

#' Calculate residuals from linear model controlling for demographics
#'
#' @param df_full Data frame with b_values and demographic variables
#' @return Data frame with residual statistics per montage and parcel
calculate_demographic_residuals <- function(df_full) {
  residual_summary <- df_full %>%
    group_by(montage, parcel) %>%
    nest() %>%
    mutate(
      model = map(data, ~ lm(b_value ~ age_group + gender, data = .x)),
      resid_rmse = map_dbl(model, ~ sqrt(mean(resid(.)^2))),
      resid_mean = map_dbl(model, ~ mean(abs(resid(.)))),
      resid_sd = map_dbl(model, ~ sd(resid(.)))
    ) %>%
    select(-data, -model)

  return(residual_summary)
}

#' Prepare PCA input matrix from residual statistics
#'
#' @param residual_summary Residual summary data frame
#' @param metric Metric to use ("resid_mean", "resid_rmse", "resid_sd")
#' @return Matrix suitable for PCA (parcels x montages)
prepare_pca_input <- function(residual_summary, metric = "resid_mean") {
  pca_input <- residual_summary %>%
    select(montage, parcel, all_of(metric)) %>%
    pivot_wider(names_from = montage, values_from = all_of(metric)) %>%
    column_to_rownames("parcel")

  return(as.matrix(pca_input))
}

#' Run PCA analysis
#'
#' @param input_matrix Matrix for PCA (parcels x features)
#' @param center Center the data (default: TRUE)
#' @param scale Scale the data (default: TRUE)
#' @return prcomp object with PCA results
run_pca <- function(input_matrix, center = TRUE, scale = TRUE) {
  pca_result <- prcomp(input_matrix, center = center, scale. = scale)
  return(pca_result)
}

#' Save PCA results to CSV files
#'
#' @param pca_result prcomp PCA result object
#' @param output_dir Directory to save CSV files
#' @param settings_id Settings identifier for filenames
#' @param n_components Number of components to save (default: 3)
save_pca_results <- function(pca_result, output_dir, settings_id, n_components = 3) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # 1. Save PCA scores (principal components)
  pc_cols <- paste0("PC", 1:n_components)
  pca_scores <- pca_result$x %>%
    as.data.frame() %>%
    tibble::rownames_to_column("parcel") %>%
    dplyr::select(parcel, all_of(pc_cols))

  write.csv(
    pca_scores,
    file.path(output_dir, paste(settings_id, "pca_results_pc1-3.csv", sep = ".")),
    row.names = FALSE
  )

  # 2. Save loading vectors (rotation matrix)
  pca_loadings <- as.data.frame(pca_result$rotation) %>%
    rownames_to_column("montage")

  write.csv(
    pca_loadings,
    file.path(output_dir, paste(settings_id, "pca_loadings.csv", sep = ".")),
    row.names = FALSE
  )

  # 3. Save center values
  pca_center <- data.frame(
    montage = names(pca_result$center),
    center = pca_result$center
  )

  write.csv(
    pca_center,
    file.path(output_dir, paste(settings_id, "pca_center.csv", sep = ".")),
    row.names = FALSE
  )

  # 4. Save scale values
  pca_scale <- data.frame(
    montage = names(pca_result$scale),
    scale = pca_result$scale
  )

  write.csv(
    pca_scale,
    file.path(output_dir, paste(settings_id, "pca_scale.csv", sep = ".")),
    row.names = FALSE
  )

  # 5. Save summary information
  pca_summary <- data.frame(
    PC = paste0("PC", 1:ncol(pca_result$x)),
    sdev = pca_result$sdev,
    variance = pca_result$sdev^2,
    prop_variance = (pca_result$sdev^2) / sum(pca_result$sdev^2),
    cumulative_variance = cumsum((pca_result$sdev^2) / sum(pca_result$sdev^2))
  )

  write.csv(
    pca_summary,
    file.path(output_dir, paste(settings_id, "pca_summary.csv", sep = ".")),
    row.names = FALSE
  )

  cat("PCA results saved to:", output_dir, "\n")
  invisible(NULL)
}

#' Calculate correlations between PCA components and variables
#'
#' @param pca_scores Data frame with PCA scores (must include PC columns)
#' @param variable_data Data frame with variables to correlate
#' @param pc_names Vector of PC column names (e.g., c("PC1", "PC2", "PC3"))
#' @param method Correlation method (default: "spearman")
#' @return Data frame with correlation results
calculate_pca_correlations <- function(pca_scores, variable_data,
                                       pc_names = c("PC1", "PC2", "PC3"),
                                       method = "spearman") {
  # Merge data
  merged_data <- pca_scores %>%
    inner_join(variable_data, by = "parcel")

  # Identify variable columns (exclude parcel and ALL PC columns, keep only numeric)
  exclude_cols <- c("parcel", "subject", "variable", "mean", "std", "network", "hemisphere")
  cor_data <- merged_data %>%
    select(-any_of(exclude_cols)) %>%
    select(-starts_with("PC")) %>% # Exclude all PC columns (PC1, PC2, PC3, ...)
    select(where(is.numeric))

  # Calculate correlations for each PC
  all_results <- list()

  for (pc_name in pc_names) {
    cor_results <- map_dfr(
      cor_data,
      ~ {
        test <- cor.test(.x, merged_data[[pc_name]], method = method)
        tibble(estimate = test$estimate, p_value = test$p.value)
      },
      .id = "variable"
    )

    cor_results <- cor_results %>%
      mutate(
        pc = pc_name,
        p_adj = p.adjust(p_value, method = "fdr"),
        significance = convert_to_r(p_adj)
      )

    all_results[[pc_name]] <- cor_results
  }

  bind_rows(all_results)
}

#' Create network grouping metadata for parcels
#'
#' @param parcel_meta Data frame with parcel and network columns
#' @return Data frame with additional grouping columns
create_network_grouping <- function(parcel_meta) {
  parcel_meta_clean <- parcel_meta %>%
    mutate(network_group = str_remove(network, "[A-Z]$"))

  parcel_meta_clean$network_grouping <- with(parcel_meta_clean, case_when(
    network_group %in% c("Cont", "Default") ~ "Higher-Order Cognition",
    network_group %in% c("DorsAttn", "SalVentAttn") ~ "Attention & Salience",
    network_group %in% c("Limbic", "TempPar", "SomMot") ~ "Emotion & Sensorimotor",
    network_group %in% c("VisCent", "VisPeri") ~ "Visual Processing",
    TRUE ~ "Other"
  ))

  parcel_meta_clean$Networks <- parcel_meta_clean$network_group

  return(parcel_meta_clean)
}
