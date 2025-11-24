#!/usr/bin/env Rscript
# ==============================================================================
# GLM Analysis - Utility Functions
# ==============================================================================

# Source common functions
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "functions.r"))

#' Calculate GLM for a single subject
#'
#' @param subject_index Index of the subject in the data
#' @param sub_data List containing all subject data matrices
#' @param regressors Vector of regressor variable names
#' @param valid_net_ids Indices of valid network nodes
#' @param valid_coarse_nets Factor of coarse network labels
#' @param valid_parcels_nets Factor of parcel labels
#' @param ind_variable Name of independent variable
#' @param interactions_list List of interaction terms
#' @param extra_terms_list Vector of extra terms
#' @return Fitted GLM model object
calculate_subject_glm <- function(subject_index, sub_data, regressors,
                                  valid_net_ids, valid_coarse_nets, valid_parcels_nets,
                                  ind_variable, interactions_list = NULL,
                                  extra_terms_list = NULL) {
  # Get data for current subject
  input_data <- lapply(sub_data, `[`, valid_net_ids, subject_index)
  glm_df <- data.frame(input_data)

  # Scale regressors
  scaled_data <- scale(glm_df[, regressors])
  glm_df[, regressors] <- data.frame(scaled_data)

  # Handle missing values
  glm_df[is.na(glm_df)] <- 0
  glm_df[["network"]] <- valid_coarse_nets
  glm_df[["parcel"]] <- valid_parcels_nets

  # Build and fit model
  glm_equation <- build_glm_equation(
    ind_variable, regressors,
    interactions_list, extra_terms_list
  )

  # Fit model with informative error messages
  temp_glm <- tryCatch(
    {
      lmer(
        as.formula(glm_equation),
        data = glm_df,
        REML = TRUE,
        lmerControl(optimizer = "bobyqa")
      )
    },
    error = function(e) {
      # Provide helpful context about the error
      stop(sprintf(
        "Failed to fit mixed-effects model for subject %d.\nError: %s\n\nSuggestions:\n  - Check if grouping factors have sufficient levels\n  - Consider simplifying random effects structure\n  - Verify data quality for this subject",
        subject_index, e$message
      ), call. = FALSE)
    }
  )

  return(temp_glm)
}

#' Extract GLM coefficients from fitted model
#'
#' @param glm_model Fitted GLM model
#' @return List of coefficients with t-values, p-values, and estimates
extract_glm_coefficients <- function(glm_model) {
  glm_summary <- summary(glm_model)
  coef_names <- setdiff(rownames(glm_summary$coefficients), "(Intercept)")

  res <- list()
  for (coef_name in coef_names) {
    res[[coef_name]] <- list(
      t = glm_summary$coefficients[coef_name, "t value"],
      b = glm_summary$coefficients[coef_name, "Estimate"]
    )
  }

  return(res)
}

#' Rescale GLM coefficients to original scale
#'
#' @param glm_results GLM results list
#' @param glm_data_list Original data list
#' @param valid_net_ids Valid network node indices
#' @param regressors Vector of regressor names
#' @param n_subjects Number of subjects
#' @return Updated GLM results with rescaled coefficients
rescale_glm_coefficients <- function(glm_results, glm_data_list, valid_net_ids,
                                     regressors, n_subjects) {
  scaled <- list()

  for (subject_index in 1:n_subjects) {
    input_data <- lapply(glm_data_list, `[`, valid_net_ids, subject_index)
    glm_df <- data.frame(input_data)

    scaled_data <- scale(glm_df[, regressors])
    scale_center <- attr(scaled_data, "scaled:center")
    scale_std <- attr(scaled_data, "scaled:scale")

    # Initialize on first iteration
    if (subject_index == 1) {
      for (reg in regressors) {
        scaled[[reg]] <- list(mean = numeric(n_subjects), std = numeric(n_subjects))
      }
      for (coef in names(glm_results)) {
        glm_results[[coef]][["b_orig"]] <- numeric(n_subjects)
      }
    }

    # Store scaling parameters
    for (regressor in regressors) {
      scaled[[regressor]][["mean"]][subject_index] <- scale_center[[regressor]]
      scaled[[regressor]][["std"]][subject_index] <- scale_std[[regressor]]
    }

    # Rescale coefficients
    for (coef in names(glm_results)) {
      effects <- strsplit(coef, ":")[[1]]
      stdev <- prod(sapply(effects, function(eff) {
        scaled[[eff]][["std"]][subject_index]
      }))
      glm_results[[coef]][["b_orig"]][subject_index] <-
        glm_results[[coef]][["b"]][subject_index] * stdev
    }
  }

  return(glm_results)
}

#' Extract b-values from nested GLM results structure
#'
#' @param glm_results Nested GLM results (hemisphere -> montage -> feature -> parcel)
#' @param hemispheres Vector of hemisphere names
#' @param subjects Vector of subject IDs
#' @return Tibble with columns: montage, feature, network, parcel, hemisphere, subject_id, b_value
extract_bvalues <- function(glm_results, hemispheres, subjects) {
  all_data <- list()

  for (hemi in hemispheres) {
    name_pattern <- paste0("\\d+Networks_", toupper(hemi), "_")

    for (montage_name in names(glm_results[[hemi]])) {
      montage <- glm_results[[hemi]][[montage_name]]

      for (feature_name in names(montage)) {
        if (feature_name == "subjects") next

        feature <- montage[[feature_name]]

        for (parcel_idx in seq_along(feature)) {
          net_name <- names(feature)[parcel_idx]
          net_info <- extract_network_info(net_name, name_pattern)

          b_vec <- feature[[parcel_idx]]$b

          df <- tibble(
            montage = montage_name,
            feature = feature_name,
            network = net_info$Network,
            parcel = paste(toupper(hemi), net_info$Network, net_info$Parcel, sep = "_"),
            hemisphere = hemi,
            subject_id = subjects[seq_along(b_vec)],
            b_value = unlist(b_vec)
          )

          all_data[[length(all_data) + 1]] <- df
        }
      }
    }
  }

  bind_rows(all_data)
}

#' Create feature importance z-scores across montages
#'
#' @param glm_results Nested GLM results
#' @param hemispheres Vector of hemisphere names
#' @param glm_terms Vector of GLM term names
#' @param simulation_type_names Named vector of simulation types
#' @return Tibble with feature, montage, and z-score columns
calculate_feature_importance <- function(glm_results, hemispheres, glm_terms,
                                         simulation_type_names) {
  mean_vals <- list(rh = numeric(0), lh = numeric(0))

  for (hemi in hemispheres) {
    for (nm in names(glm_results[[hemi]])) {
      for (rg in names(glm_results[[hemi]][[nm]])) {
        if (rg == "subjects") next

        temp_data <- abs(unlist(glm_results[[hemi]][[nm]][[rg]][["b"]]))
        temp_t_test <- t.test(temp_data)

        if (temp_t_test$p.value > 0.05) {
          mean_vals[[hemi]] <- append(mean_vals[[hemi]], 0)
        } else {
          mean_vals[[hemi]] <- append(mean_vals[[hemi]], mean(temp_data))
        }
      }
    }
  }

  mean_vals[["total"]] <- mean_vals$rh + mean_vals$lh

  df <- tibble(
    feature = rep(c(unlist(glm_terms)), times = length(simulation_type_names)),
    montage = rep(c(unlist(unname(simulation_type_names))), each = length(glm_terms)),
    score = mean_vals$total
  )

  return(df)
}
