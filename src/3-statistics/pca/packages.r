#!/usr/bin/env Rscript
# ==============================================================================
# PCA Analysis - Package Requirements
# ==============================================================================

# Source common packages
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "packages.r"))

# PCA-specific packages
suppressPackageStartupMessages({
  # PCA and dimensionality reduction
  library(ggfortify)
  
  # Mixed effects models (for residual calculation)
  library(lme4)
  library(car)
  library(emmeans)
  
  # Plotting
  library(ggplot2)
  library(ggpubr)
  library(pheatmap)
  
  # Model diagnostics
  library(DHARMa)
  library(report)
  library(sjPlot)
  
  # Timing
  library(tictoc)
})

