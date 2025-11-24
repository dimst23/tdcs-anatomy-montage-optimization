#!/usr/bin/env Rscript
# ==============================================================================
# GLM Analysis - Package Requirements
# ==============================================================================

# Source common packages
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "packages.r"))

# GLM-specific packages
suppressPackageStartupMessages({
  # Mixed effects models
  library(lme4)
  library(car)
  library(emmeans)
  library(performance)

  # Model diagnostics
  library(DHARMa)

  # Reporting and tables
  library(report)
  library(sjPlot)
  library(flextable)

  # Parallel processing
  library(future.apply)

  # Data manipulation
  library(reshape2)

  # Plotting
  library(ggpubr)
  library(pheatmap)
  library(ggfortify)

  # Timing
  library(tictoc)
})
