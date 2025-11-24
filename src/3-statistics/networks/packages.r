#!/usr/bin/env Rscript
# ==============================================================================
# Network Analysis - Package Requirements
# ==============================================================================

# Source common packages
script_dir <- dirname(sys.frame(1)$ofile)
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
}
source(file.path(dirname(script_dir), "common", "packages.r"))

# Network analysis-specific packages
suppressPackageStartupMessages({
  # Plotting
  library(ggplot2)
  library(ggpubr)
  library(ggh4x)
  library(egg)
  library(grid)
  
  # Image handling
  library(png)
  library(magick)
  
  # Brain visualization
  library(fsbrain)
  
  # Statistics
  library(car)
})

