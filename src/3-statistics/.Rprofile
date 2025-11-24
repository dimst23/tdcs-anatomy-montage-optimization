#!/usr/bin/env Rscript
# ==============================================================================
# R Project Startup Configuration
# tDCS Anatomy Montage Optimization - Statistical Analysis
# ==============================================================================

# Set project-specific options
options(
  repos = c(CRAN = "https://cloud.r-project.org/"),
  scipen = 999, # Disable scientific notation
  stringsAsFactors = FALSE,
  max.print = 1000,
  warn = 1 # Show warnings as they occur
)

# Helper function to check if running interactively
.First <- function() {
  if (interactive()) {
    # Print welcome banner
    cat("\n")
    cat("==================================================================\n")
    cat("  tDCS Anatomy Montage Optimization - Statistical Analysis\n")
    cat("==================================================================\n\n")

    cat("Available Analysis Modules:\n")
    cat("  |-- glm/        : GLM analysis pipeline (per-subject, per-parcel)\n")
    cat("  |-- networks/   : Functional network analysis\n")
    cat("  |-- pca/        : Principal component analysis\n")
    cat("  |-- other/      : Specialized analyses (feature importance, spatial, population)\n")
    cat("  `-- common/     : Shared utilities and settings parser\n\n")

    # Check for common packages
    if (file.exists("common/packages.r")) {
      cat("TIP: Source common packages with: source('common/packages.r')\n")
    }

    # Check if renv is active
    if (requireNamespace("renv", quietly = TRUE)) {
      if (file.exists("renv.lock")) {
        cat("[renv] Active (package versions locked)\n")
      } else {
        cat("[renv] Available but not initialized\n")
        cat("       Run: renv::init() to start tracking packages\n")
      }
    } else {
      cat("[renv] Not installed\n")
      cat("       Install with: install.packages('renv')\n")
      cat("       Then run: renv::init() to start tracking packages\n")
    }

    cat("\n------------------------------------------------------------------\n")
    cat("Working directory: ", getwd(), "\n")
    cat("R version: ", R.version.string, "\n\n")
  }
}

# Activate renv if it exists
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Helper for string concatenation (used in .First)
`%+%` <- function(x, y) paste0(x, y)
