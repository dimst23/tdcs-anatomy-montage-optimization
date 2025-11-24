#!/usr/bin/env Rscript
# ==============================================================================
# Common Package Requirements
# Purpose: Load packages used across multiple analysis scripts
# ==============================================================================

# Suppress package startup messages
suppressPackageStartupMessages({
  # Core data manipulation
  library(tidyverse)
  library(tidyr)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(stringr)

  # JSON handling
  library(jsonlite)

  # FreeSurfer and neuroimaging
  library(freesurferformats)
})
