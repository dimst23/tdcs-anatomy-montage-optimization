# Statistical Analysis Module

This directory contains the statistical analysis pipeline for examining relationships between individual anatomical features and tDCS-induced electric field distributions. All analyses are configured via JSON files for reproducibility and support the associated publication.

## Overview

The statistical module implements three primary analysis approaches:

1. **Generalized Linear Mixed Models (GLM)**: Vertex-wise and parcel-wise analysis of anatomy-field relationships using mixed-effects regression with network and parcel random effects

2. **Network-Based Analysis**: Parcellation-based examination of field distribution patterns across functional brain networks (Schaefer, Yeo atlases)

3. **Principal Component Analysis (PCA)**: Multivariate decomposition of anatomical determinants and their relationship to field characteristics

Additional analyses include feature importance quantification, population statistics, spatial electrode analysis, and subject-level correlations.

## Directory Structure

```
3-statistics/
├── common/                      # Shared utilities
│   ├── packages.r               # Common R packages
│   ├── functions.r              # Utility functions
│   └── settings_parser.r        # JSON configuration parser
│
├── glm/                         # Generalized Linear Models
│   ├── main.r                   # Entry point for GLM analysis
│   ├── packages.r               # GLM-specific packages
│   ├── functions.r              # GLM helper functions
│   ├── calculations.r           # Model fitting and statistics
│   └── plots.r                  # Brain map visualization
│
├── networks/                    # Network-based analysis
│   ├── main.r                   # Entry point for network analysis
│   ├── packages.r               # Network analysis packages
│   ├── functions.r              # Network-specific functions
│   ├── calculations.r           # Network statistics
│   └── plots.r                  # Network visualizations
│
├── pca/                         # Principal Component Analysis
│   ├── main.r                   # Entry point for PCA
│   ├── packages.r               # PCA packages
│   ├── functions.r              # PCA utilities
│   ├── calculations.r           # PCA computation
│   └── plots.r                  # PCA visualizations
│
├── other/                       # Additional analyses
│   ├── feature_importance.r     # Cross-montage feature analysis
│   ├── population_stats.r       # Demographic statistics
│   └── spatial_location.r       # Electrode spatial analysis
│
├── 3-statistics.Rproj           # RStudio project file
├── renv.lock                    # Locked R package versions
├── SETUP.md                     # Environment setup guide
└── README.md                    # This file
```

## Setup

### Prerequisites

- R 4.0 or later
- RStudio (recommended)
- FreeSurfer installation (for surface I/O)

### Installation

```bash
cd src/3-statistics

# Open in RStudio
open 3-statistics.Rproj

# In R console, restore package environment
install.packages("renv")
renv::restore()
```

This will install all required packages with versions matching those used in the publication. See `SETUP.md` for detailed installation instructions and troubleshooting.

### Key R Packages

**Core packages:**
- `tidyverse`: Data manipulation and visualization
- `jsonlite`: Configuration file parsing
- `freesurferformats`: FreeSurfer surface I/O

**Statistical modeling:**
- `lme4`: Mixed-effects linear models
- `car`: ANOVA and regression diagnostics
- `emmeans`: Estimated marginal means
- `performance`, `DHARMa`: Model diagnostics

**Visualization:**
- `ggplot2`, `ggh4x`, `ggpubr`: Publication-quality plots
- `pheatmap`: Heatmaps
- `fsbrain`: Brain surface visualization

## Usage

All analysis scripts follow a consistent command-line interface with JSON configuration files. Configurations are located in `../../config/3-statistics/`.

### GLM Analysis

Examine how anatomical features predict electric field distribution using mixed-effects models.

**Basic usage:**
```bash
Rscript glm/main.r \
  <JSON_CONFIG_PATH> \
  <BASE_DATA_DIR> \
  <HEMISPHERE> \
  <SIMULATION_TYPE> \
  [ANALYSIS_MODE]
```

**Example:**
```bash
Rscript glm/main.r \
  ../../config/3-statistics/data_analysis/glm_within/glm_analysis_within_Emagn.json \
  /path/to/data \
  lh \
  rect_5x7-F3-Fp2 \
  within
```

**Parameters:**
- `JSON_CONFIG_PATH`: Configuration file specifying model, paths, and visualization settings
- `BASE_DATA_DIR`: Root directory containing field simulations and anatomical data
- `HEMISPHERE`: `lh` (left) or `rh` (right)
- `SIMULATION_TYPE`: Electrode configuration (e.g., `rect_5x7-F3-Fp2`, `circ_1x1-P3-P4`)
- `ANALYSIS_MODE`: `within` (single montage) or `across` (compare montages)

**Anatomical predictors:**
- Cortical thickness
- Surface area
- Local gyrification index (LGI)
- Sulcal depth
- CSF thickness
- Skull thickness
- Skin thickness
- Interaction terms (configurable in JSON)

**Output:**
- Z-statistic brain maps (FDR-corrected)
- Model summaries and coefficients
- Diagnostic plots
- Multi-format figures (PNG, PDF, EPS)

### Network Analysis

Analyze field distribution patterns across functional brain networks.

**Basic usage:**
```bash
Rscript networks/main.r \
  <JSON_CONFIG_PATH> \
  <BASE_DATA_DIR> \
  <SUBJECT_LIST_FILE> \
  [ANALYSIS_MODE]
```

**Example:**
```bash
Rscript networks/main.r \
  ../../config/3-statistics/data_analysis/F3_network_analysis_schaefer_7_magnitude.json \
  /path/to/data \
  ../../data/subject_ids.txt \
  network
```

**Parameters:**
- `JSON_CONFIG_PATH`: Configuration specifying parcellation scheme and networks
- `BASE_DATA_DIR`: Root directory with simulation data
- `SUBJECT_LIST_FILE`: Text file with subject IDs (one per line)
- `ANALYSIS_MODE`: `network` (network-level) or `node` (parcel-level)

**Parcellation schemes:**
- Schaefer 2018 (100, 500 parcels; 7 or 17 networks)
- Yeo 2011 (7 networks)
- Kong 2022 (17 networks)

**Output:**
- Network-averaged field distributions
- Between-network comparisons
- Parcel-level statistics
- Network overlay brain maps

### PCA Analysis

Perform dimensionality reduction on anatomical features and examine relationships to field patterns.

**Basic usage:**
```bash
Rscript pca/main.r \
  <JSON_CONFIG_PATH> \
  <BASE_DATA_DIR>
```

**Example:**
```bash
Rscript pca/main.r \
  ../../config/3-statistics/data_analysis/glm_within/glm_analysis_within_Emagn.json \
  /path/to/data
```

**Output:**
- Principal component loadings
- Scree plots and variance explained
- PC-field correlation maps
- Biplot visualizations

### Feature Importance Analysis

Quantify the relative importance of anatomical features across montages.

**Usage:**
```bash
Rscript other/feature_importance.r \
  <JSON_CONFIG_PATH> \
  <BASE_DATA_DIR>
```

**Output:**
- Cross-montage feature importance heatmaps
- Standardized effect sizes
- Network-stratified importance measures

### Population Statistics

Generate demographic and anatomical summary statistics.

**Usage:**
```bash
Rscript other/population_stats.r
```

**Note:** May require path configuration for demographic data files.

### Spatial Electrode Analysis

Analyze spatial relationships between electrode positions and field outcomes.

**Usage:**
```bash
Rscript other/spatial_location.r
```

## Configuration Files

All analyses are controlled via JSON configuration files in `../../config/3-statistics/`. These ensure reproducibility and allow easy parameter modification.

### Configuration Structure

```json
{
  "general": {
    "paths": {
      "fsaverage_template": "/path/to/freesurfer/subjects/fsaverage"
    },
    "cortex_only": true,
    "statistic": "z_fdr"
  },
  "glm": {
    "regressors": {
      "thickness": "Cortical Thickness",
      "pial_lgi": "Local Gyrification Index",
      "sulc": "Sulcal Depth",
      "area": "Cortical Surface Area",
      "csf_thickness": "CSF Thickness",
      "skull_thickness": "Skull Thickness",
      "skin_thickness": "Skin Thickness"
    },
    "interactions": [
      ["thickness", "csf_thickness"],
      ["thickness", "area"],
      ["thickness", "sulc", "pial_lgi"]
    ],
    "extra_terms": [
      "(1 | network:parcel)",
      "(1 | network)"
    ],
    "z_threshold": 1.96,
    "paths": {
      "results": "data/statistical_maps"
    }
  },
  "simulation": {
    "field": {
      "type": {
        "E.magn": "Magnitude"
      }
    },
    "space_type": "fsaverage.fwhm10",
    "simulation_types": {
      "rect_5x7-F3-Fp2": "rF3-Fp2",
      "rect_5x7-F3-F4": "rF3-F4"
    }
  },
  "network": {
    "atlas": "Schaefer2018_500Parcels_17Networks_order",
    "paths": {
      "atlas": "atlas/schaefer2018/fsaverage/label",
      "mappings": "config/3-statistics/network_mappings"
    }
  },
  "figure": {
    "with_brain_annotations": true,
    "dimensions": {
      "width": 9.58,
      "aspect": 1.9
    },
    "paths": {
      "save": "data/figures",
      "temp": "data/temp"
    }
  }
}
```

### Key Configuration Sections

**`general`**: Global settings, paths, statistical thresholds

**`glm`**: Model specification, regressors, interactions, random effects

**`simulation`**: Field types, montage definitions, spatial smoothing

**`network`**: Parcellation scheme, atlas paths, network mappings

**`figure`**: Visualization parameters, output formats, dimensions

### Available Configurations

Located in `../../config/3-statistics/data_analysis/`:

- `glm_within/glm_analysis_within_Emagn.json` - Within-montage GLM for E-field magnitude
- `F3_network_analysis_schaefer_7_magnitude.json` - F3-based montage network analysis
- `P3_network_analysis_schaefer_7_magnitude.json` - P3-based montage network analysis

Network mappings in `../../config/3-statistics/network_mappings/`:
- `Schaefer2018_100Parcels_7Networks_order.json`
- `Schaefer2018_500Parcels_7Networks_order.json`
- `Yeo2011_7Networks.json`

## Module Architecture

Each analysis module (`glm/`, `networks/`, `pca/`) follows a consistent structure:

### `main.r`
Entry point script that:
1. Parses command-line arguments
2. Loads configuration from JSON
3. Sources module-specific packages and functions
4. Executes the analysis pipeline
5. Saves results and figures

### `packages.r`
Loads all required R packages for the module, including common packages from `common/packages.r`.

### `functions.r`
Module-specific utility functions for data processing, transformation, and formatting. Sources common functions from `common/functions.r`.

### `calculations.r`
Core statistical computations:
- Data loading and preprocessing
- Model fitting (GLM, PCA, etc.)
- Statistical testing and multiple comparison correction
- Results aggregation

### `plots.r`
Visualization functions:
- Brain surface plotting
- Statistical maps
- Diagnostic plots
- Multi-format export (PNG, PDF, EPS)

## Common Utilities

Located in `common/`, these utilities are shared across all analyses.

### `settings_parser.r`

**`parse_cli_args(min_args, arg_names)`**
Parse and validate command-line arguments.

**`parse_settings(json_path, base_dir)`**
Load and parse JSON configuration files with path resolution.

**`setup_figure_directories(save_path)`**
Create output directory structure for figures.

**`read_subjects(file_path)`**
Load subject IDs from text file.

**`validate_hemisphere(hemisphere)`**
Validate hemisphere specification (`lh` or `rh`).

**`load_cortex_nodes(fs_subject_dir, subject, hemisphere)`**
Load FreeSurfer cortex label to exclude non-cortical vertices.

### `functions.r`

**`tstat_to_zscore(tstat, df)`**
Convert t-statistics to z-scores for standardized effect sizes.

**`age_groups(age, bin_width)`**
Create age group factors from continuous age data.

**`extract_network_info(annotation_label)`**
Parse network names from parcellation annotation strings.

**`build_glm_equation(outcome, predictors, interactions, extra_terms)`**
Construct GLM formula string from components.

**`save_plot_multiple_formats(plot, filename, output_paths, width, height)`**
Save plots in PNG, PDF, and EPS formats.

**`get_field_quantile(field_data, quantile, handle_zeros)`**
Calculate field quantiles with special handling for zero values.

## Output Files

Analyses generate multiple output types organized by configuration settings:

### Statistical Maps
- **Location**: Specified in `config$glm$paths$results` or `config$network$paths$results`
- **Format**: FreeSurfer overlay files (`.mgh`) and CSV summaries
- **Content**: Z-statistics, p-values, effect sizes

### Figures
- **Location**: Specified in `config$figure$paths$save`
- **Formats**: PNG (high-res), PDF (vector), EPS (publication)
- **Types**: Brain maps, heatmaps, scatterplots, diagnostic plots

### Model Summaries
- **Location**: Analysis-specific subdirectories
- **Format**: CSV tables, RData objects
- **Content**: Coefficients, standard errors, confidence intervals, model diagnostics

### Diagnostic Outputs
- **Location**: Temporary directory specified in `config$figure$paths$temp`
- **Content**: Model diagnostics, residual plots, Q-Q plots, leverage plots

## Best Practices

### Running Analyses

1. **Always use configuration files**: Do not hardcode parameters in scripts
2. **Specify absolute paths**: Use full paths for `BASE_DATA_DIR` to avoid ambiguity
3. **Check FreeSurfer setup**: Ensure `$FREESURFER_HOME` is set correctly
4. **Verify data structure**: Ensure simulation outputs match expected naming conventions
5. **Monitor memory usage**: Large datasets may require 16+ GB RAM

### Reproducibility

1. **Use `renv`**: Lock package versions with `renv::snapshot()`
2. **Document modifications**: If changing configurations, save new JSON files
3. **Version control**: Track configuration changes in git
4. **Record environment**: Note R version, platform, and key package versions

### Troubleshooting

**Error: Cannot find FreeSurfer subject**
```r
# Check FREESURFER_HOME is set
Sys.getenv("FREESURFER_HOME")
# Verify path in JSON config
```

**Error: Package not found**
```r
# Restore packages from lock file
renv::restore()
# Or install specific package
install.packages("package_name")
```

**Error: Out of memory**
```r
# Increase memory limit (if applicable)
memory.limit(size = 16000)
# Or process hemispheres separately
```

**Error: File path not found**
- Verify `BASE_DATA_DIR` is correct
- Check that simulation outputs exist
- Ensure paths in JSON are relative to `BASE_DATA_DIR`

## Data Requirements

### Input Data Structure

Analyses expect the following directory structure:

```
BASE_DATA_DIR/
├── fsaverage/
│   └── <SIMULATION_TYPE>/
│       ├── <FIELD_TYPE>.lh.fsaverage.fwhm10.mgh
│       ├── <FIELD_TYPE>.rh.fsaverage.fwhm10.mgh
│       └── ...
├── anatomical/
│   ├── thickness.lh.fsaverage.fwhm10.mgh
│   ├── area.lh.fsaverage.fwhm10.mgh
│   └── ...
└── demographics/
    └── subject_info.csv
```

### Field Types

- `E.magn`: Electric field magnitude (V/m)
- `E.normal`: Normal component of E-field
- `J.magn`: Current density magnitude (A/m²)

### Anatomical Features

- `thickness`: Cortical thickness (mm)
- `area`: Surface area (mm²)
- `pial_lgi`: Local gyrification index
- `sulc`: Sulcal depth (mm)
- `csf_thickness`: CSF layer thickness (mm)
- `skull_thickness`: Skull thickness (mm)
- `skin_thickness`: Skin thickness (mm)

## Interactive Usage

For exploratory analysis or debugging, you can source modules interactively in RStudio:

```r
# Load common utilities
source("common/packages.r")
source("common/functions.r")
source("common/settings_parser.r")

# Load GLM module
source("glm/packages.r")
source("glm/functions.r")
source("glm/calculations.r")
source("glm/plots.r")

# Parse configuration
config <- parse_settings(
  "../../config/3-statistics/data_analysis/glm_within/glm_analysis_within_Emagn.json",
  "/path/to/base_data_dir"
)

# Access configuration
print(config$glm$regressors)
print(config$simulation$simulation_types)

# Run specific analysis components
# ... your custom analysis code ...
```

## Citation

If you use these analysis scripts, please cite the associated publication:

```
[Citation to be added upon publication]
```

## Support

For questions or issues:

1. Check `SETUP.md` for environment setup
2. Review example configurations in `../../config/3-statistics/`
3. Verify data structure matches requirements
4. Consult function documentation: `?function_name` in R

## Version Information

- **R**: 4.0+
- **RStudio**: Latest stable release recommended
- **Package versions**: Locked in `renv.lock`

