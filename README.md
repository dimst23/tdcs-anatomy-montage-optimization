# tDCS Anatomy-Based Montage Optimization

This repository contains the complete computational pipeline for investigating the relationship between individual anatomical features and electric field distribution in transcranial direct current stimulation (tDCS). The code supports the associated publication and enables reproduction of all analyses and figures.

## Overview

The pipeline performs comprehensive analysis of how anatomical variability affects tDCS outcomes across different electrode montages and configurations. Key analyses include:

- **Head modeling and segmentation**: Automated processing of structural MRI data to create subject-specific head models
- **Electric field simulation**: Forward modeling of current flow and field distribution using finite element methods
- **Anatomical feature extraction**: Quantification of cortical morphology, tissue thickness, and gyrification metrics
- **Statistical modeling**: Generalized linear mixed models (GLMs) examining anatomy-field relationships
- **Network-based analysis**: Parcellation-based analysis using Schaefer and Yeo atlases
- **Multivariate analysis**: Principal component analysis of anatomical determinants

## Repository Structure

```
├── config/                              # Configuration files
│   ├── 3-simulation/                    # SimNIBS simulation settings
│   │   └── sim_settings_publication.json
│   ├── 3-statistics/                    # Statistical analysis configurations
│   │   ├── data_analysis/               # GLM and network analysis settings
│   │   └── network_mappings/            # Parcellation scheme mappings
│   └── dwi_config.sh                    # Diffusion imaging configuration
│
├── data/                                # Supplementary data (manuscript support)
│   ├── parcel_stats/                    # Parcellated anatomical statistics by demographics
│   ├── parcel_stats_total.*.csv         # Aggregate statistics across all subjects
│   ├── sample_input.csv                 # Example input data format
│   └── subject_ids.txt                  # Subject identifiers
│
├── src/
│   ├── 0-segmentation/                  # Head tissue segmentation [README](src/0-segmentation/README.md)
│   │   ├── charm_run.sh                 # CHARM segmentation wrapper
│   │   └── settings/                    # CHARM configuration files
│   │
│   ├── 2-analysis/                      # Core analysis pipeline [README](src/2-analysis/README.md)
│   │   ├── 0-fitting/                   # Surface reconstruction and atlas fitting
│   │   ├── 1-extraction/                # Feature extraction from surfaces
│   │   ├── 2-conversion/                # Data format conversion and standardization
│   │   └── 3-simulation/                # tDCS electric field simulation
│   │
│   ├── 3-statistics/                    # Statistical analysis modules [README](src/3-statistics/README.md)
│   │   ├── common/                      # Shared utilities and parsers
│   │   ├── glm/                         # Generalized linear modeling
│   │   ├── networks/                    # Network-based analysis
│   │   ├── pca/                         # Principal component analysis
│   │   └── other/                       # Additional analyses
│   │
│   └── hpc/                             # High-performance computing scripts [README](src/hpc/instructions.md)
│       ├── charm_array.sh               # Batch segmentation
│       ├── lgi_array.sh                 # Batch gyrification computation
│       ├── simulation_array.sh          # Batch field simulation
│       └── instructions.md              # HPC usage guide
│
├── assets/                              # Visualization resources
│   └── networks/                        # Network parcellation images
│
├── subject_correlations.r               # Standalone correlation analysis tool (see below)
├── pyproject.toml                       # Python dependencies
└── poetry.lock                          # Locked Python environment
```

## Prerequisites

### Software Dependencies

**Core Requirements:**

- [SimNIBS](https://simnibs.github.io/simnibs/) 4.0+ (head modeling and field simulation)
- [FreeSurfer](https://surfer.nmr.mgh.harvard.edu/) 6.0+ (cortical reconstruction)
- Python 3.11+ with Poetry for dependency management
- R 4.0+ with RStudio (recommended for statistical analyses)

**Python Packages** (managed via Poetry):

- `numpy`, `scipy`, `pandas` - numerical computing
- `nibabel`, `meshio`, `h5py` - neuroimaging data I/O
- `matplotlib`, `seaborn`, `pyvista` - visualization
- `neuromaps` - brain map transformations

**R Packages** (see `src/3-statistics/SETUP.md`):

- Core: `tidyverse`, `dplyr`, `ggplot2`, `jsonlite`
- Statistical modeling: `lme4`, `car`, `emmeans`, `performance`
- Visualization: `ggh4x`, `pheatmap`, `ggpubr`
- Neuroimaging: `freesurferformats`, `fsbrain`

### Data Requirements

This pipeline requires preprocessed structural MRI data:

- T1-weighted images (preferably ACPC-aligned)
- T2-weighted images (optional, improves segmentation)
- Existing FreeSurfer reconstructions (for cortical surface analysis)

The `data/` directory contains supplementary anatomical statistics used in the manuscript analyses.

## Installation

### Python Environment

```bash
# Clone the repository
git clone <repository-url>
cd tdcs-anatomy-montage-optimization

# Install Python dependencies using Poetry
poetry install

# Activate the environment
poetry shell
```

### R Environment

```bash
cd src/3-statistics

# Open in RStudio
open 3-statistics.Rproj

# In R console, initialize package management
install.packages("renv")
renv::init()
renv::restore()  # Install all required packages
```

See `src/3-statistics/SETUP.md` for detailed R environment setup.

## Workflow

The pipeline follows a sequential workflow across four main stages:

### Stage 0: Segmentation

Head tissue segmentation using SimNIBS CHARM (Complete Head Anatomy Reconstruction Method).

```bash
cd src/0-segmentation

./charm_run.sh \
  --subject <SUBJECT_ID> \
  --charm-executable <PATH_TO_CHARM> \
  --charm-settings-path settings/settings_base.ini \
  --fs-subj-dir <FREESURFER_SUBJECTS_DIR> \
  --run-dir <OUTPUT_DIR> \
  --t1-path <T1_IMAGE> \
  --t2-path <T2_IMAGE>
```

**Output:** SimNIBS head models (`m2m_<subject>/`) with tissue segmentations and tetrahedral meshes.

**See [`src/0-segmentation/README.md`](src/0-segmentation/README.md) for detailed documentation, configuration options, and troubleshooting.**

### Stage 2: Analysis

Multi-step processing of anatomical features and field simulations. **See [`src/2-analysis/README.md`](src/2-analysis/README.md) for comprehensive documentation of all analysis stages.**

#### 2.0 Fitting

Surface reconstruction and atlas registration:

```bash
cd src/2-analysis/0-fitting
./0-0_fit_fs_atlas.sh <SUBJECT_ID> <FREESURFER_DIR>
./0-1_local_gi_index.sh <SUBJECT_ID> <FREESURFER_DIR>
```

#### 2.1 Extraction

Feature extraction from surfaces:

```bash
cd src/2-analysis/1-extraction
./1-0_fs_stats.sh <SUBJECT_ID>
./3-0_simnibs_stats.sh <SUBJECT_ID> <M2M_DIR>
```

#### 2.2 Conversion

Data standardization to FSAverage space:

```bash
cd src/2-analysis/2-conversion
./2-1_fs_to_fsaverage.sh <SUBJECT_ID>
./2-2_simnibs_to_fsaverage.sh <SUBJECT_ID>
```

#### 2.3 Simulation

tDCS electric field simulation:

```bash
cd src/2-analysis/3-simulation

# Run simulation using configuration
python main_sim.py <M2M_MODEL_PATH> ../../config/3-simulation/sim_settings_publication.json
```

The simulation configuration (`sim_settings_publication.json`) defines:

- Electrode montages (e.g., F3-Fp2, P3-P4)
- Electrode types (rectangular, circular, various sizes)
- Current parameters (typically 1 mA)
- Output fields (electric field magnitude, normal component, current density)

### Stage 3: Statistics

Statistical analysis performed in R, using JSON configuration files for reproducibility. **See [`src/3-statistics/README.md`](src/3-statistics/README.md) for complete analysis documentation and usage examples.**

#### GLM Analysis

Examine relationships between anatomical features and field distribution:

```bash
cd src/3-statistics

Rscript glm/main.r \
  ../../config/3-statistics/data_analysis/glm_within/glm_analysis_within_Emagn.json \
  <BASE_DATA_DIR> \
  <HEMISPHERE> \
  <SIMULATION_TYPE> \
  [within|across]
```

**Key features:**

- Mixed-effects models with random effects for network and parcel
- Anatomical predictors: cortical thickness, surface area, gyrification, sulcal depth, tissue thicknesses
- Interaction terms between correlated features
- Z-statistic brain maps with FDR correction

#### Network Analysis

Parcellation-based analysis using Schaefer/Yeo atlases:

```bash
Rscript networks/main.r \
  ../../config/3-statistics/data_analysis/F3_network_analysis_schaefer_7_magnitude.json \
  <BASE_DATA_DIR> \
  <SUBJECT_FILE>
```

Analyzes field distribution patterns across functional networks.

#### PCA Analysis

Dimensionality reduction of anatomical determinants:

```bash
Rscript pca/main.r \
  <JSON_CONFIG> \
  <BASE_DATA_DIR>
```

#### Additional Analyses

```bash
# Feature importance analysis
Rscript other/feature_importance.r <JSON_CONFIG> <BASE_DIR>

# Population statistics
Rscript other/population_stats.r

# Spatial location analysis
Rscript other/spatial_location.r
```

## Configuration

All analyses are controlled via JSON configuration files in `config/3-statistics/`. These files specify:

- **Paths**: Data locations, output directories, atlas files
- **Parameters**: Statistical thresholds, scaling factors, hemisphere selection
- **Model specification**: GLM equations, interaction terms, random effects
- **Visualization**: Figure dimensions, color scales, annotation settings

Example configuration structure:

```json
{
  "general": {
    "paths": { "fsaverage_template": "..." },
    "cortex_only": true,
    "statistic": "z_fdr"
  },
  "glm": {
    "regressors": { "thickness": "Cortical Thickness", ... },
    "interactions": [["thickness", "area"], ...],
    "z_threshold": 1.96
  },
  "simulation": {
    "field": { "type": { "E.magn": "Magnitude" } },
    "simulation_types": { "rect_5x7-F3-Fp2": "rF3-Fp2", ... }
  }
}
```

## HPC Usage

Scripts in `src/hpc/` are designed for SLURM-based computing clusters. They implement array jobs for parallel processing across subjects.

```bash
# Submit segmentation array job
sbatch --array=1-100%10 hpc/charm_array.sh

# Submit simulation array job
sbatch --array=1-100%10 hpc/simulation_array.sh
```

The `%10` parameter limits concurrent jobs. Adjust array indices based on your subject count.

See `src/hpc/instructions.md` for detailed HPC usage.

## Data Description

The `data/` directory contains supplementary anatomical statistics referenced in the manuscript:

- **`parcel_stats/`**: Parcellated anatomical measures stratified by:

  - Age bins (35-40, 40-45, ..., 75-80 years)
  - Sex (M/F)
  - Using Schaefer 500-parcel 17-network atlas
  - Smoothed with 10mm FWHM kernel on fsaverage

- **`parcel_stats_total.*.csv`**: Aggregate statistics across all N=590 subjects

- **`sample_input.csv`**: Example data format for pipeline inputs

These files enable reproduction of population-level analyses without requiring access to raw imaging data.

## Reproducibility Tool: Correlation Analysis

The repository includes **`subject_correlations.r`**, a standalone R script for researchers to quantify relationships between PCA component loadings and morphological variables. This tool enables reproduction of the anatomical-PCA correlation analyses described in the manuscript.

### Quick Start

```bash
# Basic usage with population data
Rscript subject_correlations.r \
  --pca pca_loadings_per_parcel.csv \
  --input data/parcel_stats_total.fsaverage.fwhm10.Schaefer2018_500Parcels_17Networks_order.n_590.csv \
  --type 1 \
  --output results.csv
```

### Key Features

- **Two input formats**: Population-level data (long format with `variable` and `mean` columns) or individual subject data (wide format with one column per feature)
- **Spearman correlations**: Robust to non-normality, captures monotonic relationships
- **Multiple testing correction**: FDR correction using Benjamini-Hochberg procedure (optional)
- **Self-contained**: Only requires `tidyverse` R package, no other dependencies

### Requirements

- R 4.0+
- `tidyverse` package: `install.packages("tidyverse")`

### Documentation

Complete usage instructions, examples, and interpretation guidance are provided in the supplementary materials (Section: PCA Component-Morphology Correlation Analysis). The script includes built-in help:

```bash
Rscript subject_correlations.r  # Display usage information
```

## Output

The pipeline generates:

1. **Head models**: Subject-specific finite element meshes
2. **Field maps**: Electric field distributions in subject space and fsaverage
3. **Anatomical features**: Vertex-wise and parcel-averaged morphometric data
4. **Statistical maps**: Z-statistic brain maps showing anatomy-field associations
5. **Figures**: Publication-ready visualizations (PNG, PDF, EPS formats)
6. **Model summaries**: GLM coefficients, significance tests, performance metrics

## Citation

If you use this code, please cite the associated publication:

```
[Citation to be added upon publication]
```

## Acknowledgements

Authors: Dimitrios Stoupis & Sara Assecondi (SA)

Computations were performed on the high-performance computing infrastructure of [Institution Name]. [Add specific grant acknowledgements, funding sources, and collaborators as appropriate.]

Results presented in this work have been partially produced using the Aristotle University of Thessaloniki (AUTh) High Performance Computing Infrastructure (https://it.auth.gr/en/hpc).

Research reported in this publication was supported by the National Institute On Aging of the National Institutes of Health under Award Number U01AG052564 and by funds provided by the McDonnell Center for Systems Neuroscience at Washington University in St. Louis. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health.

SA is named inventor on a patent application (publication number WO/2022/106850) jointly submitted by the University of Birmingham and Dalhousie University, titled “Improving cognitive function,” currently in the PCT phase (international application No. PCT/G82021/053019)

SA is supported by Fondazione Cassa di Risparmio di Trento e Rovereto (CARITRO) [grant number 40105185]

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

For questions or issues:

- Review the detailed documentation in module-specific README files:
  - **Segmentation**: [`src/0-segmentation/README.md`](src/0-segmentation/README.md)
  - **Analysis Pipeline**: [`src/2-analysis/README.md`](src/2-analysis/README.md)
  - **Statistical Analysis**: [`src/3-statistics/README.md`](src/3-statistics/README.md)
  - **HPC Usage**: [`src/hpc/instructions.md`](src/hpc/instructions.md)
- Check configuration examples in `config/`
- Refer to individual script help messages (e.g., `./charm_run.sh --help`)

## Version Information

- **SimNIBS**: 4.0+
- **FreeSurfer**: 6.0+
- **Python**: 3.11+
- **R**: 4.0+

Specific package versions are locked in `poetry.lock` (Python) and managed via `renv` (R).
