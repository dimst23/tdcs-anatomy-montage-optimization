# Analysis Pipeline

This directory contains the core processing pipeline for extracting anatomical features, simulating tDCS electric fields, and preparing data for statistical analysis. The pipeline transforms raw neuroimaging data and head models into standardized, analysis-ready datasets.

## Overview

The analysis pipeline consists of four sequential stages:

1. **Fitting (0-fitting)**: Surface atlas registration and gyrification computation
2. **Extraction (1-extraction)**: Morphometric feature extraction and quality control
3. **Conversion (2-conversion)**: Standardization to common template space (fsaverage)
4. **Simulation (3-simulation)**: tDCS electric field modeling using SimNIBS

All scripts are designed to process individual subjects and can be parallelized across subjects using the HPC batch scripts in `../hpc/`.

## Directory Structure

```
2-analysis/
├── 0-fitting/                   # Atlas fitting and surface metrics
│   ├── 0-0_fit_fs_atlas.sh      # Register parcellation atlases
│   ├── 0-1_local_gi_index.sh    # Compute local gyrification index
│   └── 0-3_fit_simnibs_atlas.sh # Fit atlases to SimNIBS surfaces
│
├── 1-extraction/                # Feature extraction
│   ├── 1-0_fs_stats.sh          # Extract FreeSurfer anatomical statistics
│   ├── 3-0_simnibs_stats.sh     # Extract SimNIBS tissue thicknesses
│   ├── 3-1_stats_to_csv.sh      # Convert statistics to CSV format
│   ├── 4-0_surface_similarity_metrics.py
│   ├── 5-0_vertices_projection_to_cortex.py
│   └── utilities/               # Helper scripts and QC tools
│
├── 2-conversion/                # Template space conversion
│   ├── 2-0_stats_to_csv.sh      # FreeSurfer stats to CSV
│   ├── 2-1_fs_to_fsaverage.sh   # Project to fsaverage template
│   ├── 2-2_simnibs_to_fsaverage.sh
│   └── 2-3_simnibs_gii_to_surf.sh
│
└── 3-simulation/                # tDCS field simulation
    ├── main_sim.py              # Main simulation script
    ├── models.py                # Pydantic data models
    ├── utility_functions.py     # Helper functions
    ├── sim_run.sh               # Wrapper script
    └── __init__.py
```

## Prerequisites

### Software Requirements

- **FreeSurfer** 6.0+: Cortical surface analysis
- **FSL**: Image processing utilities
- **SimNIBS** 4.0+: Head modeling and field simulation
- **Python** 3.11+: Simulation scripts
- **Bash**: Shell scripts

### Environment Setup

```bash
# FreeSurfer
export FREESURFER_HOME=/path/to/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh

# FSL
export FSLDIR=/path/to/fsl
source $FSLDIR/etc/fslconf/fsl.sh

# SimNIBS
export PATH=/path/to/simnibs/bin:$PATH
```

### Configuration File

Many scripts require a configuration file (typically `../../config/dwi_config.sh`) that defines:

```bash
DWIConfig["SUBJECTS_DIR"]="/path/to/subjects"
DWIConfig["FSL_DIR"]="/path/to/fsl"
DWIConfig["FREESURFER_DIR"]="/path/to/freesurfer"
```

## Workflow

### Stage 0: Fitting

Register parcellation atlases and compute surface-based metrics.

#### 0-0: Fit FreeSurfer Atlas

Project parcellation schemes from fsaverage to subject space.

**Usage:**

```bash
cd 0-fitting

./0-0_fit_fs_atlas.sh \
  <CONFIG_FILE> \
  <SOURCE_ANNOTATION_DIR> \
  <ANNOTATION_NAME> \
  <SUBJECTS_DIR> \
  <SUBJECT_ID>
```

**Example:**

```bash
./0-0_fit_fs_atlas.sh \
  ../../config/dwi_config.sh \
  $FREESURFER_HOME/subjects/fsaverage \
  Schaefer2018_500Parcels_17Networks_order \
  /data/subjects \
  sub-001
```

**Output:**

- `$SUBJECTS_DIR/<SUBJECT>/label/lh.<ANNOTATION>.annot`
- `$SUBJECTS_DIR/<SUBJECT>/label/rh.<ANNOTATION>.annot`

**Common atlases:**

- `Schaefer2018_500Parcels_17Networks_order`
- `Schaefer2018_100Parcels_7Networks_order`
- `aparc.a2009s` (Destrieux)

#### 0-1: Local Gyrification Index

Compute local gyrification index (LGI) on pial surfaces.

**Usage:**

```bash
./0-1_local_gi_index.sh \
  <CONFIG_FILE> \
  <SUBJECT_ID> \
  <SUBJECTS_DIR>
```

**Example:**

```bash
./0-1_local_gi_index.sh \
  ../../config/dwi_config.sh \
  sub-001 \
  /data/subjects
```

**Output:**

- `$SUBJECTS_DIR/<SUBJECT>/surf/lh.pial_lgi`
- `$SUBJECTS_DIR/<SUBJECT>/surf/rh.pial_lgi`

**Note:** LGI computation is memory-intensive and may take 10-30 minutes per subject.

#### 0-3: Fit SimNIBS Atlas

Project atlases to SimNIBS-generated surfaces.

**Usage:**

```bash
./0-3_fit_simnibs_atlas.sh \
  <CONFIG_FILE> \
  <SOURCE_ANNOTATION_DIR> \
  <ANNOTATION_NAME> \
  <SUBJECTS_DIR> \
  <SUBJECT_ID>
```

Similar to 0-0 but operates on SimNIBS surface reconstructions.

### Stage 1: Extraction

Extract morphometric features and tissue properties.

#### 1-0: FreeSurfer Statistics

Extract anatomical statistics for each parcel using FreeSurfer tools.

**Usage:**

```bash
cd 1-extraction

./1-0_fs_stats.sh \
  <CONFIG_FILE> \
  <ANNOTATION_NAME> \
  <SUBJECT_ID>
```

**Example:**

```bash
./1-0_fs_stats.sh \
  ../../config/dwi_config.sh \
  Schaefer2018_500Parcels_17Networks_order \
  sub-001
```

**Output structure:**

```
$SUBJECTS_DIR/<SUBJECT>/T1w/fs_stats/
├── anat/
│   ├── lh.<ANNOTATION>.anat.stats
│   └── rh.<ANNOTATION>.anat.stats
└── lgi/
    ├── lh.<ANNOTATION>.pial_lgi.stats
    └── rh.<ANNOTATION>.pial_lgi.stats
```

**Extracted metrics:**

- Cortical thickness (mean, std per parcel)
- Surface area
- Gray matter volume
- Mean curvature
- Gaussian curvature
- Local gyrification index

#### 3-0: SimNIBS Statistics

Extract tissue thickness measurements from SimNIBS head models.

**Usage:**

```bash
./3-0_simnibs_stats.sh \
  <SUBJECT_ID> \
  <M2M_DIR>
```

**Example:**

```bash
./3-0_simnibs_stats.sh \
  sub-001 \
  /data/simnibs/m2m_sub-001
```

**Output:**
Vertex-wise tissue thicknesses:

- CSF thickness
- Skull thickness
- Skin (scalp) thickness

**Format:** CSV files with vertex indices and thickness values

#### Quality Control

The `utilities/check_subjects/` directory contains scripts for verifying processing completeness:

```bash
cd 1-extraction/utilities/check_subjects

# Check CHARM segmentation status
./check_charm.sh <SUBJECTS_FILE> <BASE_DIR>

# Check LGI computation status
./check_lgi.sh <SUBJECTS_FILE> <BASE_DIR>
```

### Stage 2: Conversion

Transform subject-space data to fsaverage template space for group analysis.

#### 2-1: FreeSurfer to FSAverage

Project FreeSurfer surface measures to fsaverage with optional smoothing.

**Usage:**

```bash
cd 2-conversion

./2-1_fs_to_fsaverage.sh \
  <CONFIG_FILE> \
  <SUBJECT_ID> \
  <SUBJECTS_DIR> \
  <FSAVERAGE_TYPE> \
  <FWHM> \
  <CURV_TYPES>
```

**Parameters:**

- `FSAVERAGE_TYPE`: Usually `fsaverage`
- `FWHM`: Smoothing kernel in mm (e.g., 10 for 10mm FWHM; 0 for no smoothing)
- `CURV_TYPES`: Comma-separated list (e.g., `thickness,area,sulc,pial_lgi`)

**Example:**

```bash
./2-1_fs_to_fsaverage.sh \
  ../../config/dwi_config.sh \
  sub-001 \
  /data/subjects \
  fsaverage \
  10 \
  "thickness,area,sulc,pial_lgi"
```

**Output:**

```
$SUBJECTS_DIR/<SUBJECT>/surf/
├── lh.fsaverage.fwhm10.thickness
├── lh.fsaverage.fwhm10.area
├── lh.fsaverage.fwhm10.sulc
├── lh.fsaverage.fwhm10.pial_lgi
└── rh.* (corresponding right hemisphere files)
```

**Note:** Smoothing improves signal-to-noise but reduces spatial specificity. 10mm FWHM is common for group analyses.

#### 2-2: SimNIBS to FSAverage

Project SimNIBS tissue thickness maps to fsaverage.

**Usage:**

```bash
./2-2_simnibs_to_fsaverage.sh \
  <CONFIG_FILE> \
  <SUBJECT_ID> \
  <SUBJECTS_DIR> \
  <FSAVERAGE_TYPE> \
  <FWHM> \
  <THICKNESS_TYPES>
```

**Example:**

```bash
./2-2_simnibs_to_fsaverage.sh \
  ../../config/dwi_config.sh \
  sub-001 \
  /data/subjects \
  fsaverage \
  10 \
  "csf_thickness,skull_thickness,skin_thickness"
```

Similar output structure to 2-1, but for tissue thickness measures.

### Stage 3: Simulation

Simulate tDCS-induced electric fields using SimNIBS finite element modeling.

#### Configuration

Simulations are configured via JSON files (e.g., `../../config/3-simulation/sim_settings_publication.json`).

**Key configuration elements:**

```json
{
  "simulation": {
    "sim_type": "single",
    "cpus": 5,
    "fields": "eEjJ",
    "output_path": "simulations/",
    "map_to_surface": true,
    "map_to_fsaverage": false,
    "electrodes": [
      [
        {
          "name": "F3",
          "type": ["rect_5x7", "circ_1x1"],
          "current": 1,
          "y_direction": "CPz"
        },
        {
          "name": "Fp2",
          "type": ["rect_5x7", "circ_1x1"],
          "current": -1,
          "y_direction": "Fp1"
        }
      ]
    ],
    "electrode_settings": [
      {
        "type": "rect_5x7",
        "shape": "rect",
        "dimensions": [50, 70],
        "thickness": [1, 1]
      }
    ]
  }
}
```

**Configuration parameters:**

- `fields`: Output fields
  - `e`: Electric field vector (V/m)
  - `E`: Electric field magnitude and components
  - `j`: Current density vector (A/m²)
  - `J`: Current density magnitude
- `electrodes`: Montage definitions
  - `name`: EEG 10-10 position
  - `type`: Electrode geometry
  - `current`: Current in mA (+anode, -cathode)
  - `y_direction`: Electrode orientation reference
- `electrode_settings`: Physical electrode properties
  - `shape`: "rect" or "ellipse"
  - `dimensions`: Size in mm [width, height]
  - `thickness`: Gel/sponge thickness in mm

#### Running Simulations

**Usage:**

```bash
cd 3-simulation

python main_sim.py \
  <M2M_MODEL_PATH> \
  <CONFIG_JSON>
```

**Example:**

```bash
python main_sim.py \
  /data/simnibs/m2m_sub-001 \
  ../../config/3-simulation/sim_settings_publication.json
```

**Output:**

```
<M2M_MODEL_PATH>/simulations/
├── rect_5x7-F3-Fp2/
│   ├── simnibs_simulation_*.mat
│   ├── subject_overlays/
│   │   ├── lh.E.magn.mgh
│   │   ├── lh.E.normal.mgh
│   │   ├── rh.E.magn.mgh
│   │   └── rh.E.normal.mgh
│   └── ...
└── circ_1x1-F3-Fp2/
    └── ...
```

**Field outputs:**

- `E.magn`: Electric field magnitude (V/m)
- `E.normal`: Normal component (perpendicular to cortex)
- `E.tangent`: Tangential component
- `J.magn`: Current density magnitude (A/m²)
- `normE`: Normalized electric field

**Simulation time:** 5-20 minutes per montage depending on mesh complexity and CPU count.

#### Batch Processing

For multiple subjects, use the HPC array script:

```bash
cd ../hpc
sbatch --array=1-100%10 simulation_array.sh
```

See `../hpc/instructions.md` for details.

## Data Flow

```
Raw MRI Data
    │
    ├─→ FreeSurfer recon-all → Cortical surfaces
    └─→ SimNIBS CHARM → Head model (m2m_*)
            │
            ├─────────────────────────────┐
            │                             │
    Stage 0: Fitting              Stage 0: Fitting
    ├── Atlas registration        └── Atlas to SimNIBS surfaces
    └── LGI computation
            │
    Stage 1: Extraction
    ├── FreeSurfer stats (anat, lgi)
    └── SimNIBS stats (tissue thickness)
            │
    Stage 2: Conversion
    ├── Project to fsaverage
    └── Apply smoothing
            │
            ├─────────────────────────────┐
            │                             │
    Stage 3: Simulation           Stage 2: Conversion
    └── tDCS field modeling       └── Project fields to fsaverage
            │                             │
            └──────────┬──────────────────┘
                       │
                Stage 3: Statistics
                (see ../3-statistics/)
```

## Output Organization

Recommended output directory structure:

```
analysis_output/
├── subjects/
│   ├── sub-001/
│   │   ├── T1w/
│   │   │   ├── fs_stats/          # Stage 1 outputs
│   │   │   └── ...
│   │   ├── surf/                  # Stage 2 outputs
│   │   │   ├── lh.fsaverage.fwhm10.thickness
│   │   │   └── ...
│   │   └── m2m_sub-001/
│   │       └── simulations/       # Stage 3 outputs
│   └── sub-002/
│       └── ...
│
└── fsaverage/                     # Group-level template data
    ├── sub-001/
    │   └── rect_5x7-F3-Fp2/
    │       ├── E.magn.lh.fsaverage.fwhm10.mgh
    │       └── E.magn.rh.fsaverage.fwhm10.mgh
    └── sub-002/
        └── ...
```

## Common Issues and Troubleshooting

### FreeSurfer Environment Not Set

**Error:** `mri_surf2surf: command not found`

**Solution:**

```bash
export FREESURFER_HOME=/path/to/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh
```

### LGI Computation Fails

**Error:** `mris_compute_lgi: out of memory`

**Solution:**

- Increase available RAM (requires 4-8 GB per process)
- Reduce parallel processing
- Run on HPC with higher memory allocation

### SimNIBS Simulation Hangs

**Error:** Simulation does not complete

**Possible causes:**

- Mesh quality issues in head model
- Invalid electrode positions
- Insufficient memory

**Solution:**

```bash
# Check mesh quality
mesh_check <M2M_MODEL>/mesh.msh

# Verify electrode positions are on scalp
# Reduce CPU count to decrease memory usage
```

### Missing Annotation Files

**Error:** `ERROR: cannot find annotation file`

**Solution:**
Ensure atlases are properly registered in Stage 0:

```bash
ls $SUBJECTS_DIR/<SUBJECT>/label/*.annot
```

If missing, rerun `0-0_fit_fs_atlas.sh`.

### File Permissions

**Error:** `Permission denied` when writing outputs

**Solution:**

```bash
# Ensure write permissions
chmod -R u+w $SUBJECTS_DIR/<SUBJECT>
```

## Performance Optimization

### Parallel Processing

Many conversion scripts support parallel hemisphere processing:

```bash
# Processes both hemispheres simultaneously
./2-1_fs_to_fsaverage.sh ... &
```

### Memory Management

**Memory requirements per stage:**

- Fitting (LGI): 4-8 GB
- Extraction: 2-4 GB
- Conversion: 2-4 GB
- Simulation: 4-16 GB (depends on mesh size and CPU count)

### HPC Deployment

For batch processing across subjects:

```bash
cd ../hpc

# Array job for LGI computation
sbatch --array=1-100%10 --mem=8G lgi_array.sh

# Array job for simulations
sbatch --array=1-100%10 --mem=16G --cpus-per-task=5 simulation_array.sh
```

## Best Practices

1. **Process in order**: Follow stages sequentially (0 → 1 → 2 → 3)
2. **Verify outputs**: Check each stage produces expected files before proceeding
3. **Use version control**: Track configuration files in git
4. **Document parameters**: Record smoothing kernels, atlas versions, simulation settings
5. **Quality control**: Visually inspect surfaces and field distributions
6. **Consistent naming**: Use standardized subject IDs and file naming conventions
7. **Backup head models**: SimNIBS m2m folders are time-consuming to regenerate

## Quality Control Checklist

After processing each subject:

- [ ] Atlas annotation files exist for both hemispheres
- [ ] LGI surfaces computed without errors
- [ ] FreeSurfer stats files contain expected metrics
- [ ] SimNIBS tissue thickness maps are reasonable (CSF: 1-5mm, Skull: 3-10mm, Skin: 3-8mm)
- [ ] Fsaverage projection files created
- [ ] Simulation completed without errors
- [ ] Field magnitudes are physiologically plausible (0.1-1 V/m for 1 mA stimulation)
- [ ] Visual inspection of field maps shows expected spatial patterns

## Additional Resources

- **FreeSurfer documentation**: https://surfer.nmr.mgh.harvard.edu/
- **SimNIBS documentation**: https://simnibs.github.io/simnibs/
- **Schaefer atlases**: https://github.com/ThomasYeoLab/CBIG
- **EEG 10-10 system**: For electrode positioning reference

## Support

For issues or questions:

- Check individual script help: `./script.sh --help` (where available)
- Review HPC batch scripts in `../hpc/` for working examples
- Consult the main repository README for overall workflow
- Verify software versions match prerequisites

## Citation

If using this pipeline, cite the associated publication and relevant tools:

- **SimNIBS**
```
Thielscher, A., Antunes, A., & Saturnino, G. B. (2015). Field modeling for transcranial magnetic stimulation: A useful tool to understand the physiological effects of TMS? 2015 37th Annual International Conference of the IEEE Engineering in Medicine and Biology Society (EMBC), 222–225. https://doi.org/10.1109/EMBC.2015.7318340
```

- **FreeSurfer**
```
Fischl, B. (2012). FreeSurfer. NeuroImage, 62(2), 774–781. https://doi.org/10.1016/j.neuroimage.2012.01.021
```

- **Schaefer atlases**
```
Schaefer, A., Kong, R., Gordon, E. M., Laumann, T. O., Zuo, X.-N., Holmes, A. J., Eickhoff, S. B., & Yeo, B. T. T. (2018). Local-Global Parcellation of the Human Cerebral Cortex from Intrinsic Functional Connectivity MRI. Cerebral Cortex, 28(9), 3095–3114. https://doi.org/10.1093/cercor/bhx179
```
