# Head Tissue Segmentation

This directory contains scripts and configuration files for automated head tissue segmentation using SimNIBS CHARM (Complete Head Anatomy Reconstruction Method). CHARM generates subject-specific head models with detailed tissue compartments required for accurate tDCS electric field simulations.

## Overview

Head segmentation is the foundational step of the tDCS modeling pipeline. It transforms structural MRI scans into finite element models that represent:

- **Gray matter (GM)**: Cortical and subcortical structures
- **White matter (WM)**: Deep brain white matter
- **Cerebrospinal fluid (CSF)**: Ventricular and subarachnoid CSF
- **Skull**: Compact and spongy bone
- **Scalp (skin)**: Outer tissue layer
- **Eyes**: Orbital structures
- **Air cavities**: Sinuses

The resulting head models enable realistic current flow simulations that account for individual anatomical variability.

## Directory Structure

```
0-segmentation/
├── charm_run.sh                 # Main segmentation wrapper script
└── settings/                    # CHARM configuration files
    ├── settings_base.ini        # Standard adult segmentation parameters
    └── atrophic/                # Settings for atrophic/elderly brains
        ├── settings_atrophy.ini
        └── shared_gmm_params_t1_t2.txt
```

## Prerequisites

### Software Requirements

- **SimNIBS** 4.0+
- **FreeSurfer** 6.0+ (for integration with FreeSurfer surfaces)
- **FSL** (optional, for additional preprocessing)

### Input Data Requirements

**Required:**
- T1-weighted MRI (1mm isotropic recommended)
  - Preferably ACPC-aligned
  - Minimal motion artifacts
  - Good gray-white matter contrast

**Optional but recommended:**
- T2-weighted MRI (improves segmentation quality)
  - Coregistered to T1 or in same session
  - Similar resolution to T1

### FreeSurfer Preprocessing

CHARM can utilize existing FreeSurfer reconstructions for improved cortical surface accuracy:

```bash
# Run FreeSurfer recon-all first (if not already done)
recon-all -s <SUBJECT_ID> \
  -i <T1_IMAGE> \
  -all \
  -parallel
```

**Expected FreeSurfer output:** `$SUBJECTS_DIR/<SUBJECT_ID>/`

## Usage

### Basic Usage

```bash
./charm_run.sh \
  --subject <SUBJECT_ID> \
  --charm-executable <PATH_TO_CHARM> \
  --charm-settings-path <SETTINGS_INI> \
  --fs-subj-dir <FREESURFER_SUBJECTS_DIR> \
  --run-dir <OUTPUT_DIR> \
  [--t1-path <T1_IMAGE>] \
  [--t2-path <T2_IMAGE>]
```

### Parameters

**Required:**

- `--subject` / `-s`: Subject identifier (e.g., `sub-001`)
- `--charm-executable`: Full path to CHARM executable
  - Usually: `$SIMNIBSDIR/charm` or `charm` if in PATH
- `--charm-settings-path`: Path to configuration INI file
  - Standard: `settings/settings_base.ini`
  - Elderly/atrophic: `settings/atrophic/settings_atrophy.ini`
- `--fs-subj-dir`: FreeSurfer SUBJECTS_DIR
- `--run-dir`: Output directory for CHARM results

**Optional:**

- `--t1-path`: Path to T1 image (default: `../T1w/T1w_acpc_dc_restore.nii.gz`)
- `--t2-path`: Path to T2 image (default: `../T1w/T2w_acpc_dc_restore.nii.gz`)

### Standard Example

```bash
./charm_run.sh \
  --subject sub-001 \
  --charm-executable /opt/simnibs/charm \
  --charm-settings-path settings/settings_base.ini \
  --fs-subj-dir /data/freesurfer/subjects \
  --run-dir /data/simnibs \
  --t1-path /data/raw/sub-001/anat/T1w.nii.gz \
  --t2-path /data/raw/sub-001/anat/T2w.nii.gz
```

### Example with HCP Data Structure

For Human Connectome Project (HCP) formatted data:

```bash
# Assumes HCP structure: <SUBJECT>/T1w/T1w_acpc_dc_restore.nii.gz
cd /data/hcp/sub-001

../path/to/charm_run.sh \
  --subject sub-001 \
  --charm-executable charm \
  --charm-settings-path ../path/to/settings/settings_base.ini \
  --fs-subj-dir /data/freesurfer/subjects \
  --run-dir /data/simnibs
```

### Elderly/Atrophic Brain Segmentation

For subjects with significant atrophy or age-related changes:

```bash
./charm_run.sh \
  --subject sub-elderly \
  --charm-executable charm \
  --charm-settings-path settings/atrophic/settings_atrophy.ini \
  --fs-subj-dir /data/freesurfer/subjects \
  --run-dir /data/simnibs \
  --t1-path /data/elderly/T1w.nii.gz \
  --t2-path /data/elderly/T2w.nii.gz
```

**Key differences in atrophic settings:**
- Adjusted tissue probability priors
- Modified initialization parameters
- Custom Gaussian mixture model parameters for T1+T2
- Relaxed constraints on CSF/GM boundaries

## Output

CHARM generates an `m2m_<SUBJECT>` directory containing:

```
m2m_<SUBJECT>/
├── tissue_labeling_upsampled.nii.gz  # Tissue segmentation volume
├── final_tissues.nii.gz               # Final tissue labels
├── <SUBJECT>_T1fs_conform.nii.gz     # Preprocessed T1
├── <SUBJECT>_T2conform.nii.gz        # Preprocessed T2 (if provided)
├── segment/                           # Segmentation intermediate files
├── surfaces/                          # Surface reconstructions
│   ├── lh.central.gii                 # Left hemisphere central surface
│   ├── lh.pial.gii                    # Left hemisphere pial surface
│   ├── rh.central.gii                 # Right hemisphere central surface
│   └── rh.pial.gii                    # Right hemisphere pial surface
├── mesh/                              # Finite element mesh
│   └── <SUBJECT>_mesh.msh            # Tetrahedral volume mesh
└── eeg_positions/                     # Standard EEG coordinates
    └── EEG10-10_UI_Jurak_2007.csv    # 10-10 system positions
```

### Key Output Files

**Segmentation:**
- `final_tissues.nii.gz`: Integer labels for each tissue type
  - 1: White matter
  - 2: Gray matter
  - 3: CSF
  - 4: Bone (skull)
  - 5: Scalp (skin)
  - 6: Blood vessels
  - 7: Eyes
  - 8: Compact bone
  - 9: Spongy bone
  - 10: Air

**Surfaces:**
- `lh/rh.central.gii`: Central (mid-thickness) cortical surfaces
- `lh/rh.pial.gii`: Outer (pial) cortical surfaces
- Used for projecting simulation results to brain surface

**Mesh:**
- `<SUBJECT>_mesh.msh`: Tetrahedral finite element mesh
- Used directly in tDCS simulations
- Typically 1-3 million elements
- Optimized for numerical stability

**EEG Positions:**
- Electrode coordinates in subject space
- Derived from 10-10 system
- Used for placing tDCS electrodes

## CHARM Settings

### Standard Settings (`settings_base.ini`)

Optimized for healthy adult brains with good image quality.

**Key parameters:**

```ini
[general]
threads = 0  # Use all available CPUs

[samseg]
atlas_name = "charm_atlas_mni"
init_type = "atlas"

[segment]
downsampling_targets = [2.0, 1.0]  # Multi-resolution segmentation
bias_kernel_width = 70              # Bias field correction
mesh_stiffness = 0.1                # Deformation regularization
csf_factor = 0.3                    # CSF downweighting (with T2)

[surfaces]
processes = 2                       # Parallel surface reconstruction
surf = ["lh", "rh"]                 # Generate both hemispheres
pial = ["lh", "rh"]                 # Include pial surfaces
no_selfintersections = true         # Ensure topological validity
```

### Atrophic Settings (`settings/atrophic/settings_atrophy.ini`)

Modified for elderly subjects or those with cortical atrophy.

**Key modifications:**
- Adjusted tissue priors
- Custom initialization parameters
- Relaxed deformation constraints
- Enhanced CSF/GM separation
- Modified GMM parameters in `shared_gmm_params_t1_t2.txt`

**When to use:**
- Age > 65 years with visible atrophy
- Significant ventricle enlargement
- Prominent sulci and reduced cortical thickness
- Mild cognitive impairment or dementia

## Command-Line Options

### CHARM Flags Used

The wrapper script passes these flags to CHARM:

- `--skipregisterT2`: Skip T2 registration (assumes T2 is already aligned to T1)
- `--noneck`: Remove neck structures from segmentation
- `--usesettings`: Specify custom settings INI file
- `--fs-dir`: Use existing FreeSurfer reconstruction

### Additional CHARM Options

Can be added to `charm_run.sh` if needed:

- `--forceqform`: Use qform instead of sform for orientation
- `--forcerun`: Overwrite existing segmentation
- `--first_only`: Run only initial segmentation (skip surfaces)
- `--no-conform`: Do not conform images to 1mm isotropic

## Processing Time

Typical processing times on modern hardware:

| Stage | Time (single CPU) | Time (8+ CPUs) |
|-------|-------------------|----------------|
| Preprocessing | 5-10 min | 5-10 min |
| Segmentation | 2-4 hours | 30-60 min |
| Surface reconstruction | 1-2 hours | 20-40 min |
| Mesh generation | 30-60 min | 20-40 min |
| **Total** | **4-8 hours** | **1.5-2.5 hours** |

**Factors affecting time:**
- Image resolution
- Brain size (larger brains take longer)
- Mesh complexity
- CPU count and speed

## Batch Processing

For processing multiple subjects, use the HPC array script:

```bash
cd ../hpc

# Submit array job
sbatch --array=1-100%10 charm_array.sh
```

This processes subjects in parallel across cluster nodes. See `../hpc/instructions.md` for details.

## Quality Control

After segmentation, verify output quality:

### Visual Inspection

```bash
# Open in gmsh (SimNIBS GUI)
gmsh_gui m2m_<SUBJECT>/<SUBJECT>_mesh.msh

# Or use SimNIBS viewer
simnibs_gui m2m_<SUBJECT>
```

**Check for:**
- [ ] Complete skull segmentation (no gaps)
- [ ] Proper tissue boundaries (GM/WM, GM/CSF, skull/scalp)
- [ ] Smooth cortical surfaces without holes
- [ ] Realistic anatomy (ventricles, sulci, gyri)
- [ ] No artifacts from motion or scanner issues

### Quantitative Checks

```bash
# Check mesh statistics
mesh_check m2m_<SUBJECT>/<SUBJECT>_mesh.msh

# Verify tissue volumes are reasonable
mri_segstats --seg m2m_<SUBJECT>/final_tissues.nii.gz \
             --sum tissue_volumes.txt
```

**Expected tissue volumes (approximate):**
- Gray matter: 500-700 cm³
- White matter: 400-600 cm³
- CSF: 150-300 cm³
- Skull: 200-350 cm³
- Scalp: 150-250 cm³

### Common Issues

**Issue: Skull segmentation incomplete**
- Cause: Poor bone/tissue contrast in T1
- Solution: Ensure T2 image is provided; check T1 quality

**Issue: Surface self-intersections**
- Cause: Aggressive surface deformation
- Solution: Increase `mesh_stiffness` parameter; use atrophic settings

**Issue: Segmentation fails to initialize**
- Cause: Non-standard orientation or poor alignment
- Solution: Use `--forceqform`; check image orientation with `fslinfo`

**Issue: CSF oversegmentation**
- Cause: Atrophy or enlarged ventricles
- Solution: Use atrophic settings; adjust `csf_factor`

## Integration with Pipeline

After segmentation, proceed to analysis stages:

```bash
# Stage 2.0: Fit atlases
cd ../2-analysis/0-fitting
./0-3_fit_simnibs_atlas.sh ...

# Stage 2.1: Extract tissue statistics
cd ../1-extraction
./3-0_simnibs_stats.sh <SUBJECT> /path/to/m2m_<SUBJECT>

# Stage 2.3: Run simulations
cd ../3-simulation
python main_sim.py /path/to/m2m_<SUBJECT> <CONFIG_JSON>
```

## Troubleshooting

### CHARM Not Found

**Error:** `charm: command not found`

**Solution:**
```bash
# Add SimNIBS to PATH
export PATH=/opt/simnibs/bin:$PATH

# Or specify full path
--charm-executable /opt/simnibs/bin/charm
```

### FreeSurfer Directory Not Found

**Error:** `ERROR: cannot find FreeSurfer subject`

**Solution:**
```bash
# Ensure FreeSurfer subject exists
ls $FREESURFER_SUBJECTS_DIR/<SUBJECT>

# Or run FreeSurfer first
recon-all -s <SUBJECT> -i <T1> -all
```

### Out of Memory

**Error:** `Killed` or segmentation crashes

**Solution:**
- Reduce image resolution (resample to 1mm isotropic)
- Increase available RAM (16+ GB recommended)
- Run on HPC with higher memory allocation

### T2 Registration Fails

**Error:** `ERROR: T2 registration failed`

**Solution:**
```bash
# Skip T2 (use T1 only)
./charm_run.sh ... # omit --t2-path

# Or manually coregister T2 to T1 first using FSL
flirt -in T2.nii.gz -ref T1.nii.gz -out T2_coreg.nii.gz
```

### Mesh Quality Issues

**Error:** `Warning: Poor quality elements detected`

**Solution:**
- Improve input image quality
- Adjust mesh parameters in settings INI:
  ```ini
  [mesh]
  elem_sizes = {"standard": {"range": [1, 5], "slope": 1.0}}
  optimize = true
  smooth_steps = 10
  ```

## Advanced Configuration

### Custom Tissue Segmentation

Modify `settings_base.ini` to adjust segmentation behavior:

```ini
[segment]
# Finer mesh near cortex
downsampling_targets = [2.0, 1.0, 0.5]

# Stronger bias field correction
bias_kernel_width = 50

# More aggressive surface smoothing
[surfaces]
smooth_steps = 15
skin_care = 30
```

### Diffusion Tensor Integration

For anisotropic conductivity modeling (advanced):

```bash
# Process DTI data (not covered here)
# Then reference DTI in simulation config
# See ../2-analysis/3-simulation/ and ../../config/dwi_config.sh
```

## Performance Optimization

### CPU Utilization

```ini
[general]
threads = 0  # Use all CPUs

[surfaces]
processes = 2  # Parallel surface reconstruction
```

### Memory Management

- **Minimum**: 8 GB RAM
- **Recommended**: 16 GB RAM
- **Optimal**: 32+ GB RAM for high-resolution images

### HPC Settings

For SLURM-based clusters:

```bash
sbatch --mem=16G --cpus-per-task=8 --time=3:00:00 \
  ../hpc/charm_array.sh
```

## Best Practices

1. **Always use T2 when available**: Significantly improves tissue boundaries
2. **Verify FreeSurfer quality first**: CHARM leverages FreeSurfer surfaces
3. **Use consistent preprocessing**: ACPC alignment improves reliability
4. **Document settings**: Track which INI file was used per subject
5. **Archive head models**: Regenerating is time-consuming
6. **Visual QC every subject**: Automated QC cannot catch all issues
7. **Keep raw images**: Reprocessing may be needed with updated CHARM versions

## Resources

- **CHARM Documentation**: https://simnibs.github.io/simnibs/build/html/documentation/head_meshing/charm.html
- **SimNIBS Tutorial**: https://simnibs.github.io/simnibs/build/html/tutorial/tutorial.html
- **SimNIBS Forum**: https://simnibs.discourse.group/

## Citation

If using CHARM for head modeling, cite:

```
Puonti, O., Van Leemput, K., Saturnino, G. B., Siebner, H. R., Madsen, K. H., & Thielscher, A. (2020). Accurate and robust whole-head segmentation from magnetic resonance images for individualized head modeling. NeuroImage, 219, 117044. https://doi.org/10.1016/j.neuroimage.2020.117044
```

And SimNIBS:

```
Thielscher, A., Antunes, A., & Saturnino, G. B. (2015). Field modeling for transcranial magnetic stimulation: A useful tool to understand the physiological effects of TMS? 2015 37th Annual International Conference of the IEEE Engineering in Medicine and Biology Society (EMBC), 222–225. https://doi.org/10.1109/EMBC.2015.7318340
```

## Support

For issues or questions:
- Check SimNIBS documentation and forum
- Verify input image quality (motion, artifacts, contrast)
- Test with standard settings before using custom configurations
- Review HPC batch scripts for working examples

## Version Information

- **SimNIBS**: 4.0+
- **FreeSurfer**: 6.0+
- **Recommended OS**: Linux (Ubuntu 20.04+, CentOS 7+) or macOS

CHARM settings in this repository are tested with SimNIBS 4.0+. Older versions may require parameter adjustments.

