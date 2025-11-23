#!/bin/bash

declare -A DWIConfig

DWIConfig["SUBJECTS_DIR"]="subjects/directory"

DWIConfig["ANALYSIS_DIR"]="analysis"
DWIConfig["SCRATCH_DIR"]="${DWIConfig["ANALYSIS_DIR"]}/scratch"
DWIConfig["LOGS_DIR"]="${DWIConfig["ANALYSIS_DIR"]}/logs"
DWIConfig["FOD_DIR"]="${DWIConfig["ANALYSIS_DIR"]}/fod"
DWIConfig["DTI_DIR"]="${DWIConfig["ANALYSIS_DIR"]}/dti"
DWIConfig["QA_DIR"]="${DWIConfig["ANALYSIS_DIR"]}/qa"

DWIConfig["FSL_DIR"]="~/software/fsl"
DWIConfig["ANTS_DIR"]="/opt/ANTs/bin/"
DWIConfig["FREESURFER_DIR"]="/opt/freesurfer6/bin"

export FSLDIR="${DWIConfig["FSL_DIR"]}"
source "$FSLDIR/etc/fslconf/fsl.sh"
