#!/bin/bash

set -e

CONFIG_FILE="${1}"
ANNOTATION="${2}"
SIMULATION="${3}"
FIELD_TYPE="${4}"
SUBJECT="${5}_V1_MR"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

HEMISPHERES="lh rh"
cd "${ROOT_DIR}/${SUBJECT}/simnibs" && mkdir -p fs_stats/sim/$SIMULATION

for hemi in $HEMISPHERES;
do
    export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}/${SUBJECT}/simnibs"
    mris_convert "m2m_$SUBJECT/surf/$hemi.white.gii" "m2m_$SUBJECT/surf/$hemi.white"

    mri_segstats --annot "m2m_$SUBJECT" ${hemi} ${ANNOTATION} --i "${SUBJECTS_DIR}/m2m_${SUBJECT}/simulations/${SIMULATION}/subject_overlays/${hemi}.${SUBJECT}_${FIELD_TYPE}" --sum "fs_stats/sim/$SIMULATION/$hemi.${ANNOTATION}.${FIELD_TYPE}.stats"
done
