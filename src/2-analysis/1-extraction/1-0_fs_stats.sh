#!/bin/bash

set -e

CONFIG_FILE="${1}"
ANNOTATION="${2}"
SUBJECT="${3}"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}/${SUBJECT}/T1w"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

HEMISPHERES="lh rh"
cd "${ROOT_DIR}/${SUBJECT}/T1w" && mkdir -p fs_stats/{anat,curv,lgi}

for hemi in $HEMISPHERES;
do
    mris_anatomical_stats -a "$ANNOTATION.annot" -f fs_stats/anat/$hemi.$ANNOTATION.anat.stats $SUBJECT $hemi
    # mris_curvature_stats -o fs_stats/curv/$hemi.curv -G $SUBJECT $hemi &
    mri_segstats --annot $SUBJECT $hemi ${ANNOTATION} --i $SUBJECTS_DIR/$SUBJECT/surf/$hemi.pial_lgi --sum fs_stats/lgi/$hemi.${ANNOTATION}.pial_lgi.stats
done
