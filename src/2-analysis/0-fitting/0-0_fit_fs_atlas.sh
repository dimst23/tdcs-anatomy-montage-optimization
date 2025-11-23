#!/bin/bash

set -e
CONFIG_FILE="${1}"
S_ANNOTATION="${2}"
T_ANNOTAION="${3}"
SUBJECTS_DIR="${4}"
SUBJECT="${5}"

# 1 -> Config sh
# 2 -> Source annot subj dir
# 3 -> Annotation name
source $CONFIG_FILE

export SUBJECTS_DIR="$SUBJECTS_DIR"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

cd "$SUBJECTS_DIR"
ln -sf "${FREESURFER_HOME}/subjects/fsaverage" fsaverage

HEMISPHERES="lh rh"

for hemi in $HEMISPHERES;
do
    mri_surf2surf --hemi "${hemi}" --srcsubject fsaverage --trgsubject "${SUBJECT}" --sval-annot "$S_ANNOTATION/label/${hemi}.$T_ANNOTAION.annot" --tval "${SUBJECTS_DIR}/${SUBJECT}/label/${hemi}.$T_ANNOTAION.annot"
done

rm fsaverage