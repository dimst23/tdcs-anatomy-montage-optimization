#!/bin/bash

set -e

CONFIG_FILE="${1}"
LOG_DIR="${2}"
SUBJECT="${3}"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}/${SUBJECT}/T1w"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

SUBJ_SUCCESSFUL=0
SUBJ_SAVE_FILE="${LOG_DIR}/subs_lgi_test.txt"

HEMISPHERES="lh rh"
cd "${ROOT_DIR}/${SUBJECT}/T1w/${SUBJECT}/surf"

if [ -f "${SUBJ_SAVE_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${SUBJ_SAVE_FILE}"; then
        echo "lgi - Subject ${SUBJECT} already completed. Exiting...."
        exit 0
    fi
fi

for hemi in $HEMISPHERES;
do
    if [ -f "${hemi}.pial_lgi" ]; then
        mri_info "${hemi}.pial_lgi"
        SUBJ_SUCCESSFUL+="$?"
    else
        echo "'${hemi}.pial_lgi' for subject '${SUBJECT}' not found. Exiting...."
        exit 1
    fi
done

if [ "$SUBJ_SUCCESSFUL" -eq "0" ]; then
    touch "$SUBJ_SAVE_FILE"
    echo "${SUBJECT}" >> "${SUBJ_SAVE_FILE}"
fi


