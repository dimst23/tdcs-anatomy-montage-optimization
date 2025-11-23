#!/bin/bash

set -e

CONFIG_FILE="${1}"
SURFACE="${2}"
SUBJECT="${3}"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}/${SUBJECT}/T1w"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

HEMISPHERES="lh rh"
cd "${ROOT_DIR}/${SUBJECT}/simnibs/m2m_${SUBJECT}/surfaces"

echo "Converting '${SUBJECT}'"

for hemi in $HEMISPHERES;
do
    if [ -f "${hemi}.${SURFACE}.gii" ]; then
        mris_convert "${hemi}.${SURFACE}.gii" "${hemi}.${SURFACE}"
    else
        echo "'${hemi}.${SURFACE}.gii' for subject '${SUBJECT}' not found."
    fi
done

echo ""
