#!/bin/bash

set -e
CONFIG_FILE="${1}"
S_ANNOTATION="${2}"
T_ANNOTAION="${3}"
SUBJECTS_DIR="${4}"
SUBJECT="${5}"

HEMISPHERES="lh rh"

source $CONFIG_FILE

source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"
SCRIPT_DIR=$(dirname `which $0`)

cd "${SUBJECTS_DIR}/${SUBJECT}/simnibs/m2m_${SUBJECT}"

for hemi in $HEMISPHERES;
do
    ln -sf surfaces surf && mkdir -p label
    mris_convert surf/$hemi.sphere.reg.gii surf/$hemi.sphere.reg
done

exec "${SCRIPT_DIR}/0-0_fit_fs_atlas.sh" "${CONFIG_FILE}" "${S_ANNOTATION}" "${T_ANNOTAION}" "${SUBJECTS_DIR}/${SUBJECT}/simnibs" "m2m_${SUBJECT}"