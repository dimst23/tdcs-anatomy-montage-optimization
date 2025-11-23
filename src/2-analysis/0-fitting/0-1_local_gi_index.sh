#!/bin/bash

set -e

CONFIG_FILE="${1}"
SUBJECT="${2}"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

cd "${ROOT_DIR}/${SUBJECT}" && mkdir -p fs_logs
recon-all -s ${SUBJECT} -localGI > fs_logs/${SUBJECT}_local_GI.log 2>&1

echo "${SUBJECT}" >> "${3}"