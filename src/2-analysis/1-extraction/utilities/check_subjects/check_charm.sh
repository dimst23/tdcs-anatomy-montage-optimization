#!/bin/bash

set -e

CONFIG_FILE="${1}"
LOG_DIR="${2}"
SUBJECT="${3}"

SUBJ_SUCCESSFUL=0
SUBJ_SAVE_FILE="${LOG_DIR}/subs_charm_test.txt"

source $CONFIG_FILE
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

CHARM_LOG_PATH="${ROOT_DIR}/${SUBJECT}/simnibs/m2m_${SUBJECT}/charm_log.html"

if [ -f "${SUBJ_SAVE_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${SUBJ_SAVE_FILE}"; then
        echo "charm - Subject ${SUBJECT} already completed. Exiting...."
        exit 0
    fi
fi

if [ ! -f "${CHARM_LOG_PATH}" ]; then
    echo "Charm log for subject '${SUBJECT}' not found. Exiting...."
    exit 1
fi

if grep -q -w "charm run finished:" "${CHARM_LOG_PATH}"; then
    touch "$SUBJ_SAVE_FILE"
    echo "${SUBJECT}" >> "${SUBJ_SAVE_FILE}"
fi


