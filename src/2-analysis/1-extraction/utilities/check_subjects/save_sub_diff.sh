#!/bin/bash

set -e

COMPARING_FILE="${1}"
DIFF_SAVE_FILE="${2}"
SUBJECT="${3}"

touch "$DIFF_SAVE_FILE"
if [ -f "${COMPARING_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${COMPARING_FILE}"; then
        echo "Subject ${SUBJECT} already completed. Exiting...."
        exit 0
    fi
else
    echo "Comparison file not found. Exiting...."
    exit 1
fi
echo "${SUBJECT}" >> "${DIFF_SAVE_FILE}"
