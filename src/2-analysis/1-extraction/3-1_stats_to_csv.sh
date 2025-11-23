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

SIM="Index,SegId,NVertices,Area_mm2,StructName,Mean,StdDev,Min,Max,Range"

HEMISPHERES="lh rh"

to_csv () {
    res="$(cat "${1}" | grep -v '#' | sed 's/ \{1,\}/,/g' | sed 's/^,//' | sed 's/,$//')"
    echo "$res"
}

cd "${ROOT_DIR}/${SUBJECT}/simnibs/fs_stats"

for hemi in $HEMISPHERES;
do
    OUTPUT_FILE="sim/$SIMULATION/$hemi.${ANNOTATION}.${FIELD_TYPE}.csv"
    INPUT_FILE="sim/$SIMULATION/$hemi.${ANNOTATION}.${FIELD_TYPE}.stats"

    echo $SIM > $OUTPUT_FILE
    to_csv $INPUT_FILE >> $OUTPUT_FILE
    
    rm $INPUT_FILE
done
