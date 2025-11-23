#!/bin/bash

set -e

CONFIG_FILE="${1}"
ANNOTATION="${2}"
SUBJECT="${3}"

source $CONFIG_FILE
SCRIPT_DIR=$(dirname `which $0`)
ROOT_DIR="${DWIConfig["SUBJECTS_DIR"]}"

LGI="Index,SegId,NVertices,Area_mm2,StructName,Mean,StdDev,Min,Max,Range"
ANATOMICAL="StructName,NumVert,SurfArea,GrayVol,ThickAvg,ThickStd,MeanCurv,GausCurv,FoldInd,CurvInd"
WHITEMATTER="Index,SegId,NVoxels,Volume_mm3,StructName,Mean,StdDev,Min,Max,Range"

HEMISPHERES="lh rh"

export SUBJECTS_DIR="${DWIConfig["SUBJECTS_DIR"]}/${SUBJECT}/T1w"
export FSLDIR="${DWIConfig["FSL_DIR"]}"
source "$FSLDIR/etc/fslconf/fsl.sh"

to_csv () {
    res="$(cat "${1}" | grep -v '#' | sed 's/ \{1,\}/,/g' | sed 's/^,//' | sed 's/,$//')"
    echo "$res"
}

cd "${ROOT_DIR}/${SUBJECT}/T1w/fs_stats"

for hemi in $HEMISPHERES;
do
    OUTPUT_FILE="$hemi.${ANNOTATION}.csv"

    echo $LGI > lgi/$OUTPUT_FILE
    to_csv lgi/$hemi.${ANNOTATION}.pial_lgi.stats >> lgi/$OUTPUT_FILE

    echo $ANATOMICAL > anat/$OUTPUT_FILE
    cat anat/$hemi.${ANNOTATION}.anat.stats | grep -v '#' | sed 's/ \{1,\}/,/g' | sed 's/^,//' | sed 's/,$//' >> anat/$OUTPUT_FILE

    # echo $ANATOMICAL > anat/wm/$OUTPUT_FILE
    # cat anat/$hemi.${2}.anat.stats | grep -v '#' | sed 's/ \{1,\}/,/g' | sed 's/^,//' | sed 's/,$//' >> anat/$OUTPUT_FILE

done
