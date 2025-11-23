#!/bin/bash

set -e

CONFIG_FILE="${1}"
SUBJECT="${2}"
SUBJECTS_DIR="${3}"
FSAVERAGE_TYPE="${4}"
FWHM="${5}" # mm

# Like thickness, sulc, pial_lgi or any curv file suffix
IFS=',' read -r -a CURV_TYPES <<< "${6}"
HEMISPHERES="lh rh"

source $CONFIG_FILE

export SUBJECTS_DIR="$SUBJECTS_DIR"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

cd "$SUBJECTS_DIR"
# cd "$SUBJECT"
# ln -sf surfaces surf
# cd ..
echo "Doing ${SUBJECT}"

ln -sf "${FREESURFER_HOME}/subjects/${FSAVERAGE_TYPE}" "$FSAVERAGE_TYPE"

for hemi in $HEMISPHERES;
do
    for type in "${CURV_TYPES[@]}";
    do
        # surf_dir="${SUBJECTS_DIR}/${SUBJECT}/surfaces"
        surf_dir="${SUBJECTS_DIR}/${SUBJECT}/surf"
        target_name="${hemi}.${FSAVERAGE_TYPE}.fwhm${FWHM}.${type}"

        if [[ "$FWHM" -eq 0 ]]; then
            mri_surf2surf --srcsubject "${SUBJECT}" --trgsubject "${FSAVERAGE_TYPE}" --hemi "${hemi}" --surfreg sphere.reg --tval "${surf_dir}/${hemi}.${FSAVERAGE_TYPE}.${type}" --sval "${surf_dir}/${hemi}.${type}" --tfmt curv >/dev/null &
        else
            # mri_surf2surf --srcsubject "${SUBJECT}" --trgsubject "${FSAVERAGE_TYPE}" --hemi "${hemi}" --surfreg sphere.reg --tval "${surf_dir}/${target_name}" --sval "${surf_dir}/${hemi}.${type}" --tfmt curv --sfmt curv --fwhm ${FWHM} >/dev/null &
            mri_surf2surf --srcsubject "${SUBJECT}" --trgsubject "${FSAVERAGE_TYPE}" --hemi "${hemi}" --surfreg sphere.reg --tval "${surf_dir}/${target_name}" --sval "${surf_dir}/${hemi}.${type}" --tfmt curv --cortex --sfmt curv --fwhm ${FWHM} >/dev/null &
        fi
    done
    wait # Wait for the parallel processes to complete for each type
done

rm "${FSAVERAGE_TYPE}"