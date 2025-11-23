#!/bin/bash

set -e

CONFIG_FILE="${1}"
SUBJECT="${2}"
SUBJECTS_DIR="${3}"
FSAVERAGE_TYPE="${4}"
FWHM="${5}" # mm
SIM_FOLDER="${6}"

IFS=',' read -r -a SIMULATIONS <<< "${7}"
HEMISPHERES="lh rh"

source $CONFIG_FILE

export SUBJECTS_DIR="$SUBJECTS_DIR"
source "${DWIConfig["FSL_DIR"]}/etc/fslconf/fsl.sh"

cd "$SUBJECTS_DIR"
ln -sf "${FREESURFER_HOME}/subjects/${FSAVERAGE_TYPE}" "$FSAVERAGE_TYPE"

echo "### Converting subject: $SUBJECT"

for sim_type in "${SIMULATIONS[@]}";
do
    SIM_DIR="${SUBJECTS_DIR}/m2m_${SUBJECT}/${SIM_FOLDER}/${sim_type}/subject_overlays"
    if [[ "$FWHM" -eq 0 ]]; then
        SAVE_DIR="${SIM_DIR}/../${FSAVERAGE_TYPE}_overlays"
    else
        SAVE_DIR="${SIM_DIR}/../${FSAVERAGE_TYPE}_fwhm${FWHM}_overlays"
    fi

    # Check if the directory exists
    if [ -d "$SAVE_DIR" ]; then
        echo "## Directory $SAVE_DIR exists. Continuing..."
        continue  # Skip to the next iteration
    fi

    mkdir -p "${SAVE_DIR}"

    for hemi in $HEMISPHERES;
    do
        file_name="${hemi}.${SUBJECT}_TDCS_1_scalar.central.*"
        files=($SIM_DIR/$file_name)

        for file in "${files[@]}"; do
            base_file=$(basename "$file")

            # echo "Converting $base_file to $FSAVERAGE_TYPE space of subject $SUBJECT"
            if [[ "$FWHM" -eq 0 ]]; then
                mri_surf2surf --srcsubject "m2m_${SUBJECT}" --trgsubject "${FSAVERAGE_TYPE}" --hemi "${hemi}" --surfreg sphere.reg --tval "${SAVE_DIR}/${base_file}" --sval "${SIM_DIR}/${base_file}" --tfmt curv --sfmt curv >/dev/null &
            else
                mri_surf2surf --srcsubject "m2m_${SUBJECT}" --trgsubject "${FSAVERAGE_TYPE}" --hemi "${hemi}" --surfreg sphere.reg --tval "${SAVE_DIR}/${base_file}" --sval "${SIM_DIR}/${base_file}" --tfmt curv --sfmt curv --fwhm ${FWHM} >/dev/null &
            fi
        done
        wait # Wait for the parallel processes to complete for each type
    done
done

rm "${FSAVERAGE_TYPE}"

echo "### Done converting subject: $SUBJECT"