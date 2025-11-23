#!/bin/bash
#SBATCH -J simnibs-charm
#SBATCH --partition=batch
#SBATCH --time=03:30:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=7G
#SBATCH --qos=small

set -e

JOB_ID="$SLURM_ARRAY_JOB_ID"

HOMEDIR="${HOME}"
SCRIPT_DIR="${HOME}/job_scripts/tdcs-hcp/simnibs"
ANALYSIS_REPO_DIR="${HOME}/repos/tdcs-variability"
SIMNIBS_CHARM="${HOMEDIR}/software/simnibs4.1/bin/charm"

set -a # automatically export all variables
source "$SCRIPT_DIR/.env"
set +a

MNT_DIR="$SCRATCH/mnt_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"

SUBJECT=`sed -n "${SLURM_ARRAY_TASK_ID} p" ${SCRIPT_DIR}/subjects.txt`
SUBJECT="${SUBJECT}_V1_MR"
SUBJECTS_DIR="${MNT_DIR}/hcpa-data/Package_${PACKAGE_ID}/fmriresults01/${SUBJECT}/"
SUBJECTS_COMPLETE_FILE="${SCRIPT_DIR}/charm-complete_subject_ids.txt"

########## Set the atrophy settings ##########
IS_ATROPHY=false
########## Set the atrophy settings ##########

export LOGS_DIR="${HOMEDIR}/job_logs"

if [ -f "${SUBJECTS_COMPLETE_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${SUBJECTS_COMPLETE_FILE}"; then
        echo "charm - Subject ${SUBJECT} already completed. Exiting..."
        exit 0
    fi
fi

mkdir -p "$MNT_DIR"
sshfs -o password_stdin -p 23 "$SSHFS_REMOTE_URL" "$MNT_DIR" <<< "$SSHFS_STORAGE_PASSWORD"

ls -l $MNT_DIR

echo "Hello from $(hostname)"
echo "Array number ${SLURM_ARRAY_TASK_ID}"
echo "Array number ${JOB_ID}"

mkdir -p "$LOGS_DIR/${JOB_ID}"

cd "${SUBJECTS_DIR}" && mkdir -p simnibs && cd simnibs

if [ "$IS_ATROPHY" = true ]; then
    CHARM_SETTINGS_NAME="atrophic/settings_atrophy"
else
    CHARM_SETTINGS_NAME="settings_base"
fi

eval "${ANALYSIS_REPO_DIR}/src/0-segmentation/charm_run.sh" -s ${SUBJECT} --charm-executable "$SIMNIBS_CHARM" --charm-settings-path "${ANALYSIS_REPO_DIR}/src/0-segmentation/settings/${CHARM_SETTINGS_NAME}.ini" --t1-path ../T1w/T1w_acpc_dc_restore.nii.gz --t2-path ../T1w/T2w_acpc_dc_restore.nii.gz --fs-subj-dir "${SUBJECTS_DIR}/T1w/${SUBJECT}" --run-dir "$(pwd)" > "$LOGS_DIR/${JOB_ID}/${SLURM_ARRAY_TASK_ID}_${SUBJECT}_charm.log" 2>&1

echo "${SLURM_ARRAY_TASK_ID} ${SUBJECT}" >> "${SUBJECTS_COMPLETE_FILE}"

# Make sure that the remote directory is unmounted and deleted
cd "$HOMEDIR"
# umount "$MNT_DIR"
# rm -rf "$MNT_DIR"
