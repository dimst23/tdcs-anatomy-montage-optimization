#!/bin/bash
#SBATCH -J simnibs-simulation
#SBATCH --partition=batch
#SBATCH --time=03:30:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=15G
#SBATCH --qos=small

set -e

JOB_ID="$SLURM_ARRAY_JOB_ID"

HOMEDIR="${HOME}"
SCRIPT_DIR="${HOME}/job_scripts/tdcs-hcp/simnibs"
ANALYSIS_REPO_DIR="${HOME}/repos/tdcs-variability"
SIMNIBS_PYTHON="${HOMEDIR}/software/simnibs4.1/bin/simnibs_python"

set -a # automatically export all variables
source "$SCRIPT_DIR/.env"
set +a

MNT_DIR="$SCRATCH/mnt_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"

SUBJECT=`sed -n "${SLURM_ARRAY_TASK_ID} p" ${SCRIPT_DIR}/subjects.txt`
SUBJECT="${SUBJECT}_V1_MR"
SUBJECTS_DIR="${MNT_DIR}/hcpa-data/Package_${PACKAGE_ID}/fmriresults01/${SUBJECT}"
SUBJECTS_COMPLETE_FILE="${SCRIPT_DIR}/sim-complete_subject_ids.txt"

export LOGS_DIR="${HOMEDIR}/job_logs"

if [ -f "${SUBJECTS_COMPLETE_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${SUBJECTS_COMPLETE_FILE}"; then
        echo "sim - Subject ${SUBJECT} already completed. Exiting..."
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

cd "${SUBJECTS_DIR}"

eval "${ANALYSIS_REPO_DIR}/src/2-analysis/3-simulation/sim_run.sh" --subj-dir "${SUBJECTS_DIR}" --sim-settings-path "${ANALYSIS_REPO_DIR}/config/3-simulation/sim_settings_thesis.json" --sim-executable "${SIMNIBS_PYTHON}" -s "${SUBJECT}" > "$LOGS_DIR/${JOB_ID}/${SLURM_ARRAY_TASK_ID}_${SUBJECT}_sim.log" 2>&1

echo "${SLURM_ARRAY_TASK_ID} ${SUBJECT}" >> "${SUBJECTS_COMPLETE_FILE}"

# Make sure that the remote directory is unmounted and deleted
# cd "$HOMEDIR"
# umount "$MNT_DIR"
# rm -rf "$MNT_DIR"
