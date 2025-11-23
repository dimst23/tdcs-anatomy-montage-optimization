#!/bin/bash
#SBATCH -J fs-lgi
#SBATCH --partition=batch
#SBATCH --time=05:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=3G
#SBATCH --qos=small

module load gcc/10.2.0 matlab

set -e

SCRIPT_DIR="${HOME}/job_scripts/tdcs-hcp/simnibs"
ANALYSIS_REPO_DIR="${HOME}/repos/tdcs-variability"

set -a # automatically export all variables
source "$SCRIPT_DIR/.env"
set +a

MNT_DIR="$SCRATCH/mnt_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}"
SUBJECTS_COMPLETE_FILE="${SCRIPT_DIR}/lgi-complete_subject_ids.txt"

mkdir -p "$MNT_DIR"
sshfs -o password_stdin -p 23 "$SSHFS_REMOTE_URL" "$MNT_DIR" <<< "$SSHFS_STORAGE_PASSWORD"

ls -l $MNT_DIR

HOMEDIR="~"
JOB_ID="$SLURM_ARRAY_JOB_ID"

SUBJECT=`sed -n "${SLURM_ARRAY_TASK_ID} p" ${SCRIPT_DIR}/subjects.txt`
SUBJECT="${SUBJECT}_V1_MR"

if [ -f "${SUBJECTS_COMPLETE_FILE}" ]; then
    if grep -q -w "${SUBJECT}$" "${SUBJECTS_COMPLETE_FILE}"; then
        echo "lgi - Subject ${SUBJECT} already completed. Exiting..."
        exit 0
    fi
fi

export NO_MINC=1
export NO_FSFAST=1
source $HOMEDIR/software/set_fs.sh

export SUBJECTS_DIR="${MNT_DIR}/hcpa-data/Package_${PACKAGE_ID}/fmriresults01/${SUBJECT}/T1w"
export LOGS_DIR="${HOMEDIR}/job_logs"

echo "Hello from $(hostname)\n"
echo "Array number ${SLURM_ARRAY_TASK_ID}\n"
echo "Array number ${JOB_ID}\n"

mkdir -p "$LOGS_DIR/${JOB_ID}"
recon-all -no-isrunning -s "${SUBJECT}" -localGI > "$LOGS_DIR/${JOB_ID}/${SLURM_ARRAY_TASK_ID}_${SUBJECT}_local_GI.log" 2>&1

echo "${SLURM_ARRAY_TASK_ID} ${SUBJECT}" >> "$SUBJECTS_COMPLETE_FILE"
