#!/bin/bash

set -e

# Define the default values
T1_PATH="../T1w/T1w_acpc_dc_restore.nii.gz"
T2_PATH="../T1w/T2w_acpc_dc_restore.nii.gz"

show_help() {
    echo "Usage: ./script.sh [OPTIONS]"
    echo "Description: Run the charm segmentation pipeline"
    echo "Options:"
    echo "  -s, --subject SUBJECT        Name of the subject"
    echo "  --charm-executable PATH      Path to the Charm executable"
    echo "  --charm-settings-path PATH   Path to the Charm settings file"
    echo "  --run-dir PATH               Path to where charm shoudl be saving the output"
    echo "  --fs-subj-dir PATH           Path to the FreeSurfer subject directory"
    echo "  --t1-path PATH               Path to the T1 file (default: $T1_PATH)"
    echo "  --t2-path PATH               Path to the T2 file (default: $T2_PATH)"
    echo "  -h, --help                   Show this help message and exit"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -s|--subject)
        SUBJECT="$2"
        shift
        shift
        ;;
        --charm-executable)
        CHARM_EXECUTABLE="$2"
        shift
        shift
        ;;
        --charm-settings-path)
        CHARM_SETTINGS_PATH="$2"
        shift
        shift
        ;;
        --fs-subj-dir)
        FS_SUBJ_DIR="$2"
        shift
        shift
        ;;
        --run-dir)
        RUN_DIR="$2"
        shift
        shift
        ;;
        --t1-path)
        T1_PATH="$2"
        shift
        shift
        ;;
        --t2-path)
        T2_PATH="$2"
        shift
        shift
        ;;
        -h|--help)
        show_help
        exit 0
        ;;
        *)
        echo "Unknown option: $1"
        show_help
        exit 1
        ;;
    esac
done

# Check if all required arguments are provided
if [[ -z $SUBJECT || -z $CHARM_EXECUTABLE || -z $CHARM_SETTINGS_PATH || -z $FS_SUBJ_DIR || -z $RUN_DIR ]]; then
    echo "Error: Some or all required options not included."
    show_help
    exit 1
fi

cd "$RUN_DIR"
eval "$CHARM_EXECUTABLE" \
"$SUBJECT" \
"$T1_PATH" \
"$T2_PATH" \
--skipregisterT2 \
--noneck \
--usesettings "$CHARM_SETTINGS_PATH" \
--fs-dir "$FS_SUBJ_DIR"
