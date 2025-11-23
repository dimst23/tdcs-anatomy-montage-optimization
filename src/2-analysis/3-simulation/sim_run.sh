#!/bin/bash

set -e

show_help() {
    echo "Usage: ./sim_run.sh [OPTIONS]"
    echo "Description: Run the simulations"
    echo "Options:"
    echo "  -s, --subject SUBJECT        Name of the subject"
    echo "  --sim-executable PATH        Path to the python executable"
    echo "  --sim-settings-path PATH     Path to the sim settings file"
    echo "  --subj-dir PATH              Path of the subject directory"
    echo "  -h, --help                   Show this help message and exit"
}

while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -s|--subject)
        SUBJECT="$2"
        shift
        shift
        ;;
        --sim-executable)
        SIM_EXECUTABLE="$2"
        shift
        shift
        ;;
        --sim-settings-path)
        SIM_SETTINGS_PATH="$2"
        shift
        shift
        ;;
        --subj-dir)
        SUBJ_DIR="$2"
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
if [[ -z $SUBJECT || -z $SIM_EXECUTABLE || -z $SIM_SETTINGS_PATH || -z $SUBJ_DIR ]]; then
    echo "Error: Some or all required options not included."
    show_help
    exit 1
fi

SCRIPT_DIR=$(dirname `which $0`)

eval "$SIM_EXECUTABLE" \
"$SCRIPT_DIR/main_sim.py" \
"$SUBJ_DIR/simnibs/m2m_$SUBJECT" \
"$SIM_SETTINGS_PATH"