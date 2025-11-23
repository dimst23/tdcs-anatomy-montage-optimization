# Instructions

Using the `parallel` utility in bash, we can run the script either for charm. lgi or any other similar way as follows:

```bash
cat temp/all_subjects.txt | parallel -j 10 $SCRIPTS_LOCATION/check_charm.sh $CONFIG_LOCATION/dwi_config.sh $LOG_DIR {}_V1_MR
```

where `all_subjects.txt` contains all the original subject names that we have. The notation `{}_V1_MR` denotes the current subject as parsed from the `all_subjects.txt` with parallel, extending the name with the `_V1_MR` suffix.

## Difference

After running the desired scripts to get the subjects that run successfully for a given software, calculating the difference between the original and the calculated to find which are missing is done as follows:

```bash
cat temp/all_subjects.txt | parallel -j 10 $SCRIPTS_LOCATION/save_sub_diff.sh COMPARISON_FILE DIFFERENCE_SAVE_FILE {}_V1_MR
```
