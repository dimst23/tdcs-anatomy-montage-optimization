import json
import logging
import pathlib

import numpy as np
import pandas as pd
import typer

app = typer.Typer()


class NpEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return super(NpEncoder, self).default(obj)


@app.command()
def age_bins(
    csv_data_path: str,
    start_age: int,
    end_age: int,
    step: int,
    age_rounding: bool = False,
    random_selection: int = 0,
    random_seed: int = 0,
    file_name: str = "age_bins_journal.json",
    save_subjects_list: bool = False,
):
    # Create an array with the starting ages per bin
    age_bins = np.arange(start_age, end_age, step)

    df_subject_data = pd.read_csv(csv_data_path, skiprows=[1], delimiter="\t")
    df_subject_data["age_years"] = df_subject_data["interview_age"].apply(
        lambda x: round(x / 12.0, 1)
    )
    list_binned_ages = []

    if age_rounding:
        df_subject_data["age_years"] = df_subject_data["age_years"].apply(
            lambda x: round(x)
        )

    saved_subjects = set()
    for bin_index, bin_value in enumerate(age_bins):
        age_difference = df_subject_data["age_years"] - bin_value
        bin_index += 1  # To prevent 0 index from interfering

        age_bin_index = np.where((age_difference >= 0) & (age_difference < step))
        age_bin_subject_id = df_subject_data["src_subject_id"][age_bin_index[0]]
        age_bin_subject_age = df_subject_data["age_years"][age_bin_index[0]]

        if random_selection:
            np.random.seed(random_seed)
            random_index = np.random.choice(
                age_bin_subject_id.index, random_selection, replace=False
            )
            age_bin_subject_id = age_bin_subject_id.loc[random_index]
            age_bin_subject_age = age_bin_subject_age.loc[random_index]

        if any(sub in saved_subjects for sub in age_bin_subject_id):
            logging.warning("Duplicate subject detected.")
        saved_subjects = saved_subjects.union(age_bin_subject_id)

        list_binned_ages.append(
            {
                "age_bin_start": bin_value,
                "age_bin_end": bin_value + step,
                "age_bin_step": step,
                "age_bin_index": bin_index,
                "age_bin_subject_metadata": {
                    "ids": age_bin_subject_id.tolist(),
                    "ages": age_bin_subject_age.tolist(),
                },
            }
        )

        if save_subjects_list:
            file_path = pathlib.Path(file_name).resolve().parent
            with open(
                file_path.joinpath("age_bins_subjects.txt").resolve(),
                "a" if bin_index > 1 else "w",
            ) as f:
                f.write("\n".join(age_bin_subject_id.tolist()))
                f.write("\n")

    with open(pathlib.Path(file_name).resolve(), "w") as f:
        f.write(json.dumps(list_binned_ages, cls=NpEncoder, indent=2))


if __name__ == "__main__":
    app()
