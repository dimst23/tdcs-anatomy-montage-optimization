# -*- coding: utf-8 -*-
"""SimNIBS simulation
"""
import argparse
import logging
import os
import pathlib
import sys
from copy import deepcopy
from typing import Any

from models import ElectrodeDefinition, ElectrodeSettings, SimulationSettings
from simnibs import __version__, run_simnibs, sim_struct
from utility_functions import parse_settings, set_object_attributes

"""
Define the electrodes manually and read the configuration from a file to decide which electrode will be used as a the reference and which will be discarded
from the calculations. The reference electrode shall appear first in the list of electrodes, and the discarded ones will not be present.
"""


def parse_arguments(argv):
    parser = argparse.ArgumentParser(
        prog="leadfield generator",
        description="Calculate the tDCS leadfields "
        " of a given model, as defined in the settings file.",
    )
    parser.add_argument(
        "model_path", help='SimNIBS model folder. Must start with "m2m"'
    )
    parser.add_argument("config", help="Simulation configuration file (JSON format)")
    parser.add_argument("--version", action="version", version=__version__)

    return parser.parse_args(argv)


def convert_electrodes_to_simnibs(
    electrodes: list[ElectrodeDefinition],
    electrode_settings: list[ElectrodeSettings],
) -> dict[str, list[dict[str, Any]]]:
    """Generates the dictionary with all the desired electrode settings

    Args:
        electrodes (list[ElectrodeDefinition]): Electrodes based on the ElectrodeDefinition model
        electrode_settings (list[ElectrodeSettings]): All settings related to the electrodes

    Returns:
        dict[str, list[dict[str, Any]]]: Dictionary to be used in the definition os the simulation later
    """
    electrode_types = {
        mdl.type: mdl.model_dump(mode="json") for mdl in electrode_settings
    }
    electrode_groups = {key: [] for key in electrode_types.keys()}

    for electrode in electrodes:
        for el_type in electrode.type:
            type_props = deepcopy(electrode_types[el_type])
            type_props["centre"] = electrode.centre
            type_props["current"] = electrode.current / 1e3

            if electrode.y_direction:
                type_props["pos_ydir"] = electrode.y_direction
            del type_props["type"]

            electrode_groups[el_type].append(type_props)

    return electrode_groups


def is_existing_simulation(output_path: pathlib.Path):
    """Check if a simulation result already exists

    Args:
        output_path (pathlib.Path): The full path to the simulation folder

    Returns:
        bool: Whether the simulation result exists
    """
    mat_file_path = output_path.joinpath("simnibs_simulation*.mat")

    # Split the directory and the wildcard part
    directory = mat_file_path.parent
    wildcard = mat_file_path.name

    # Use Path.glob with the wildcard part
    files_list = list(directory.glob(wildcard))

    if files_list:
        return True

    return False


def main(sim_settings: SimulationSettings, model_path: pathlib.Path):
    electrodes = sim_settings.electrodes
    electrode_settings = sim_settings.electrode_settings
    sim_objects = []

    if sim_settings.tensor:
        tensor_dict = {
            "fname_tensor": model_path.joinpath(sim_settings.tensor.tensor_path)
            .resolve()
            .as_posix(),
            "anisotropy_type": sim_settings.tensor.tensor_type.name,
        }
    else:
        tensor_dict = {}

    for sim_setup in electrodes:
        electrode_groups = convert_electrodes_to_simnibs(sim_setup, electrode_settings)

        for key, el_set in electrode_groups.items():
            if not el_set:
                logging.warning(f"Electrode setup is empty for '{key}'. Skipping....")
                continue

            setup_list = sim_struct.TDCSLIST()
            sim_session = sim_struct.SESSION()
            setup_list.currents = []
            sim_output_name = f"{key}"

            for i, electrode in enumerate(el_set):
                setup_list.currents.append(electrode["current"])
                del electrode["current"]

                elec = setup_list.add_electrode()
                electrode["channelnr"] = i + 1

                set_object_attributes(elec, electrode)
                sim_output_name += f"-{electrode['centre']}"

            sim_path = model_path.joinpath(
                sim_settings.output_path,
                sim_output_name,
            )

            is_sim_run = is_existing_simulation(sim_path)
            if is_sim_run:
                logging.warning(f"Simulation '{sim_path}' already exists. Skipping....")
                continue

            sim_props = {
                "subpath": model_path,
                "fields": sim_settings.fields,
                "pathfem": sim_path,
                "open_in_gmsh": sim_settings.open_in_gmsh,
                "map_to_surf": sim_settings.map_to_surface,
                "map_to_fsavg": sim_settings.map_to_fsaverage,
            }

            if bool(tensor_dict):
                setup_list.anisotropy_type = tensor_dict.get("anisotropy_type")
                sim_props["fname_tensor"] = tensor_dict.get("fname_tensor")

            sim_session.add_tdcslist(setup_list)
            set_object_attributes(sim_session, sim_props)
            sim_objects.append(sim_session)

    return sim_objects


if __name__ == "__main__":
    cli_args = parse_arguments(sys.argv[1:])
    settings_json = parse_settings(pathlib.Path(cli_args.config))

    if settings_json:
        sim_settings = SimulationSettings(**settings_json["simulation"])
    else:
        logging.error("Could not parse the settings. Exiting...")
        exit(1)

    model_path = pathlib.Path(cli_args.model_path)
    sim_list = main(sim_settings, model_path)

    sim_out_path = model_path.joinpath(sim_settings.output_path)
    if not sim_out_path.exists():
        os.mkdir(sim_out_path)

    for sim in sim_list:
        run_simnibs(sim, cpus=sim_settings.cpus)
