import logging
import pathlib
import sys

import nibabel.freesurfer.io as fio
import numpy as np
import simnibs.mesh_tools.mesh_io as msio
from numpy import genfromtxt
from simnibs.utils.transformations import project_points_on_surface

SURFACE_PROPS = {
    "csf": {"tag": 1003},
    "skin": {"tag": 1005},
    "skull": {"tag": 1007},
    "bone_porus": {"tag": 1008},
    "general_bone": {"tag": 1004},
}


def distance_from_surface(
    source_surface: np.ndarray,
    target_surface: np.ndarray,
) -> np.ndarray:
    """Calculates the distance of each point between source and target surfaces

    The distance is calculated using the source surface as the base surface.

    Args:
        source_surface (np.ndarray): Base surface to calculate the distance from
        target_surface (np.ndarray): Target surface to calculate the distance to

    Returns:
        np.ndarray: Distance between source and target surfaces
    """
    surface_distance = np.linalg.norm(target_surface - source_surface, axis=1)
    return surface_distance


if __name__ == "__main__":
    # /home/dimitris/software/simnibs4.1/bin/simnibs_python /home/dimitris/research/neuroscience/msc_thesis/repo/src/2-analysis/1-extraction/5-0_vertices_projection_to_cortex.py /home/dimitris/research/neuroscience/msc_thesis/nda_data/hcpa-data/Package_1228194/fmriresults01 csf lh {}
    cli_args = sys.argv
    logging.basicConfig(
        level=logging.INFO,
        format="{asctime} - [{levelname}] - {message}",
        style="{",
        datefmt="%Y-%m-%d %H:%M:%S",
    )

    ROOT_PATH = pathlib.Path(cli_args[1])

    SURFACES = str(cli_args[2]).lower()
    SUBJECT = cli_args[3]

    SURFACES = SURFACES.split(",")
    OUTPUT_PATH = ROOT_PATH / f"{SUBJECT}/simnibs/m2m_{SUBJECT}/surfaces"

    logging.info(f"Processing '{SUBJECT}'")
    mesh = msio.read(ROOT_PATH / f"{SUBJECT}/simnibs/m2m_{SUBJECT}/{SUBJECT}.msh")

    for hemisphere in ["lh", "rh"]:
        cortex_coors = fio.read_geometry(
            ROOT_PATH / f"{SUBJECT}/simnibs/m2m_{SUBJECT}/surfaces/{hemisphere}.central"
        )
        cortex_vertices = cortex_coors[0]

        for surface in SURFACES:
            logging.info(f"Calculating projection for {surface}")
            OUTPUT_FILE_NAME = f"{hemisphere}.{surface}_distance"
            projection_points = project_points_on_surface(
                mesh,
                cortex_vertices,
                surface_tags=SURFACE_PROPS[surface]["tag"],
            )

            logging.info(f"Calculating distance for {surface}")
            distance = distance_from_surface(
                cortex_vertices,  # type: ignore
                projection_points,
            )

            logging.info(f"Writing output file: {OUTPUT_FILE_NAME}\n")
            with open(OUTPUT_PATH / OUTPUT_FILE_NAME, "wb") as f:
                fio.write_morph_data(f, distance)
