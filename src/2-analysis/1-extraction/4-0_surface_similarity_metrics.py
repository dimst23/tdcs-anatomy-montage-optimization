import pathlib
import sys

import nibabel as nib
import numpy as np
from scipy.spatial import KDTree
from scipy.stats import wasserstein_distance


def compute_nearest_neighbor_mean_distance(surface1, surface2):
    """
    Computes the mean Euclidean distance using nearest neighbors between two 3D surfaces.

    Parameters:
    - surface1: ndarray of shape (n_points1, 3), representing points of surface 1 in 3D space.
    - surface2: ndarray of shape (n_points2, 3), representing points of surface 2 in 3D space.

    Returns:
    - mean_distance: float, the mean nearest-neighbor Euclidean distance between the points.
    """
    # Build KD-trees for fast nearest-neighbor search
    tree1 = KDTree(surface1)
    tree2 = KDTree(surface2)

    # Nearest neighbors from surface1 to surface2 and vice versa
    dists1, _ = tree1.query(surface2)
    dists2, _ = tree2.query(surface1)

    # Calculate mean of all nearest neighbor distances
    mean_distance = np.mean(np.concatenate((dists1, dists2)))
    return mean_distance


# Earth Mover's Distance (EMD)
def compute_emd(surface1, surface2):
    # Flatten the surfaces into 1D arrays
    flat_surface1 = surface1.flatten()
    flat_surface2 = surface2.flatten()

    # Compute Earth Mover's Distance using the Wasserstein distance
    emd = wasserstein_distance(flat_surface1, flat_surface2)
    return emd


if __name__ == "__main__":
    cli_args = sys.argv

    ROOT_PATH = pathlib.Path(cli_args[1])
    FILE_OUTPUT = pathlib.Path(cli_args[2])
    HEMISPHERE = cli_args[3]
    SUBJECT = cli_args[4]

    surface_fs = nib.freesurfer.io.read_geometry(
        ROOT_PATH / f"{SUBJECT}/T1w/{SUBJECT}/surf/{HEMISPHERE}.pial"
    )
    surface_sim = nib.freesurfer.io.read_geometry(
        ROOT_PATH / f"{SUBJECT}/simnibs/m2m_{SUBJECT}/surfaces/{HEMISPHERE}.pial"
    )

    pts_surface_fs = np.asarray(surface_fs[0])
    pts_surface_sim = np.asarray(surface_sim[0])

    # Compute the metrics for the given surfaces
    emd = compute_emd(pts_surface_fs, pts_surface_sim)
    kde = compute_nearest_neighbor_mean_distance(pts_surface_fs, pts_surface_sim)

    with open(FILE_OUTPUT, "a") as f:
        f.write(f"{SUBJECT};{emd:.4f};{kde:.4f}\n")
