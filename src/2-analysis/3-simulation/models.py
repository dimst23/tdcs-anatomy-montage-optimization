import enum
import os
from pathlib import Path
from typing import Optional, Union

from pydantic import BaseModel, Field


class EnumSimulationTypes(str, enum.Enum):
    single = "single"


class EnumElectrodeShapes(str, enum.Enum):
    ellipse = "ellipse"
    rect = "rect"


class TensorType(str, enum.Enum):
    vn = "volume_normalized"
    mc = "mean_conductivity"
    dir = "direct_mapping"
    scalar = "isotropic"


class ElectrodeDefinition(BaseModel):
    centre: str = Field(alias="name")
    type: Union[list[str], str]
    current: float
    y_direction: Optional[str] = None


class ElectrodeSettings(BaseModel):
    type: str
    shape: EnumElectrodeShapes
    dimensions: list[float]
    thickness: list[float]


class TensorSettings(BaseModel):
    tensor_path: Path
    tensor_type: TensorType


class SimulationSettings(BaseModel):
    sim_type: EnumSimulationTypes
    cpus: int = Field(gt=0, le=os.cpu_count(), default=os.cpu_count())
    fields: str
    output_path: Path
    open_in_gmsh: bool = False
    map_to_surface: bool
    map_to_fsaverage: bool
    tensor: Optional[TensorSettings] = None
    electrodes: list[list[ElectrodeDefinition]]
    electrode_settings: list[ElectrodeSettings]
