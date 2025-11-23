import json
import pathlib
import typing
import warnings


def parse_settings(
    path: pathlib.Path,
) -> typing.Union[typing.Mapping[str, typing.Any], None]:
    if path.is_file():
        with open(path, "r") as f:
            return json.loads(f.read())

    return None


def set_object_attributes(
    obj: typing.Any, properties: dict[str, typing.Any]
) -> typing.Any:
    for key, value in properties.items():
        if hasattr(obj, key):
            setattr(obj, key, value)
        else:
            warnings.warn(f'The "{key}" attribute is not defined for the object.')
            return None

    return obj
