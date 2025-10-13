from pathlib import Path
from typing import Dict, List

from lib.old_db.unmarshall.v2.dbdisk import BaseVersion, RecordAccess

from .base_by_name import BaseByName, setup_decorator


def ByFirstname(
    version: BaseVersion,
    bname: Path,
    strings: RecordAccess[str],
    inv_idx: Dict[str, List[int]],
) -> "BaseByName":
    """Factory function to create ByFirstname instance"""
    match version:
        case BaseVersion.GnWb0024 | BaseVersion.GnWb0023:
            return ByFirstNameV23(bname, strings, inv_idx)
        case _:
            raise NotImplementedError(
                f"strings.ByFirstName not implemented for version {version}"
            )


class ByFirstNameV23(BaseByName):
    """Latest version of strings.ByFirstName"""

    @setup_decorator(
        offset_acc=1,
        offset_inx=0,
        split=lambda s: s.split("-"),
        get=lambda p: p.firstname,
    )
    def __call__(self, firstname: str) -> List[int]: ...


class ByFirstNameV20(BaseByName):
    """Old version of strings.ByFirstname for backward compatibility"""

    def __call__(self, surname: str) -> List[int]:
        raise NotImplementedError("ByFirstNameV20 is not implemented yet")
