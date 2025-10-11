from typing import List, Dict
from pathlib import Path

from lib.old_db.unmarshall.v2.dbdisk import BaseVersion, RecordAccess
from .base_by_name import BaseByName, setup_decorator


def BySurname(
    version: BaseVersion,
    bname: Path,
    strings: RecordAccess[str],
    inv_idx: Dict[str, List[int]],
) -> "BaseByName":
    """Factory function to create BySurname instance"""
    match version:
        case BaseVersion.GnWb0024 | BaseVersion.GnWb0023:
            return BySurnameV23(bname, strings, inv_idx)
        case _:
            raise NotImplementedError(
                f"strings.BySurname not implemented for version {version}"
            )


class BySurnameV23(BaseByName):
    """Latest version of strings.BySurname"""

    @setup_decorator(
        offset_acc=1,
        offset_inx=0,
        split=lambda s: s.split("-"),
        get=lambda p: p.surname,
    )
    def __call__(self, surname: str) -> List[int]: ...


class BySurnameV20(BaseByName):
    """Old version of strings.BySurname for backward compatibility"""

    def __call__(self, surname: str) -> List[int]:
        raise NotImplementedError("BySurnameV20 is not implemented yet")
