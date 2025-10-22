"""
Function creator for MessagePack database.

Creates a complete BaseFunc instance with all functions.
"""

from .base_data import BaseData
from .base_func import BaseFunc
from .func_factory import FuncFactory


def create_base_func(data: BaseData) -> BaseFunc:
    """
    Create a complete BaseFunc instance.

    Args:
        data: MessagePack database data container

    Returns:
        Complete BaseFunc instance with all functions
    """
    factory = FuncFactory(data)

    return BaseFunc(
        # Search functions
        person_of_key=factory.create_person_of_key(),
        persons_of_name=factory.create_persons_of_name(),
        strings_of_fname=factory.create_strings_of_fname(),
        strings_of_sname=factory.create_strings_of_sname(),
        persons_of_surname=factory.create_persons_of_surname(),
        persons_of_first_name=factory.create_persons_of_first_name(),
        families_of_marriage_date=factory.create_families_of_marriage_date(),
        # Patch functions
        patch_person=factory.create_patch_person(),
        patch_ascend=factory.create_patch_ascend(),
        patch_union=factory.create_patch_union(),
        patch_family=factory.create_patch_family(),
        patch_couple=factory.create_patch_couple(),
        patch_descend=factory.create_patch_descend(),
        # String functions
        insert_string=factory.create_insert_string(),
        get_string=factory.create_get_string(),
        # Commit function
        commit_patches=factory.create_commit_patches(),
        # Existence checks
        iper_exists=factory.create_iper_exists(),
        ifam_exists=factory.create_ifam_exists(),
        # Statistics
        nb_of_real_persons=factory.create_nb_of_real_persons(),
        nb_of_persons=factory.create_nb_of_persons(),
        nb_of_families=factory.create_nb_of_families(),
        nb_of_strings=factory.create_nb_of_strings(),
    )
