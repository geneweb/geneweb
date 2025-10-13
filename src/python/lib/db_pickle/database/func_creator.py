"""
Function creator for pickle database.

Creates a complete PickleBaseFunc instance with all functions.
"""

from .base_data import PickleBaseData
from .base_func import PickleBaseFunc
from .func_factory import PickleFuncFactory


def create_pickle_base_func(data: PickleBaseData) -> PickleBaseFunc:
    """
    Create a complete PickleBaseFunc instance.

    Args:
        data: Pickle database data container

    Returns:
        Complete PickleBaseFunc instance with all functions
    """
    factory = PickleFuncFactory(data)

    return PickleBaseFunc(
        # Search functions
        person_of_key=factory.create_person_of_key(),
        persons_of_name=factory.create_persons_of_name(),
        strings_of_fname=factory.create_strings_of_fname(),
        strings_of_sname=factory.create_strings_of_sname(),
        persons_of_surname=factory.create_persons_of_surname(),
        persons_of_first_name=factory.create_persons_of_first_name(),
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
