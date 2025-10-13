"""
Database patching operations.

Handles all modification operations on the database including:
- Person modifications
- Family modifications
- String insertion
- Recursive deletion
"""

from typing import Optional, Set, Tuple

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion


class DatabasePatches:
    """
    Handles all database modification operations.
    """

    def __init__(self, base):
        """
        Initialize patches handler.

        Args:
            base: Database instance
        """
        self.base = base

    # ========================================================================
    # Modification Operations
    # ========================================================================

    def patch_person(self, iper: int, person: GenPerson) -> None:
        """
        Modify or add person (pending until commit).

        Args:
            iper: Person ID
            person: New person data
        """
        self.base.func.patch_person(iper, person)

        # Update name index
        s = self.base.sou(person.first_name) + " " + self.base.sou(person.surname)
        self.base.func.patch_name(s, iper)

        # Update misc names for person and related families
        self._patch_misc_names(iper, person)

        # Update misc names for family members
        union = self.base.data.unions.get(iper)
        for ifam in union.family:
            couple = self.base.data.couples.get(ifam)
            father = couple.father
            mother = couple.mother

            self._patch_misc_names(father, self.base.data.persons.get(father))
            self._patch_misc_names(mother, self.base.data.persons.get(mother))

            # Update children
            descend = self.base.data.descends.get(ifam)
            for child in descend.children:
                self._patch_misc_names(child, self.base.data.persons.get(child))

    def _patch_misc_names(self, ip: int, person: GenPerson) -> None:
        """
        Update all name variations for person.

        Args:
            ip: Person ID
            person: Person data
        """
        from ..utils.names import NameUtils

        # Get all name variations
        misc_names = NameUtils.person_misc_names(self.base, person, lambda p: p.titles)

        for name in misc_names:
            self.base.func.patch_name(name, ip)

    def patch_ascend(self, iper: int, ascend: GenAscend) -> None:
        """
        Modify person's ascendants (pending until commit).

        Args:
            iper: Person ID
            ascend: New ascendant data
        """
        self.base.func.patch_ascend(iper, ascend)

    def patch_union(self, iper: int, union: GenUnion) -> None:
        """
        Modify person's unions (pending until commit).

        Args:
            iper: Person ID
            union: New union data
        """
        self.base.func.patch_union(iper, union)

    def patch_family(self, ifam: int, family: GenFamily) -> None:
        """
        Modify or add family (pending until commit).

        Args:
            ifam: Family ID
            family: New family data
        """
        self.base.func.patch_family(ifam, family)

    def patch_couple(self, ifam: int, couple: GenCouple) -> None:
        """
        Modify family's couple (pending until commit).

        Args:
            ifam: Family ID
            couple: New couple data
        """
        self.base.func.patch_couple(ifam, couple)

    def patch_descend(self, ifam: int, descend: GenDescend) -> None:
        """
        Modify family's descendants (pending until commit).

        Args:
            ifam: Family ID
            descend: New descend data
        """
        self.base.func.patch_descend(ifam, descend)

    def insert_string(self, s: str) -> int:
        """
        Insert or get string ID.
        If string already exists, return its id.
        Modification stays blocked until call of commit_patches.

        Args:
            s: String to insert

        Returns:
            String ID (new or existing)
        """
        return self.base.func.insert_string(s)

    def commit_patches(self) -> None:
        """
        Commit all pending modifications to disk.
        Update database files to apply modifications.
        """
        self.base.func.commit_patches()

    # ========================================================================
    # Deletion Operations
    # ========================================================================

    def delete_person(self, iper: int) -> None:
        """
        Delete person (replace with empty '? ?' entry).

        Args:
            iper: Person ID to delete
        """
        empty = GenPerson(
            first_name=Istr.quest(), surname=Istr.quest(), occ=0, key_index=iper
        )
        self.patch_person(iper, empty)

    def delete_ascend(self, iper: int) -> None:
        """
        Clear person's ascendants data structure.

        Args:
            iper: Person ID
        """
        self.patch_ascend(iper, GenAscend(parents=None, consang=0.0))

    def delete_union(self, iper: int) -> None:
        """
        Clear person's union data structure.

        Args:
            iper: Person ID
        """
        self.patch_union(iper, GenUnion(family=[]))

    def delete_family(self, ifam: int) -> None:
        """
        Delete family (replace with dummy entry).

        Args:
            ifam: Family ID to delete
        """
        empty = GenFamily(fam_index=ifam)
        self.patch_family(ifam, empty)

    def delete_couple(self, ifam: int) -> None:
        """
        Clear family's couple data structure.

        Args:
            ifam: Family ID
        """
        self.patch_couple(ifam, GenCouple(father=Iper.dummy(), mother=Iper.dummy()))

    def delete_descend(self, ifam: int) -> None:
        """
        Clear family's descendants data structure.

        Args:
            ifam: Family ID
        """
        self.patch_descend(ifam, GenDescend(children=[]))

    # ========================================================================
    # Recursive Deletion
    # ========================================================================

    def delete_person_rec(self, iper: int) -> None:
        """
        Recursively delete person and related empty data.

        This function:
        - Deletes the person
        - Removes them from parent family
        - Deletes empty parent families
        - Deletes empty spouse families

        Args:
            iper: Person ID to delete recursively
        """
        self._delete_person_aux(set(), set(), iper)

    def delete_family_rec(self, ifam: int) -> None:
        """
        Recursively delete family and related empty data.

        This function:
        - Deletes the family
        - Removes family from parents' unions
        - Clears children's ascendants
        - Deletes empty related persons

        Args:
            ifam: Family ID to delete recursively
        """
        self._delete_family_aux(set(), set(), ifam)

    def _delete_person_aux(
        self, excluded_ipers: Set[int], excluded_ifams: Set[int], iper: int
    ) -> Tuple[Set[int], Set[int]]:
        """
        Internal recursive person deletion.

        Args:
            excluded_ipers: Already processed person IDs
            excluded_ifams: Already processed family IDs
            iper: Person ID to delete

        Returns:
            Updated exclusion sets
        """
        if Iper.is_dummy(iper) or iper in excluded_ipers:
            raise ValueError(
                f"gwdb.delete_person({iper},[{','.join(map(str, excluded_ipers))}])"
            )

        ascend = self.base.data.ascends.get(iper)

        ipers_to_delete = []
        ifams_to_delete = []

        # Handle parent family
        if ascend.parents is not None:
            ifam = ascend.parents

            # Delete ascendants
            self.delete_ascend(iper)

            # Remove from family's children
            descend = self.base.data.descends.get(ifam)
            children = [c for c in descend.children if c != iper]
            self.patch_descend(ifam, GenDescend(children=children))

            # Check if should delete family
            if len(children) == 1 and children[0] == iper:
                couple = self.base.data.couples.get(ifam)
                father = couple.father
                mother = couple.mother

                if self._is_empty_person(father, ifam) and self._is_empty_person(
                    mother, ifam
                ):
                    ipers_to_delete.extend([father, mother])
                    ifams_to_delete.append(ifam)

        # Handle spouse families
        union = self.base.data.unions.get(iper)
        should_delete = True

        if len(union.family) == 0:
            should_delete = True
        else:
            for ifam in union.family:
                couple = self.base.data.couples.get(ifam)
                father = couple.father
                mother = couple.mother

                # Check if iper is really in this union
                if father != iper and mother != iper:
                    # Remove stale union
                    self._remove_union(iper, ifam)
                    continue

                descend = self.base.data.descends.get(ifam)

                if len(descend.children) > 1:
                    should_delete = False
                else:
                    spouse = mother if father == iper else father

                    if spouse in excluded_ipers:
                        pass
                    elif self._is_empty_person(spouse, ifam):
                        ipers_to_delete.append(spouse)
                        ifams_to_delete.append(ifam)
                    else:
                        should_delete = False

        # Delete or mark as empty
        if should_delete:
            self.delete_person(iper)
            excluded_ipers.add(iper)
        else:
            # Mark as "? ?" but keep structure
            empty = GenPerson(
                first_name=Istr.quest(), surname=Istr.quest(), occ=0, key_index=iper
            )
            self.patch_person(iper, empty)

        # Recursively delete
        for ip in ipers_to_delete:
            excluded_ipers, excluded_ifams = self._delete_person_aux(
                excluded_ipers, excluded_ifams, ip
            )

        for ifam in ifams_to_delete:
            excluded_ipers, excluded_ifams = self._delete_family_aux(
                excluded_ipers, excluded_ifams, ifam
            )

        return excluded_ipers, excluded_ifams

    def _delete_family_aux(
        self, excluded_ipers: Set[int], excluded_ifams: Set[int], ifam: int
    ) -> Tuple[Set[int], Set[int]]:
        """
        Internal recursive family deletion.

        Args:
            excluded_ipers: Already processed person IDs
            excluded_ifams: Already processed family IDs
            ifam: Family ID to delete

        Returns:
            Updated exclusion sets
        """
        if Ifam.is_dummy(ifam) or ifam in excluded_ifams:
            raise ValueError(
                f"gwdb.delete_family({ifam},[{','.join(map(str, excluded_ifams))}])"
            )

        couple = self.base.data.couples.get(ifam)
        descend = self.base.data.descends.get(ifam)

        father = couple.father
        mother = couple.mother
        children = descend.children

        # Remove family from parents' unions
        self._remove_union(father, ifam)
        self._remove_union(mother, ifam)

        # Clear children's ascendants
        for child in children:
            self.patch_ascend(child, GenAscend(parents=None, consang=0.0))

        # Delete family structures
        self.delete_family(ifam)
        self.delete_couple(ifam)
        self.delete_descend(ifam)

        excluded_ifams.add(ifam)

        # Recursively delete empty persons
        for person_id in [father, mother] + list(children):
            if person_id not in excluded_ipers and self._is_empty_person(person_id):
                excluded_ipers, excluded_ifams = self._delete_person_aux(
                    excluded_ipers, excluded_ifams, person_id
                )

        return excluded_ipers, excluded_ifams

    def _is_empty_person(self, iper: int, exclude_ifam: Optional[int] = None) -> bool:
        """
        Check if person is effectively empty.

        Args:
            iper: Person ID
            exclude_ifam: Family ID to exclude from union check

        Returns:
            True if person has no real data
        """
        ascend = self.base.data.ascends.get(iper)
        union = self.base.data.unions.get(iper)
        person = self.base.data.persons.get(iper)

        # Check ascendants
        if ascend.parents is not None:
            return False

        # Check unions
        expected_union = [exclude_ifam] if exclude_ifam is not None else []
        if union.family != expected_union:
            return False

        # Check if person is "? ?"
        return person.first_name == Istr.quest() and person.surname == Istr.quest()

    def _remove_union(self, iper: int, ifam: int) -> None:
        """
        Remove family from person's union list.

        Args:
            iper: Person ID
            ifam: Family ID to remove
        """
        union = self.base.data.unions.get(iper)
        new_families = [f for f in union.family if f != ifam]
        self.patch_union(iper, GenUnion(family=new_families))
