"""
Relationship checker module for GeneWeb data.

This module checks parent-child relationships and detects inconsistencies.
"""


class RelationshipChecker:
    """Checker for parent-child relationships in GeneWeb data."""

    def __init__(self, logger, options=None):
        """Initialize relationship checker with logger and options."""
        self.logger = logger
        self.options = options

    def check_parents_children(self, geneweb_data) -> None:
        """Check parent-child relationships like OCaml check_parents_children."""
        for family_id, couple in geneweb_data.couples.items():
            if family_id not in geneweb_data.descends:
                continue

            descend = geneweb_data.descends[family_id]
            husband_id = couple.father
            wife_id = couple.mother

            for child_id in descend.children:
                if child_id not in geneweb_data.persons:
                    continue

                child = geneweb_data.persons[child_id]

                if child_id in geneweb_data.ascends:
                    ascend = geneweb_data.ascends[child_id]
                    if ascend.parents and ascend.parents != family_id:
                        child_name = self.designation(geneweb_data, child)
                        husband_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[husband_id]
                            )
                            if husband_id in geneweb_data.persons
                            else "?.? ?"
                        )
                        wife_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[wife_id]
                            )
                            if wife_id in geneweb_data.persons
                            else "?.? ?"
                        )

                        self.logger.info(f"Other parents for {child_name}")
                        self.logger.info(f"- {husband_name}")
                        self.logger.info(f"- {wife_name}")
                        self.logger.info("=> deleted in this family")
                        self.logger.info("")
                    elif not ascend.parents:
                        child_name = self.designation(geneweb_data, child)
                        husband_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[husband_id]
                            )
                            if husband_id in geneweb_data.persons
                            else "?.? ?"
                        )
                        wife_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[wife_id]
                            )
                            if wife_id in geneweb_data.persons
                            else "?.? ?"
                        )

                        self.logger.info(
                            f"{child_name} has no parents but is the child of"
                        )
                        self.logger.info(f"- {husband_name}")
                        self.logger.info(f"- {wife_name}")
                        self.logger.info("")

                if hasattr(child, "additional_parents") and child.additional_parents:
                    if family_id in child.additional_parents:
                        child_name = self.designation(geneweb_data, child)
                        husband_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[husband_id]
                            )
                            if husband_id in geneweb_data.persons
                            else "?.? ?"
                        )
                        wife_name = (
                            self.designation(
                                geneweb_data, geneweb_data.persons[wife_id]
                            )
                            if wife_id in geneweb_data.persons
                            else "?.? ?"
                        )

                        self.logger.info(f"Other parents for {child_name}")
                        self.logger.info(f"- {husband_name}")
                        self.logger.info(f"- {wife_name}")
                        self.logger.info("=> deleted in this family")
                        self.logger.info("")

    def designation(self, geneweb_data, person) -> str:
        first_name = geneweb_data.strings.get(person.first_name, "?")
        surname = geneweb_data.strings.get(person.surname, "?")
        occ = person.occ if hasattr(person, "occ") else 0
        return f"{first_name}.{occ} {surname}"
