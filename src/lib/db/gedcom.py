"""
Parser GEDCOM complet en Python
Supporte GEDCOM 5.5 et 5.5.1
"""

from dataclasses import dataclass, field
from typing import List, Dict, Optional, Any
from datetime import datetime
import re


@dataclass
class GedcomLine:
    """Représente une ligne GEDCOM"""

    level: int
    xref_id: Optional[str]
    tag: str
    value: str
    children: List["GedcomLine"] = field(default_factory=list)


@dataclass
class Individual:
    """Représente un individu"""

    xref: str
    name: str = ""
    given_name: str = ""
    surname: str = ""
    sex: str = ""
    birth_date: str = ""
    birth_place: str = ""
    death_date: str = ""
    death_place: str = ""
    occupation: str = ""
    notes: List[str] = field(default_factory=list)
    families_spouse: List[str] = field(default_factory=list)
    families_child: List[str] = field(default_factory=list)
    events: List[Dict[str, str]] = field(default_factory=list)
    attributes: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Family:
    """Représente une famille"""

    xref: str
    husband: Optional[str] = None
    wife: Optional[str] = None
    children: List[str] = field(default_factory=list)
    marriage_date: str = ""
    marriage_place: str = ""
    divorce_date: str = ""
    events: List[Dict[str, str]] = field(default_factory=list)
    notes: List[str] = field(default_factory=list)


@dataclass
class Source:
    """Représente une source"""

    xref: str
    title: str = ""
    author: str = ""
    publication: str = ""
    repository: str = ""
    notes: List[str] = field(default_factory=list)


class GedcomParser:
    """Parser GEDCOM complet"""

    def __init__(self):
        self.individuals: Dict[str, Individual] = {}
        self.families: Dict[str, Family] = {}
        self.sources: Dict[str, Source] = {}
        self.header: Dict[str, Any] = {}
        self.submitter: Dict[str, str] = {}
        self.raw_data: List[GedcomLine] = []

    def parse_file(self, filepath: str, encoding: str = "utf-8") -> None:
        """Parse un fichier GEDCOM"""
        with open(filepath, "r", encoding=encoding, errors="ignore") as f:
            content = f.read()
        self.parse_string(content)

    def parse_string(self, content: str) -> None:
        """Parse une chaîne GEDCOM"""
        lines = content.split("\n")
        self.raw_data = self._parse_lines(lines)
        self._process_records()

    def _parse_lines(self, lines: List[str]) -> List[GedcomLine]:
        """Parse les lignes en structure hiérarchique"""
        stack: List[GedcomLine] = []
        root_nodes: List[GedcomLine] = []

        for line in lines:
            line = line.strip()
            if not line:
                continue

            gedcom_line = self._parse_line(line)
            if gedcom_line is None:
                continue

            # Gestion de la hiérarchie
            while stack and stack[-1].level >= gedcom_line.level:
                stack.pop()

            if stack:
                stack[-1].children.append(gedcom_line)
            else:
                root_nodes.append(gedcom_line)

            stack.append(gedcom_line)

        return root_nodes

    def _parse_line(self, line: str) -> Optional[GedcomLine]:
        """Parse une ligne GEDCOM individuelle"""
        # Format: LEVEL [XREF] TAG [VALUE]
        pattern = r"^(\d+)\s+(@\w+@\s+)?(\w+)(\s+(.*))?$"
        match = re.match(pattern, line)

        if not match:
            return None

        level = int(match.group(1))
        xref_id = match.group(2).strip() if match.group(2) else None
        tag = match.group(3)
        value = match.group(5) if match.group(5) else ""

        return GedcomLine(level, xref_id, tag, value)

    def _process_records(self) -> None:
        """Traite tous les enregistrements"""
        for node in self.raw_data:
            if node.tag == "HEAD":
                self._process_header(node)
            elif node.tag == "SUBM":
                self._process_submitter(node)
            elif node.xref_id and node.tag == "INDI":
                self._process_individual(node)
            elif node.xref_id and node.tag == "FAM":
                self._process_family(node)
            elif node.xref_id and node.tag == "SOUR":
                self._process_source(node)

    def _process_header(self, node: GedcomLine) -> None:
        """Traite l'en-tête GEDCOM"""
        for child in node.children:
            if child.tag == "SOUR":
                self.header["source"] = child.value
                for subchild in child.children:
                    if subchild.tag == "VERS":
                        self.header["source_version"] = subchild.value
                    elif subchild.tag == "NAME":
                        self.header["source_name"] = subchild.value
            elif child.tag == "DATE":
                self.header["date"] = child.value
            elif child.tag == "CHAR":
                self.header["charset"] = child.value
            elif child.tag == "GEDC":
                for subchild in child.children:
                    if subchild.tag == "VERS":
                        self.header["gedcom_version"] = subchild.value

    def _process_submitter(self, node: GedcomLine) -> None:
        """Traite les informations du soumissionnaire"""
        for child in node.children:
            if child.tag == "NAME":
                self.submitter["name"] = child.value
            elif child.tag == "ADDR":
                self.submitter["address"] = child.value

    def _process_individual(self, node: GedcomLine) -> None:
        """Traite un individu"""
        individual = Individual(xref=node.xref_id)

        for child in node.children:
            if child.tag == "NAME":
                individual.name = child.value
                # Extraire prénom et nom
                parts = child.value.split("/")
                if len(parts) >= 2:
                    individual.given_name = parts[0].strip()
                    individual.surname = parts[1].strip()
                elif len(parts) == 1:
                    individual.given_name = parts[0].strip()

                # Traiter les sous-tags NAME
                for subchild in child.children:
                    if subchild.tag == "GIVN":
                        individual.given_name = subchild.value
                    elif subchild.tag == "SURN":
                        individual.surname = subchild.value

            elif child.tag == "SEX":
                individual.sex = child.value

            elif child.tag == "BIRT":
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        individual.birth_date = subchild.value
                    elif subchild.tag == "PLAC":
                        individual.birth_place = subchild.value

            elif child.tag == "DEAT":
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        individual.death_date = subchild.value
                    elif subchild.tag == "PLAC":
                        individual.death_place = subchild.value

            elif child.tag == "OCCU":
                individual.occupation = child.value

            elif child.tag == "NOTE":
                individual.notes.append(child.value)

            elif child.tag == "FAMS":
                individual.families_spouse.append(child.value)

            elif child.tag == "FAMC":
                individual.families_child.append(child.value)

            # Autres événements (baptême, enterrement, etc.)
            elif child.tag in [
                "BAPM",
                "CHR",
                "BURI",
                "CREM",
                "ADOP",
                "CONF",
                "GRAD",
                "RESI",
            ]:
                event = {"type": child.tag}
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        event["date"] = subchild.value
                    elif subchild.tag == "PLAC":
                        event["place"] = subchild.value
                individual.events.append(event)

            # Attributs personnalisés
            else:
                individual.attributes[child.tag] = child.value

        self.individuals[individual.xref] = individual

    def _process_family(self, node: GedcomLine) -> None:
        """Traite une famille"""
        family = Family(xref=node.xref_id)

        for child in node.children:
            if child.tag == "HUSB":
                family.husband = child.value
            elif child.tag == "WIFE":
                family.wife = child.value
            elif child.tag == "CHIL":
                family.children.append(child.value)
            elif child.tag == "MARR":
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        family.marriage_date = subchild.value
                    elif subchild.tag == "PLAC":
                        family.marriage_place = subchild.value
            elif child.tag == "DIV":
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        family.divorce_date = subchild.value
            elif child.tag == "NOTE":
                family.notes.append(child.value)
            elif child.tag in ["ENGA", "MARB", "MARC"]:
                event = {"type": child.tag}
                for subchild in child.children:
                    if subchild.tag == "DATE":
                        event["date"] = subchild.value
                    elif subchild.tag == "PLAC":
                        event["place"] = subchild.value
                family.events.append(event)

        self.families[family.xref] = family

    def _process_source(self, node: GedcomLine) -> None:
        """Traite une source"""
        source = Source(xref=node.xref_id)

        for child in node.children:
            if child.tag == "TITL":
                source.title = child.value
            elif child.tag == "AUTH":
                source.author = child.value
            elif child.tag == "PUBL":
                source.publication = child.value
            elif child.tag == "REPO":
                source.repository = child.value
            elif child.tag == "NOTE":
                source.notes.append(child.value)

        self.sources[source.xref] = source

    def get_individual(self, xref: str) -> Optional[Individual]:
        """Récupère un individu par sa référence"""
        return self.individuals.get(xref)

    def get_family(self, xref: str) -> Optional[Family]:
        """Récupère une famille par sa référence"""
        return self.families.get(xref)

    def get_parents(
        self, individual_xref: str
    ) -> tuple[Optional[Individual], Optional[Individual]]:
        """Récupère les parents d'un individu (père, mère)"""
        individual = self.get_individual(individual_xref)
        if not individual or not individual.families_child:
            return None, None

        family = self.get_family(individual.families_child[0])
        if not family:
            return None, None

        father = self.get_individual(family.husband) if family.husband else None
        mother = self.get_individual(family.wife) if family.wife else None

        return father, mother

    def get_children(self, individual_xref: str) -> List[Individual]:
        """Récupère les enfants d'un individu"""
        individual = self.get_individual(individual_xref)
        if not individual:
            return []

        children = []
        for family_xref in individual.families_spouse:
            family = self.get_family(family_xref)
            if family:
                for child_xref in family.children:
                    child = self.get_individual(child_xref)
                    if child:
                        children.append(child)

        return children

    def get_spouses(self, individual_xref: str) -> List[Individual]:
        """Récupère les conjoints d'un individu"""
        individual = self.get_individual(individual_xref)
        if not individual:
            return []

        spouses = []
        for family_xref in individual.families_spouse:
            family = self.get_family(family_xref)
            if family:
                if family.husband and family.husband != individual_xref:
                    spouse = self.get_individual(family.husband)
                    if spouse:
                        spouses.append(spouse)
                if family.wife and family.wife != individual_xref:
                    spouse = self.get_individual(family.wife)
                    if spouse:
                        spouses.append(spouse)

        return spouses

    def export_summary(self) -> Dict[str, Any]:
        """Exporte un résumé des données"""
        return {
            "header": self.header,
            "submitter": self.submitter,
            "individuals_count": len(self.individuals),
            "families_count": len(self.families),
            "sources_count": len(self.sources),
        }


# Exemple d'utilisation
if __name__ == "__main__":
    # Créer un exemple de fichier GEDCOM simple
    sample_gedcom = """0 HEAD
1 SOUR Family Tree Maker
2 VERS 1.0
1 GEDC
2 VERS 5.5
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean /Dupont/
2 GIVN Jean
2 SURN Dupont
1 SEX M
1 BIRT
2 DATE 15 JAN 1950
2 PLAC Paris, France
1 OCCU Ingénieur
1 FAMS @F1@
0 @I2@ INDI
1 NAME Marie /Martin/
2 GIVN Marie
2 SURN Martin
1 SEX F
1 BIRT
2 DATE 20 MAR 1952
2 PLAC Lyon, France
1 FAMS @F1@
0 @I3@ INDI
1 NAME Pierre /Dupont/
2 GIVN Pierre
2 SURN Dupont
1 SEX M
1 BIRT
2 DATE 10 JUL 1975
2 PLAC Paris, France
1 FAMC @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
1 MARR
2 DATE 5 JUN 1974
2 PLAC Paris, France
0 TRLR"""

    # Parser le GEDCOM
    parser = GedcomParser()
    parser.parse_string(sample_gedcom)

    # Afficher le résumé
    print("=== Résumé ===")
    summary = parser.export_summary()
    print(f"Nombre d'individus: {summary['individuals_count']}")
    print(f"Nombre de familles: {summary['families_count']}")
    print()

    # Afficher tous les individus
    print("=== Individus ===")
    for xref, individual in parser.individuals.items():
        print(f"{individual.name} ({individual.sex})")
        if individual.birth_date:
            print(f"  Né(e): {individual.birth_date} à {individual.birth_place}")
        if individual.occupation:
            print(f"  Profession: {individual.occupation}")
        print()

    # Exemple de navigation dans l'arbre
    print("=== Relations familiales ===")
    if "@I3@" in parser.individuals:
        pierre = parser.get_individual("@I3@")
        print(f"Enfant: {pierre.name}")

        father, mother = parser.get_parents("@I3@")
        if father:
            print(f"  Père: {father.name}")
        if mother:
            print(f"  Mère: {mother.name}")
