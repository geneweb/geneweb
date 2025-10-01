from typing import List, Dict, Set, Tuple, Optional, Any
from dataclasses import dataclass
from enum import Enum, auto
import sys
from models import Gen, Record
from records import designation
from date_parser import compare_date, cdate_to_dmy_opt
from utils import print_location

class ErrorSeverity(Enum):
    WARNING = auto()
    ERROR = auto()
    CRITICAL = auto()

@dataclass
class ValidationError:
    severity: ErrorSeverity
    code: str
    message: str
    person_id: Optional[int] = None
    family_id: Optional[int] = None
    position: Optional[int] = None
    details: Optional[Dict[str, Any]] = None

class BaseValidator:
    """Base validation engine for genealogical data"""

    def __init__(self, gen: Gen, log_output=None):
        self.gen = gen
        self.log_oc = log_output or sys.stdout
        self.errors: List[ValidationError] = []
        self.warnings: List[ValidationError] = []

    def add_error(self, severity: ErrorSeverity, code: str, message: str, **kwargs):
        """Add validation error"""
        error = ValidationError(severity, code, message, **kwargs)
        if severity == ErrorSeverity.WARNING:
            self.warnings.append(error)
        else:
            self.errors.append(error)

    def print_error(self, error: ValidationError):
        """Print error message like OCaml"""
        if error.position:
            print_location(error.position)

        prefix = "Warning" if error.severity == ErrorSeverity.WARNING else "Error"
        print(f"{prefix}: {error.message}", file=self.log_oc)

        if error.details:
            for key, value in error.details.items():
                print(f"  {key}: {value}", file=self.log_oc)

def check_parents_children(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check parent-child consistency - exact OCaml logic"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if not (isinstance(person_entry, tuple) and person_entry[0] == "Right3"):
            continue

        person, ascend, union = person_entry[1]

        if ascend.get("parents") is not None:
            ifam = ascend["parents"]
            if ifam >= gen.g_fam.tlen:
                validator.add_error(
                    ErrorSeverity.ERROR, "INVALID_FAMILY_REF",
                    f"Person {designation(strings, person)} references invalid family {ifam}",
                    person_id=i, family_id=ifam
                )
                continue

            family_entry = gen.g_fam.arr[ifam]
            if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
                continue

            fam, cpl, des = family_entry[1]

            if i not in des.get("children", []):
                print(f"{designation(strings, person)} is not the child of his/her parents", file=log_oc)
                if isinstance(cpl, tuple) and len(cpl) >= 2:
                    father_entry = gen.g_per.arr[cpl[0]]
                    mother_entry = gen.g_per.arr[cpl[1]]
                    if isinstance(father_entry, tuple) and father_entry[0] == "Right3":
                        print(f"- {designation(strings, father_entry[1][0])}", file=log_oc)
                    if isinstance(mother_entry, tuple) and mother_entry[0] == "Right3":
                        print(f"- {designation(strings, mother_entry[1][0])}", file=log_oc)
                print("=> no more parents for him/her\n", file=log_oc)

                new_ascend = {**ascend, "parents": None}
                gen.g_per.arr[i] = ("Right3", (person, new_ascend, union))

        families_to_delete = []
        for ifam in union.get("family", []):
            if ifam >= gen.g_fam.tlen:
                continue

            family_entry = gen.g_fam.arr[ifam]
            if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
                continue

            fam, cpl, des = family_entry[1]

            if not isinstance(cpl, tuple) or len(cpl) < 2:
                continue

            father_idx, mother_idx = cpl[0], cpl[1]

            if i != father_idx and i != mother_idx:
                print(f"{designation(strings, person)} is spouse in this family but neither husband nor wife:", file=log_oc)

                father_entry = gen.g_per.arr[father_idx] if father_idx < gen.g_per.tlen else None
                mother_entry = gen.g_per.arr[mother_idx] if mother_idx < gen.g_per.tlen else None

                if father_entry and isinstance(father_entry, tuple) and father_entry[0] == "Right3":
                    print(f"- {designation(strings, father_entry[1][0])}", file=log_oc)
                if mother_entry and isinstance(mother_entry, tuple) and mother_entry[0] == "Right3":
                    print(f"- {designation(strings, mother_entry[1][0])}", file=log_oc)

                fixed = False
                if (father_entry and isinstance(father_entry, tuple) and father_entry[0] == "Right3"):
                    fath_person = father_entry[1][0]
                    ffn = strings[fath_person.get("first_name", 1)]
                    fsn = strings[fath_person.get("surname", 1)]

                    if ffn == "?" and fsn == "?" and mother_entry:
                        moth_person = mother_entry[1][0]
                        mfn = strings[moth_person.get("first_name", 1)]
                        msn = strings[moth_person.get("surname", 1)]

                        if mfn != "?" and msn != "?":
                            print("However, the husband is unknown, I set him as husband", file=log_oc)
                            gen.g_per.arr[father_idx] = ("Right3", (fath_person, father_entry[1][1], {"family": []}))
                            new_cpl = (i, mother_idx)
                            gen.g_fam.arr[ifam] = ("Right3", (fam, new_cpl, des))
                            fixed = True

                if not fixed and (mother_entry and isinstance(mother_entry, tuple) and mother_entry[0] == "Right3"):
                    moth_person = mother_entry[1][0]
                    mfn = strings[moth_person.get("first_name", 1)]
                    msn = strings[moth_person.get("surname", 1)]

                    if mfn == "?" and msn == "?" and father_entry:
                        fath_person = father_entry[1][0]
                        ffn = strings[fath_person.get("first_name", 1)]
                        fsn = strings[fath_person.get("surname", 1)]

                        if ffn != "?" and fsn != "?":
                            print("However, the wife is unknown, I set her as wife", file=log_oc)
                            gen.g_per.arr[mother_idx] = ("Right3", (moth_person, mother_entry[1][1], {"family": []}))
                            new_cpl = (father_idx, i)
                            gen.g_fam.arr[ifam] = ("Right3", (fam, new_cpl, des))
                            fixed = True

                if not fixed:
                    print("=> deleted this family for him/her", file=log_oc)
                    families_to_delete.append(ifam)

                print("", file=log_oc)

        if families_to_delete:
            remaining_families = [f for f in union.get("family", []) if f not in families_to_delete]
            new_union = {"family": remaining_families}
            gen.g_per.arr[i] = ("Right3", (person, ascend, new_union))

    for i in range(gen.g_fam.tlen):
        family_entry = gen.g_fam.arr[i]
        if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
            continue

        fam, cpl, des = family_entry[1]
        children_to_delete = []

        if not isinstance(cpl, tuple) or len(cpl) < 2:
            continue

        for child_idx in des.get("children", []):
            if child_idx >= gen.g_per.tlen:
                continue

            child_entry = gen.g_per.arr[child_idx]
            if not (isinstance(child_entry, tuple) and child_entry[0] == "Right3"):
                continue

            child_person, child_ascend, child_union = child_entry[1]

            if child_ascend.get("parents") != i:
                if child_ascend.get("parents") is not None:
                    print(f"Other parents for {designation(strings, child_person)}", file=log_oc)
                    if isinstance(cpl, tuple):
                        father_entry = gen.g_per.arr[cpl[0]]
                        mother_entry = gen.g_per.arr[cpl[1]]
                        if isinstance(father_entry, tuple) and father_entry[0] == "Right3":
                            print(f"- {designation(strings, father_entry[1][0])}", file=log_oc)
                        if isinstance(mother_entry, tuple) and mother_entry[0] == "Right3":
                            print(f"- {designation(strings, mother_entry[1][0])}", file=log_oc)
                    print("=> deleted in this family\n", file=log_oc)
                    children_to_delete.append(child_idx)
                else:
                    print(f"{designation(strings, child_person)} has no parents but is the child of", file=log_oc)
                    if isinstance(cpl, tuple):
                        father_entry = gen.g_per.arr[cpl[0]]
                        mother_entry = gen.g_per.arr[cpl[1]]
                        if isinstance(father_entry, tuple) and father_entry[0] == "Right3":
                            print(f"- {designation(strings, father_entry[1][0])}", file=log_oc)
                        if isinstance(mother_entry, tuple) and mother_entry[0] == "Right3":
                            print(f"- {designation(strings, mother_entry[1][0])}", file=log_oc)
                    print("=> added parents\n", file=log_oc)

                    new_ascend = {**child_ascend, "parents": i}
                    gen.g_per.arr[child_idx] = ("Right3", (child_person, new_ascend, child_union))

        if children_to_delete:
            remaining_children = [c for c in des.get("children", []) if c not in children_to_delete]
            new_des = {"children": remaining_children}
            gen.g_fam.arr[i] = ("Right3", (fam, cpl, new_des))

    return validator.errors + validator.warnings

def check_parents_sex(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check parent sex consistency - exact OCaml logic"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    for i in range(gen.g_fam.tlen):
        family_entry = gen.g_fam.arr[i]
        if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
            continue

        fam, cpl, des = family_entry[1]

        if not isinstance(cpl, tuple) or len(cpl) < 2:
            continue

        ifath, imoth = cpl[0], cpl[1]

        if ifath >= gen.g_per.tlen or imoth >= gen.g_per.tlen:
            continue

        father_entry = gen.g_per.arr[ifath]
        mother_entry = gen.g_per.arr[imoth]

        if not (isinstance(father_entry, tuple) and father_entry[0] == "Right3"):
            continue
        if not (isinstance(mother_entry, tuple) and mother_entry[0] == "Right3"):
            continue

        fath_person, fath_ascend, fath_union = father_entry[1]
        moth_person, moth_ascend, moth_union = mother_entry[1]

        relation = fam.get("relation", "Married")
        if relation in ["NoSexesCheckNotMarried", "NoSexesCheckMarried"]:
            continue

        needs_fix = False
        if fath_person.get("sex") == "Female":
            print(f"Warning - husband with female sex: {designation(strings, fath_person)}", file=log_oc)
            needs_fix = True

        if moth_person.get("sex") == "Male":
            print(f"Warning - wife with male sex: {designation(strings, moth_person)}", file=log_oc)
            needs_fix = True

        if needs_fix:
            new_fam = {**fam, "relation": "NoSexesCheckNotMarried"}
            gen.g_fam.arr[i] = ("Right3", (new_fam, cpl, des))
        else:
            if fath_person.get("sex") != "Male":
                new_fath_person = {**fath_person, "sex": "Male"}
                gen.g_per.arr[ifath] = ("Right3", (new_fath_person, fath_ascend, fath_union))

            if moth_person.get("sex") != "Female":
                new_moth_person = {**moth_person, "sex": "Female"}
                gen.g_per.arr[imoth] = ("Right3", (new_moth_person, moth_ascend, moth_union))

    return validator.errors + validator.warnings

def check_undefined(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check for undefined persons and families - exact OCaml logic"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)

    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if isinstance(person_entry, tuple) and person_entry[0] == "Left3":
            lab = person_entry[1]
            from gen_arrays import unknown_per
            p, a, u = unknown_per(i, "Neuter")
            print(f"Warning: undefined person {lab}", file=log_oc)
            gen.g_per.arr[i] = ("Right3", (p, a, u))

            validator.add_error(
                ErrorSeverity.WARNING, "UNDEFINED_PERSON",
                f"Undefined person {lab} replaced with placeholder",
                person_id=i
            )

    for i in range(gen.g_fam.tlen):
        family_entry = gen.g_fam.arr[i]
        if isinstance(family_entry, tuple) and family_entry[0] == "Left3":
            lab = family_entry[1]
            from gen_arrays import unknown_fam
            f, c, d = unknown_fam(gen, i)
            print(f"Warning: undefined family {lab}", file=log_oc)
            gen.g_fam.arr[i] = ("Right3", (f, c, d))

            validator.add_error(
                ErrorSeverity.WARNING, "UNDEFINED_FAMILY",
                f"Undefined family {lab} replaced with placeholder",
                family_id=i
            )

    return validator.errors + validator.warnings

def check_birth_death_consistency(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check birth/death date consistency"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if not (isinstance(person_entry, tuple) and person_entry[0] == "Right3"):
            continue

        person, ascend, union = person_entry[1]

        birth_date = person.get("birth")
        death_info = person.get("death")

        if not birth_date or not death_info:
            continue

        death_date = None
        if isinstance(death_info, tuple) and len(death_info) >= 2:
            death_type, death_data = death_info[0], death_info[1]
            if death_type in ["Death", "Murdered", "Killed", "Executed", "Disappeared"]:
                if isinstance(death_data, tuple) and len(death_data) >= 2:
                    death_date = death_data[1]  # (cause, date)
                else:
                    death_date = death_data
        elif isinstance(death_info, str) and death_info not in ["NotDead", "DeadDontKnowWhen", "DontKnowIfDead"]:
            death_date = death_info

        if birth_date and death_date:
            birth_dmy = cdate_to_dmy_opt(birth_date)
            death_dmy = cdate_to_dmy_opt(death_date)

            if birth_dmy and death_dmy:
                if compare_date(birth_dmy, death_dmy) > 0:
                    validator.add_error(
                        ErrorSeverity.WARNING, "BIRTH_AFTER_DEATH",
                        f"Person {designation(strings, person)} has birth date after death date",
                        person_id=i,
                        details={
                            "birth_date": str(birth_dmy),
                            "death_date": str(death_dmy)
                        }
                    )

    return validator.errors + validator.warnings

def check_marriage_consistency(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check marriage date consistency"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    for i in range(gen.g_fam.tlen):
        family_entry = gen.g_fam.arr[i]
        if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
            continue

        fam, cpl, des = family_entry[1]

        marriage_date = fam.get("marriage")
        if not marriage_date:
            continue

        if not isinstance(cpl, tuple) or len(cpl) < 2:
            continue

        ifath, imoth = cpl[0], cpl[1]

        for spouse_idx in [ifath, imoth]:
            if spouse_idx >= gen.g_per.tlen:
                continue

            spouse_entry = gen.g_per.arr[spouse_idx]
            if not (isinstance(spouse_entry, tuple) and spouse_entry[0] == "Right3"):
                continue

            spouse_person, _, _ = spouse_entry[1]
            birth_date = spouse_person.get("birth")

            if birth_date:
                birth_dmy = cdate_to_dmy_opt(birth_date)
                marriage_dmy = cdate_to_dmy_opt(marriage_date)

                if birth_dmy and marriage_dmy:
                    if compare_date(birth_dmy, marriage_dmy) > 0:
                        validator.add_error(
                            ErrorSeverity.WARNING, "MARRIAGE_BEFORE_BIRTH",
                            f"Person {designation(strings, spouse_person)} married before birth",
                            person_id=spouse_idx, family_id=i,
                            details={
                                "birth_date": str(birth_dmy),
                                "marriage_date": str(marriage_dmy)
                            }
                        )

        for child_idx in des.get("children", []):
            if child_idx >= gen.g_per.tlen:
                continue

            child_entry = gen.g_per.arr[child_idx]
            if not (isinstance(child_entry, tuple) and child_entry[0] == "Right3"):
                continue

            child_person, _, _ = child_entry[1]
            birth_date = child_person.get("birth")

            if birth_date:
                birth_dmy = cdate_to_dmy_opt(birth_date)
                marriage_dmy = cdate_to_dmy_opt(marriage_date)

                if birth_dmy and marriage_dmy:
                    if compare_date(marriage_dmy, birth_dmy) > 0:
                        validator.add_error(
                            ErrorSeverity.WARNING, "CHILD_BORN_BEFORE_MARRIAGE",
                            f"Child {designation(strings, child_person)} born before parents' marriage",
                            person_id=child_idx, family_id=i,
                            details={
                                "child_birth_date": str(birth_dmy),
                                "marriage_date": str(marriage_dmy)
                            }
                        )

    return validator.errors + validator.warnings

def check_age_consistency(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check age-related consistency issues"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    for i in range(gen.g_fam.tlen):
        family_entry = gen.g_fam.arr[i]
        if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
            continue

        fam, cpl, des = family_entry[1]

        if not isinstance(cpl, tuple) or len(cpl) < 2:
            continue

        ifath, imoth = cpl[0], cpl[1]

        parent_births = {}
        for spouse_idx, role in [(ifath, "father"), (imoth, "mother")]:
            if spouse_idx >= gen.g_per.tlen:
                continue

            spouse_entry = gen.g_per.arr[spouse_idx]
            if isinstance(spouse_entry, tuple) and spouse_entry[0] == "Right3":
                spouse_person, _, _ = spouse_entry[1]
                birth_date = spouse_person.get("birth")
                if birth_date:
                    parent_births[role] = (spouse_idx, cdate_to_dmy_opt(birth_date), spouse_person)

        for child_idx in des.get("children", []):
            if child_idx >= gen.g_per.tlen:
                continue

            child_entry = gen.g_per.arr[child_idx]
            if not (isinstance(child_entry, tuple) and child_entry[0] == "Right3"):
                continue

            child_person, _, _ = child_entry[1]
            child_birth = child_person.get("birth")

            if not child_birth:
                continue

            child_birth_dmy = cdate_to_dmy_opt(child_birth)
            if not child_birth_dmy:
                continue

            for role, (parent_idx, parent_birth_dmy, parent_person) in parent_births.items():
                if not parent_birth_dmy:
                    continue

                age_diff = child_birth_dmy.year - parent_birth_dmy.year

                if (child_birth_dmy.month and parent_birth_dmy.month and
                    child_birth_dmy.day and parent_birth_dmy.day):
                    if (child_birth_dmy.month < parent_birth_dmy.month or
                        (child_birth_dmy.month == parent_birth_dmy.month and
                         child_birth_dmy.day < parent_birth_dmy.day)):
                        age_diff -= 1

                if age_diff < 12:  # Parent too young
                    validator.add_error(
                        ErrorSeverity.WARNING, "PARENT_TOO_YOUNG",
                        f"{role.title()} {designation(strings, parent_person)} was {age_diff} when child {designation(strings, child_person)} was born",
                        person_id=parent_idx, family_id=i,
                        details={"age_at_birth": age_diff, "role": role}
                    )
                elif age_diff > 80:  # Parent too old
                    validator.add_error(
                        ErrorSeverity.WARNING, "PARENT_TOO_OLD",
                        f"{role.title()} {designation(strings, parent_person)} was {age_diff} when child {designation(strings, child_person)} was born",
                        person_id=parent_idx, family_id=i,
                        details={"age_at_birth": age_diff, "role": role}
                    )

    return validator.errors + validator.warnings

def check_duplicate_persons(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check for potential duplicate persons"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    name_groups: Dict[str, List[Tuple[int, Dict]]] = {}

    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if not (isinstance(person_entry, tuple) and person_entry[0] == "Right3"):
            continue

        person, _, _ = person_entry[1]

        first_name = strings[person.get("first_name", 0)].lower()
        surname = strings[person.get("surname", 0)].lower()

        if first_name in ["?", "x"] or surname in ["?", "x"]:
            continue

        name_key = f"{first_name}|{surname}"

        if name_key not in name_groups:
            name_groups[name_key] = []
        name_groups[name_key].append((i, person))

    for name_key, persons in name_groups.items():
        if len(persons) < 2:
            continue

        for i in range(len(persons)):
            for j in range(i + 1, len(persons)):
                idx1, person1 = persons[i]
                idx2, person2 = persons[j]

                birth1 = person1.get("birth")
                birth2 = person2.get("birth")

                dates_similar = False
                if birth1 and birth2:
                    birth1_dmy = cdate_to_dmy_opt(birth1)
                    birth2_dmy = cdate_to_dmy_opt(birth2)

                    if birth1_dmy and birth2_dmy:
                        if abs(birth1_dmy.year - birth2_dmy.year) <= 1:
                            dates_similar = True

                if dates_similar or (not birth1 and not birth2):
                    validator.add_error(
                        ErrorSeverity.WARNING, "POSSIBLE_DUPLICATE",
                        f"Possible duplicate persons: {designation(strings, person1)} and {designation(strings, person2)}",
                        details={
                            "person1_id": idx1,
                            "person2_id": idx2,
                            "name": name_key.replace("|", " ")
                        }
                    )

    return validator.errors + validator.warnings

def check_circular_ancestry(gen: Gen, log_oc=None) -> List[ValidationError]:
    """Check for circular references in ancestry"""
    if not log_oc:
        log_oc = sys.stdout

    validator = BaseValidator(gen, log_oc)
    strings = gen.g_str.arr

    def detect_cycle(person_idx: int, visited: Set[int], path: List[int]) -> Optional[List[int]]:
        """Detect cycle in ancestry starting from person_idx"""
        if person_idx in visited:
            cycle_start = path.index(person_idx)
            return path[cycle_start:] + [person_idx]

        if person_idx >= gen.g_per.tlen:
            return None

        person_entry = gen.g_per.arr[person_idx]
        if not (isinstance(person_entry, tuple) and person_entry[0] == "Right3"):
            return None

        _, ascend, _ = person_entry[1]
        parent_family = ascend.get("parents")

        if parent_family is None or parent_family >= gen.g_fam.tlen:
            return None

        family_entry = gen.g_fam.arr[parent_family]
        if not (isinstance(family_entry, tuple) and family_entry[0] == "Right3"):
            return None

        _, cpl, _ = family_entry[1]

        if not isinstance(cpl, tuple) or len(cpl) < 2:
            return None

        visited.add(person_idx)
        path.append(person_idx)

        for parent_idx in [cpl[0], cpl[1]]:
            if parent_idx is not None:
                cycle = detect_cycle(parent_idx, visited.copy(), path.copy())
                if cycle:
                    return cycle

        return None

    checked = set()

    for i in range(gen.g_per.tlen):
        if i in checked:
            continue

        cycle = detect_cycle(i, set(), [])
        if cycle:
            for person_idx in cycle:
                checked.add(person_idx)

            cycle_names = []
            for person_idx in cycle[:-1]:  # Exclude duplicate at end
                if person_idx < gen.g_per.tlen:
                    person_entry = gen.g_per.arr[person_idx]
                    if isinstance(person_entry, tuple) and person_entry[0] == "Right3":
                        person, _, _ = person_entry[1]
                        cycle_names.append(designation(strings, person))

            validator.add_error(
                ErrorSeverity.ERROR, "CIRCULAR_ANCESTRY",
                f"Circular ancestry detected: {' -> '.join(cycle_names)} -> {cycle_names[0] if cycle_names else '?'}",
                details={"cycle_person_ids": cycle[:-1]}
            )

    return validator.errors + validator.warnings

def run_all_checks(gen: Gen, log_oc=None) -> Tuple[List[ValidationError], List[ValidationError]]:
    """Run all validation checks - main entry point like OCaml Check.check_base"""
    if not log_oc:
        log_oc = sys.stdout

    all_errors = []
    all_warnings = []

    print("*** Checking undefined references", file=log_oc)
    errors = check_undefined(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking parent-child relationships", file=log_oc)
    errors = check_parents_children(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking parent sex consistency", file=log_oc)
    errors = check_parents_sex(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking birth/death consistency", file=log_oc)
    errors = check_birth_death_consistency(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking marriage consistency", file=log_oc)
    errors = check_marriage_consistency(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking age consistency", file=log_oc)
    errors = check_age_consistency(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking for circular ancestry", file=log_oc)
    errors = check_circular_ancestry(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    print("*** Checking for duplicate persons", file=log_oc)
    errors = check_duplicate_persons(gen, log_oc)
    all_errors.extend([e for e in errors if e.severity != ErrorSeverity.WARNING])
    all_warnings.extend([e for e in errors if e.severity == ErrorSeverity.WARNING])

    if all_errors:
        print(f"\nFound {len(all_errors)} errors:", file=log_oc)
        for error in all_errors[:10]:  # Show first 10
            validator = BaseValidator(gen, log_oc)
            validator.print_error(error)

    if all_warnings:
        print(f"\nFound {len(all_warnings)} warnings:", file=log_oc)
        for warning in all_warnings[:10]:  # Show first 10
            validator = BaseValidator(gen, log_oc)
            validator.print_error(warning)

    if not all_errors and not all_warnings:
        print("*** No consistency issues found", file=log_oc)

    return all_errors, all_warnings


def print_base_error(log_oc, base, error: ValidationError):
    """Print base error in OCaml-compatible format"""
    print(f"Error: {error.message}", file=log_oc)

def print_base_warning(log_oc, base, warning: ValidationError):
    """Print base warning in OCaml-compatible format"""
    print(f"Warning: {warning.message}", file=log_oc)

class CheckResult:
    """OCaml-compatible check result"""
    def __init__(self, errors: List[ValidationError], warnings: List[ValidationError]):
        self.errors = errors
        self.warnings = warnings

def check_base(gen: Gen, error_callback=None, warning_callback=None, progress_callback=None) -> CheckResult:
    """Main check function compatible with OCaml Check.check_base"""
    errors, warnings = run_all_checks(gen)

    if error_callback:
        for error in errors:
            error_callback(error)

    if warning_callback:
        for warning in warnings:
            warning_callback(warning)

    return CheckResult(errors, warnings)
