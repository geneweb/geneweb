from dataclasses import dataclass
from typing import List, Optional, Tuple, Any
from enum import Enum, auto
from models import Gen, Record
from records import find_field, find_all_fields, treat_notes, source, find_field_with_value
from date_parser import date_of_field, cdate_of_od
from gen_arrays import per_index

class PersonEventType(Enum):
    Birth = auto()
    Baptism = auto()
    Death = auto()
    Burial = auto()
    Cremation = auto()
    Accomplishment = auto()
    Acquisition = auto()
    BaptismLDS = auto()
    BarMitzvah = auto()
    BatMitzvah = auto()
    Benediction = auto()
    Recensement = auto()
    Confirmation = auto()
    ConfirmationLDS = auto()
    Decoration = auto()
    Education = auto()
    Emigration = auto()
    Immigration = auto()
    FirstCommunion = auto()
    Funeral = auto()
    Graduate = auto()
    Occupation = auto()
    Ordination = auto()
    Naturalisation = auto()
    Residence = auto()
    Retired = auto()
    Will = auto()
    Custom = auto()

class FamilyEventType(Enum):
    Marriage = auto()
    NoMarriage = auto()
    NoMention = auto()
    Engage = auto()
    Divorce = auto()
    Separated = auto()
    Annulation = auto()
    MarriageBann = auto()
    MarriageContract = auto()
    MarriageLicense = auto()
    PACS = auto()
    Residence = auto()
    Custom = auto()

class WitnessType(Enum):
    Witness = auto()
    GodParent = auto()
    CivilOfficer = auto()
    ReligiousOfficer = auto()
    Informant = auto()
    Attending = auto()
    Mentioned = auto()
    Other = auto()

@dataclass
class Event:
    type: PersonEventType | FamilyEventType
    date: Optional[str] = None
    place: str = ""
    note: str = ""
    source: str = ""
    witnesses: List[Tuple[int, WitnessType]] = None
    custom_name: str = ""

    def __post_init__(self):
        if self.witnesses is None:
            self.witnesses = []

def witness_kind_of_rval(rval: str) -> WitnessType:
    """Map GEDCOM witness type to internal enum"""
    mapping = {
        "GODP": WitnessType.GodParent,
        "officer": WitnessType.CivilOfficer,
        "Civil officer": WitnessType.CivilOfficer,
        "Registry officer": WitnessType.CivilOfficer,
        "Religious officer": WitnessType.ReligiousOfficer,
        "Officiating priest": WitnessType.ReligiousOfficer,
        "Informant": WitnessType.Informant,
        "Attending": WitnessType.Attending,
        "Mentioned": WitnessType.Mentioned,
        "Other": WitnessType.Other
    }
    return mapping.get(rval, WitnessType.Witness)

def forward_pevent_witn(gen: Gen, ip: int, rval: str) -> int:
    """Forward person event witness - placeholder"""
    return per_index(gen, rval)

def find_event_witness(gen: Gen, tag: str, person_id: int, record: Record) -> List[Tuple[int, WitnessType]]:
    """Find witnesses for an event"""
    witnesses = []

    for r in find_all_fields("ASSO", record.rsons):
        has_matching_type = find_field_with_value("TYPE", tag, r.rsons)

        if has_matching_type or not find_field("TYPE", r.rsons):
            witness_id = r.rval.strip()
            if not witness_id:
                continue

            witness_idx = forward_pevent_witn(gen, person_id, witness_id)

            kind = WitnessType.Witness  # default
            if rela_rec := find_field("RELA", r.rsons):
                kind = witness_kind_of_rval(rela_rec.rval)

            witnesses.append((witness_idx, kind))
        else:
            r.rused = False

    return witnesses

def treat_indi_pevent(gen: Gen, person_id: int, record: Record) -> List:
    """Process individual events - placeholder"""
    return []

def treat_fam_fevent(gen: Gen, family_id: int, record: Record) -> List:
    """Process family events - placeholder"""
    return []

def reconstitute_from_pevents(pevents: List[Event], birth_info: Tuple, baptism_info: Tuple,
                             death_info: Tuple, burial_info: Tuple) -> Tuple:
    """Reconstitute main events from event list - like OCaml"""
    found_birth = False
    found_baptism = False
    found_death = False
    found_burial = False

    bi, bp, de, bu = birth_info, baptism_info, death_info, burial_info

    for evt in pevents:
        if evt.type == PersonEventType.Birth and not found_birth:
            bi = (evt.date, evt.place, evt.note, evt.source)
            found_birth = True
        elif evt.type == PersonEventType.Baptism and not found_baptism:
            bp = (evt.date, evt.place, evt.note, evt.source)
            found_baptism = True
        elif evt.type == PersonEventType.Death and not found_death:
            if evt.date:
                death = ("Death", ("Unspecified", evt.date))
            else:
                death = ("DeadDontKnowWhen", None)
            de = (death, evt.place, evt.note, evt.source)
            found_death = True
        elif evt.type == PersonEventType.Burial and not found_burial:
            burial = ("Buried", evt.date)
            bu = (burial, evt.place, evt.note, evt.source)
            found_burial = True
        elif evt.type == PersonEventType.Cremation and not found_burial:
            cremation = ("Cremated", evt.date)
            bu = (cremation, evt.place, evt.note, evt.source)
            found_burial = True

    return bi, bp, de, bu

def reconstitute_from_fevents(gen: Gen, gay: bool, fevents: List[Event],
                             marriage_info: Tuple, witnesses: List[int],
                             divorce_info: Any) -> Tuple:
    """Reconstitute main family events from event list - like OCaml"""
    found_marriage = False
    found_divorce = False
    found_separation = False

    marr, witn, div = marriage_info, witnesses, divorce_info

    for evt in reversed(fevents):
        if evt.type == FamilyEventType.Engage and not found_marriage:
            witnesses_ids = [w[0] for w in evt.witnesses] if evt.witnesses else []
            marr = ("Engaged", evt.date, evt.place, evt.note, evt.source)
            witn = witnesses_ids
            found_marriage = True

        elif evt.type == FamilyEventType.Marriage:
            witnesses_ids = [w[0] for w in evt.witnesses] if evt.witnesses else []
            marr = ("Married", evt.date, evt.place, evt.note, evt.source)
            witn = witnesses_ids
            found_marriage = True

        elif (evt.type == FamilyEventType.MarriageContract and not found_marriage):
            witnesses_ids = [w[0] for w in evt.witnesses] if evt.witnesses else []
            date = evt.date
            if date and date != "None":
                pass
            marr = ("Married", date, "", evt.note, evt.source)
            witn = witnesses_ids
            found_marriage = True

        elif (evt.type in [FamilyEventType.NoMention, FamilyEventType.MarriageBann,
                          FamilyEventType.MarriageLicense, FamilyEventType.Annulation,
                          FamilyEventType.PACS] and not found_marriage):
            witnesses_ids = [w[0] for w in evt.witnesses] if evt.witnesses else []
            marr = ("NoMention", evt.date, evt.place, evt.note, evt.source)
            witn = witnesses_ids
            found_marriage = True

        elif evt.type == FamilyEventType.NoMarriage and not found_marriage:
            witnesses_ids = [w[0] for w in evt.witnesses] if evt.witnesses else []
            marr = ("NotMarried", evt.date, evt.place, evt.note, evt.source)
            witn = witnesses_ids
            found_marriage = True

        elif evt.type == FamilyEventType.Divorce and not found_divorce:
            div = ("Divorced", evt.date)
            found_divorce = True

        elif evt.type == FamilyEventType.Separated and not found_separation:
            div = ("Separated", evt.date)
            found_separation = True

    if gay:
        relation, date, place, note, src = marr
        if relation in ["Married", "NoSexesCheckMarried"]:
            relation = "NoSexesCheckMarried"
        else:
            relation = "NoSexesCheckNotMarried"
        marr = (relation, date, place, note, src)

    return marr, witn, div

def sort_events(events: List[Event]) -> List[Event]:
    """Sort events by date like OCaml Event.sort_events"""
    def event_date_key(evt: Event):
        if evt.date and evt.date != "None":
            return (0, evt.date)
        else:
            return (1, "")

    return sorted(events, key=event_date_key)
