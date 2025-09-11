from dataclasses import dataclass
from typing import List, Optional, Tuple
from enum import Enum, auto

from .types import Gen, Record
from .records import find_field, find_all_fields, find_field_with_value, treat_notes
from .date_parser import date_of_field


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

def find_event_witness(gen: Gen, tag: str, person_id: int, record: Record) -> List[Tuple[int, WitnessType]]:
    """Find witnesses for an event"""
    def find_witnesses(asso_records: List[Record]) -> List[Tuple[int, WitnessType]]:
        witnesses = []
        for r in asso_records:
            if find_field_with_value("TYPE", tag, r.rsons):
                witness = forward_pevent_witn(gen, person_id, r.rval.strip())
                kind = WitnessType.Witness
                if rela := find_field("RELA", r.rsons):
                    kind = witness_kind_of_rval(rela.rval)
                witnesses.append((witness, kind))
            else:
                # No type specified - treat as regular witness
                witness = forward_pevent_witn(gen, person_id, r.rval.strip())
                kind = WitnessType.Witness
                if rela := find_field("RELA", r.rsons):
                    kind = witness_kind_of_rval(rela.rval)
                witnesses.append((witness, kind))
        return witnesses

    asso_records = find_all_fields("ASSO", record.rsons)
    if not asso_records:
        return []
    return find_witnesses(asso_records)

def treat_indi_pevent(gen: Gen, person_id: int, record: Record) -> List[Event]:
    """Extract person events from GEDCOM record"""
    events = []

    # Process primary events (BIRT/BAPM/DEAT etc)
    primary_tags = {
        "BIRT": PersonEventType.Birth,
        "BAPM": PersonEventType.Baptism,
        "CHR": PersonEventType.Baptism,
        "DEAT": PersonEventType.Death,
        "BURI": PersonEventType.Burial,
        "CREM": PersonEventType.Cremation,
        # ... other mappings
    }

    for tag, event_type in primary_tags.items():
        for r in find_all_fields(tag, record.rsons):
            date = None
            if date_rec := find_field("DATE", r.rsons):
                date = date_of_field(date_rec.rval)

            place = ""
            if place_rec := find_field("PLAC", r.rsons):
                place = place_rec.rval.strip()

            note = ""
            if note_recs := find_all_fields("NOTE", r.rsons):
                note = treat_notes(gen, note_recs)

            source = ""
            if source_recs := find_all_fields("SOUR", r.rsons):
                sources = []
                for sr in source_recs:
                    src_content, _ = treat_source(gen, sr)
                    if src_content:
                        sources.append(src_content)
                source = " ".join(sources)

            witnesses = find_event_witness(gen, "INDI", person_id, r)

            if (date or place or note or source or witnesses or
                r.rval == "Y"):
                events.append(Event(
                    type=event_type,
                    date=date,
                    place=place,
                    note=note,
                    source=source,
                    witnesses=witnesses
                ))

    return events

def forward_pevent_witn(gen: Gen, person_id: int, witness_id: str) -> int:
    """Register person event witness"""
    witness_idx = per_index(gen, witness_id)
    gen.g_prelated.append((witness_idx, person_id))
    return witness_idx

def forward_fevent_witn(gen: Gen, person_id: int, witness_id: str) -> int:
    """Register family event witness"""
    witness_idx = per_index(gen, witness_id)
    gen.g_frelated.append((witness_idx, person_id))
    return witness_idx

def treat_fam_fevent(gen: Gen, family_id: int, record: Record) -> List[Event]:
    """Extract family events from GEDCOM record"""
    events = []

    # Process primary events (MARR/DIV etc)
    primary_tags = {
        "MARR": FamilyEventType.Marriage,
        "DIV": FamilyEventType.Divorce,
        "ENGA": FamilyEventType.Engage,
        "ANUL": FamilyEventType.Annulation,
        "MARB": FamilyEventType.MarriageBann,
        "MARC": FamilyEventType.MarriageContract,
        "MARL": FamilyEventType.MarriageLicense,
        "RESI": FamilyEventType.Residence,
        # ... other mappings
    }

    for tag, event_type in primary_tags.items():
        for r in find_all_fields(tag, record.rsons):
            # Check for "unmarried" in place field
            is_unmarried = False
            if place_recs := find_all_fields("PLAC", r.rsons):
                for pr in place_recs:
                    if pr.rval.lower() == "unmarried":
                        is_unmarried = True
                        break

            if is_unmarried:
                event_type = FamilyEventType.NoMarriage

            date = None
            if date_rec := find_field("DATE", r.rsons):
                date = date_of_field(date_rec.rval)

            place = ""
            if place_rec := find_field("PLAC", r.rsons):
                place = place_rec.rval.strip()
                if place.lower() == "unmarried":
                    place = ""

            note = ""
            if note_recs := find_all_fields("NOTE", r.rsons):
                note = treat_notes(gen, note_recs)

            source = ""
            if source_recs := find_all_fields("SOUR", r.rsons):
                sources = []
                for sr in source_recs:
                    src_content, _ = treat_source(gen, sr)
                    if src_content:
                        sources.append(src_content)
                source = " ".join(sources)

            witnesses = find_event_witness(gen, "FAM", family_id, r)

            if (date or place or note or source or witnesses or
                r.rval == "Y" or is_unmarried):
                events.append(Event(
                    type=event_type,
                    date=date,
                    place=place,
                    note=note,
                    source=source,
                    witnesses=witnesses
                ))

    return events
