(* nocamlp5 *)

module rec Api_app_piqi:
  sig
    type protobuf_int32 = int32
    type calendar =
      [
        | `gregorian
        | `julian
        | `french
        | `hebrew
      ]
    type precision =
      [
        | `sure
        | `about
        | `maybe
        | `before
        | `after
        | `oryear
        | `yearint
      ]
    type sex =
      [
        | `male
        | `female
        | `unknown
      ]
    type death_type =
      [
        | `not_dead
        | `dead
        | `dead_young
        | `dead_dont_know_when
        | `dont_know_if_dead
        | `of_course_dead
      ]
    type marriage_type =
      [
        | `married
        | `not_married
        | `engaged
        | `no_sexes_check_not_married
        | `no_mention
        | `no_sexes_check_married
      ]
    type divorce_type =
      [
        | `not_divorced
        | `divorced
        | `separated
      ]
    type relation_parent_type =
      [
        | `rpt_adoption
        | `rpt_recognition
        | `rpt_candidate_parent
        | `rpt_god_parent
        | `rpt_foster_parent
      ]
    type title_type =
      [
        | `title_main
        | `title_name
        | `title_none
      ]
    type access =
      [
        | `access_iftitles
        | `access_public
        | `access_private
      ]
    type event_name =
      [
        | `epers_birth
        | `epers_baptism
        | `epers_death
        | `epers_burial
        | `epers_cremation
        | `epers_accomplishment
        | `epers_acquisition
        | `epers_adhesion
        | `epers_baptismlds
        | `epers_barmitzvah
        | `epers_batmitzvah
        | `epers_benediction
        | `epers_changename
        | `epers_circumcision
        | `epers_confirmation
        | `epers_confirmationlds
        | `epers_decoration
        | `epers_demobilisationmilitaire
        | `epers_diploma
        | `epers_distinction
        | `epers_dotation
        | `epers_dotationlds
        | `epers_education
        | `epers_election
        | `epers_emigration
        | `epers_excommunication
        | `epers_familylinklds
        | `epers_firstcommunion
        | `epers_funeral
        | `epers_graduate
        | `epers_hospitalisation
        | `epers_illness
        | `epers_immigration
        | `epers_listepassenger
        | `epers_militarydistinction
        | `epers_militarypromotion
        | `epers_militaryservice
        | `epers_mobilisationmilitaire
        | `epers_naturalisation
        | `epers_occupation
        | `epers_ordination
        | `epers_property
        | `epers_recensement
        | `epers_residence
        | `epers_retired
        | `epers_scellentchildlds
        | `epers_scellentparentlds
        | `epers_scellentspouselds
        | `epers_ventebien
        | `epers_will
        | `efam_marriage
        | `efam_no_marriage
        | `efam_no_mention
        | `efam_engage
        | `efam_divorce
        | `efam_separated
        | `efam_annulation
        | `efam_marriage_bann
        | `efam_marriage_contract
        | `efam_marriage_license
        | `efam_pacs
        | `efam_residence
      ]
    type witness_type =
      [
        | `witness
        | `witness_godparent
        | `witness_officer
      ]
    type dmy = Dmy.t
    type date = Date.t
    type relation_parent = Relation_parent.t
    type title = Title.t
    type witness_event = Witness_event.t
    type event = Event.t
    type person = Person.t
    type family = Family.t
    type base_warnings = Base_warnings.t
    type modification_status = Modification_status.t
  end = Api_app_piqi
and Dmy:
  sig
    type t = {
      mutable day: Api_app_piqi.protobuf_int32;
      mutable month: Api_app_piqi.protobuf_int32;
      mutable year: Api_app_piqi.protobuf_int32;
      mutable delta: Api_app_piqi.protobuf_int32;
    }
  end = Dmy
and Date:
  sig
    type t = {
      mutable cal: Api_app_piqi.calendar option;
      mutable prec: Api_app_piqi.precision option;
      mutable dmy: Api_app_piqi.dmy option;
      mutable dmy2: Api_app_piqi.dmy option;
      mutable text: string option;
    }
  end = Date
and Relation_parent:
  sig
    type t = {
      mutable father: Api_app_piqi.protobuf_int32 option;
      mutable mother: Api_app_piqi.protobuf_int32 option;
      mutable source: string option;
      mutable rpt_type: Api_app_piqi.relation_parent_type;
    }
  end = Relation_parent
and Title:
  sig
    type t = {
      mutable title_type: Api_app_piqi.title_type;
      mutable name: string option;
      mutable title: string option;
      mutable fief: string option;
      mutable date_begin: Api_app_piqi.date option;
      mutable date_end: Api_app_piqi.date option;
      mutable nth: Api_app_piqi.protobuf_int32 option;
    }
  end = Title
and Witness_event:
  sig
    type t = {
      mutable witness_type: Api_app_piqi.witness_type;
      mutable witness: Api_app_piqi.protobuf_int32;
    }
  end = Witness_event
and Event:
  sig
    type t = {
      mutable name: Api_app_piqi.event_name option;
      mutable text: string option;
      mutable date: Api_app_piqi.date option;
      mutable place: string option;
      mutable reason: string option;
      mutable note: string option;
      mutable src: string option;
      mutable witnesses: Api_app_piqi.witness_event list;
      mutable index_spouse: Api_app_piqi.protobuf_int32 option;
    }
  end = Event
and Person:
  sig
    type t = {
      mutable index: Api_app_piqi.protobuf_int32;
      mutable sex: Api_app_piqi.sex;
      mutable lastname: string;
      mutable firstname: string;
      mutable occ: Api_app_piqi.protobuf_int32;
      mutable public_name: string option;
      mutable aliases: string list;
      mutable qualifiers: string list;
      mutable firstname_aliases: string list;
      mutable surname_aliases: string list;
      mutable image: bool;
      mutable birth_date: Api_app_piqi.date option;
      mutable birth_place: string option;
      mutable birth_src: string option;
      mutable baptism_date: Api_app_piqi.date option;
      mutable baptism_place: string option;
      mutable baptism_src: string option;
      mutable death_date: Api_app_piqi.date option;
      mutable death_place: string option;
      mutable death_src: string option;
      mutable death_type: Api_app_piqi.death_type;
      mutable burial_date: Api_app_piqi.date option;
      mutable burial_place: string option;
      mutable burial_src: string option;
      mutable occupation: string option;
      mutable psources: string option;
      mutable titles: Api_app_piqi.title list;
      mutable related: Api_app_piqi.protobuf_int32 list;
      mutable rparents: Api_app_piqi.relation_parent list;
      mutable access: Api_app_piqi.access;
      mutable parents: Api_app_piqi.protobuf_int32 option;
      mutable families: Api_app_piqi.protobuf_int32 list;
      mutable events: Api_app_piqi.event list;
    }
  end = Person
and Family:
  sig
    type t = {
      mutable index: Api_app_piqi.protobuf_int32;
      mutable marriage_date: Api_app_piqi.date option;
      mutable marriage_place: string option;
      mutable marriage_src: string option;
      mutable marriage_type: Api_app_piqi.marriage_type;
      mutable divorce_type: Api_app_piqi.divorce_type;
      mutable divorce_date: Api_app_piqi.date option;
      mutable witnesses: Api_app_piqi.protobuf_int32 list;
      mutable fsources: string option;
      mutable father: Api_app_piqi.protobuf_int32;
      mutable mother: Api_app_piqi.protobuf_int32;
      mutable children: Api_app_piqi.protobuf_int32 list;
    }
  end = Family
and Base_warnings:
  sig
    type t = {
      mutable already_defined: bool;
      mutable own_ancestor: bool;
      mutable bad_sex_of_married_person: bool;
      mutable birth_after_death: bool;
      mutable incoherent_sex: bool;
      mutable changed_order_of_children: bool;
      mutable children_not_in_order: bool;
      mutable dead_too_early_to_be_father: bool;
      mutable incoherent_ancestor_date: bool;
      mutable marriage_date_after_death: bool;
      mutable marriage_date_before_birth: bool;
      mutable mother_dead_before_child_birth: bool;
      mutable parent_born_after_child: bool;
      mutable parent_too_young: bool;
      mutable title_dates_error: bool;
      mutable undefined_sex: bool;
      mutable young_for_marriage: bool;
      mutable close_children: bool;
      mutable parent_too_old: bool;
      mutable changed_order_of_marriages: bool;
      mutable big_age_between_spouses: bool;
      mutable dead_old: bool;
      mutable old_individual: bool;
      mutable witness_date_after_death: bool;
      mutable witness_date_before_birth: bool;
      mutable fevent_order: bool;
      mutable fwitness_event_after_death: bool;
      mutable fwitness_event_before_birth: bool;
      mutable pevent_order: bool;
      mutable pwitness_event_after_death: bool;
      mutable pwitness_event_before_birth: bool;
    }
  end = Base_warnings
and Modification_status:
  sig
    type t = {
      mutable status: bool;
      mutable base_warnings: Api_app_piqi.base_warnings;
      mutable index: Api_app_piqi.protobuf_int32 option;
    }
  end = Modification_status


let rec parse_int32 x = Piqirun.int32_of_zigzag_varint x
and packed_parse_int32 x = Piqirun.int32_of_packed_zigzag_varint x

and parse_protobuf_int32 x = Piqirun.int32_of_signed_varint x
and packed_parse_protobuf_int32 x = Piqirun.int32_of_packed_signed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_dmy x =
  let x = Piqirun.parse_record x in
  let _day, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _month, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  let _year, x = Piqirun.parse_required_field 3 parse_protobuf_int32 x in
  let _delta, x = Piqirun.parse_required_field 4 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Dmy.day = _day;
    Dmy.month = _month;
    Dmy.year = _year;
    Dmy.delta = _delta;
  }

and parse_date x =
  let x = Piqirun.parse_record x in
  let _cal, x = Piqirun.parse_optional_field 2 parse_calendar x in
  let _prec, x = Piqirun.parse_optional_field 3 parse_precision x in
  let _dmy, x = Piqirun.parse_optional_field 4 parse_dmy x in
  let _dmy2, x = Piqirun.parse_optional_field 5 parse_dmy x in
  let _text, x = Piqirun.parse_optional_field 6 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Date.cal = _cal;
    Date.prec = _prec;
    Date.dmy = _dmy;
    Date.dmy2 = _dmy2;
    Date.text = _text;
  }

and parse_relation_parent x =
  let x = Piqirun.parse_record x in
  let _father, x = Piqirun.parse_optional_field 1 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_optional_field 2 parse_protobuf_int32 x in
  let _source, x = Piqirun.parse_optional_field 3 parse_string x in
  let _rpt_type, x = Piqirun.parse_required_field 4 parse_relation_parent_type x in
  Piqirun.check_unparsed_fields x;
  {
    Relation_parent.father = _father;
    Relation_parent.mother = _mother;
    Relation_parent.source = _source;
    Relation_parent.rpt_type = _rpt_type;
  }

and parse_title x =
  let x = Piqirun.parse_record x in
  let _title_type, x = Piqirun.parse_required_field 1 parse_title_type x in
  let _name, x = Piqirun.parse_optional_field 2 parse_string x in
  let _title, x = Piqirun.parse_optional_field 3 parse_string x in
  let _fief, x = Piqirun.parse_optional_field 4 parse_string x in
  let _date_begin, x = Piqirun.parse_optional_field 5 parse_date x in
  let _date_end, x = Piqirun.parse_optional_field 6 parse_date x in
  let _nth, x = Piqirun.parse_optional_field 7 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Title.title_type = _title_type;
    Title.name = _name;
    Title.title = _title;
    Title.fief = _fief;
    Title.date_begin = _date_begin;
    Title.date_end = _date_end;
    Title.nth = _nth;
  }

and parse_witness_event x =
  let x = Piqirun.parse_record x in
  let _witness_type, x = Piqirun.parse_required_field 1 parse_witness_type x in
  let _witness, x = Piqirun.parse_required_field 2 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Witness_event.witness_type = _witness_type;
    Witness_event.witness = _witness;
  }

and parse_event x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_optional_field 1 parse_event_name x in
  let _text, x = Piqirun.parse_optional_field 2 parse_string x in
  let _date, x = Piqirun.parse_optional_field 3 parse_date x in
  let _place, x = Piqirun.parse_optional_field 4 parse_string x in
  let _reason, x = Piqirun.parse_optional_field 5 parse_string x in
  let _note, x = Piqirun.parse_optional_field 6 parse_string x in
  let _src, x = Piqirun.parse_optional_field 7 parse_string x in
  let _witnesses, x = Piqirun.parse_repeated_field 8 parse_witness_event x in
  let _index_spouse, x = Piqirun.parse_optional_field 9 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Event.name = _name;
    Event.text = _text;
    Event.date = _date;
    Event.place = _place;
    Event.reason = _reason;
    Event.note = _note;
    Event.src = _src;
    Event.witnesses = _witnesses;
    Event.index_spouse = _index_spouse;
  }

and parse_person x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _sex, x = Piqirun.parse_required_field 2 parse_sex x in
  let _lastname, x = Piqirun.parse_required_field 3 parse_string x in
  let _firstname, x = Piqirun.parse_required_field 4 parse_string x in
  let _occ, x = Piqirun.parse_required_field 5 parse_protobuf_int32 x in
  let _public_name, x = Piqirun.parse_optional_field 6 parse_string x in
  let _aliases, x = Piqirun.parse_repeated_field 7 parse_string x in
  let _qualifiers, x = Piqirun.parse_repeated_field 8 parse_string x in
  let _firstname_aliases, x = Piqirun.parse_repeated_field 9 parse_string x in
  let _surname_aliases, x = Piqirun.parse_repeated_field 10 parse_string x in
  let _image, x = Piqirun.parse_required_field 11 parse_bool x ~default:"\b\000" in
  let _birth_date, x = Piqirun.parse_optional_field 12 parse_date x in
  let _birth_place, x = Piqirun.parse_optional_field 13 parse_string x in
  let _birth_src, x = Piqirun.parse_optional_field 14 parse_string x in
  let _baptism_date, x = Piqirun.parse_optional_field 15 parse_date x in
  let _baptism_place, x = Piqirun.parse_optional_field 16 parse_string x in
  let _baptism_src, x = Piqirun.parse_optional_field 17 parse_string x in
  let _death_date, x = Piqirun.parse_optional_field 18 parse_date x in
  let _death_place, x = Piqirun.parse_optional_field 19 parse_string x in
  let _death_src, x = Piqirun.parse_optional_field 20 parse_string x in
  let _death_type, x = Piqirun.parse_required_field 21 parse_death_type x in
  let _burial_date, x = Piqirun.parse_optional_field 22 parse_date x in
  let _burial_place, x = Piqirun.parse_optional_field 23 parse_string x in
  let _burial_src, x = Piqirun.parse_optional_field 24 parse_string x in
  let _occupation, x = Piqirun.parse_optional_field 25 parse_string x in
  let _psources, x = Piqirun.parse_optional_field 26 parse_string x in
  let _titles, x = Piqirun.parse_repeated_field 27 parse_title x in
  let _related, x = Piqirun.parse_repeated_field 28 parse_protobuf_int32 x in
  let _rparents, x = Piqirun.parse_repeated_field 29 parse_relation_parent x in
  let _access, x = Piqirun.parse_required_field 30 parse_access x ~default:"\b\000" in
  let _parents, x = Piqirun.parse_optional_field 31 parse_protobuf_int32 x in
  let _families, x = Piqirun.parse_repeated_field 32 parse_protobuf_int32 x in
  let _events, x = Piqirun.parse_repeated_field 33 parse_event x in
  Piqirun.check_unparsed_fields x;
  {
    Person.index = _index;
    Person.sex = _sex;
    Person.lastname = _lastname;
    Person.firstname = _firstname;
    Person.occ = _occ;
    Person.public_name = _public_name;
    Person.aliases = _aliases;
    Person.qualifiers = _qualifiers;
    Person.firstname_aliases = _firstname_aliases;
    Person.surname_aliases = _surname_aliases;
    Person.image = _image;
    Person.birth_date = _birth_date;
    Person.birth_place = _birth_place;
    Person.birth_src = _birth_src;
    Person.baptism_date = _baptism_date;
    Person.baptism_place = _baptism_place;
    Person.baptism_src = _baptism_src;
    Person.death_date = _death_date;
    Person.death_place = _death_place;
    Person.death_src = _death_src;
    Person.death_type = _death_type;
    Person.burial_date = _burial_date;
    Person.burial_place = _burial_place;
    Person.burial_src = _burial_src;
    Person.occupation = _occupation;
    Person.psources = _psources;
    Person.titles = _titles;
    Person.related = _related;
    Person.rparents = _rparents;
    Person.access = _access;
    Person.parents = _parents;
    Person.families = _families;
    Person.events = _events;
  }

and parse_family x =
  let x = Piqirun.parse_record x in
  let _index, x = Piqirun.parse_required_field 1 parse_protobuf_int32 x in
  let _marriage_date, x = Piqirun.parse_optional_field 2 parse_date x in
  let _marriage_place, x = Piqirun.parse_optional_field 3 parse_string x in
  let _marriage_src, x = Piqirun.parse_optional_field 4 parse_string x in
  let _marriage_type, x = Piqirun.parse_required_field 5 parse_marriage_type x in
  let _divorce_type, x = Piqirun.parse_required_field 6 parse_divorce_type x in
  let _divorce_date, x = Piqirun.parse_optional_field 7 parse_date x in
  let _witnesses, x = Piqirun.parse_repeated_field 8 parse_protobuf_int32 x in
  let _fsources, x = Piqirun.parse_optional_field 9 parse_string x in
  let _father, x = Piqirun.parse_required_field 10 parse_protobuf_int32 x in
  let _mother, x = Piqirun.parse_required_field 11 parse_protobuf_int32 x in
  let _children, x = Piqirun.parse_repeated_field 12 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Family.index = _index;
    Family.marriage_date = _marriage_date;
    Family.marriage_place = _marriage_place;
    Family.marriage_src = _marriage_src;
    Family.marriage_type = _marriage_type;
    Family.divorce_type = _divorce_type;
    Family.divorce_date = _divorce_date;
    Family.witnesses = _witnesses;
    Family.fsources = _fsources;
    Family.father = _father;
    Family.mother = _mother;
    Family.children = _children;
  }

and parse_base_warnings x =
  let x = Piqirun.parse_record x in
  let _already_defined, x = Piqirun.parse_required_field 1 parse_bool x ~default:"\b\000" in
  let _own_ancestor, x = Piqirun.parse_required_field 2 parse_bool x ~default:"\b\000" in
  let _bad_sex_of_married_person, x = Piqirun.parse_required_field 3 parse_bool x ~default:"\b\000" in
  let _birth_after_death, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _incoherent_sex, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  let _changed_order_of_children, x = Piqirun.parse_required_field 6 parse_bool x ~default:"\b\000" in
  let _children_not_in_order, x = Piqirun.parse_required_field 7 parse_bool x ~default:"\b\000" in
  let _dead_too_early_to_be_father, x = Piqirun.parse_required_field 8 parse_bool x ~default:"\b\000" in
  let _incoherent_ancestor_date, x = Piqirun.parse_required_field 9 parse_bool x ~default:"\b\000" in
  let _marriage_date_after_death, x = Piqirun.parse_required_field 10 parse_bool x ~default:"\b\000" in
  let _marriage_date_before_birth, x = Piqirun.parse_required_field 11 parse_bool x ~default:"\b\000" in
  let _mother_dead_before_child_birth, x = Piqirun.parse_required_field 12 parse_bool x ~default:"\b\000" in
  let _parent_born_after_child, x = Piqirun.parse_required_field 13 parse_bool x ~default:"\b\000" in
  let _parent_too_young, x = Piqirun.parse_required_field 14 parse_bool x ~default:"\b\000" in
  let _title_dates_error, x = Piqirun.parse_required_field 15 parse_bool x ~default:"\b\000" in
  let _undefined_sex, x = Piqirun.parse_required_field 16 parse_bool x ~default:"\b\000" in
  let _young_for_marriage, x = Piqirun.parse_required_field 17 parse_bool x ~default:"\b\000" in
  let _close_children, x = Piqirun.parse_required_field 18 parse_bool x ~default:"\b\000" in
  let _parent_too_old, x = Piqirun.parse_required_field 19 parse_bool x ~default:"\b\000" in
  let _changed_order_of_marriages, x = Piqirun.parse_required_field 20 parse_bool x ~default:"\b\000" in
  let _big_age_between_spouses, x = Piqirun.parse_required_field 21 parse_bool x ~default:"\b\000" in
  let _dead_old, x = Piqirun.parse_required_field 22 parse_bool x ~default:"\b\000" in
  let _old_individual, x = Piqirun.parse_required_field 23 parse_bool x ~default:"\b\000" in
  let _witness_date_after_death, x = Piqirun.parse_required_field 24 parse_bool x ~default:"\b\000" in
  let _witness_date_before_birth, x = Piqirun.parse_required_field 25 parse_bool x ~default:"\b\000" in
  let _fevent_order, x = Piqirun.parse_required_field 26 parse_bool x ~default:"\b\000" in
  let _fwitness_event_after_death, x = Piqirun.parse_required_field 27 parse_bool x ~default:"\b\000" in
  let _fwitness_event_before_birth, x = Piqirun.parse_required_field 28 parse_bool x ~default:"\b\000" in
  let _pevent_order, x = Piqirun.parse_required_field 29 parse_bool x ~default:"\b\000" in
  let _pwitness_event_after_death, x = Piqirun.parse_required_field 30 parse_bool x ~default:"\b\000" in
  let _pwitness_event_before_birth, x = Piqirun.parse_required_field 31 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    Base_warnings.already_defined = _already_defined;
    Base_warnings.own_ancestor = _own_ancestor;
    Base_warnings.bad_sex_of_married_person = _bad_sex_of_married_person;
    Base_warnings.birth_after_death = _birth_after_death;
    Base_warnings.incoherent_sex = _incoherent_sex;
    Base_warnings.changed_order_of_children = _changed_order_of_children;
    Base_warnings.children_not_in_order = _children_not_in_order;
    Base_warnings.dead_too_early_to_be_father = _dead_too_early_to_be_father;
    Base_warnings.incoherent_ancestor_date = _incoherent_ancestor_date;
    Base_warnings.marriage_date_after_death = _marriage_date_after_death;
    Base_warnings.marriage_date_before_birth = _marriage_date_before_birth;
    Base_warnings.mother_dead_before_child_birth = _mother_dead_before_child_birth;
    Base_warnings.parent_born_after_child = _parent_born_after_child;
    Base_warnings.parent_too_young = _parent_too_young;
    Base_warnings.title_dates_error = _title_dates_error;
    Base_warnings.undefined_sex = _undefined_sex;
    Base_warnings.young_for_marriage = _young_for_marriage;
    Base_warnings.close_children = _close_children;
    Base_warnings.parent_too_old = _parent_too_old;
    Base_warnings.changed_order_of_marriages = _changed_order_of_marriages;
    Base_warnings.big_age_between_spouses = _big_age_between_spouses;
    Base_warnings.dead_old = _dead_old;
    Base_warnings.old_individual = _old_individual;
    Base_warnings.witness_date_after_death = _witness_date_after_death;
    Base_warnings.witness_date_before_birth = _witness_date_before_birth;
    Base_warnings.fevent_order = _fevent_order;
    Base_warnings.fwitness_event_after_death = _fwitness_event_after_death;
    Base_warnings.fwitness_event_before_birth = _fwitness_event_before_birth;
    Base_warnings.pevent_order = _pevent_order;
    Base_warnings.pwitness_event_after_death = _pwitness_event_after_death;
    Base_warnings.pwitness_event_before_birth = _pwitness_event_before_birth;
  }

and parse_modification_status x =
  let x = Piqirun.parse_record x in
  let _status, x = Piqirun.parse_required_field 1 parse_bool x in
  let _base_warnings, x = Piqirun.parse_required_field 2 parse_base_warnings x in
  let _index, x = Piqirun.parse_optional_field 3 parse_protobuf_int32 x in
  Piqirun.check_unparsed_fields x;
  {
    Modification_status.status = _status;
    Modification_status.base_warnings = _base_warnings;
    Modification_status.index = _index;
  }

and parse_calendar x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x
and packed_parse_calendar x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `gregorian
    | 1l -> `julian
    | 2l -> `french
    | 3l -> `hebrew
    | x -> Piqirun.error_enum_const x

and parse_precision x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x
and packed_parse_precision x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `sure
    | 1l -> `about
    | 2l -> `maybe
    | 3l -> `before
    | 4l -> `after
    | 5l -> `oryear
    | 6l -> `yearint
    | x -> Piqirun.error_enum_const x

and parse_sex x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x
and packed_parse_sex x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `male
    | 1l -> `female
    | 2l -> `unknown
    | x -> Piqirun.error_enum_const x

and parse_death_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x
and packed_parse_death_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_dead
    | 1l -> `dead
    | 2l -> `dead_young
    | 3l -> `dead_dont_know_when
    | 4l -> `dont_know_if_dead
    | 5l -> `of_course_dead
    | x -> Piqirun.error_enum_const x

and parse_marriage_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x
and packed_parse_marriage_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `married
    | 1l -> `not_married
    | 2l -> `engaged
    | 3l -> `no_sexes_check_not_married
    | 4l -> `no_mention
    | 5l -> `no_sexes_check_married
    | x -> Piqirun.error_enum_const x

and parse_divorce_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x
and packed_parse_divorce_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `not_divorced
    | 1l -> `divorced
    | 2l -> `separated
    | x -> Piqirun.error_enum_const x

and parse_relation_parent_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x
and packed_parse_relation_parent_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `rpt_adoption
    | 1l -> `rpt_recognition
    | 2l -> `rpt_candidate_parent
    | 3l -> `rpt_god_parent
    | 4l -> `rpt_foster_parent
    | x -> Piqirun.error_enum_const x

and parse_title_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x
and packed_parse_title_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `title_main
    | 1l -> `title_name
    | 2l -> `title_none
    | x -> Piqirun.error_enum_const x

and parse_access x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `access_iftitles
    | 1l -> `access_public
    | 2l -> `access_private
    | x -> Piqirun.error_enum_const x
and packed_parse_access x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `access_iftitles
    | 1l -> `access_public
    | 2l -> `access_private
    | x -> Piqirun.error_enum_const x

and parse_event_name x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | 50l -> `efam_marriage
    | 51l -> `efam_no_marriage
    | 52l -> `efam_no_mention
    | 53l -> `efam_engage
    | 54l -> `efam_divorce
    | 55l -> `efam_separated
    | 56l -> `efam_annulation
    | 57l -> `efam_marriage_bann
    | 58l -> `efam_marriage_contract
    | 59l -> `efam_marriage_license
    | 60l -> `efam_pacs
    | 61l -> `efam_residence
    | x -> Piqirun.error_enum_const x
and packed_parse_event_name x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `epers_birth
    | 1l -> `epers_baptism
    | 2l -> `epers_death
    | 3l -> `epers_burial
    | 4l -> `epers_cremation
    | 5l -> `epers_accomplishment
    | 6l -> `epers_acquisition
    | 7l -> `epers_adhesion
    | 8l -> `epers_baptismlds
    | 9l -> `epers_barmitzvah
    | 10l -> `epers_batmitzvah
    | 11l -> `epers_benediction
    | 12l -> `epers_changename
    | 13l -> `epers_circumcision
    | 14l -> `epers_confirmation
    | 15l -> `epers_confirmationlds
    | 16l -> `epers_decoration
    | 17l -> `epers_demobilisationmilitaire
    | 18l -> `epers_diploma
    | 19l -> `epers_distinction
    | 20l -> `epers_dotation
    | 21l -> `epers_dotationlds
    | 22l -> `epers_education
    | 23l -> `epers_election
    | 24l -> `epers_emigration
    | 25l -> `epers_excommunication
    | 26l -> `epers_familylinklds
    | 27l -> `epers_firstcommunion
    | 28l -> `epers_funeral
    | 29l -> `epers_graduate
    | 30l -> `epers_hospitalisation
    | 31l -> `epers_illness
    | 32l -> `epers_immigration
    | 33l -> `epers_listepassenger
    | 34l -> `epers_militarydistinction
    | 35l -> `epers_militarypromotion
    | 36l -> `epers_militaryservice
    | 37l -> `epers_mobilisationmilitaire
    | 38l -> `epers_naturalisation
    | 39l -> `epers_occupation
    | 40l -> `epers_ordination
    | 41l -> `epers_property
    | 42l -> `epers_recensement
    | 43l -> `epers_residence
    | 44l -> `epers_retired
    | 45l -> `epers_scellentchildlds
    | 46l -> `epers_scellentparentlds
    | 47l -> `epers_scellentspouselds
    | 48l -> `epers_ventebien
    | 49l -> `epers_will
    | 50l -> `efam_marriage
    | 51l -> `efam_no_marriage
    | 52l -> `efam_no_mention
    | 53l -> `efam_engage
    | 54l -> `efam_divorce
    | 55l -> `efam_separated
    | 56l -> `efam_annulation
    | 57l -> `efam_marriage_bann
    | 58l -> `efam_marriage_contract
    | 59l -> `efam_marriage_license
    | 60l -> `efam_pacs
    | 61l -> `efam_residence
    | x -> Piqirun.error_enum_const x

and parse_witness_type x =
  match Piqirun.int32_of_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | 2l -> `witness_officer
    | x -> Piqirun.error_enum_const x
and packed_parse_witness_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 0l -> `witness
    | 1l -> `witness_godparent
    | 2l -> `witness_officer
    | x -> Piqirun.error_enum_const x


let rec gen__int32 code x = Piqirun.int32_to_zigzag_varint code x
and packed_gen__int32 x = Piqirun.int32_to_packed_zigzag_varint x

and gen__protobuf_int32 code x = Piqirun.int32_to_signed_varint code x
and packed_gen__protobuf_int32 x = Piqirun.int32_to_packed_signed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__dmy code x =
  let _day = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Dmy.day in
  let _month = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Dmy.month in
  let _year = Piqirun.gen_required_field 3 gen__protobuf_int32 x.Dmy.year in
  let _delta = Piqirun.gen_required_field 4 gen__protobuf_int32 x.Dmy.delta in
  Piqirun.gen_record code (_day :: _month :: _year :: _delta :: [])

and gen__date code x =
  let _cal = Piqirun.gen_optional_field 2 gen__calendar x.Date.cal in
  let _prec = Piqirun.gen_optional_field 3 gen__precision x.Date.prec in
  let _dmy = Piqirun.gen_optional_field 4 gen__dmy x.Date.dmy in
  let _dmy2 = Piqirun.gen_optional_field 5 gen__dmy x.Date.dmy2 in
  let _text = Piqirun.gen_optional_field 6 gen__string x.Date.text in
  Piqirun.gen_record code (_cal :: _prec :: _dmy :: _dmy2 :: _text :: [])

and gen__relation_parent code x =
  let _father = Piqirun.gen_optional_field 1 gen__protobuf_int32 x.Relation_parent.father in
  let _mother = Piqirun.gen_optional_field 2 gen__protobuf_int32 x.Relation_parent.mother in
  let _source = Piqirun.gen_optional_field 3 gen__string x.Relation_parent.source in
  let _rpt_type = Piqirun.gen_required_field 4 gen__relation_parent_type x.Relation_parent.rpt_type in
  Piqirun.gen_record code (_father :: _mother :: _source :: _rpt_type :: [])

and gen__title code x =
  let _title_type = Piqirun.gen_required_field 1 gen__title_type x.Title.title_type in
  let _name = Piqirun.gen_optional_field 2 gen__string x.Title.name in
  let _title = Piqirun.gen_optional_field 3 gen__string x.Title.title in
  let _fief = Piqirun.gen_optional_field 4 gen__string x.Title.fief in
  let _date_begin = Piqirun.gen_optional_field 5 gen__date x.Title.date_begin in
  let _date_end = Piqirun.gen_optional_field 6 gen__date x.Title.date_end in
  let _nth = Piqirun.gen_optional_field 7 gen__protobuf_int32 x.Title.nth in
  Piqirun.gen_record code (_title_type :: _name :: _title :: _fief :: _date_begin :: _date_end :: _nth :: [])

and gen__witness_event code x =
  let _witness_type = Piqirun.gen_required_field 1 gen__witness_type x.Witness_event.witness_type in
  let _witness = Piqirun.gen_required_field 2 gen__protobuf_int32 x.Witness_event.witness in
  Piqirun.gen_record code (_witness_type :: _witness :: [])

and gen__event code x =
  let _name = Piqirun.gen_optional_field 1 gen__event_name x.Event.name in
  let _text = Piqirun.gen_optional_field 2 gen__string x.Event.text in
  let _date = Piqirun.gen_optional_field 3 gen__date x.Event.date in
  let _place = Piqirun.gen_optional_field 4 gen__string x.Event.place in
  let _reason = Piqirun.gen_optional_field 5 gen__string x.Event.reason in
  let _note = Piqirun.gen_optional_field 6 gen__string x.Event.note in
  let _src = Piqirun.gen_optional_field 7 gen__string x.Event.src in
  let _witnesses = Piqirun.gen_repeated_field 8 gen__witness_event x.Event.witnesses in
  let _index_spouse = Piqirun.gen_optional_field 9 gen__protobuf_int32 x.Event.index_spouse in
  Piqirun.gen_record code (_name :: _text :: _date :: _place :: _reason :: _note :: _src :: _witnesses :: _index_spouse :: [])

and gen__person code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Person.index in
  let _sex = Piqirun.gen_required_field 2 gen__sex x.Person.sex in
  let _lastname = Piqirun.gen_required_field 3 gen__string x.Person.lastname in
  let _firstname = Piqirun.gen_required_field 4 gen__string x.Person.firstname in
  let _occ = Piqirun.gen_required_field 5 gen__protobuf_int32 x.Person.occ in
  let _public_name = Piqirun.gen_optional_field 6 gen__string x.Person.public_name in
  let _aliases = Piqirun.gen_repeated_field 7 gen__string x.Person.aliases in
  let _qualifiers = Piqirun.gen_repeated_field 8 gen__string x.Person.qualifiers in
  let _firstname_aliases = Piqirun.gen_repeated_field 9 gen__string x.Person.firstname_aliases in
  let _surname_aliases = Piqirun.gen_repeated_field 10 gen__string x.Person.surname_aliases in
  let _image = Piqirun.gen_required_field 11 gen__bool x.Person.image in
  let _birth_date = Piqirun.gen_optional_field 12 gen__date x.Person.birth_date in
  let _birth_place = Piqirun.gen_optional_field 13 gen__string x.Person.birth_place in
  let _birth_src = Piqirun.gen_optional_field 14 gen__string x.Person.birth_src in
  let _baptism_date = Piqirun.gen_optional_field 15 gen__date x.Person.baptism_date in
  let _baptism_place = Piqirun.gen_optional_field 16 gen__string x.Person.baptism_place in
  let _baptism_src = Piqirun.gen_optional_field 17 gen__string x.Person.baptism_src in
  let _death_date = Piqirun.gen_optional_field 18 gen__date x.Person.death_date in
  let _death_place = Piqirun.gen_optional_field 19 gen__string x.Person.death_place in
  let _death_src = Piqirun.gen_optional_field 20 gen__string x.Person.death_src in
  let _death_type = Piqirun.gen_required_field 21 gen__death_type x.Person.death_type in
  let _burial_date = Piqirun.gen_optional_field 22 gen__date x.Person.burial_date in
  let _burial_place = Piqirun.gen_optional_field 23 gen__string x.Person.burial_place in
  let _burial_src = Piqirun.gen_optional_field 24 gen__string x.Person.burial_src in
  let _occupation = Piqirun.gen_optional_field 25 gen__string x.Person.occupation in
  let _psources = Piqirun.gen_optional_field 26 gen__string x.Person.psources in
  let _titles = Piqirun.gen_repeated_field 27 gen__title x.Person.titles in
  let _related = Piqirun.gen_repeated_field 28 gen__protobuf_int32 x.Person.related in
  let _rparents = Piqirun.gen_repeated_field 29 gen__relation_parent x.Person.rparents in
  let _access = Piqirun.gen_required_field 30 gen__access x.Person.access in
  let _parents = Piqirun.gen_optional_field 31 gen__protobuf_int32 x.Person.parents in
  let _families = Piqirun.gen_repeated_field 32 gen__protobuf_int32 x.Person.families in
  let _events = Piqirun.gen_repeated_field 33 gen__event x.Person.events in
  Piqirun.gen_record code (_index :: _sex :: _lastname :: _firstname :: _occ :: _public_name :: _aliases :: _qualifiers :: _firstname_aliases :: _surname_aliases :: _image :: _birth_date :: _birth_place :: _birth_src :: _baptism_date :: _baptism_place :: _baptism_src :: _death_date :: _death_place :: _death_src :: _death_type :: _burial_date :: _burial_place :: _burial_src :: _occupation :: _psources :: _titles :: _related :: _rparents :: _access :: _parents :: _families :: _events :: [])

and gen__family code x =
  let _index = Piqirun.gen_required_field 1 gen__protobuf_int32 x.Family.index in
  let _marriage_date = Piqirun.gen_optional_field 2 gen__date x.Family.marriage_date in
  let _marriage_place = Piqirun.gen_optional_field 3 gen__string x.Family.marriage_place in
  let _marriage_src = Piqirun.gen_optional_field 4 gen__string x.Family.marriage_src in
  let _marriage_type = Piqirun.gen_required_field 5 gen__marriage_type x.Family.marriage_type in
  let _divorce_type = Piqirun.gen_required_field 6 gen__divorce_type x.Family.divorce_type in
  let _divorce_date = Piqirun.gen_optional_field 7 gen__date x.Family.divorce_date in
  let _witnesses = Piqirun.gen_repeated_field 8 gen__protobuf_int32 x.Family.witnesses in
  let _fsources = Piqirun.gen_optional_field 9 gen__string x.Family.fsources in
  let _father = Piqirun.gen_required_field 10 gen__protobuf_int32 x.Family.father in
  let _mother = Piqirun.gen_required_field 11 gen__protobuf_int32 x.Family.mother in
  let _children = Piqirun.gen_repeated_field 12 gen__protobuf_int32 x.Family.children in
  Piqirun.gen_record code (_index :: _marriage_date :: _marriage_place :: _marriage_src :: _marriage_type :: _divorce_type :: _divorce_date :: _witnesses :: _fsources :: _father :: _mother :: _children :: [])

and gen__base_warnings code x =
  let _already_defined = Piqirun.gen_required_field 1 gen__bool x.Base_warnings.already_defined in
  let _own_ancestor = Piqirun.gen_required_field 2 gen__bool x.Base_warnings.own_ancestor in
  let _bad_sex_of_married_person = Piqirun.gen_required_field 3 gen__bool x.Base_warnings.bad_sex_of_married_person in
  let _birth_after_death = Piqirun.gen_required_field 4 gen__bool x.Base_warnings.birth_after_death in
  let _incoherent_sex = Piqirun.gen_required_field 5 gen__bool x.Base_warnings.incoherent_sex in
  let _changed_order_of_children = Piqirun.gen_required_field 6 gen__bool x.Base_warnings.changed_order_of_children in
  let _children_not_in_order = Piqirun.gen_required_field 7 gen__bool x.Base_warnings.children_not_in_order in
  let _dead_too_early_to_be_father = Piqirun.gen_required_field 8 gen__bool x.Base_warnings.dead_too_early_to_be_father in
  let _incoherent_ancestor_date = Piqirun.gen_required_field 9 gen__bool x.Base_warnings.incoherent_ancestor_date in
  let _marriage_date_after_death = Piqirun.gen_required_field 10 gen__bool x.Base_warnings.marriage_date_after_death in
  let _marriage_date_before_birth = Piqirun.gen_required_field 11 gen__bool x.Base_warnings.marriage_date_before_birth in
  let _mother_dead_before_child_birth = Piqirun.gen_required_field 12 gen__bool x.Base_warnings.mother_dead_before_child_birth in
  let _parent_born_after_child = Piqirun.gen_required_field 13 gen__bool x.Base_warnings.parent_born_after_child in
  let _parent_too_young = Piqirun.gen_required_field 14 gen__bool x.Base_warnings.parent_too_young in
  let _title_dates_error = Piqirun.gen_required_field 15 gen__bool x.Base_warnings.title_dates_error in
  let _undefined_sex = Piqirun.gen_required_field 16 gen__bool x.Base_warnings.undefined_sex in
  let _young_for_marriage = Piqirun.gen_required_field 17 gen__bool x.Base_warnings.young_for_marriage in
  let _close_children = Piqirun.gen_required_field 18 gen__bool x.Base_warnings.close_children in
  let _parent_too_old = Piqirun.gen_required_field 19 gen__bool x.Base_warnings.parent_too_old in
  let _changed_order_of_marriages = Piqirun.gen_required_field 20 gen__bool x.Base_warnings.changed_order_of_marriages in
  let _big_age_between_spouses = Piqirun.gen_required_field 21 gen__bool x.Base_warnings.big_age_between_spouses in
  let _dead_old = Piqirun.gen_required_field 22 gen__bool x.Base_warnings.dead_old in
  let _old_individual = Piqirun.gen_required_field 23 gen__bool x.Base_warnings.old_individual in
  let _witness_date_after_death = Piqirun.gen_required_field 24 gen__bool x.Base_warnings.witness_date_after_death in
  let _witness_date_before_birth = Piqirun.gen_required_field 25 gen__bool x.Base_warnings.witness_date_before_birth in
  let _fevent_order = Piqirun.gen_required_field 26 gen__bool x.Base_warnings.fevent_order in
  let _fwitness_event_after_death = Piqirun.gen_required_field 27 gen__bool x.Base_warnings.fwitness_event_after_death in
  let _fwitness_event_before_birth = Piqirun.gen_required_field 28 gen__bool x.Base_warnings.fwitness_event_before_birth in
  let _pevent_order = Piqirun.gen_required_field 29 gen__bool x.Base_warnings.pevent_order in
  let _pwitness_event_after_death = Piqirun.gen_required_field 30 gen__bool x.Base_warnings.pwitness_event_after_death in
  let _pwitness_event_before_birth = Piqirun.gen_required_field 31 gen__bool x.Base_warnings.pwitness_event_before_birth in
  Piqirun.gen_record code (_already_defined :: _own_ancestor :: _bad_sex_of_married_person :: _birth_after_death :: _incoherent_sex :: _changed_order_of_children :: _children_not_in_order :: _dead_too_early_to_be_father :: _incoherent_ancestor_date :: _marriage_date_after_death :: _marriage_date_before_birth :: _mother_dead_before_child_birth :: _parent_born_after_child :: _parent_too_young :: _title_dates_error :: _undefined_sex :: _young_for_marriage :: _close_children :: _parent_too_old :: _changed_order_of_marriages :: _big_age_between_spouses :: _dead_old :: _old_individual :: _witness_date_after_death :: _witness_date_before_birth :: _fevent_order :: _fwitness_event_after_death :: _fwitness_event_before_birth :: _pevent_order :: _pwitness_event_after_death :: _pwitness_event_before_birth :: [])

and gen__modification_status code x =
  let _status = Piqirun.gen_required_field 1 gen__bool x.Modification_status.status in
  let _base_warnings = Piqirun.gen_required_field 2 gen__base_warnings x.Modification_status.base_warnings in
  let _index = Piqirun.gen_optional_field 3 gen__protobuf_int32 x.Modification_status.index in
  Piqirun.gen_record code (_status :: _base_warnings :: _index :: [])

and gen__calendar code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )
and packed_gen__calendar x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `gregorian -> 0l
    | `julian -> 1l
    | `french -> 2l
    | `hebrew -> 3l
  )

and gen__precision code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )
and packed_gen__precision x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `sure -> 0l
    | `about -> 1l
    | `maybe -> 2l
    | `before -> 3l
    | `after -> 4l
    | `oryear -> 5l
    | `yearint -> 6l
  )

and gen__sex code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )
and packed_gen__sex x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `male -> 0l
    | `female -> 1l
    | `unknown -> 2l
  )

and gen__death_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )
and packed_gen__death_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_dead -> 0l
    | `dead -> 1l
    | `dead_young -> 2l
    | `dead_dont_know_when -> 3l
    | `dont_know_if_dead -> 4l
    | `of_course_dead -> 5l
  )

and gen__marriage_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )
and packed_gen__marriage_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `married -> 0l
    | `not_married -> 1l
    | `engaged -> 2l
    | `no_sexes_check_not_married -> 3l
    | `no_mention -> 4l
    | `no_sexes_check_married -> 5l
  )

and gen__divorce_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )
and packed_gen__divorce_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `not_divorced -> 0l
    | `divorced -> 1l
    | `separated -> 2l
  )

and gen__relation_parent_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )
and packed_gen__relation_parent_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `rpt_adoption -> 0l
    | `rpt_recognition -> 1l
    | `rpt_candidate_parent -> 2l
    | `rpt_god_parent -> 3l
    | `rpt_foster_parent -> 4l
  )

and gen__title_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )
and packed_gen__title_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `title_main -> 0l
    | `title_name -> 1l
    | `title_none -> 2l
  )

and gen__access code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `access_iftitles -> 0l
    | `access_public -> 1l
    | `access_private -> 2l
  )
and packed_gen__access x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `access_iftitles -> 0l
    | `access_public -> 1l
    | `access_private -> 2l
  )

and gen__event_name code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
    | `efam_marriage -> 50l
    | `efam_no_marriage -> 51l
    | `efam_no_mention -> 52l
    | `efam_engage -> 53l
    | `efam_divorce -> 54l
    | `efam_separated -> 55l
    | `efam_annulation -> 56l
    | `efam_marriage_bann -> 57l
    | `efam_marriage_contract -> 58l
    | `efam_marriage_license -> 59l
    | `efam_pacs -> 60l
    | `efam_residence -> 61l
  )
and packed_gen__event_name x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `epers_birth -> 0l
    | `epers_baptism -> 1l
    | `epers_death -> 2l
    | `epers_burial -> 3l
    | `epers_cremation -> 4l
    | `epers_accomplishment -> 5l
    | `epers_acquisition -> 6l
    | `epers_adhesion -> 7l
    | `epers_baptismlds -> 8l
    | `epers_barmitzvah -> 9l
    | `epers_batmitzvah -> 10l
    | `epers_benediction -> 11l
    | `epers_changename -> 12l
    | `epers_circumcision -> 13l
    | `epers_confirmation -> 14l
    | `epers_confirmationlds -> 15l
    | `epers_decoration -> 16l
    | `epers_demobilisationmilitaire -> 17l
    | `epers_diploma -> 18l
    | `epers_distinction -> 19l
    | `epers_dotation -> 20l
    | `epers_dotationlds -> 21l
    | `epers_education -> 22l
    | `epers_election -> 23l
    | `epers_emigration -> 24l
    | `epers_excommunication -> 25l
    | `epers_familylinklds -> 26l
    | `epers_firstcommunion -> 27l
    | `epers_funeral -> 28l
    | `epers_graduate -> 29l
    | `epers_hospitalisation -> 30l
    | `epers_illness -> 31l
    | `epers_immigration -> 32l
    | `epers_listepassenger -> 33l
    | `epers_militarydistinction -> 34l
    | `epers_militarypromotion -> 35l
    | `epers_militaryservice -> 36l
    | `epers_mobilisationmilitaire -> 37l
    | `epers_naturalisation -> 38l
    | `epers_occupation -> 39l
    | `epers_ordination -> 40l
    | `epers_property -> 41l
    | `epers_recensement -> 42l
    | `epers_residence -> 43l
    | `epers_retired -> 44l
    | `epers_scellentchildlds -> 45l
    | `epers_scellentparentlds -> 46l
    | `epers_scellentspouselds -> 47l
    | `epers_ventebien -> 48l
    | `epers_will -> 49l
    | `efam_marriage -> 50l
    | `efam_no_marriage -> 51l
    | `efam_no_mention -> 52l
    | `efam_engage -> 53l
    | `efam_divorce -> 54l
    | `efam_separated -> 55l
    | `efam_annulation -> 56l
    | `efam_marriage_bann -> 57l
    | `efam_marriage_contract -> 58l
    | `efam_marriage_license -> 59l
    | `efam_pacs -> 60l
    | `efam_residence -> 61l
  )

and gen__witness_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
    | `witness_officer -> 2l
  )
and packed_gen__witness_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `witness -> 0l
    | `witness_godparent -> 1l
    | `witness_officer -> 2l
  )


let gen_int32 x = gen__int32 (-1) x
let gen_protobuf_int32 x = gen__protobuf_int32 (-1) x
let gen_string x = gen__string (-1) x
let gen_bool x = gen__bool (-1) x
let gen_dmy x = gen__dmy (-1) x
let gen_date x = gen__date (-1) x
let gen_relation_parent x = gen__relation_parent (-1) x
let gen_title x = gen__title (-1) x
let gen_witness_event x = gen__witness_event (-1) x
let gen_event x = gen__event (-1) x
let gen_person x = gen__person (-1) x
let gen_family x = gen__family (-1) x
let gen_base_warnings x = gen__base_warnings (-1) x
let gen_modification_status x = gen__modification_status (-1) x
let gen_calendar x = gen__calendar (-1) x
let gen_precision x = gen__precision (-1) x
let gen_sex x = gen__sex (-1) x
let gen_death_type x = gen__death_type (-1) x
let gen_marriage_type x = gen__marriage_type (-1) x
let gen_divorce_type x = gen__divorce_type (-1) x
let gen_relation_parent_type x = gen__relation_parent_type (-1) x
let gen_title_type x = gen__title_type (-1) x
let gen_access x = gen__access (-1) x
let gen_event_name x = gen__event_name (-1) x
let gen_witness_type x = gen__witness_type (-1) x


let rec default_int32 () = 0l
and default_protobuf_int32 () = default_int32 ()
and default_string () = ""
and default_bool () = false
and default_dmy () =
  {
    Dmy.day = default_protobuf_int32 ();
    Dmy.month = default_protobuf_int32 ();
    Dmy.year = default_protobuf_int32 ();
    Dmy.delta = default_protobuf_int32 ();
  }
and default_date () =
  {
    Date.cal = None;
    Date.prec = None;
    Date.dmy = None;
    Date.dmy2 = None;
    Date.text = None;
  }
and default_relation_parent () =
  {
    Relation_parent.father = None;
    Relation_parent.mother = None;
    Relation_parent.source = None;
    Relation_parent.rpt_type = default_relation_parent_type ();
  }
and default_title () =
  {
    Title.title_type = default_title_type ();
    Title.name = None;
    Title.title = None;
    Title.fief = None;
    Title.date_begin = None;
    Title.date_end = None;
    Title.nth = None;
  }
and default_witness_event () =
  {
    Witness_event.witness_type = default_witness_type ();
    Witness_event.witness = default_protobuf_int32 ();
  }
and default_event () =
  {
    Event.name = None;
    Event.text = None;
    Event.date = None;
    Event.place = None;
    Event.reason = None;
    Event.note = None;
    Event.src = None;
    Event.witnesses = [];
    Event.index_spouse = None;
  }
and default_person () =
  {
    Person.index = default_protobuf_int32 ();
    Person.sex = default_sex ();
    Person.lastname = default_string ();
    Person.firstname = default_string ();
    Person.occ = default_protobuf_int32 ();
    Person.public_name = None;
    Person.aliases = [];
    Person.qualifiers = [];
    Person.firstname_aliases = [];
    Person.surname_aliases = [];
    Person.image = parse_bool (Piqirun.parse_default "\b\000");
    Person.birth_date = None;
    Person.birth_place = None;
    Person.birth_src = None;
    Person.baptism_date = None;
    Person.baptism_place = None;
    Person.baptism_src = None;
    Person.death_date = None;
    Person.death_place = None;
    Person.death_src = None;
    Person.death_type = default_death_type ();
    Person.burial_date = None;
    Person.burial_place = None;
    Person.burial_src = None;
    Person.occupation = None;
    Person.psources = None;
    Person.titles = [];
    Person.related = [];
    Person.rparents = [];
    Person.access = parse_access (Piqirun.parse_default "\b\000");
    Person.parents = None;
    Person.families = [];
    Person.events = [];
  }
and default_family () =
  {
    Family.index = default_protobuf_int32 ();
    Family.marriage_date = None;
    Family.marriage_place = None;
    Family.marriage_src = None;
    Family.marriage_type = default_marriage_type ();
    Family.divorce_type = default_divorce_type ();
    Family.divorce_date = None;
    Family.witnesses = [];
    Family.fsources = None;
    Family.father = default_protobuf_int32 ();
    Family.mother = default_protobuf_int32 ();
    Family.children = [];
  }
and default_base_warnings () =
  {
    Base_warnings.already_defined = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.own_ancestor = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.bad_sex_of_married_person = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.birth_after_death = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.incoherent_sex = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.changed_order_of_children = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.children_not_in_order = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.dead_too_early_to_be_father = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.incoherent_ancestor_date = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.marriage_date_after_death = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.marriage_date_before_birth = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.mother_dead_before_child_birth = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.parent_born_after_child = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.parent_too_young = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.title_dates_error = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.undefined_sex = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.young_for_marriage = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.close_children = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.parent_too_old = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.changed_order_of_marriages = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.big_age_between_spouses = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.dead_old = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.old_individual = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.witness_date_after_death = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.witness_date_before_birth = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.fevent_order = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.fwitness_event_after_death = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.fwitness_event_before_birth = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.pevent_order = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.pwitness_event_after_death = parse_bool (Piqirun.parse_default "\b\000");
    Base_warnings.pwitness_event_before_birth = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_modification_status () =
  {
    Modification_status.status = default_bool ();
    Modification_status.base_warnings = default_base_warnings ();
    Modification_status.index = None;
  }
and default_calendar () = `gregorian
and default_precision () = `sure
and default_sex () = `male
and default_death_type () = `not_dead
and default_marriage_type () = `married
and default_divorce_type () = `not_divorced
and default_relation_parent_type () = `rpt_adoption
and default_title_type () = `title_main
and default_access () = `access_iftitles
and default_event_name () = `epers_birth
and default_witness_type () = `witness


let piqi = "\226\202\2304\007api_app\226\231\249\238\001\018api_app.proto.piqi\162\244\146\155\011\022geneweb.api.app.object\218\244\134\182\012\217\001\138\233\142\251\014\210\001\210\203\242$,\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003day\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005month\210\171\158\194\006\014protobuf-int32\210\203\242$-\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\004year\210\171\158\194\006\014protobuf-int32\210\203\242$.\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005delta\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\003dmy\218\244\134\182\012\224\001\138\233\142\251\014\217\001\210\203\242$&\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003cal\210\171\158\194\006\bcalendar\210\203\242$(\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004prec\210\171\158\194\006\tprecision\210\203\242$!\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003dmy\210\171\158\194\006\003dmy\210\203\242$\"\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004dmy2\210\171\158\194\006\003dmy\210\203\242$%\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\218\164\238\191\004\004date\218\244\134\182\012\236\001\138\233\142\251\014\229\001\210\203\242$/\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006father\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006mother\210\171\158\194\006\014protobuf-int32\210\203\242$'\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006source\210\171\158\194\006\006string\210\203\242$7\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\brpt-type\210\171\158\194\006\020relation-parent-type\218\164\238\191\004\015relation-parent\218\244\134\182\012\208\002\138\233\142\251\014\201\002\210\203\242$/\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ntitle-type\210\171\158\194\006\ntitle-type\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004name\210\171\158\194\006\006string\210\203\242$&\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005title\210\171\158\194\006\006string\210\203\242$%\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004fief\210\171\158\194\006\006string\210\203\242$)\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndate-begin\210\171\158\194\006\004date\210\203\242$'\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdate-end\210\171\158\194\006\004date\210\203\242$,\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003nth\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005title\218\244\134\182\012\135\001\138\233\142\251\014\128\001\210\203\242$3\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012witness-type\210\171\158\194\006\012witness-type\210\203\242$0\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\007witness\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\rwitness-event\218\244\134\182\012\172\003\138\233\142\251\014\165\003\210\203\242$)\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004name\210\171\158\194\006\nevent-name\210\203\242$%\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004text\210\171\158\194\006\006string\210\203\242$#\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004date\210\171\158\194\006\004date\210\203\242$&\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005place\210\171\158\194\006\006string\210\203\242$'\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006reason\210\171\158\194\006\006string\210\203\242$%\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\004note\210\171\158\194\006\006string\210\203\242$$\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\003src\210\171\158\194\006\006string\210\203\242$1\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\rwitness-event\210\203\242$5\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012index-spouse\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\005event\218\244\134\182\012\129\r\138\233\142\251\014\250\012\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$!\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003sex\210\171\158\194\006\003sex\210\203\242$)\232\146\150q\006\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\blastname\210\171\158\194\006\006string\210\203\242$*\232\146\150q\b\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\tfirstname\210\171\158\194\006\006string\210\203\242$,\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\003occ\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011public-name\210\171\158\194\006\006string\210\203\242$(\232\146\150q\014\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007aliases\210\171\158\194\006\006string\210\203\242$+\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\nqualifiers\210\171\158\194\006\006string\210\203\242$2\232\146\150q\018\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\017firstname-aliases\210\171\158\194\006\006string\210\203\242$0\232\146\150q\020\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\015surname-aliases\210\171\158\194\006\006string\210\203\242$;\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005image\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$)\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nbirth-date\210\171\158\194\006\004date\210\203\242$,\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011birth-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tbirth-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012baptism-date\210\171\158\194\006\004date\210\203\242$.\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rbaptism-place\210\171\158\194\006\006string\210\203\242$,\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011baptism-src\210\171\158\194\006\006string\210\203\242$)\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\ndeath-date\210\171\158\194\006\004date\210\203\242$,\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011death-place\210\171\158\194\006\006string\210\203\242$*\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\tdeath-src\210\171\158\194\006\006string\210\203\242$/\232\146\150q*\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\ndeath-type\210\171\158\194\006\ndeath-type\210\203\242$*\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\011burial-date\210\171\158\194\006\004date\210\203\242$-\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012burial-place\210\171\158\194\006\006string\210\203\242$+\232\146\150q0\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\nburial-src\210\171\158\194\006\006string\210\203\242$+\232\146\150q2\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\noccupation\210\171\158\194\006\006string\210\203\242$)\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bpsources\210\171\158\194\006\006string\210\203\242$&\232\146\150q6\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006titles\210\171\158\194\006\005title\210\203\242$0\232\146\150q8\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\007related\210\171\158\194\006\014protobuf-int32\210\203\242$2\232\146\150q:\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\brparents\210\171\158\194\006\015relation-parent\210\203\242$H\232\146\150q<\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\006access\210\171\158\194\006\006access\138\140\251\240\r\027\218\148\211\024\002\b\000\210\171\158\194\006\014api_app/access\210\203\242$0\232\146\150q>\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\007parents\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q@\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bfamilies\210\171\158\194\006\014protobuf-int32\210\203\242$&\232\146\150qB\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\006events\210\171\158\194\006\005event\218\164\238\191\004\006person\218\244\134\182\012\130\005\138\233\142\251\014\251\004\210\203\242$.\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\210\203\242$,\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rmarriage-date\210\171\158\194\006\004date\210\203\242$/\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014marriage-place\210\171\158\194\006\006string\210\203\242$-\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012marriage-src\210\171\158\194\006\006string\210\203\242$5\232\146\150q\n\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rmarriage-type\210\171\158\194\006\rmarriage-type\210\203\242$3\232\146\150q\012\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\012divorce-type\210\171\158\194\006\012divorce-type\210\203\242$+\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012divorce-date\210\171\158\194\006\004date\210\203\242$2\232\146\150q\016\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\twitnesses\210\171\158\194\006\014protobuf-int32\210\203\242$)\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bfsources\210\171\158\194\006\006string\210\203\242$/\232\146\150q\020\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006father\210\171\158\194\006\014protobuf-int32\210\203\242$/\232\146\150q\022\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006mother\210\171\158\194\006\014protobuf-int32\210\203\242$1\232\146\150q\024\152\182\154\152\004\250\248\214\130\001\218\164\238\191\004\bchildren\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\006family\218\244\134\182\012\177\019\138\233\142\251\014\170\019\210\203\242$E\232\146\150q\002\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\015already-defined\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$B\232\146\150q\004\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012own-ancestor\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$O\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\025bad-sex-of-married-person\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$G\232\146\150q\b\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017birth-after-death\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$D\232\146\150q\n\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014incoherent-sex\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$O\232\146\150q\012\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\025changed-order-of-children\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$K\232\146\150q\014\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\021children-not-in-order\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$Q\232\146\150q\016\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\027dead-too-early-to-be-father\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$N\232\146\150q\018\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\024incoherent-ancestor-date\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$O\232\146\150q\020\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\025marriage-date-after-death\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$P\232\146\150q\022\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\026marriage-date-before-birth\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$T\232\146\150q\024\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\030mother-dead-before-child-birth\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$M\232\146\150q\026\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\023parent-born-after-child\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$F\232\146\150q\028\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\016parent-too-young\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$G\232\146\150q\030\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\017title-dates-error\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$C\232\146\150q \152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\rundefined-sex\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$H\232\146\150q\"\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\018young-for-marriage\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$D\232\146\150q$\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014close-children\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$D\232\146\150q&\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014parent-too-old\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$P\232\146\150q(\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\026changed-order-of-marriages\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$M\232\146\150q*\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\023big-age-between-spouses\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$>\232\146\150q,\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\bdead-old\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$D\232\146\150q.\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\014old-individual\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$N\232\146\150q0\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\024witness-date-after-death\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$O\232\146\150q2\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\025witness-date-before-birth\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$B\232\146\150q4\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012fevent-order\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$P\232\146\150q6\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\026fwitness-event-after-death\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$Q\232\146\150q8\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\027fwitness-event-before-birth\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$B\232\146\150q:\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\012pevent-order\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$P\232\146\150q<\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\026pwitness-event-after-death\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\210\203\242$Q\232\146\150q>\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\027pwitness-event-before-birth\210\171\158\194\006\004bool\138\140\251\240\r\017\218\148\211\024\002\b\000\210\171\158\194\006\004bool\218\164\238\191\004\rbase-warnings\218\244\134\182\012\183\001\138\233\142\251\014\176\001\210\203\242$%\232\146\150q\002\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\006status\210\171\158\194\006\004bool\210\203\242$5\232\146\150q\004\152\182\154\152\004\223\162\138\147\001\218\164\238\191\004\rbase-warnings\210\171\158\194\006\rbase-warnings\210\203\242$.\232\146\150q\006\152\182\154\152\004\160\223\186\243\001\218\164\238\191\004\005index\210\171\158\194\006\014protobuf-int32\218\164\238\191\004\019modification-status\218\244\134\182\012s\138\176\205\197\001m\218\164\238\191\004\bcalendar\170\183\218\222\005\020\232\146\150q\000\218\164\238\191\004\tgregorian\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006julian\170\183\218\222\005\017\232\146\150q\004\218\164\238\191\004\006french\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006hebrew\218\244\134\182\012\179\001\138\176\205\197\001\172\001\218\164\238\191\004\tprecision\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004sure\170\183\218\222\005\016\232\146\150q\002\218\164\238\191\004\005about\170\183\218\222\005\016\232\146\150q\004\218\164\238\191\004\005maybe\170\183\218\222\005\017\232\146\150q\006\218\164\238\191\004\006before\170\183\218\222\005\016\232\146\150q\b\218\164\238\191\004\005after\170\183\218\222\005\017\232\146\150q\n\218\164\238\191\004\006oryear\170\183\218\222\005\018\232\146\150q\012\218\164\238\191\004\007yearint\218\244\134\182\012S\138\176\205\197\001M\218\164\238\191\004\003sex\170\183\218\222\005\015\232\146\150q\000\218\164\238\191\004\004male\170\183\218\222\005\017\232\146\150q\002\218\164\238\191\004\006female\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007unknown\218\244\134\182\012\197\001\138\176\205\197\001\190\001\218\164\238\191\004\ndeath-type\170\183\218\222\005\019\232\146\150q\000\218\164\238\191\004\bnot-dead\170\183\218\222\005\015\232\146\150q\002\218\164\238\191\004\004dead\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ndead-young\170\183\218\222\005\030\232\146\150q\006\218\164\238\191\004\019dead-dont-know-when\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017dont-know-if-dead\170\183\218\222\005\025\232\146\150q\n\218\164\238\191\004\014of-course-dead\218\244\134\182\012\211\001\138\176\205\197\001\204\001\218\164\238\191\004\rmarriage-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007married\170\183\218\222\005\022\232\146\150q\002\218\164\238\191\004\011not-married\170\183\218\222\005\018\232\146\150q\004\218\164\238\191\004\007engaged\170\183\218\222\005%\232\146\150q\006\218\164\238\191\004\026no-sexes-check-not-married\170\183\218\222\005\021\232\146\150q\b\218\164\238\191\004\nno-mention\170\183\218\222\005!\232\146\150q\n\218\164\238\191\004\022no-sexes-check-married\218\244\134\182\012h\138\176\205\197\001b\218\164\238\191\004\012divorce-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012not-divorced\170\183\218\222\005\019\232\146\150q\002\218\164\238\191\004\bdivorced\170\183\218\222\005\020\232\146\150q\004\218\164\238\191\004\tseparated\218\244\134\182\012\196\001\138\176\205\197\001\189\001\218\164\238\191\004\020relation-parent-type\170\183\218\222\005\023\232\146\150q\000\218\164\238\191\004\012rpt-adoption\170\183\218\222\005\026\232\146\150q\002\218\164\238\191\004\015rpt-recognition\170\183\218\222\005\031\232\146\150q\004\218\164\238\191\004\020rpt-candidate-parent\170\183\218\222\005\025\232\146\150q\006\218\164\238\191\004\014rpt-god-parent\170\183\218\222\005\028\232\146\150q\b\218\164\238\191\004\017rpt-foster-parent\218\244\134\182\012g\138\176\205\197\001a\218\164\238\191\004\ntitle-type\170\183\218\222\005\021\232\146\150q\000\218\164\238\191\004\ntitle-main\170\183\218\222\005\021\232\146\150q\002\218\164\238\191\004\ntitle-name\170\183\218\222\005\021\232\146\150q\004\218\164\238\191\004\ntitle-none\218\244\134\182\012o\138\176\205\197\001i\218\164\238\191\004\006access\170\183\218\222\005\026\232\146\150q\000\218\164\238\191\004\015access-iftitles\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\raccess-public\170\183\218\222\005\025\232\146\150q\004\218\164\238\191\004\014access-private\218\244\134\182\012\197\016\138\176\205\197\001\190\016\218\164\238\191\004\nevent-name\170\183\218\222\005\022\232\146\150q\000\218\164\238\191\004\011epers-birth\170\183\218\222\005\024\232\146\150q\002\218\164\238\191\004\repers-baptism\170\183\218\222\005\022\232\146\150q\004\218\164\238\191\004\011epers-death\170\183\218\222\005\023\232\146\150q\006\218\164\238\191\004\012epers-burial\170\183\218\222\005\026\232\146\150q\b\218\164\238\191\004\015epers-cremation\170\183\218\222\005\031\232\146\150q\n\218\164\238\191\004\020epers-accomplishment\170\183\218\222\005\028\232\146\150q\012\218\164\238\191\004\017epers-acquisition\170\183\218\222\005\025\232\146\150q\014\218\164\238\191\004\014epers-adhesion\170\183\218\222\005\027\232\146\150q\016\218\164\238\191\004\016epers-baptismlds\170\183\218\222\005\027\232\146\150q\018\218\164\238\191\004\016epers-barmitzvah\170\183\218\222\005\027\232\146\150q\020\218\164\238\191\004\016epers-batmitzvah\170\183\218\222\005\028\232\146\150q\022\218\164\238\191\004\017epers-benediction\170\183\218\222\005\027\232\146\150q\024\218\164\238\191\004\016epers-changename\170\183\218\222\005\029\232\146\150q\026\218\164\238\191\004\018epers-circumcision\170\183\218\222\005\029\232\146\150q\028\218\164\238\191\004\018epers-confirmation\170\183\218\222\005 \232\146\150q\030\218\164\238\191\004\021epers-confirmationlds\170\183\218\222\005\027\232\146\150q \218\164\238\191\004\016epers-decoration\170\183\218\222\005(\232\146\150q\"\218\164\238\191\004\029epers-demobilisationmilitaire\170\183\218\222\005\024\232\146\150q$\218\164\238\191\004\repers-diploma\170\183\218\222\005\028\232\146\150q&\218\164\238\191\004\017epers-distinction\170\183\218\222\005\025\232\146\150q(\218\164\238\191\004\014epers-dotation\170\183\218\222\005\028\232\146\150q*\218\164\238\191\004\017epers-dotationlds\170\183\218\222\005\026\232\146\150q,\218\164\238\191\004\015epers-education\170\183\218\222\005\025\232\146\150q.\218\164\238\191\004\014epers-election\170\183\218\222\005\027\232\146\150q0\218\164\238\191\004\016epers-emigration\170\183\218\222\005 \232\146\150q2\218\164\238\191\004\021epers-excommunication\170\183\218\222\005\030\232\146\150q4\218\164\238\191\004\019epers-familylinklds\170\183\218\222\005\031\232\146\150q6\218\164\238\191\004\020epers-firstcommunion\170\183\218\222\005\024\232\146\150q8\218\164\238\191\004\repers-funeral\170\183\218\222\005\025\232\146\150q:\218\164\238\191\004\014epers-graduate\170\183\218\222\005 \232\146\150q<\218\164\238\191\004\021epers-hospitalisation\170\183\218\222\005\024\232\146\150q>\218\164\238\191\004\repers-illness\170\183\218\222\005\028\232\146\150q@\218\164\238\191\004\017epers-immigration\170\183\218\222\005\031\232\146\150qB\218\164\238\191\004\020epers-listepassenger\170\183\218\222\005$\232\146\150qD\218\164\238\191\004\025epers-militarydistinction\170\183\218\222\005\"\232\146\150qF\218\164\238\191\004\023epers-militarypromotion\170\183\218\222\005 \232\146\150qH\218\164\238\191\004\021epers-militaryservice\170\183\218\222\005&\232\146\150qJ\218\164\238\191\004\027epers-mobilisationmilitaire\170\183\218\222\005\031\232\146\150qL\218\164\238\191\004\020epers-naturalisation\170\183\218\222\005\027\232\146\150qN\218\164\238\191\004\016epers-occupation\170\183\218\222\005\027\232\146\150qP\218\164\238\191\004\016epers-ordination\170\183\218\222\005\025\232\146\150qR\218\164\238\191\004\014epers-property\170\183\218\222\005\028\232\146\150qT\218\164\238\191\004\017epers-recensement\170\183\218\222\005\026\232\146\150qV\218\164\238\191\004\015epers-residence\170\183\218\222\005\024\232\146\150qX\218\164\238\191\004\repers-retired\170\183\218\222\005!\232\146\150qZ\218\164\238\191\004\022epers-scellentchildlds\170\183\218\222\005\"\232\146\150q\\\218\164\238\191\004\023epers-scellentparentlds\170\183\218\222\005\"\232\146\150q^\218\164\238\191\004\023epers-scellentspouselds\170\183\218\222\005\026\232\146\150q`\218\164\238\191\004\015epers-ventebien\170\183\218\222\005\021\232\146\150qb\218\164\238\191\004\nepers-will\170\183\218\222\005\024\232\146\150qd\218\164\238\191\004\refam-marriage\170\183\218\222\005\027\232\146\150qf\218\164\238\191\004\016efam-no-marriage\170\183\218\222\005\026\232\146\150qh\218\164\238\191\004\015efam-no-mention\170\183\218\222\005\022\232\146\150qj\218\164\238\191\004\011efam-engage\170\183\218\222\005\023\232\146\150ql\218\164\238\191\004\012efam-divorce\170\183\218\222\005\025\232\146\150qn\218\164\238\191\004\014efam-separated\170\183\218\222\005\026\232\146\150qp\218\164\238\191\004\015efam-annulation\170\183\218\222\005\029\232\146\150qr\218\164\238\191\004\018efam-marriage-bann\170\183\218\222\005!\232\146\150qt\218\164\238\191\004\022efam-marriage-contract\170\183\218\222\005 \232\146\150qv\218\164\238\191\004\021efam-marriage-license\170\183\218\222\005\020\232\146\150qx\218\164\238\191\004\tefam-pacs\170\183\218\222\005\025\232\146\150qz\218\164\238\191\004\014efam-residence\218\244\134\182\012r\138\176\205\197\001l\218\164\238\191\004\012witness-type\170\183\218\222\005\018\232\146\150q\000\218\164\238\191\004\007witness\170\183\218\222\005\028\232\146\150q\002\218\164\238\191\004\017witness-godparent\170\183\218\222\005\026\232\146\150q\004\218\164\238\191\004\015witness-officer"
include Api_app_piqi
