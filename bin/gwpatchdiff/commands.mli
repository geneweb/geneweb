

type person_reference = {
  first_name : string;
  surname: string;
  occ: int
}

type event_data = {
  date: Def.cdate;
  place: string;
}

type death_data = {
  death: Def.death;
  death_place : string;
}

type burial_data = {
  burial : Def.burial;
  burial_place : string;
}

type union_data = (person_reference * Def.cdate) list

type ascends_data = {
  father : person_reference option;
  mother : person_reference option;
}
type children_data = person_reference list

type person_data = {
  reference : person_reference;
  public_name : string;
  qualifiers : string list;
  aliases : string list;
  first_names_aliases : string list;
  surnames_aliases : string list;
  father : person_reference option;
  mother : person_reference option;
  occupation : string;
  sex : Def.sex;
  birth : event_data;
  baptism : event_data;
  death : death_data;
  burial : burial_data;
  unions : union_data;
  children : children_data;
  access : Def.access
}

type t =
    Add of person_data
  | Update of person_reference * person_data
  | Delete of person_reference


val command_of_person_diff : Gwdb.base -> Diff_types.Person_diff.t -> t

val string_of_command : t -> string

val cmp_command : t -> t -> int
