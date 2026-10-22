(* Records as stored by format GnWb0024 (and earlier) and by patches
   GnPa0001: event witnesses are (person, kind) pairs, without the witness
   note added by GnWb0025.

   The field order and types must stay exactly those of the GnWb0024
   definitions in [Def]: values are unmarshaled into these types. *)

type ('person, 'string) gen_pers_event = {
  epers_name : 'string Def.gen_pers_event_name;
  epers_date : Adef.cdate;
  epers_place : 'string;
  epers_reason : 'string;
  epers_note : 'string;
  epers_src : 'string;
  epers_witnesses : ('person * Def.witness_kind) array;
}

type ('person, 'string) gen_fam_event = {
  efam_name : 'string Def.gen_fam_event_name;
  efam_date : Adef.cdate;
  efam_place : 'string;
  efam_reason : 'string;
  efam_note : 'string;
  efam_src : 'string;
  efam_witnesses : ('person * Def.witness_kind) array;
}

type ('iper, 'person, 'string) gen_person = {
  first_name : 'string;
  surname : 'string;
  occ : int;
  image : 'string;
  public_name : 'string;
  qualifiers : 'string list;
  aliases : 'string list;
  first_names_aliases : 'string list;
  surnames_aliases : 'string list;
  titles : 'string Def.gen_title list;
  rparents : ('person, 'string) Def.gen_relation list;
  related : 'person list;
  occupation : 'string;
  sex : Def.sex;
  access : Def.access;
  birth : Adef.cdate;
  birth_place : 'string;
  birth_note : 'string;
  birth_src : 'string;
  baptism : Adef.cdate;
  baptism_place : 'string;
  baptism_note : 'string;
  baptism_src : 'string;
  death : Def.death;
  death_place : 'string;
  death_note : 'string;
  death_src : 'string;
  burial : Def.burial;
  burial_place : 'string;
  burial_note : 'string;
  burial_src : 'string;
  pevents : ('person, 'string) gen_pers_event list;
  notes : 'string;
  psources : 'string;
  key_index : 'iper;
}

type ('person, 'ifam, 'string) gen_family = {
  marriage : Adef.cdate;
  marriage_place : 'string;
  marriage_note : 'string;
  marriage_src : 'string;
  witnesses : 'person array;
  relation : Def.relation_kind;
  divorce : Def.divorce;
  fevents : ('person, 'string) gen_fam_event list;
  comment : 'string;
  origin_file : 'string;
  fsources : 'string;
  fam_index : 'ifam;
}

let pers_event ~empty (e : _ gen_pers_event) : _ Def.gen_pers_event =
  {
    epers_name = e.epers_name;
    epers_date = e.epers_date;
    epers_place = e.epers_place;
    epers_reason = e.epers_reason;
    epers_note = e.epers_note;
    epers_src = e.epers_src;
    epers_witnesses =
      Array.map (fun (p, wk) -> (p, wk, empty)) e.epers_witnesses;
  }

let fam_event ~empty (e : _ gen_fam_event) : _ Def.gen_fam_event =
  {
    efam_name = e.efam_name;
    efam_date = e.efam_date;
    efam_place = e.efam_place;
    efam_reason = e.efam_reason;
    efam_note = e.efam_note;
    efam_src = e.efam_src;
    efam_witnesses = Array.map (fun (p, wk) -> (p, wk, empty)) e.efam_witnesses;
  }

let person ~empty (p : _ gen_person) : _ Def.gen_person =
  {
    first_name = p.first_name;
    surname = p.surname;
    occ = p.occ;
    image = p.image;
    public_name = p.public_name;
    qualifiers = p.qualifiers;
    aliases = p.aliases;
    first_names_aliases = p.first_names_aliases;
    surnames_aliases = p.surnames_aliases;
    titles = p.titles;
    rparents = p.rparents;
    related = p.related;
    occupation = p.occupation;
    sex = p.sex;
    access = p.access;
    birth = p.birth;
    birth_place = p.birth_place;
    birth_note = p.birth_note;
    birth_src = p.birth_src;
    baptism = p.baptism;
    baptism_place = p.baptism_place;
    baptism_note = p.baptism_note;
    baptism_src = p.baptism_src;
    death = p.death;
    death_place = p.death_place;
    death_note = p.death_note;
    death_src = p.death_src;
    burial = p.burial;
    burial_place = p.burial_place;
    burial_note = p.burial_note;
    burial_src = p.burial_src;
    pevents = List.map (pers_event ~empty) p.pevents;
    notes = p.notes;
    psources = p.psources;
    key_index = p.key_index;
  }

let family ~empty (f : _ gen_family) : _ Def.gen_family =
  {
    marriage = f.marriage;
    marriage_place = f.marriage_place;
    marriage_note = f.marriage_note;
    marriage_src = f.marriage_src;
    witnesses = f.witnesses;
    relation = f.relation;
    divorce = f.divorce;
    fevents = List.map (fam_event ~empty) f.fevents;
    comment = f.comment;
    origin_file = f.origin_file;
    fsources = f.fsources;
    fam_index = f.fam_index;
  }
