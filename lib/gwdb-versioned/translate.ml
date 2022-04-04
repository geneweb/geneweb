

(* TODO modify according to changes to def *)
(*
external legacy_to_def_person :
  ('iper, 'person, 'string) Gwdb_legacy.Gwdb_driver.legacy_dsk_person ->
  ('iper, 'person, 'string) Def.gen_person = "%identity"
 *)
let rec legacy_to_def_person value p =
  {
    Def.first_name = p.Gwdb_legacy.Dbdisk.first_name;
    surname = p.surname;
    occ = p.occ;
    image = p.image;
    public_name = p.public_name;
    qualifiers = p.qualifiers;
    aliases = p.aliases;
    first_names_aliases = p.first_names_aliases;
    surnames_aliases = p.surnames_aliases;
    titles = p.titles;
    (* relations with not native parents *)
    rparents = p.rparents;
    (* related persons like (father of witnessed family,
      concerned person of witnessed event, adopted child, etc.) *)
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
    pevents = List.map (legacy_to_def_pevent value) p.pevents;
    notes = p.notes;
    psources = p.psources;
    key_index = p.key_index
  }
  
and legacy_to_def_pevent value e =
  { Def.epers_name = e.Gwdb_legacy.Dbdisk.epers_name;
    epers_date = e.epers_date;
    epers_place = e.epers_place;
    epers_reason = e.epers_reason;
    epers_note = e.epers_note;
    epers_src = e.epers_src;
    epers_witnesses = Array.map (fun (ip, wk) -> ip, wk, value) e.epers_witnesses
  }


and as_legacy_person p =
  {
    Gwdb_legacy.Dbdisk.first_name = p.Def.first_name;
    surname = p.surname;
    occ = p.occ;
    image = p.image;
    public_name = p.public_name;
    qualifiers = p.qualifiers;
    aliases = p.aliases;
    first_names_aliases = p.first_names_aliases;
    surnames_aliases = p.surnames_aliases;
    titles = p.titles;
    (* relations with not native parents *)
    rparents = p.rparents;
    (* related persons like (father of witnessed family,
      concerned person of witnessed event, adopted child, etc.) *)
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
    pevents = List.map as_legacy_pevent p.pevents;
    notes = p.notes;
    psources = p.psources;
    key_index = p.key_index
  }

and as_legacy_pevent e = 
  { Gwdb_legacy.Dbdisk.epers_name = e.Def.epers_name;
    epers_date = e.epers_date;
    epers_place = e.epers_place;
    epers_reason = e.epers_reason;
    epers_note = e.epers_note;
    epers_src = e.epers_src;
    epers_witnesses = Array.map (fun (ip, wk, _) -> ip, wk) e.epers_witnesses
  }
