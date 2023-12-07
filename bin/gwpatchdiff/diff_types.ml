
type 'a diff = {
  previously : 'a option;
  now : 'a option;
}

type title_diff = {
  t_name : string Def.gen_title_name diff option;
  t_ident : string diff option;
  t_place : string diff option;
  t_date_start : Def.cdate diff option;
  t_date_end : Def.cdate diff option;
  t_nth : int diff option;
}

module Npoc_diff = struct
  type t = {
    first_name : Gwdb.istr diff option;
    surname : Gwdb.istr diff option;
    occ : int diff option;
  }
end

module Ascend_diff = struct
  type t = {
    father : Npoc_diff.t option;
    mother : Npoc_diff.t option;
  }
end

module Descend_diff = struct
  type t = Gwdb.iper list diff
end

module Union_diff = struct
  type t = (Gwdb.iper * Def.cdate option) list diff
end

module Person_diff = struct
  type t = {
    iper : Gwdb.iper;
    first_name : Gwdb.istr diff option;
    surname : Gwdb.istr diff option;
    occ : int diff option;
    public_name : Gwdb.istr diff option;
    qualifiers : Gwdb.istr list diff option;
    aliases : Gwdb.istr list diff option;
    first_names_aliases : Gwdb.istr list diff option;
    surnames_aliases : Gwdb.istr list diff option;
    titles : title_diff option;
    (* relations with not native parents *)
    rparents : (Gwdb.iper, Gwdb.istr) Def.gen_relation list diff option;
    (* related persons like (father of witnessed family,
       concerned person of witnessed event, adopted child, etc.) *)
    occupation : Gwdb.istr diff option;
    sex : Def.sex diff option;
    birth : Def.cdate diff option;
    birth_place : Gwdb.istr diff option;
    baptism : Def.cdate diff option;
    baptism_place : Gwdb.istr diff option;
    death : Def.death diff option;
    death_place : Gwdb.istr diff option;
    burial : Def.burial diff option;
    burial_place : Gwdb.istr diff option;
    unions : Union_diff.t option;
    ascends : Ascend_diff.t option;
    children : Descend_diff.t option;
  }
end

(*type family_diff = {
  marriage : Def.cdate;
  marriage_place : 'string;
  marriage_note : 'string;
  marriage_src : 'string;
  witnesses : 'person array;
  relation : relation_kind;
  divorce : divorce;
  fevents : ('person, 'string) gen_fam_event list;
  comment : 'string;
  origin_file : 'string; (* .gw filename where family is defined *)
  fsources : 'string;
  fam_index : 'ifam;
  }*)


let no_diff_person = Person_diff.{
    iper = Gwdb.dummy_iper;
    first_name = None;
    surname = None;
    occ = None;
    public_name = None;
    qualifiers = None;
    aliases = None;
    first_names_aliases = None;
    surnames_aliases = None;
    titles = None;
    rparents = None;
    occupation = None;
    sex = None;
    birth = None;
    birth_place = None;
    baptism = None;
    baptism_place = None;
    death = None;
    death_place = None;
    burial = None;
    burial_place = None;
    unions = None;
    ascends = None;
    children = None;
  }
