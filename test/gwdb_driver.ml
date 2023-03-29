


type ifam = int
type iper = int
type istr = int

let string_of_ifam = string_of_int
let string_of_iper = string_of_int
let string_of_istr = string_of_int

let ifam_of_string = int_of_string
let iper_of_string = int_of_string
let istr_of_string = int_of_string

let dummy_ifam = -1
let dummy_iper = -1
let empty_string = 0
let quest_string = 1

let eq_istr = ( = )
let eq_ifam = ( = )
let eq_iper = ( = )

let is_empty_string = eq_istr empty_string
let is_quest_string = eq_istr quest_string

type base = {
  mutable persons : (iper, iper, istr) Def.gen_person array;
  mutable ascends : ifam Def.gen_ascend array;
  mutable unions : ifam Def.gen_union array;
  mutable families : (iper, ifam, istr) Def.gen_family array;
  mutable couples : iper Def.gen_couple array;
  mutable descends : iper Def.gen_descend array;
  mutable strings : string array;
}

type family = base * ifam
type person = base * iper
type relation = (iper, istr) Def.gen_relation
type string_person_index
type title = istr Def.gen_title

module Utils = struct
  let is_in_range arr i =
    0 <= i && i < Array.length arr
  let map_snd f (_first, second) = f second
end


module Person = struct
  open Def

  let create_empty_person empty what iper =
    {
      Def.first_name = what;
      surname = what;
      occ = 0;
      public_name = empty;
      image = empty;
      qualifiers = [];
      aliases = [];
      first_names_aliases = [];
      surnames_aliases = [];
      titles = [];
      rparents = [];
      related = [];
      occupation = empty;
      sex = Neuter;
      access = IfTitles;
      birth = Date.cdate_None;
      birth_place = empty;
      birth_note = empty;
      birth_src = empty;
      baptism = Date.cdate_None;
      baptism_place = empty;
      baptism_note = empty;
      baptism_src = empty;
      death = DontKnowIfDead;
      death_place = empty;
      death_note = empty;
      death_src = empty;
      burial = UnknownBurial;
      burial_place = empty;
      burial_note = empty;
      burial_src = empty;
      pevents = [];
      notes = empty;
      psources = empty;
      key_index = iper;
    }

  let empty_person (base : base) (iper : iper) : person =
    base, assert false (*create_empty_person empty_string empty_string iper*)

  let get_first_name (_, p) = p.first_name
  let get_surname (_, p) = p.surname
  let get_occ (_, p) = p.occ
  let get_public_name (_, p) = p.public_name
  let get_image (_, p) = p.image
  let get_qualifiers (_, p) = p.qualifiers
  let get_first_names_aliases (_, p) = p.first_names_aliases
  let get_surnames_aliases (_, p) = p.surnames_aliases
  let get_aliases (_, p) = p.aliases
  let get_access (_, p) = p.access

  let get_birth (_, p) = p.birth
  let get_birth_note (_, p) = p.birth_note
  let get_birth_place (_, p) = p.birth_place
  let get_birth_src (_, p) = p.birth_src

  let get_baptism (_, p) = p.baptism
  let get_baptism_note (_, p) = p.baptism_note
  let get_baptism_place (_, p) = p.baptism_place
  let get_baptism_src (_, p) = p.baptism_src

  let get_burial (_, p) = p.burial
  let get_burial_note (_, p) = p.burial_note
  let get_burial_place (_, p) = p.burial_place
  let get_burial_src (_, p) = p.burial_src

  let get_death (_, p) = p.death
  let get_death_note (_, p) = p.death_note
  let get_death_place (_, p) = p.death_place
  let get_death_src (_, p) = p.death_src

  let get_iper (_, p) = p.key_index
  let get_notes (_, p) = p.notes

  let get_occupation (_, p) = p.occupation

  let get_consang (base, p) = base.ascends.(p.key_index).consang
  let get_family (base, p) = base.unions.(p.key_index).family
  let get_parents (base, p) = assert false
  let get_pevents (_, p) = p.pevents
  let get_psources (_, p) = p.psources
  let get_related (_, p) = p.related
  let get_rparents (_, p) = p.rparents
  let get_sex (_, p) = p.sex
  let get_titles (_, p) = p.titles

  let gen_person_of_person (_, p) = p
  let gen_ascend_of_person (base, p) = base.ascends.(p.key_index)
  let gen_union_of_person (base, p) = base.unions.(p.key_index)
  let person_of_gen_person base p = base, p
  let poi base iper = base.persons.(iper)

  let no_person ip = create_empty_person empty_string empty_string ip

end

module Family = struct
  open Def


 let create_empty_family empty ifam =
   {
     Def.marriage = Date.cdate_None;
     marriage_place = empty;
     marriage_note = empty;
     marriage_src = empty;
     witnesses = [||];
     relation = Def.NoMention;
     divorce = Def.NotDivorced;
     fevents = [];
     comment = empty;
     origin_file = empty;
     fsources = empty;
     fam_index = ifam;
   }
  
 let empty_family base ifam = assert false
 (*   create_empty_family empty_string ifam*)

 let get_marriage (_, f) = f.marriage
 let get_marriage_place (_, f) = f.marriage_place
 let get_marriage_note (_, f) = f.marriage_note
 let get_marriage_src (_, f) = f.marriage_src
 let get_witnesses (_, f) = f.witnesses
 let get_relation (_, f) = f.relation
 let get_divorce (_, f) = f.divorce
 let get_fevents (_, f) = f.fevents
 let get_comment (_, f) = f.comment
 let get_origin_file (_, f) = f.origin_file
 let get_fsources (_, f) = f.fsources
 let get_ifam (_, f) = f.fam_index
 let get_children (base, f) = base.descends.(f.fam_index)
 let get_father (base, f) = base.couples.(f.fam_index) |> Adef.father
 let get_mother (base, f) = base.couples.(f.fam_index) |> Adef.mother
 let get_parent_array (base, f) = base.couples.(f.fam_index) |> Adef.parent_array
 let get_related (_, f) = f.related
 let get_relation (_, f) = f.relation
 let gen_couple_of_family (base, f) = base.couples.(f.fam_index)
 let gen_descend_of_family (base, f) = base.descends.(f.fam_index)
 let gen_family_of_family (_, f) = f
 let family_of_gen_family base f = base, f
 let foi base ifam = base.families.(ifam)
 let no_family ifam = create_empty_family empty_string ifam
end

module PersEvent = struct
  open Def
  type pers_event = (iper, istr) Def.gen_pers_event
  let get_pevent_name pe = pe.epers_name
  let get_pevent_date pe = pe.epers_date
  let get_pevent_place pe = pe.epers_place
  let get_pevent_reason pe = pe.epers_reason
  let get_pevent_note pe = pe.epers_note
  let get_pevent_src pe = pe.epers_src
  let get_pevent_witnesses pe = Array.map (fun (w, wk,_) -> w, wk) pe.epers_witnesses
  let get_pevent_witness_notes pe = Array.map (fun (_,_,note) -> note) pe.epers_witnesses
  let get_pevent_witnesses_and_notes pe = pe.epers_witnesses
  let gen_pevent_of_pers_event pe = pe
  let pers_event_of_gen_pevent base pe = pe
  let eq_pevent _ _ = assert false

end

module FamEvent = struct
  open Def
  type fam_event = (iper, istr) Def.gen_fam_event
  let get_fevent_name pe = pe.efam_name
  let get_fevent_date pe = pe.efam_date
  let get_fevent_place pe = pe.efam_place
  let get_fevent_reason pe = pe.efam_reason
  let get_fevent_note pe = pe.efam_note
  let get_fevent_src pe = pe.efam_src
  let get_fevent_witnesses pe = Array.map (fun (w, wk,_) -> w, wk) pe.efam_witnesses
  let get_fevent_witness_notes pe = Array.map (fun (_,_,note) -> note) pe.efam_witnesses
  let get_fevent_witnesses_and_notes pe = pe.efam_witnesses
  let gen_fevent_of_fam_event pe = pe
  let fam_event_of_gen_fevent base pe = pe
  let eq_fevent _ _ = assert false
end

include PersEvent
include Person
include FamEvent
include Family

let open_base _ = assert false

let close_base _ = assert false

let sou base istr = base.strings.(istr)

let iper_exists base iper = Utils.is_in_range base.persons iper
let ifam_exists base ifam = Utils.is_in_range base.families ifam

let no_ascend = { Def.parents = None; consang = Adef.no_consang }
let no_union = { Def.family = [||] }
let no_descend = { Def.children = [||] }
let no_couple = Adef.couple dummy_iper dummy_iper
let nb_of_persons base = Array.length base.persons
let nb_of_real_persons base = nb_of_persons base (* TODO : WRONG, FIX *)
let nb_of_families base = Array.length base.families
let bname base = "prout" (* TODO : WRONG FIX *)

let insert array index data =
  let len = Array.length array in
  assert ((index < len && index >= 0) || index = len);
  if index = len then
    let a = Array.init (len + 1) (fun i -> array.(i)) in
    a.(len) <- data;
    a
  else begin
    array.(index) <- data;
    array
  end

(* WRONG INSERTIONS *)
let patch_person base ip p = base.persons.(ip) <- p
let patch_ascend base ip a = base.ascends.(ip) <- a
let patch_union base ip u = base.unions.(ip) <- u

let patch_family base ifam f = base.families.(ifam) <- f
let patch_descend base ifam d = base.descends.(ifam) <- d
let patch_couple base ifam c = base.couples.(ifam) <- c

let insert_string base s =
  let s = Mutil.normalize_utf_8 s in
  let len = Array.length base.strings in
  let a = Array.init (len + 1) (fun i -> base.strings.(i)) in
  a.(len) <- s;
  base.strings <- a

let commit_patches _base = ()
let commit_notes _base = ()

let new_iper base =
  Array.length base.persons

let new_ifam base =
  Array.length base.families

let insert_person base ip p =
  let a = insert base.persons ip p in
  base.persons <- a

let insert_family base ifam f =
  let a = insert base.families ifam f in
  base.families <- a

let insert_ascend base ip ascend =
  let a = insert base.ascends ip ascend in
  base.ascends <- a

let insert_union base ip u =
  let a = insert base.unions ip u in
  base.unions <- a

let insert_descend base ifam d =
  let a = insert base.descends ifam d in
  base.descends <- a

let insert_couple base ifam c =
  let a = insert base.couples ifam c in
  base.couples <- a

let delete_person base ip =
  let delp = Person.create_empty_person empty_string empty_string dummy_iper in
  patch_person base ip delp

let delete_family base ifam =
  let delf = Family.create_empty_family empty_string dummy_ifam in
  patch_family base ifam delf

let delete_ascend base ip asc =
  patch_ascend base ip { parents = None; consang = Adef.no_consang }

let delete_union base ip =
  patch_union base ip { family = [||] }

let delete_descend base ifam =
  patch_descend base ifam { Def.children = [||] }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple dummy_iper dummy_iper)

(* TODO : implement *)
let person_of_key base fn sn occ = assert false

let persons_of_name base name = assert false

let persons_of_first_name base fn = assert false

let persons_of_surname base sn = assert false

let spi_first spi str = assert false

let spi_next spi istr = assert false

let spi_find spi istr = assert false

let base_visible_get bsae pf iper = assert false

let base_visible_write base = assert false
let base_particles base=assert false
let base_strings_of_first_name base str = assert false
let base_strings_of_surname base str = assert false

let load_ascends_array base = assert false
let load_unions_array base = assert false
let load_couples_array base = assert false
let load_descends_array base = assert false
let load_strings_array base = assert false
let load_persons_array base = assert false
let load_families_array base = assert false

let clear_ascends_array base = assert false
let clear_unions_array base = assert false
let clear_couples_array base = assert false
let clear_descends_array base = assert false
let clear_strings_array base = assert false
let clear_persons_array base = assert false
let clear_families_array base = assert false

let read_nldb _ = assert false
let write_nldb _ = assert false

let make = assert false
let sync = assert false
let gc _ = assert false
let set_fpoi_cache base = assert false

module Collection = struct

  let opt_map o f = match o with Some v -> Some (f v) | None -> None
  
  type 'a t = {length : int; get : int -> 'a option; }

  let length c = c.length

  let map f c =
    let get i = opt_map (c.get i) f in
    let length = c.length in
    {length; get}

  let iteri f c =
    let rec aux i =
      if i = c.length then ()
      else
        let _ = opt_map (c.get i) (f i) in
        aux (i + 1)
    in
    aux 0

  let iter f c = iteri (fun _ -> f) c

  let fold ?from ?until fn acc { length; get } =
    let from = match from with Some x -> x | None -> 0 in
    let until = match until with Some x -> x + 1 | None -> length in
    let rec loop acc i =
      if i = until then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc from
  
  let fold_until continue fn acc { get; length } =
    let rec loop acc i =
      if (not (continue acc)) || i = length then acc
      else loop (match get i with Some x -> fn acc x | None -> acc) (i + 1)
    in
    loop acc 0
      
  let iterator { get; length } =
    let cursor = ref 0 in
    let rec next () =
      if !cursor < length then
        match get !cursor with
        | None ->
          incr cursor;
          next ()
        | v ->
          incr cursor;
          v
      else None
    in
    next
end

module Marker = struct
  
  type ('k, 'v) t = { get : 'k -> 'v; set : 'k -> 'v -> unit }

  let make (k : 'a -> int) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t =
    let a = Array.make c.Collection.length i in
    {
      get = (fun x -> Array.get a (k x));
      set = (fun x v -> Array.set a (k x) v);
    }

  let get ({ get; _ } : _ t) k = get k
  let set ({ set; _ } : _ t) k = set k
end

let dummy_marker = assert false
let ifam_marker = assert false
let iper_marker = assert false
let families = assert false
let persons = assert false
let dummy_collection = assert false
let ifams = assert false
let ipers = assert false
let date_of_last_change = assert false
let base_wiznotes_dir = assert false
let base_notes_dir = assert false
let base_notes_origin_file = assert false
let base_notes_are_empty = assert false
let base_notes_read_first_line = assert false
let base_notes_read = assert false
(*
let rec base_notes_are_empty _ = assert false
and base_notes_dir _ = assert false
and base_notes_origin_file _ = assert false
and base_notes_read _ = assert false
and base_notes_read_first_line _ = assert false
and base_particles _ = assert false
and base_strings_of_first_name _ = assert false
and base_strings_of_surname _ = assert false
and base_visible_get _ = assert false
and base_visible_write _ = assert false
and base_wiznotes_dir _ = assert false
and bname _ = assert false
and clear_ascends_array _ = ()
and clear_couples_array _ = ()
and clear_descends_array _ = ()
and clear_families_array _ = ()
and clear_persons_array _ = ()
and clear_strings_array _ = ()
and clear_unions_array _ = ()
and close_base _ = assert false
and commit_notes _ = assert false
and commit_patches _ = assert false
and date_of_last_change _ = assert false
and delete_ascend _ = assert false
and delete_couple _ = assert false
and delete_descend _ = assert false
and delete_family _ = assert false
and delete_person _ = assert false
and delete_union _ = assert false
and dummy_collection _ = assert false

and dummy_marker _ = assert false
and empty_family _ = assert false
and empty_person _ = assert false
and empty_string = 0
and eq_istr = ( = )
and families ?select:_ _ = assert false
and family_of_gen_family _ = assert false
and foi base i = (base, i)
and gen_ascend_of_person _ = assert false
and gen_couple_of_family _ = assert false
and gen_descend_of_family _ = assert false
and gen_family_of_family _ = assert false
and gen_person_of_person _ = assert false
and gen_union_of_person _ = assert false
and get_access _ = assert false
and get_aliases _ = assert false
and get_baptism _ = assert false
and get_baptism_note _ = assert false
and get_baptism_place _ = assert false
and get_baptism_src _ = assert false
and get_birth _ = assert false
and get_birth_note _ = assert false
and get_birth_place _ = assert false
and get_birth_src _ = assert false
and get_burial _ = assert false
and get_burial_note _ = assert false
and get_burial_place _ = assert false
and get_burial_src _ = assert false
and get_children _ = assert false
and get_comment _ = assert false
and get_consang _ = assert false
and get_death _ = assert false
and get_death_note _ = assert false
and get_death_place _ = assert false
and get_death_src _ = assert false
and get_divorce _ = assert false
and get_family (base, i) = (Array.get base.unions i).family
and get_father (base, i) = Adef.father (Array.get base.couples i)
and get_fevents _ = assert false
and get_first_name _ = assert false
and get_first_names_aliases _ = assert false
and get_fsources _ = assert false
and get_ifam (_base, i) = i
and get_image _ = assert false
and get_iper (_base, i) = i
and get_marriage _ = assert false
and get_marriage_note _ = assert false
and get_marriage_place _ = assert false
and get_marriage_src _ = assert false
and get_mother (base, i) = Adef.mother (Array.get base.couples i)
and get_notes _ = assert false
and get_occ _ = assert false
and get_occupation _ = assert false
and get_origin_file _ = assert false
and get_parent_array _ = assert false
and get_parents (base, i) = (Array.get base.ascends i).parents
and get_pevents _ = assert false
and get_psources _ = assert false
and get_public_name _ = assert false
and get_qualifiers _ = assert false
and get_related _ = assert false
and get_relation _ = assert false
and get_rparents _ = assert false
and get_sex _ = assert false
and get_surname _ = assert false
and get_surnames_aliases _ = assert false
and get_titles _ = assert false
and get_witnesses _ = assert false
and ifam_exists _ = assert false
and ifam_marker _ = assert false

and ifams ?select:_ _ = assert false
and insert_ascend _ = assert false
and insert_couple _ = assert false
and insert_descend _ = assert false
and insert_family _ = assert false
and insert_person _ = assert false
and insert_string _ = assert false
and insert_union _ = assert false
and iper_exists _ = assert false
and iper_marker _ = assert false

and ipers _ = assert false
and is_empty_string i = eq_istr empty_string i
and is_quest_string i = eq_istr quest_string i

and load_ascends_array _ = ()
and load_couples_array _ = ()
and load_descends_array _ = ()
and load_families_array _ = ()
and load_persons_array _ = ()
and load_strings_array _ = ()
and load_unions_array _ = ()

and make _ _
    ((persons, ascends, unions), (families, couples, descends), strings, _) =
  { persons; ascends; unions; families; couples; descends; strings }

and nb_of_families _ = assert false
and nb_of_persons _ = assert false
and nb_of_real_persons _ = assert false
and new_ifam _ = assert false
and new_iper _ = assert false
and no_ascend = { Def.parents = None; consang = Adef.no_consang }
and no_descend = { Def.children = [||] }
and no_family ifam = { (Mutil.empty_family empty_string) with fam_index = ifam }

and no_person ip =
  { (Mutil.empty_person empty_string quest_string) with key_index = ip }

and no_union = { Def.family = [||] }

and patch_ascend _ = assert false
and patch_couple _ = assert false
and patch_descend _ = assert false
and patch_family _ = assert false
and patch_person _ = assert false
and patch_union _ = assert false
and person_of_gen_person _ = assert false
and person_of_key _ = assert false
and persons _ = assert false
and persons_of_first_name _ = assert false
and persons_of_name _ = assert false
and persons_of_surname _ = assert false
and poi base i = (base, i)
and quest_string = 1
and read_nldb _ = assert false
and sou _ = assert false
and spi_find _ = assert false
and spi_first _ = assert false
and spi_next _ = assert false

and sync ?scratch:_ = assert false
and write_nldb _ = assert false



and set_fpoi_cache _ = assert false

let no_couple = Adef.couple dummy_iper dummy_iper
let eq_iper _ = assert false
let eq_ifam _ = assert false
let gc ?dry_run:_ ~save_mem:_ _ = assert false
*)
