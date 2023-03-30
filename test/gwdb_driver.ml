


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

type family = base * (iper, ifam, istr) Def.gen_family
type person = base * (iper, iper, istr) Def.gen_person
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
    base, create_empty_person empty_string empty_string iper

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
  let get_parents (base, p) = base.ascends.(p.key_index).parents
  let get_pevents (_, p) = p.pevents
  let get_psources (_, p) = p.psources
  let get_related (_, p) = p.related
  let get_rparents (_, p) = p.rparents
  let get_sex (_, p) = p.sex
  let get_titles (_, p) = p.titles

  let gen_person_of_person (_, p) = p
  let gen_ascend_of_person (base, p) = base.ascends.(p.key_index)
  let gen_union_of_person (base, p) = base.unions.(p.key_index)
  let person_of_gen_person base (p, a, u) = base, p
  let poi base iper = base, base.persons.(iper)

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
 let get_children (base, f) = base.descends.(f.fam_index).children
 let get_father (base, f) = base.couples.(f.fam_index) |> Adef.father
 let get_mother (base, f) = base.couples.(f.fam_index) |> Adef.mother
 let get_parent_array (base, f) = base.couples.(f.fam_index) |> Adef.parent_array
 let get_related (_, f) = f.related
 let get_relation (_, f) = f.relation
 let gen_couple_of_family (base, f) = base.couples.(f.fam_index)
 let gen_descend_of_family (base, f) = base.descends.(f.fam_index)
 let gen_family_of_family (_, f) = f
 let family_of_gen_family base (f, c, d) = base, f
 let foi base ifam = base, base.families.(ifam)
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

let open_base _ = print_endline "open"; assert false

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
  base.strings <- a;
  len

let commit_patches _base = ()
let commit_notes _base _ _ = ()

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

let delete_ascend base ip =
  patch_ascend base ip { parents = None; consang = Adef.no_consang }

let delete_union base ip =
  patch_union base ip { family = [||] }

let delete_descend base ifam =
  patch_descend base ifam { Def.children = [||] }

let delete_couple base ifam =
  patch_couple base ifam (Adef.couple dummy_iper dummy_iper)


let make _bname _particles ((persons, ascends, unions), (families, couples, descends), strings, _notes) =
  { persons; ascends; unions; families; couples; descends; strings }
  
let sync ?scratch ~save_mem _base = assert false
let gc ?dry_run ~save_mem base = assert false
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

(* TODO : implement *)
let person_of_key base fn sn occ = assert false

let persons_of_name base name = assert false

let persons_of_first_name base = assert false

let persons_of_surname base = assert false

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

let dummy_marker base = assert false
let ifam_marker _ = assert false
let iper_marker _ = assert false
let families ?select _ = assert false
let persons _ = assert false
let dummy_collection _ = assert false
let ifams ?select _ = assert false
let ipers _ = assert false
let date_of_last_change _ = assert false
let base_wiznotes_dir _ = assert false
let base_notes_dir _ = assert false
let base_notes_origin_file _ = assert false
let base_notes_are_empty _ _ = true (* TODO : change later     *)
let base_notes_read_first_line _ = assert false 
let base_notes_read _ = assert false
let read_nldb _ = []
let write_nldb _ = assert false
