(*include Gwdb_legacy.Gwdb_driver*)

open Geneweb_dsk_format

module GLegacy = Gwdb_legacy.Gwdb_driver
   
let not_impl _ = assert false

type iper = int
type istr = int
type ifam = int

type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event
type relation = (iper, istr) Def.gen_relation
type couple = iper Def.gen_couple
type title = istr Def.gen_title
type descend = iper Def.gen_descend
type gen_family = (iper, ifam, istr) Def.gen_family



type string_person_index

type g25_person
type person =
  | Legacy_person of Gwdb_legacy.Gwdb_driver.person
  | G25_person of g25_person
   
type family
(*
module PersonAccess =
  Partition_system.MakeAccessPartition (struct
      let filename = "persons_access.acc.gnw25"
      let ipart = Partition_system.solo_partition_ipart
    end)

module PersonPartition =
  Partition_system.MakeVariableSize
    (struct
      type data = person
      let to_bytes = not_impl
      let of_bytes = not_impl
      let filename = "persons.data.gnw25"
      let ipart = Partition_system.solo_partition_ipart
    end)
    (PersonAccess)
 *)
type gnwb25_base
   
type base_extension = {
    witness_notes_read : person -> istr;
    witness_notes_write : person -> unit
  }

type legacy_compat_base = {
    legacy_base : Gwdb_legacy.Gwdb_driver.base;
    gnwb25_extension : base_extension
  }

type base =
  | UpToDate of gnwb25_base
  | Legacy of legacy_compat_base

(* TODO wrong *)
let wrap_base base f g = match base with
  | Legacy b -> f b
  | UpToDate b -> g b

let wrap_person person f g = match person with
  | Legacy_person p -> f p
  | G25_person p -> g p
                
(* {
    persons_inchan : person Partition.inchan option
  }*)

let open_base_extension bname =
  {
    witness_notes_read = not_impl;
    witness_notes_write = not_impl
  }

let open_legacy_base bname =
  let legacy_base = Gwdb_legacy.Gwdb_driver.open_base bname in
  let gnwb25_extension = open_base_extension bname in
  {legacy_base; gnwb25_extension}

let open_gnwb25_base bname = assert false
  
let open_base bname =
  
  let ic = Secure.open_in_bin (Filename.concat bname "base") in

  let version_opt =
    try
      let v = really_input_string ic 8 in
      print_endline ("VERSION FOUND : " ^ v);
      Geneweb_dsk_format.Version.check_version v
    with _ -> failwith "could not find version number in base"
  in
  
  match version_opt with
  | Some Geneweb_dsk_format.Version.GnWb25 ->
     print_endline "====================YATA25================";
     UpToDate (open_gnwb25_base bname)
  | Some v ->
     print_endline @@ "====================YATAV================" ^ (Geneweb_dsk_format.Version.string_of_version v);
(*     Dsk_format.test bname;
     print_endline "====================TEST PASSED================";*)
     Legacy (open_legacy_base bname)
  | None -> assert false (* should not happen *)

let close_base = not_impl
let close_base base = wrap_base base (fun b -> GLegacy.close_base b.legacy_base) close_base

let empty_person = not_impl
let empty_person base iper =
  wrap_base base
    (fun b -> GLegacy.empty_person b.legacy_base iper)
    (fun b -> empty_person b iper)

let get_access person = assert false                
let get_access person =
  wrap_person person GLegacy.get_access get_access

let get_aliases person = assert false
let get_aliases person =
  wrap_person person GLegacy.get_aliases get_aliases

let get_baptism : person -> Adef.cdate = not_impl
let get_baptism_note : person -> istr = not_impl
let get_baptism_place : person -> istr = not_impl
let get_baptism_src : person -> istr = not_impl
let get_birth : person -> Adef.cdate = not_impl
let get_birth_note : person -> istr = not_impl
let get_birth_place : person -> istr = not_impl
let get_birth_src : person -> istr = not_impl
let get_burial : person -> Def.burial = not_impl
let get_burial_note : person -> istr = not_impl
let get_burial_place : person -> istr = not_impl
let get_burial_src : person -> istr = not_impl
let get_consang : person -> Adef.fix = not_impl
let get_death : person -> Def.death = not_impl
let get_death_note : person -> istr = not_impl
let get_death_place : person -> istr = not_impl
let get_death_src : person -> istr = not_impl
let get_family : person -> ifam array = not_impl
let get_first_name : person -> istr = not_impl
let get_first_names_aliases : person -> istr list = not_impl
let get_image : person -> istr = not_impl
let get_iper : person -> iper = not_impl
let get_notes : person -> istr = not_impl
let get_occ : person -> int = not_impl
let get_occupation : person -> istr = not_impl
let get_parents : person -> ifam option = not_impl
let get_pevents : person -> pers_event list = not_impl
let get_psources : person -> istr = not_impl
let get_public_name : person -> istr = not_impl
let get_qualifiers : person -> istr list = not_impl
let get_related : person -> iper list = not_impl
let get_rparents : person -> relation list = not_impl
let get_sex : person -> Def.sex = not_impl
let get_surname : person -> istr = not_impl
let get_surnames_aliases : person -> istr list = not_impl
let get_titles : person -> title list = not_impl
let gen_person_of_person : person -> (iper, iper, istr) Def.gen_person = not_impl
let gen_ascend_of_person : person -> ifam Def.gen_ascend = not_impl
let gen_union_of_person : person -> ifam Def.gen_union = not_impl

let legacy_person_of_gen_person b (p, a, u) =
  let p = Translate.as_legacy_person p in
  Legacy_person (Gwdb_legacy.Gwdb_driver.person_of_gen_person b.legacy_base (p, a, u))
let g25_person_of_gen_person b (p, a, u) =
  assert false

let person_of_gen_person :  base -> (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union -> person =
  fun base (p, a, u) ->
  wrap_base base legacy_person_of_gen_person g25_person_of_gen_person (p, a, u)

let legacy_poi b iper =
  Legacy_person (Gwdb_legacy.Gwdb_driver.poi b.legacy_base iper)
let g25_poi b iper = assert false
let poi : base -> iper -> person = fun base ->
  wrap_base base legacy_poi g25_poi

let base_visible_get : base -> (person -> bool) -> iper -> bool = not_impl

module Collection : sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val length : 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val fold : from:int option -> until:int option -> ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_until : ('b -> bool) -> ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val iterator : 'a t -> unit -> 'a option
end = struct
  type 'a t
  let map _ = not_impl
  let length = not_impl
  let iter = not_impl
  let iteri = not_impl
  let fold ~from ~until = not_impl
  let fold_until = not_impl
  let iterator = not_impl
end
module Marker : sig

  type ('k, 'v) t

  val get : ('k, 'v) t -> 'k -> 'v

  val set : ('k, 'v) t -> 'k -> 'v -> unit

  val make : ('a -> int) -> 'a Collection.t -> 'v -> ('a, 'v) t
end = struct

  type ('k, 'v) t

  let get m k = assert false
  let set m k = assert false

  let make (k:'a -> int) (c : 'a Collection.t) (i : 'v) : ('a, 'v) t = assert false

end


let persons : base -> person Collection.t = not_impl
let empty_family : base -> ifam -> family = not_impl
let iper_exists : base -> iper -> bool = not_impl
let get_children : family -> iper array = not_impl
let get_comment : family -> istr = not_impl
let get_divorce : family -> Def.divorce = not_impl
let get_father : family -> iper = not_impl
let get_mother : family -> iper = not_impl
let get_fevents : family -> fam_event list = not_impl
let get_fsources : family -> istr = not_impl
let get_ifam : family -> ifam = not_impl
let get_marriage : family -> Def.cdate = not_impl
let get_marriage_note : family -> istr = not_impl
let get_marriage_place : family -> istr = not_impl
let get_marriage_src : family -> istr = not_impl
let get_origin_file : family -> istr = not_impl
let get_parent_array : family -> iper array = not_impl
let get_relation : family -> Def.relation_kind = not_impl
let get_witnesses : family -> iper array = not_impl
let gen_couple_of_family : family -> couple = not_impl
let gen_descend_of_family : family -> descend = not_impl
let gen_family_of_family : family -> gen_family = not_impl
let family_of_gen_family : base -> (gen_family * couple * descend) -> family = not_impl
let foi : base -> ifam -> family = not_impl
let persons : base -> person Collection.t = not_impl
let ipers : base -> iper Collection.t = not_impl
let ifams : base -> ?select:(ifam -> bool) -> ifam Collection.t = not_impl
let families : ?select:(family -> bool) -> base -> family Collection.t =
  fun ?(select = fun _ -> true) -> not_impl

let iper_marker c i = Marker.make (fun i -> i) c i
let ifam_marker c i = Marker.make (fun i -> i) c i

let spi_first : string_person_index -> string -> istr = not_impl
let spi_next : string_person_index -> istr -> istr = not_impl
let spi_find : string_person_index -> istr -> iper list = not_impl
let persons_of_first_name : base -> string_person_index = not_impl
let persons_of_surname : base -> string_person_index = not_impl
let no_person : iper -> (iper, ifam, istr) Def.gen_person = not_impl
(*let no_ascend : ifam Def.gen_ascend = assert false
let no_union : ifam Def.gen_union = {family = [||] }
let no_family : ifam -> (iper, ifam, istr) Def.gen_family = not_impl
let no_couple : iper -> (iper, ifam, istr) Def.gen_person = not_impl
let no_descend : iper -> (iper, ifam, istr) Def.gen_person = not_impl*)
let patch_person : base -> iper -> (iper, ifam, istr) Def.gen_person -> unit = not_impl
let insert_person : base -> iper -> (iper, ifam, istr) Def.gen_person -> unit = not_impl

let make :
      string ->
      string list ->
      (int, int, int) Def.gen_person array ->
      int Def.gen_ascend array ->
      int Def.gen_union array ->
      (int, int, int) Def.gen_family array ->
      int Def.gen_couple array ->
      int Def.gen_descend array ->
      string array ->
      Def.base_notes ->
      base
  = fun bname particles persons ascends unions families couples descends strings notes ->
  assert false