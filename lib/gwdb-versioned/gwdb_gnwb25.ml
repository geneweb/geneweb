(*include Gwdb_legacy.Gwdb_driver*)

open Geneweb_dsk_format

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
type person
type family

type base = {
    persons_inchan : person Partition.inchan option
  }



let open_base bname = assert false
let close_base : base -> unit = not_impl
let empty_person : base -> iper -> person = not_impl
let get_access : person -> Def.access = not_impl
let get_aliases : person -> istr list = not_impl
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
let get_notes : person -> iper = not_impl
let get_occ : person -> istr = not_impl
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
let person_of_gen_person :  base -> (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union -> person = not_impl
let poi : base -> iper -> person = not_impl
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
