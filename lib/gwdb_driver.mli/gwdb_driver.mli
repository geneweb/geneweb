(* Copyright (c) 1998-2007 INRIA *)

type iper
type ifam
type istr

val string_of_iper : iper -> string
val string_of_ifam : ifam -> string
val string_of_istr : istr -> string

val iper_of_string : string -> iper
val ifam_of_string : string -> ifam
val istr_of_string : string -> istr

type person
type family

type relation = (iper, istr) Def.gen_relation
type title = istr Def.gen_title
type pers_event = (iper, istr) Def.gen_pers_event
type fam_event = (iper, istr) Def.gen_fam_event

type string_person_index

type base

val open_base : string -> base
val close_base : base -> unit

val dummy_iper : iper
val dummy_ifam : ifam

val eq_istr : istr -> istr -> bool
val is_empty_string : istr -> bool
val is_quest_string : istr -> bool
val empty_string : istr
val quest_string : istr
val empty_person : base -> iper -> person
val empty_family : base -> ifam -> family

val get_access : person -> Def.access
val get_aliases : person -> istr list
val get_baptism : person -> Def.cdate
val get_baptism_note : person -> istr
val get_baptism_place : person -> istr
val get_baptism_src : person -> istr
val get_birth : person -> Def.cdate
val get_birth_note : person -> istr
val get_birth_place : person -> istr
val get_birth_src : person -> istr
val get_burial : person -> Def.burial
val get_burial_note : person -> istr
val get_burial_place : person -> istr
val get_burial_src : person -> istr
val get_children : family -> iper array
val get_comment : family -> istr
val get_consang : person -> Adef.fix
val get_death : person -> Def.death
val get_death_note : person -> istr
val get_death_place : person -> istr
val get_death_src : person -> istr
val get_divorce : family -> Def.divorce
val get_family : person -> ifam array
val get_father : family -> iper
val get_fevents : family -> fam_event list
val get_first_name : person -> istr
val get_first_names_aliases : person -> istr list
val get_fsources : family -> istr
val get_ifam : family -> ifam
val get_image : person -> istr
val get_iper : person -> iper
val get_marriage : family -> Def.cdate
val get_marriage_note : family -> istr
val get_marriage_place : family -> istr
val get_marriage_src : family -> istr
val get_mother : family -> iper
val get_notes : person -> istr
val get_occ : person -> int
val get_occupation : person -> istr
val get_origin_file : family -> istr
val get_parent_array : family -> iper array
val get_parents : person -> ifam option
val get_pevents : person -> pers_event list
val get_psources : person -> istr
val get_public_name : person -> istr
val get_qualifiers : person -> istr list
val get_related : person -> iper list
val get_relation : family -> Def.relation_kind
val get_rparents : person -> relation list
val get_sex : person -> Def.sex
val get_surname : person -> istr
val get_surnames_aliases : person -> istr list
val get_titles : person -> title list
val get_witnesses : family -> iper array

val gen_couple_of_family : family -> iper Def.gen_couple
val gen_descend_of_family : family -> iper Def.gen_descend
val gen_family_of_family : family -> (iper, ifam, istr) Def.gen_family
val gen_person_of_person : person -> (iper, iper, istr) Def.gen_person
val gen_ascend_of_person : person -> ifam Def.gen_ascend
val gen_union_of_person : person -> ifam Def.gen_union

val family_of_gen_family : base -> (iper, ifam, istr) Def.gen_family * iper Def.gen_couple * iper Def.gen_descend -> family
val person_of_gen_person : base -> (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union -> person

val poi : base -> iper -> person
val foi : base -> ifam -> family
val sou : base -> istr -> string

val no_person : iper -> (iper, iper, istr) Def.gen_person
val no_ascend : ifam Def.gen_ascend
val no_union : ifam Def.gen_union
val no_family : ifam -> (iper, ifam, istr) Def.gen_family
val no_descend :iper Def.gen_descend
val no_couple : iper Def.gen_couple

val nb_of_persons : base -> int
val nb_of_real_persons : base -> int
val nb_of_families : base -> int
val bname : base -> string

val patch_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
val patch_ascend : base -> iper -> ifam Def.gen_ascend -> unit
val patch_union : base -> iper -> ifam Def.gen_union -> unit
val patch_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
val patch_descend : base -> ifam -> iper Def.gen_descend -> unit
val patch_couple : base -> ifam -> iper Def.gen_couple -> unit

val insert_string : base -> string -> istr
val commit_patches : base -> unit
val commit_notes : base -> string -> string -> unit

val new_iper : base -> iper
val new_ifam : base -> ifam

val insert_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
val insert_ascend : base -> iper -> ifam Def.gen_ascend -> unit
val insert_union : base -> iper -> ifam Def.gen_union -> unit
val insert_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
val insert_descend : base -> ifam -> iper Def.gen_descend -> unit
val insert_couple : base -> ifam -> iper Def.gen_couple -> unit

val delete_person : base -> iper -> unit
val delete_ascend : base -> iper -> unit
val delete_union : base -> iper -> unit
val delete_family : base -> ifam -> unit
val delete_descend : base -> ifam -> unit
val delete_couple : base -> ifam -> unit

val person_of_key : base -> string -> string -> int -> iper option
val persons_of_name : base -> string -> iper list
val persons_of_first_name : base -> string_person_index
val persons_of_surname : base -> string_person_index

(** first [first/sur]name starting with that string *)
val spi_first : string_person_index -> string -> istr

(** next [first/sur]name by Gutil.alphabetical order *)
val spi_next : string_person_index -> istr -> istr

(** all persons having that [first/sur]name *)
val spi_find : string_person_index -> istr -> iper list

val base_visible_get : base -> (person -> bool) -> iper -> bool
val base_visible_write : base -> unit
val base_particles : base -> string list

(** [base_strings_of_first_name base x]
    Return the list of first names (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison.
*)
val base_strings_of_first_name : base -> string -> istr list

(** [base_strings_of_surname base x]
    Return the list of surnames (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison.
*)
val base_strings_of_surname : base -> string -> istr list

val load_ascends_array : base -> unit
val load_unions_array : base -> unit
val load_couples_array : base -> unit
val load_descends_array : base -> unit
val load_strings_array : base -> unit
val load_persons_array : base -> unit
val load_families_array : base -> unit

val clear_ascends_array : base -> unit
val clear_unions_array : base -> unit
val clear_couples_array : base -> unit
val clear_descends_array : base -> unit
val clear_strings_array : base -> unit
val clear_persons_array : base -> unit
val clear_families_array : base -> unit

val base_notes_read : base -> string -> string
val base_notes_read_first_line : base -> string -> string
val base_notes_are_empty : base -> string -> bool
val base_notes_origin_file : base -> string
val base_notes_dir : base -> string
val base_wiznotes_dir : base -> string

val date_of_last_change : base -> float

module Collection : sig

  (** Collections are sets of elements you want to traverse. *)
  type 'a t

  (** Return the number of elements of a colletion *)
  val length : 'a t -> int

  (** [map fn c]
      Return a collection corresponding to [c]
      where [fn] would have been applied to each of its elements.
  *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [iter fn c]
      Apply [fn] would have been applied to each elements of [c].
  *)
  val iter : ('a -> unit) -> 'a t -> unit

  (** [iter fn c]
      Apply [fn i] would have been applied to each elements of [c]
      where [i] is the index (starting with 0) of the element.
  *)
  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  (** [fold fn acc c]
      Combine each element of [c] into a single value using [fn].
      [fn] first argument is the result computed so far as we traverse the
      collection, and second element is the current element being combined.
      [acc] is the starting combined value.
      Start at [from]-nth and finish with [until]-nth element (included).
  *)
  val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [fold_until continue fn acc c]
      Same as [fold fn acc c], but computation stops as soon as [continue]
      is not satisfied by combined value anymore.
  *)
  val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

  (** [iterator c]
      Return a function returning [Some next_element] when it is called,
      or [None] if you reached the end of the collection.
  *)
  val iterator : 'a t -> (unit -> 'a option)

end

module Marker : sig

  (** Markers are way to annotate (add extra information to) elements of a {!val:Collection.t}. *)
  type ('k, 'v) t

  (** [get marker key]
      Return the annotation associated to [key].
  *)
  val get : ('k, 'v) t -> 'k -> 'v

  (** [set marker key value]
      Set [value] as annotation associated to [key].
  *)
  val set : ('k, 'v) t -> 'k -> 'v -> unit

end

(** {2 Useful collections} *)

val ipers : base -> iper Collection.t
val persons : base -> person Collection.t
val ifams : base -> ifam Collection.t
val families : base -> family Collection.t

(** [dummy_collection x] create a dummy collection with no element.
    [x] is only used for typing.
    Useful for placeholders or for typing purpose. *)
val dummy_collection : 'a -> 'a Collection.t

(** {2 Useful markers} *)

val iper_marker : iper Collection.t -> 'a -> (iper, 'a) Marker.t
val ifam_marker : ifam Collection.t -> 'a -> (ifam, 'a) Marker.t

(** [dummy_marker k v] create a dummy collection with no element.
    [k] and [v] are only used for typing.
    Useful for placeholders or for typing purpose. *)
val dummy_marker : 'a -> 'b -> ('a, 'b) Marker.t

(** {2 Database creation} *)

(** [make bname particles arrays] create a base with [bname] name and [arrays] as content. *)
val make
  : string
  -> string list
  -> ( ( (int, int, int) Def.gen_person array
         * int Def.gen_ascend array
         * int Def.gen_union array )
       * ( (int, int, int) Def.gen_family array
           * int Def.gen_couple array
           * int Def.gen_descend array )
       * string array
       * Def.base_notes )
  -> base

val read_nldb : base -> (iper, ifam) Def.NLDB.t
val write_nldb : base -> (iper, ifam) Def.NLDB.t -> unit

(** [sync scratch base]
    Ensure that everything is synced on disk.

    Depending on the backend,
    it may perform various operation such as indexes rebuilding,
    and it might be a lengthy operation.

    Use [scratch] (default false) to sync and rebuild
    the whole database. Otherwise, only changes that occured
    since the last [sync] call are treated.
*)
val sync : ?scratch:bool -> base -> unit
