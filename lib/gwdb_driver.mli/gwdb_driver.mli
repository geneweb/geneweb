(* Copyright (c) 1998-2007 INRIA *)

type istr
(** String id *)

type ifam
(** Family id *)

type iper
(** Person id *)

val string_of_iper : iper -> string
(** Convert [iper] to string *)

val string_of_ifam : ifam -> string
(** Convert [ifam] to string *)

val string_of_istr : istr -> string
(** Convert [istr] to string *)

val iper_of_string : string -> iper
(** Convert [iper] from string *)

val ifam_of_string : string -> ifam
(** Convert [ifam] from string *)

val istr_of_string : string -> istr
(** Convert [istr] from string *)

val compare_iper : iper -> iper -> int
(** Comparison over ipers **)

val compare_ifam : ifam -> ifam -> int
(** Comparison over ifams **)

val compare_istr : istr -> istr -> int
(** Comparison over istrs **)

type person
(** Person data structure *)

type family
(** Family data structure *)

type relation = (iper, istr) Def.gen_relation
(** Database implementation for [Def.gen_relation] *)

type title = istr Def.gen_title
(** Database implementation for [Def.gen_title] *)

type string_person_index
(** Data structure for optimised search throughout index by name
    (surname or first name). *)

type base
(** The database representation. *)

type pers_event
(** Database implementation for [Def.pers_event] *)
(*= (iper, istr) Def.gen_pers_event*)

val get_pevent_name : pers_event -> istr Def.gen_pers_event_name
val get_pevent_date : pers_event -> Def.cdate
val get_pevent_place : pers_event -> istr
val get_pevent_reason : pers_event -> istr
val get_pevent_note : pers_event -> istr
val get_pevent_src : pers_event -> istr
val get_pevent_witnesses : pers_event -> (iper * Def.witness_kind) array
val get_pevent_witness_notes : pers_event -> istr array

val get_pevent_witnesses_and_notes :
  pers_event -> (iper * Def.witness_kind * istr) array

val gen_pevent_of_pers_event : pers_event -> (iper, istr) Def.gen_pers_event

val pers_event_of_gen_pevent :
  base -> (iper, istr) Def.gen_pers_event -> pers_event

val eq_pevent : pers_event -> pers_event -> bool

type fam_event
(** Database implementation for [Def.fam_event] *)
(*= (iper, istr) Def.gen_fam_event*)

val get_fevent_name : fam_event -> istr Def.gen_fam_event_name
val get_fevent_date : fam_event -> Def.cdate
val get_fevent_place : fam_event -> istr
val get_fevent_reason : fam_event -> istr
val get_fevent_note : fam_event -> istr
val get_fevent_src : fam_event -> istr
val get_fevent_witnesses : fam_event -> (iper * Def.witness_kind) array
val get_fevent_witness_notes : fam_event -> istr array

val get_fevent_witnesses_and_notes :
  fam_event -> (iper * Def.witness_kind * istr) array

val gen_fevent_of_fam_event : fam_event -> (iper, istr) Def.gen_fam_event

val fam_event_of_gen_fevent :
  base -> (iper, istr) Def.gen_fam_event -> fam_event

val eq_fevent : fam_event -> fam_event -> bool

val open_base : string -> base
(** Open database associated with (likely situated in) the specified directory. *)

val close_base : base -> unit
(** Close database. May perform some clean up tasks. *)

val dummy_iper : iper
(** Dummy person id *)

val dummy_ifam : ifam
(** Dummy family id *)

val eq_istr : istr -> istr -> bool
(** [true] if strings with the giving ids are equal *)

val eq_ifam : ifam -> ifam -> bool
(** [true] if families with the giving ids are equal *)

val eq_iper : iper -> iper -> bool
(** [true] if persons with the giving ids are equal *)

val is_empty_string : istr -> bool
(** [true] if string with the giving id is empty ("") *)

val is_quest_string : istr -> bool
(** [true] if string with the giving id is a question mark ("?") *)

val empty_string : istr
(** Id of the empty string ("") *)

val quest_string : istr
(** Id of the question mark ("?") *)

val empty_person : base -> iper -> person
(** Returns unitialised person with the giving id. *)

val empty_family : base -> ifam -> family
(** Returns unitialised family with the giving id. *)

val iper_exists : base -> iper -> bool
(** Tells if person with giving id exists in the base. *)

val ifam_exists : base -> ifam -> bool
(** Tells if family with giving id exists in the base. *)

(** {2 Getters}
    Getters are used to extract information about person and family.
    If corresponding information part isn't present, driver load it from
    the disk and cache it so further gets will return result immediately. *)

val get_access : person -> Def.access
(** Get privacy settings that define access to person's data *)

val get_aliases : person -> istr list
(** Get person's aliases ids *)

val get_baptism : person -> Def.cdate
(** Get person's baptism date *)

val get_baptism_note : person -> istr
(** Get person's baptism note id *)

val get_baptism_place : person -> istr
(** Get person's baptism place id *)

val get_baptism_src : person -> istr
(** Get person's baptism source id *)

val get_birth : person -> Def.cdate
(** Get person's birth date *)

val get_birth_note : person -> istr
(** Get person's birth note id *)

val get_birth_place : person -> istr
(** Get person's birth place id *)

val get_birth_src : person -> istr
(** Get person's birth source id *)

val get_burial : person -> Def.burial
(** Get information about person's burial *)

val get_burial_note : person -> istr
(** Get person's burial note id *)

val get_burial_place : person -> istr
(** Get person's burial place id *)

val get_burial_src : person -> istr
(** Get person's burial source id *)

val get_children : family -> iper array
(** Get array of family's children ids *)

val get_comment : family -> istr
(** Get family's comment (notes) id *)

val get_consang : person -> Adef.fix
(** Get person's consanguinity degree with his ascendants *)

val get_death : person -> Def.death
(** Get person's death status *)

val get_death_note : person -> istr
(** Get person's death note id *)

val get_death_place : person -> istr
(** Get person's death place id *)

val get_death_src : person -> istr
(** Get person's death source id *)

val get_divorce : family -> Def.divorce
(** Get family's divorce status *)

val get_family : person -> ifam array
(** Get array of family's ids to which a person belongs as parent (person's union) *)

val get_father : family -> iper
(** Get family's father id (from the family's couple) *)

val get_fevents : family -> fam_event list
(** Get family's event list *)

val get_first_name : person -> istr
(** Get person's first name id *)

val get_first_names_aliases : person -> istr list
(** Get list of person's first name aliases ids *)

val get_fsources : family -> istr
(** Get family's sources id *)

val get_ifam : family -> ifam
(** Get family's id *)

val get_image : person -> istr
(** Get id of path to person's image *)

val get_iper : person -> iper
(** Get person's id *)

val get_marriage : family -> Def.cdate
(** Get family's marriage date *)

val get_marriage_note : family -> istr
(** Get family's marriage note id *)

val get_marriage_place : family -> istr
(** Get family's marriage place id *)

val get_marriage_src : family -> istr
(** Get family's marriage source id *)

val get_mother : family -> iper
(** Get family's mother id (from the family's couple) *)

val get_notes : person -> istr
(** Get person's notes id *)

val get_occ : person -> int
(** Get person's occurence number *)

val get_occupation : person -> istr
(** Get person's occupation id *)

val get_origin_file : family -> istr
(** Get family's origin file (e.g. a .gw or .ged filename) id *)

val get_parent_array : family -> iper array
(** Get family's parents ids (father and mother from family's couple) *)

val get_parents : person -> ifam option
(** Get person's family id to which his parents belong (as family's couple) *)

val get_pevents : person -> pers_event list
(** Get person's event list *)

val get_psources : person -> istr
(** Get person's sources id *)

val get_public_name : person -> istr
(** Get person's public name id *)

val get_qualifiers : person -> istr list
(** Get list of person's qualifiers ids *)

val get_related : person -> iper list
(** Get person's related persons ids *)

val get_relation : family -> Def.relation_kind
(** Get relation kind between couple in the family *)

val get_rparents : person -> relation list
(** Get person's relations with not native parents *)

val get_sex : person -> Def.sex
(** Get person's sex *)

val get_surname : person -> istr
(** Get person's surname id *)

val get_surnames_aliases : person -> istr list
(** Get person's surname aliases ids *)

val get_titles : person -> title list
(** Get list of person's nobility titles *)

val get_witnesses : family -> iper array
(** Get array of family's witnesses ids *)

val gen_couple_of_family : family -> iper Def.gen_couple
(** Extract [gen_couple] from [family]. *)

val gen_descend_of_family : family -> iper Def.gen_descend
(** Extract [gen_descend] from [family]. *)

val gen_family_of_family : family -> (iper, ifam, istr) Def.gen_family
(** Extract [gen_family] from [family]. *)

val gen_person_of_person : person -> (iper, iper, istr) Def.gen_person
(** Extract [gen_person] from [person]. *)

val gen_ascend_of_person : person -> ifam Def.gen_ascend
(** Extract [gen_ascend] from [person]. *)

val gen_union_of_person : person -> ifam Def.gen_union
(** Extract [gen_union] from [person]. *)

val family_of_gen_family :
  base ->
  (iper, ifam, istr) Def.gen_family * iper Def.gen_couple * iper Def.gen_descend ->
  family
(** Create [family] from associated values. *)

val person_of_gen_person :
  base ->
  (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend * ifam Def.gen_union ->
  person
(** Create [person] from associated values. *)

val poi : base -> iper -> person
(** Create uninitialised person with giving id *)

val foi : base -> ifam -> family
(** Create uninitialised family with giving id *)

val sou : base -> istr -> string
(** Returns string that has giving id from the base *)

val no_person : iper -> (iper, iper, istr) Def.gen_person
(** Returns unitialised [gen_person] with giving id *)

val no_ascend : ifam Def.gen_ascend
(** Returns unitialised [gen_ascend] *)

val no_union : ifam Def.gen_union
(** Returns unitialised [gen_union] *)

val no_family : ifam -> (iper, ifam, istr) Def.gen_family
(** Returns unitialised [gen_family] with giving id *)

val no_descend : iper Def.gen_descend
(** Returns unitialised [gen_descend] *)

val no_couple : iper Def.gen_couple
(** Returns unitialised [gen_couple] *)

val nb_of_persons : base -> int
(** Returns number of persons inside the database *)

val nb_of_real_persons : base -> int
(** Returns number of defined persons (without bogus definition "? ?")
    inside the database *)

val nb_of_families : base -> int
(** Returns number of families inside the database *)

val bname : base -> string
(** Returns database name *)

val patch_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
(** Modify/add person with the giving id in the base. New names are added
    to the patched name index for the cosidered person and for evey member of family to
    which he belongs. Modification stay blocked until call of [commit_patches]. *)

val patch_ascend : base -> iper -> ifam Def.gen_ascend -> unit
(** Modify/add ascendants of a person with a giving id. Modification stay blocked until
    call of [commit_patches]. *)

val patch_union : base -> iper -> ifam Def.gen_union -> unit
(** Modify/add union of a person with a giving id. Modification stay blocked until
    call of [commit_patches]. *)

val patch_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
(** Modify/add family with a giving id. Modification stay blocked until
    call of [commit_patches]. *)

val patch_descend : base -> ifam -> iper Def.gen_descend -> unit
(** Modify/add descendants of a family with a giving id. Modification stay blocked until
    call of [commit_patches]. *)

val patch_couple : base -> ifam -> iper Def.gen_couple -> unit
(** Modify/add couple of a family with a giving id. Modification stay blocked until
    call of [commit_patches]. *)

val insert_string : base -> string -> istr
(** Modify/add string with a giving id. If string already exists return its id.
    Modification stay blocked until call of [commit_patches]. *)

val commit_patches : base -> unit
(** Commit blocked modifications (patches) and update database files in order to
    apply modifications on the disk.  *)

val commit_notes : base -> string -> string -> unit
(** [commit_notes fname s] Update content of the notes/extended page file [fname] if exists. *)

val new_iper : base -> iper
(** Retruns new unused person's id *)

val new_ifam : base -> ifam
(** Retruns new unused family's id *)

val insert_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
(** Same as [patch_person] *)

val insert_ascend : base -> iper -> ifam Def.gen_ascend -> unit
(** Same as [patch_ascend] *)

val insert_union : base -> iper -> ifam Def.gen_union -> unit
(** Same as [patch_union] *)

val insert_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
(** Same as [patch_family] *)

val insert_descend : base -> ifam -> iper Def.gen_descend -> unit
(** Same as [patch_couple] *)

val insert_couple : base -> ifam -> iper Def.gen_couple -> unit
(** Same as [patch_descend] *)

val delete_person : base -> iper -> unit
(** Remplace person with the giving id by bogus definition and clear
    person's data structure. *)

val delete_ascend : base -> iper -> unit
(** Clear person's ascendants data structure *)

val delete_union : base -> iper -> unit
(** Clear person's union data structure *)

val delete_family : base -> ifam -> unit
(** Remplace family with the giving id by dummy family and clear
    family's data structure. *)

val delete_descend : base -> ifam -> unit
(** Clear family's descendants data structure *)

val delete_couple : base -> ifam -> unit
(** Clear family's couple data structure *)

val person_of_key : base -> string -> string -> int -> iper option
(** [person_of_key first_name surname occ] returns person from his key information
    (first name, surname and occurence number) *)

val persons_of_name : base -> string -> iper list
(** Return list of person ids that have giving name (could be one of the mix). *)

val persons_of_first_name : base -> string_person_index
(** Returns data structure that allows to make optimised search throughout
    index by first name *)

val persons_of_surname : base -> string_person_index
(** Returns data structure that allows to make optimised search throughout
    index by surname *)

val persons_of_alias : base -> string_person_index
(** Returns data structure that allows to make optimised search throughout
    index by alias *)

val spi_first : string_person_index -> string -> istr
(** Returns first [first/sur]name id starting with that string *)

val spi_next : string_person_index -> istr -> istr
(** Retruns next [first/sur]name id that follows giving name's id by
    Gutil.alphabetical order *)

val spi_find : string_person_index -> istr -> iper list
(** Retruns all persons id having that [first/sur]name. *)

val base_visible_get : base -> (person -> bool) -> iper -> bool
(** [base_visible_get base fct ip] get visibility of person [ip] ([true] for not visible
    (restrited)) from the [base]. If file {i restrict} is present then read it to get
    visibility information. If person's visibility isn't known, then set it with [fct].
    Used when mode `use_restrict` is ativated *)

val base_visible_write : base -> unit
(** Write updated visibility information to the {i restricted} file. *)

val base_particles : base -> Re.re
(** Return regular expression that matches all defined in the [base] particles. *)

val base_strings_of_first_name : base -> string -> istr list
(** [base_strings_of_first_name base x]
    Return the list of first names (as [istr]) being equal or to [x]
    using {!val:Name.crush_lower} comparison. [x] could be also a substring
    of the matched first name. *)

val base_strings_of_surname : base -> string -> istr list
(** [base_strings_of_surname base x]
    Return the list of surnames (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison. [x] could be also a substring
    of the matched surname. *)

val base_strings_of_alias : base -> string -> istr list
(** [base_strings_of_alias base x]
    Return the list of aliases (as [istr]) being equal to [x]
    using  {!val:Name.crush_lower} comparison. [x] could be also a substring
    of the matched alias. *)

val load_ascends_array : base -> unit
(** Load array of ascendants in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_ascends_array] is called. *)

val load_unions_array : base -> unit
(** Load array of unions in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_unions_array] is called. *)

val load_couples_array : base -> unit
(** Load array of couples in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_couples_array] is called. *)

val load_descends_array : base -> unit
(** Load array of descendants in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_descends_array] is called. *)

val load_strings_array : base -> unit
(** Load array of strings in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_strings_array] is called. *)

val load_persons_array : base -> unit
(** Load array of persons in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_persons_array] is called. *)

val load_families_array : base -> unit
(** Load array of families in the memory and cache it so it could be accessed
    instantly by other functions unless [clear_families_array] is called. *)

val clear_ascends_array : base -> unit
(** Remove array of ascendants from the memory *)

val clear_unions_array : base -> unit
(** Remove array of unions from the memory *)

val clear_couples_array : base -> unit
(** Remove array of couples from the memory *)

val clear_descends_array : base -> unit
(** Remove array of descendants from the memory *)

val clear_strings_array : base -> unit
(** Remove array of strings from the memory *)

val clear_persons_array : base -> unit
(** Remove array of persons from the memory *)

val clear_families_array : base -> unit
(** Remove array of families from the memory *)

val base_notes_read : base -> string -> string
(** [base_notes_read base fname] read and return content of [fname] note
    (either database note either extended page). *)

val base_notes_read_first_line : base -> string -> string
(** [base_notes_read_first_line base fname] read and return first line of [fname] note *)

val base_notes_are_empty : base -> string -> bool
(** Says if note has empty content *)

val base_notes_origin_file : base -> string
(** Retruns origin file (.gw file) of the note *)

val base_notes_dir : base -> string
(** Directory where extended pages are stored *)

val base_wiznotes_dir : base -> string
(** Directory where wizard notes are stored *)

val date_of_last_change : base -> float
(** Returns last modification time of the database on disk *)

(** Collections of elements *)
module Collection : sig
  type 'a t
  (** Collections are sets of elements you want to traverse. *)

  val length : 'a t -> int
  (** Return the number of elements of a colletion *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn c]
      Return a collection corresponding to [c]
      where [fn] would have been applied to each of its elements.
   *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter fn c]
      Apply [fn] would have been applied to each elements of [c].
   *)

  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** [iter fn c]
      Apply [fn i] would have been applied to each elements of [c]
      where [i] is the index (starting with 0) of the element.
   *)

  val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold fn acc c]
      Combine each element of [c] into a single value using [fn].
      [fn] first argument is the result computed so far as we traverse the
      collection, and second element is the current element being combined.
      [acc] is the starting combined value.
      Start at [from]-nth and finish with [until]-nth element (included).
   *)

  val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** [fold_until continue fn acc c]
      Same as [fold fn acc c], but computation stops as soon as [continue]
      is not satisfied by combined value anymore.
   *)

  val iterator : 'a t -> unit -> 'a option
  (** [iterator c]
      Return a function returning [Some next_element] when it is called,
      or [None] if you reached the end of the collection.
   *)
end

(** Markers for elements inside [Collection.t] *)
module Marker : sig
  type ('k, 'v) t
  (** Markers are way to annotate (add extra information to) elements of a {!val:Collection.t}. *)

  val get : ('k, 'v) t -> 'k -> 'v
  (** [get marker key]
      Return the annotation associated to [key].
   *)

  val set : ('k, 'v) t -> 'k -> 'v -> unit
  (** [set marker key value]
      Set [value] as annotation associated to [key].
   *)
end

(** {2 Useful collections} *)

val ipers : base -> iper Collection.t
(** Collection of person's ids *)

val persons : base -> person Collection.t
(** Collection of persons *)

val ifams : ?select:(ifam -> bool) -> base -> ifam Collection.t
(** Collection of family's ids *)

val families : ?select:(family -> bool) -> base -> family Collection.t
(** Collection of families *)

val dummy_collection : 'a -> 'a Collection.t
(** [dummy_collection x] create a dummy collection with no element.
    [x] is only used for typing.
    Useful for placeholders or for typing purpose. *)

(** {2 Useful markers} *)

val iper_marker : iper Collection.t -> 'a -> (iper, 'a) Marker.t
(** [iper_marker c v] create marker over collection of person's ids and initialise it
    for every element with [v] *)

val ifam_marker : ifam Collection.t -> 'a -> (ifam, 'a) Marker.t
(** [ifam_marker c v] create marker over collection of family's ids and initialise it
    for every element with [v] *)

val dummy_marker : 'a -> 'b -> ('a, 'b) Marker.t
(** [dummy_marker k v] create a dummy collection with no element.
    [k] and [v] are only used for typing.
    Useful for placeholders or for typing purpose. *)

(** {2 Database creation} *)

val make :
  string ->
  string list ->
  ((int, int, int) Def.gen_person array
  * int Def.gen_ascend array
  * int Def.gen_union array)
  * ((int, int, int) Def.gen_family array
    * int Def.gen_couple array
    * int Def.gen_descend array)
  * string array
  * Def.base_notes ->
  base
(** [make bname particles arrays] create a base with [bname] name and [arrays] as content. *)

val read_nldb : base -> (iper, ifam) Def.NLDB.t
(** TODOOCP : doc *)

val write_nldb : base -> (iper, ifam) Def.NLDB.t -> unit

val sync : ?scratch:bool -> save_mem:bool -> base -> unit
(** [sync scratch base]
    Ensure that everything is synced on disk.

    Depending on the backend,
    it may perform various operation such as indexes rebuilding,
    and it might be a lengthy operation.

    Use [scratch] (default false) to sync and rebuild
    the whole database. Otherwise, only changes that occured
    since the last [sync] call are treated.
*)

val gc :
  ?dry_run:bool -> save_mem:bool -> base -> int list * int list * int list

val set_fpoi_cache : base -> bool -> unit
