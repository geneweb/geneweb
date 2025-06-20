(* Copyright (c) 1998-2007 INRIA *)

type istr
(** String id *)

type ifam
(** Family id *)

type iper
(** Person id *)

module type Indexed = sig
  type t

  val dummy : t
  val is_dummy : t -> bool

  val hash : t -> int
  (** Compute a hash of an indexed value. This function is just the identity
      because a [t] value is already a hash.

      The hash should use only on values from a common database index. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : t -> string
  val of_string : string -> t
  val pp : t Fmt.t

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
  module Table : Hashtbl.S with type key = t
end

module Istr : sig
  type t = istr

  include Indexed with type t := t

  val empty : t
  (** Identifier of the empty string (""). *)

  val quest : t
  (** Identifier of the question mark ("?") *)

  val is_empty : t -> bool
  val is_quest : t -> bool
end

module Ifam : Indexed with type t = ifam
module Iper : Indexed with type t = iper

(** {2 Database management} *)

type base
(** The database representation. *)

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
  (base -> 'a) ->
  'a
(** [make bname particles arrays k] create a base with [bname] name and [arrays]
    as content and invokes the continuation [k] with it. *)

val load_database : string -> unit
(** [load_database bname] loads the database [bname] into memory.

    The base is read-only and any attempt to modify its values will result in
    failure.

    A caveat of this function is that the allocated memory cannot be freed
    before the current process terminates. Under the hood, it uses the Ancient
    library to map the database into memory, outside the OCaml heap. As a
    result, the OCaml garbage collector cannot reclaim it and the delete
    function of Ancient is flawed. For this reason, this function should only be
    used with databases that are intended to remain in memory during the entire
    execution of the current process.

    @raise Failwith if the base has already been loaded. *)

val with_database : string -> (base -> 'a) -> 'a
(** [with_database bname k] loads the database [bname] and invokes the
    continuation [k] with it.

    If the database [bname] has already been loaded into memory with
    [load_database], the function uses this in-memory base instead of reloading
    it.

    If the database [bname] was not loaded in memory, it is unloaded after [k]
    is executed. *)

val sync : ?scratch:bool -> base -> unit
(** [sync scratch base] Ensure that everything is synced on disk.

    Depending on the backend, it may perform various operation such as indexes
    rebuilding, and it might be a lengthy operation.

    Use [scratch] (default false) to sync and rebuild the whole database.
    Otherwise, only changes that occured since the last [sync] call are treated.
*)

(** {2 Unique identifiers} *)

type person
(** Person data structure *)

type family
(** Family data structure *)

type relation = (iper, istr) Def.gen_relation
(** Database implementation for [Def.gen_relation] *)

type title = istr Def.gen_title
(** Database implementation for [Def.gen_title] *)

type pers_event = (iper, istr) Def.gen_pers_event
(** Database implementation for [Def.pers_event] *)

type fam_event = (iper, istr) Def.gen_fam_event
(** Database implementation for [Def.fam_event] *)

type string_person_index
(** Data structure for optimised search throughout index by name (surname or
    first name). *)

val empty_person : base -> iper -> person
(** Returns unitialised person with the giving id. *)

val empty_family : base -> ifam -> family
(** Returns unitialised family with the giving id. *)

val iper_exists : base -> iper -> bool
(** Tells if person with giving id exists in the base. *)

val ifam_exists : base -> ifam -> bool
(** Tells if family with giving id exists in the base. *)

(** {2 Getters}
    Getters are used to extract information about person and family. If
    corresponding information part isn't present, driver load it from the disk
    and cache it so further gets will return result immediately. *)

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

val get_separation : family -> Def.divorce
(** Get family's separation status *)

val get_family : person -> ifam array
(** Get array of family's ids to which a person belongs as parent (person's
    union) *)

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
(** Returns number of defined persons (without bogus definition "? ?") inside
    the database *)

val nb_of_families : base -> int
(** Returns number of families inside the database *)

val bname : base -> string
(** Returns database name *)

val patch_person : base -> iper -> (iper, iper, istr) Def.gen_person -> unit
(** Modify/add person with the giving id in the base. New names are added to the
    patched name index for the cosidered person and for evey member of family to
    which he belongs. Modification stay blocked until call of [commit_patches].
*)

val patch_ascend : base -> iper -> ifam Def.gen_ascend -> unit
(** Modify/add ascendants of a person with a giving id. Modification stay
    blocked until call of [commit_patches]. *)

val patch_union : base -> iper -> ifam Def.gen_union -> unit
(** Modify/add union of a person with a giving id. Modification stay blocked
    until call of [commit_patches]. *)

val patch_family : base -> ifam -> (iper, ifam, istr) Def.gen_family -> unit
(** Modify/add family with a giving id. Modification stay blocked until call of
    [commit_patches]. *)

val patch_descend : base -> ifam -> iper Def.gen_descend -> unit
(** Modify/add descendants of a family with a giving id. Modification stay
    blocked until call of [commit_patches]. *)

val patch_couple : base -> ifam -> iper Def.gen_couple -> unit
(** Modify/add couple of a family with a giving id. Modification stay blocked
    until call of [commit_patches]. *)

val insert_string : base -> string -> istr
(** Modify/add string with a giving id. If string already exists return its id.
    Modification stay blocked until call of [commit_patches]. *)

val commit_patches : base -> unit
(** Commit blocked modifications (patches) and update database files in order to
    apply modifications on the disk. *)

val commit_notes : base -> string -> string -> unit
(** [commit_notes fname s] Update content of the notes/extended page file
    [fname] if exists. *)

val commit_wiznotes : base -> string -> string -> unit
(** [commit_wiznotes fname s] Update content of the wizard notes page file
    [fname] if exists. *)

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

val insert_family_with_couple_and_descendants :
  base ->
  (iper, ifam, istr) Def.gen_family ->
  iper Def.gen_couple ->
  iper Def.gen_descend ->
  ifam
(** [insert_family base f c d] Add a new family with its couple and descendants
    the in the [base]. Allocate and returns the fresh new id for this family.
    [f] SHOULD be defined using [dummy_ifam]. *)

val insert_descend : base -> ifam -> iper Def.gen_descend -> unit
(** Same as [patch_couple] *)

val insert_person_with_union_and_ascendants :
  base ->
  (iper, iper, istr) Def.gen_person ->
  ifam Def.gen_ascend ->
  ifam Def.gen_union ->
  iper
(** [insert_person base p a u] Add a new person with its union and ascendants in
    the [base]. Allocate and returns the fresh new id for this person. [p]
    SHOULD be defined using [dummy_iper]. *)

val insert_couple : base -> ifam -> iper Def.gen_couple -> unit
(** Same as [patch_descend] *)

val delete_person : base -> iper -> unit
(** Remplace person with the giving id by bogus definition and clear person's
    data structure. *)

val delete_person_rec : base -> iper -> unit
(** [delete_person_rec base iper] recursively deletes data as follows:
    - If data to be deleted is linked and useful, it is replaced by empty data.
      Otherwise, it is deleted.
    - If empty data is linked to deleted data, it is clean up as well. *)

val delete_family_rec : base -> ifam -> unit
(** [delete_family_rec base iper] recursively deletes data as follows:
    - If data to be deleted is linked and useful, it is replaced by empty data.
      Otherwise, it is deleted.
    - If empty data is linked to deleted data, it is clean up as well. *)

val delete_ascend : base -> iper -> unit
(** Clear person's ascendants data structure *)

val delete_union : base -> iper -> unit
(** Clear person's union data structure *)

val delete_family : base -> ifam -> unit
(** Remplace family with the giving id by dummy family and clear family's data
    structure. *)

val delete_descend : base -> ifam -> unit
(** Clear family's descendants data structure *)

val delete_couple : base -> ifam -> unit
(** Clear family's couple data structure *)

val person_of_key : base -> string -> string -> int -> iper option
(** [person_of_key first_name surname occ] returns person from his key
    information (first name, surname and occurence number) *)

val persons_of_name : base -> string -> iper list
(** Return list of person ids that have giving name (could be one of the mix).
*)

val persons_of_first_name : base -> string_person_index
(** Returns data structure that allows to make optimised search throughout index
    by first name *)

val persons_of_surname : base -> string_person_index
(** Returns data structure that allows to make optimised search throughout index
    by surname *)

val spi_first : string_person_index -> string -> istr
(** Returns first [first/sur]name id starting with that string *)

val spi_next : string_person_index -> istr -> istr
(** Retruns next [first/sur]name id that follows giving name's id by
    Gutil.alphabetical order *)

val spi_find : string_person_index -> istr -> iper list
(** Retruns all persons id having that [first/sur]name. *)

val base_visible_get : base -> (person -> bool) -> iper -> bool
(** [base_visible_get base fct ip] get visibility of person [ip] ([true] for not
    visible (restrited)) from the [base]. If file {i restrict} is present then
    read it to get visibility information. If person's visibility isn't known,
    then set it with [fct]. Used when mode `use_restrict` is ativated *)

val base_visible_write : base -> unit
(** Write updated visibility information to the {i restricted} file. *)

val base_particles : base -> Re.re
(** Return regular expression that matches all defined in the [base] particles.
*)

val base_strings_of_first_name : base -> string -> istr list
(** [base_strings_of_first_name base x] Return the list of first names (as
    [istr]) being equal or to [x] using {!val:Name.crush_lower} comparison. [x]
    could be also a substring of the matched first name. *)

val base_strings_of_surname : base -> string -> istr list
(** [base_strings_of_surname base x] Return the list of surnames (as [istr])
    being equal to [x] using {!val:Name.crush_lower} comparison. [x] could be
    also a substring of the matched surname. *)

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
(** [base_notes_read base fname] read and return content of [fname] note (either
    database note either extended page). *)

val base_wiznotes_read : base -> string -> string
(** [base_wiznotes_read base fname] read and return content of [fname] note
    (either database note either extended page). *)

val base_notes_read_first_line : base -> string -> string
(** [base_notes_read base fname] read and return first line of [fname] note *)

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

(** {2 Useful collections} *)

val ipers : base -> iper Collection.t
(** Collection of person's ids *)

val persons : base -> person Collection.t
(** Collection of persons *)

val ifams : ?select:(ifam -> bool) -> base -> ifam Collection.t
(** Collection of family's ids *)

val families : ?select:(family -> bool) -> base -> family Collection.t
(** Collection of families *)

(** {2 Useful markers} *)

val iper_marker : iper Collection.t -> 'a -> (iper, 'a) Collection.Marker.t
(** [iper_marker c v] create marker over collection of person's ids and
    initialise it for every element with [v] *)

val ifam_marker : ifam Collection.t -> 'a -> (ifam, 'a) Collection.Marker.t
(** [ifam_marker c v] create marker over collection of family's ids and
    initialise it for every element with [v] *)

val read_nldb : base -> (iper, ifam) Def.NLDB.t
(** TODOOCP : doc *)

val write_nldb : base -> (iper, ifam) Def.NLDB.t -> unit

val person_misc_names : base -> person -> (person -> title list) -> string list
(** [person_misc_names base p nobtit] computes various mix between all kind of
    names of a person's entry [p] from the database [base]. [nobtit] is used to
    return a title entries for passed in argument person. *)

val p_first_name : base -> person -> string
(** Returns first name of person. *)

val p_surname : base -> person -> string
(** Returns surname of person *)

val children_of_p : base -> person -> iper list
(** Returns list of children ids for every family for giving person *)

val nobtitles :
  base -> string list Lazy.t -> string list Lazy.t -> person -> title list
(** [nobtitles base allowed_titles denied_titles p] returns list of titles of a
    person [p] that apprears in [allowed_titles] and doesn't appears in
    [denied_titles]. If [allowed_titles] is empty the every title is allowed *)
