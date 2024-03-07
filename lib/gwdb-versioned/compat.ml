module type Driver_S = sig
  type base
  (** The database representation. *)

  type person
  (** Person data structure *)

  type family
  (** Family data structure *)

  type istr = int
  (** String id *)

  type ifam = int
  (** Family id *)

  type iper = int
  (** Person id *)

  type relation = (iper, istr) Def.gen_relation
  (** Database implementation for [Def.gen_relation] *)

  type title = istr Def.gen_title
  (** Database implementation for [Def.gen_title] *)

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

  type string_person_index
  (** Data structure for optimised search throughout index by name
    (surname or first name). *)

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
    (iper, ifam, istr) Def.gen_family
    * iper Def.gen_couple
    * iper Def.gen_descend ->
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

  (** Collections of elemetns *)
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
end

module type DriverImpl = sig
  include Driver_S

  val versions : Version.t list
end

module type DriverImplCompat = sig
  include DriverImpl

  val compatibility_directory : string
end

let dummy_iper = -1
let dummy_ifam = -1
let empty_string = 0
let quest_string = 1

module Make (Legacy : DriverImplCompat) (Current : DriverImpl) : Driver_S =
struct
  type base = Legacy_base of Legacy.base | Current_base of Current.base

  type person =
    | Legacy_person of Legacy.person
    | Current_person of Current.person

  type family =
    | Legacy_family of Legacy.family
    | Current_family of Current.family

  type istr = int
  type ifam = int
  type iper = int
  type relation = (iper, istr) Def.gen_relation
  type title = istr Def.gen_title

  type pers_event =
    | Legacy_pevent of Legacy.pers_event
    | Current_pevent of Current.pers_event

  type fam_event =
    | Legacy_fevent of Legacy.fam_event
    | Current_fevent of Current.fam_event

  type string_person_index =
    | Legacy_string_person_index of Legacy.string_person_index
    | Current_string_person_index of Current.string_person_index

  module Util : sig
    val wrap_base : (Legacy.base -> 'a) -> (Current.base -> 'a) -> base -> 'a

    val wrap_person :
      (Legacy.person -> 'a) -> (Current.person -> 'a) -> person -> 'a

    val wrap_family :
      (Legacy.family -> 'a) -> (Current.family -> 'a) -> family -> 'a

    val wrap_spi :
      (Legacy.string_person_index -> 'a) ->
      (Current.string_person_index -> 'a) ->
      string_person_index ->
      'a

    val wrap_pevent :
      (Legacy.pers_event -> 'a) ->
      (Current.pers_event -> 'a) ->
      pers_event ->
      'a

    val wrap_fevent :
      (Legacy.fam_event -> 'a) -> (Current.fam_event -> 'a) -> fam_event -> 'a
  end = struct
    let wrap_base legacyf currentf = function
      | Legacy_base b -> legacyf b
      | Current_base b -> currentf b

    let wrap_person legacyf currentf = function
      | Legacy_person person -> legacyf person
      | Current_person person -> currentf person

    let wrap_family legacyf currentf = function
      | Legacy_family family -> legacyf family
      | Current_family family -> currentf family

    let wrap_spi legacyf currentf = function
      | Legacy_string_person_index spi -> legacyf spi
      | Current_string_person_index spi -> currentf spi

    let wrap_pevent legacyf currentf = function
      | Legacy_pevent pe -> legacyf pe
      | Current_pevent pe -> currentf pe

    let wrap_fevent legacyf currentf = function
      | Legacy_fevent fe -> legacyf fe
      | Current_fevent fe -> currentf fe
  end

  let string_of_iper = string_of_int
  let string_of_ifam = string_of_int
  let string_of_istr = string_of_int
  let iper_of_string = int_of_string
  let ifam_of_string = int_of_string
  let istr_of_string = int_of_string
  let compare_iper = Int.compare
  let compare_ifam = Int.compare
  let compare_istr = Int.compare

  (*  let log = print_endline*)

  let log _ = ()

  let get_pevent_name =
    Util.wrap_pevent Legacy.get_pevent_name Current.get_pevent_name

  let get_pevent_date =
    Util.wrap_pevent Legacy.get_pevent_date Current.get_pevent_date

  let get_pevent_place =
    Util.wrap_pevent Legacy.get_pevent_place Current.get_pevent_place

  let get_pevent_reason =
    Util.wrap_pevent Legacy.get_pevent_reason Current.get_pevent_reason

  let get_pevent_note =
    Util.wrap_pevent Legacy.get_pevent_note Current.get_pevent_note

  let get_pevent_src =
    Util.wrap_pevent Legacy.get_pevent_src Current.get_pevent_src

  let get_pevent_witnesses =
    Util.wrap_pevent Legacy.get_pevent_witnesses Current.get_pevent_witnesses

  let get_pevent_witness_notes =
    Util.wrap_pevent Legacy.get_pevent_witness_notes
      Current.get_pevent_witness_notes

  let get_pevent_witnesses_and_notes =
    Util.wrap_pevent Legacy.get_pevent_witnesses_and_notes
      Current.get_pevent_witnesses_and_notes

  let gen_pevent_of_pers_event =
    Util.wrap_pevent Legacy.gen_pevent_of_pers_event
      Current.gen_pevent_of_pers_event

  let pers_event_of_gen_pevent base pe =
    match base with
    | Legacy_base base ->
        let pe = Legacy.pers_event_of_gen_pevent base pe in
        Legacy_pevent pe
    | Current_base base ->
        let pe = Current.pers_event_of_gen_pevent base pe in
        Current_pevent pe

  let eq_pevent p1 p2 =
    match (p1, p2) with
    | Legacy_pevent p1, Legacy_pevent p2 -> Legacy.eq_pevent p1 p2
    | Current_pevent p1, Current_pevent p2 -> Current.eq_pevent p1 p2
    | _ -> assert false

  let eq_fevent f1 f2 =
    match (f1, f2) with
    | Legacy_fevent f1, Legacy_fevent f2 -> Legacy.eq_fevent f1 f2
    | Current_fevent f1, Current_fevent f2 -> Current.eq_fevent f1 f2
    | _ -> assert false

  let get_fevent_name =
    Util.wrap_fevent Legacy.get_fevent_name Current.get_fevent_name

  let get_fevent_date =
    Util.wrap_fevent Legacy.get_fevent_date Current.get_fevent_date

  let get_fevent_place =
    Util.wrap_fevent Legacy.get_fevent_place Current.get_fevent_place

  let get_fevent_reason =
    Util.wrap_fevent Legacy.get_fevent_reason Current.get_fevent_reason

  let get_fevent_note =
    Util.wrap_fevent Legacy.get_fevent_note Current.get_fevent_note

  let get_fevent_src =
    Util.wrap_fevent Legacy.get_fevent_src Current.get_fevent_src

  let get_fevent_witnesses =
    Util.wrap_fevent Legacy.get_fevent_witnesses Current.get_fevent_witnesses

  let get_fevent_witness_notes =
    Util.wrap_fevent Legacy.get_fevent_witness_notes
      Current.get_fevent_witness_notes

  let get_fevent_witnesses_and_notes =
    Util.wrap_fevent Legacy.get_fevent_witnesses_and_notes
      Current.get_fevent_witnesses_and_notes

  let gen_fevent_of_fam_event =
    Util.wrap_fevent Legacy.gen_fevent_of_fam_event
      Current.gen_fevent_of_fam_event

  let fam_event_of_gen_fevent base pe =
    match base with
    | Legacy_base base ->
        let pe = Legacy.fam_event_of_gen_fevent base pe in
        Legacy_fevent pe
    | Current_base base ->
        let pe = Current.fam_event_of_gen_fevent base pe in
        Current_fevent pe

  let open_base bname =
    let bname =
      if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
    in
    let ic = Secure.open_in_bin (Filename.concat bname "base") in
    let version_opt =
      try
        let v = really_input_string ic 8 in
        log ("VERSION FOUND : " ^ v);
        Version.check_version v
      with
      | Version.Unsupported_base ->
          failwith "this is a GeneWeb base, but not compatible"
      | Version.Not_a_geneweb_base ->
          failwith "this is not a GeneWeb base, or it is a very old version"
    in
    close_in ic;
    match version_opt with
    | Some version
      when List.exists (Version.eq_version version) Current.versions ->
        log "opening CURRENT";
        let base = Current.open_base bname in
        log "CURRENT opened";
        Current_base base
    | Some version when List.exists (Version.eq_version version) Legacy.versions
      ->
        let base = Legacy.open_base bname in
        Legacy_base base
    | _ -> assert false (* should not happen *)

  let close_base = Util.wrap_base Legacy.close_base Current.close_base
  let dummy_iper = dummy_iper
  let dummy_ifam = dummy_ifam
  let empty_string = empty_string
  let quest_string = quest_string
  let eq_istr = ( = )
  let eq_ifam = ( = )
  let eq_iper = ( = )
  let is_empty_string = eq_istr empty_string
  let is_quest_string = eq_istr quest_string

  let empty_person base iper =
    match base with
    | Legacy_base base -> Legacy_person (Legacy.empty_person base iper)
    | Current_base base -> Current_person (Current.empty_person base iper)

  let empty_family base ifam =
    match base with
    | Legacy_base base -> Legacy_family (Legacy.empty_family base ifam)
    | Current_base base -> Current_family (Current.empty_family base ifam)

  let iper_exists = Util.wrap_base Legacy.iper_exists Current.iper_exists
  let ifam_exists = Util.wrap_base Legacy.ifam_exists Current.ifam_exists
  let get_access = Util.wrap_person Legacy.get_access Current.get_access
  let get_aliases = Util.wrap_person Legacy.get_aliases Current.get_aliases
  let get_consang = Util.wrap_person Legacy.get_consang Current.get_consang
  let get_family = Util.wrap_person Legacy.get_family Current.get_family

  let get_first_name =
    Util.wrap_person Legacy.get_first_name Current.get_first_name

  let get_first_names_aliases =
    Util.wrap_person Legacy.get_first_names_aliases
      Current.get_first_names_aliases

  let get_image = Util.wrap_person Legacy.get_image Current.get_image
  let get_iper = Util.wrap_person Legacy.get_iper Current.get_iper
  let get_notes = Util.wrap_person Legacy.get_notes Current.get_notes
  let get_occ = Util.wrap_person Legacy.get_occ Current.get_occ

  let get_occupation =
    Util.wrap_person Legacy.get_occupation Current.get_occupation

  let get_parents = Util.wrap_person Legacy.get_parents Current.get_parents

  let get_pevents =
    Util.wrap_person
      (fun p ->
        let l = Legacy.get_pevents p in
        List.map (fun pe -> Legacy_pevent pe) l)
      (fun p ->
        let l = Current.get_pevents p in
        List.map (fun pe -> Current_pevent pe) l)

  let get_psources = Util.wrap_person Legacy.get_psources Current.get_psources

  let get_public_name =
    Util.wrap_person Legacy.get_public_name Current.get_public_name

  let get_qualifiers =
    Util.wrap_person Legacy.get_qualifiers Current.get_qualifiers

  let get_related = Util.wrap_person Legacy.get_related Current.get_related
  let get_rparents = Util.wrap_person Legacy.get_rparents Current.get_rparents
  let get_sex = Util.wrap_person Legacy.get_sex Current.get_sex
  let get_surname = Util.wrap_person Legacy.get_surname Current.get_surname

  let get_surnames_aliases =
    Util.wrap_person Legacy.get_surnames_aliases Current.get_surnames_aliases

  let get_titles = Util.wrap_person Legacy.get_titles Current.get_titles

  let gen_person_of_person =
    Util.wrap_person Legacy.gen_person_of_person Current.gen_person_of_person

  let gen_ascend_of_person =
    Util.wrap_person Legacy.gen_ascend_of_person Current.gen_ascend_of_person

  let gen_union_of_person =
    Util.wrap_person Legacy.gen_union_of_person Current.gen_union_of_person

  let person_of_gen_person base genperson =
    match base with
    | Legacy_base base ->
        let person = Legacy.person_of_gen_person base genperson in
        Legacy_person person
    | Current_base base ->
        let person = Current.person_of_gen_person base genperson in
        Current_person person

  let get_baptism = Util.wrap_person Legacy.get_baptism Current.get_baptism

  let get_baptism_note =
    Util.wrap_person Legacy.get_baptism_note Current.get_baptism_note

  let get_baptism_place =
    Util.wrap_person Legacy.get_baptism_place Current.get_baptism_place

  let get_baptism_src =
    Util.wrap_person Legacy.get_baptism_src Current.get_baptism_src

  let get_birth = Util.wrap_person Legacy.get_birth Current.get_birth

  let get_birth_note =
    Util.wrap_person Legacy.get_birth_note Current.get_birth_note

  let get_birth_place =
    Util.wrap_person Legacy.get_birth_place Current.get_birth_place

  let get_birth_src =
    Util.wrap_person Legacy.get_birth_src Current.get_birth_src

  let get_burial = Util.wrap_person Legacy.get_burial Current.get_burial

  let get_burial_note =
    Util.wrap_person Legacy.get_burial_note Current.get_burial_note

  let get_burial_place =
    Util.wrap_person Legacy.get_burial_place Current.get_burial_place

  let get_burial_src =
    Util.wrap_person Legacy.get_burial_src Current.get_burial_src

  let get_death = Util.wrap_person Legacy.get_death Current.get_death

  let get_death_note =
    Util.wrap_person Legacy.get_death_note Current.get_death_note

  let get_death_place =
    Util.wrap_person Legacy.get_death_place Current.get_death_place

  let get_death_src =
    Util.wrap_person Legacy.get_death_src Current.get_death_src

  let get_children = Util.wrap_family Legacy.get_children Current.get_children
  let get_comment = Util.wrap_family Legacy.get_comment Current.get_comment
  let get_divorce = Util.wrap_family Legacy.get_divorce Current.get_divorce
  let get_father = Util.wrap_family Legacy.get_father Current.get_father
  let get_mother = Util.wrap_family Legacy.get_mother Current.get_mother

  let get_fevents =
    Util.wrap_family
      (fun f -> List.map (fun f -> Legacy_fevent f) (Legacy.get_fevents f))
      (fun f -> List.map (fun f -> Current_fevent f) (Current.get_fevents f))

  let get_fsources = Util.wrap_family Legacy.get_fsources Current.get_fsources
  let get_ifam = Util.wrap_family Legacy.get_ifam Current.get_ifam

  let get_origin_file =
    Util.wrap_family Legacy.get_origin_file Current.get_origin_file

  let get_parent_array =
    Util.wrap_family Legacy.get_parent_array Current.get_parent_array

  let get_relation = Util.wrap_family Legacy.get_relation Current.get_relation

  let get_witnesses =
    Util.wrap_family Legacy.get_witnesses Current.get_witnesses

  let gen_couple_of_family =
    Util.wrap_family Legacy.gen_couple_of_family Current.gen_couple_of_family

  let gen_descend_of_family =
    Util.wrap_family Legacy.gen_descend_of_family Current.gen_descend_of_family

  let gen_family_of_family =
    Util.wrap_family Legacy.gen_family_of_family Current.gen_family_of_family

  let family_of_gen_family base genfam =
    match base with
    | Legacy_base base ->
        let fam = Legacy.family_of_gen_family base genfam in
        Legacy_family fam
    | Current_base base ->
        let fam = Current.family_of_gen_family base genfam in
        Current_family fam

  let get_marriage = Util.wrap_family Legacy.get_marriage Current.get_marriage

  let get_marriage_note =
    Util.wrap_family Legacy.get_marriage_note Current.get_marriage_note

  let get_marriage_place =
    Util.wrap_family Legacy.get_marriage_place Current.get_marriage_place

  let get_marriage_src =
    Util.wrap_family Legacy.get_marriage_src Current.get_marriage_src

  let poi =
    let legacy_poi base iper =
      let person = Legacy.poi base iper in
      Legacy_person person
    in
    let current_poi base iper =
      let person = Current.poi base iper in
      Current_person person
    in
    Util.wrap_base legacy_poi current_poi

  let foi =
    let legacy_foi base ifam =
      let family = Legacy.foi base ifam in
      Legacy_family family
    in
    let current_foi base ifam =
      let family = Current.foi base ifam in
      Current_family family
    in
    Util.wrap_base legacy_foi current_foi

  let sou = Util.wrap_base Legacy.sou Current.sou

  (* wrong *)
  let no_person iper = Current.no_person iper
  let no_ascend = Current.no_ascend
  let no_union = Current.no_union
  let no_family ifam = Current.no_family ifam
  let no_descend = Current.no_descend
  let no_couple = Current.no_couple

  (* end wrong *)

  let nb_of_persons = Util.wrap_base Legacy.nb_of_persons Current.nb_of_persons

  let nb_of_real_persons =
    Util.wrap_base Legacy.nb_of_real_persons Current.nb_of_real_persons

  let nb_of_families =
    Util.wrap_base Legacy.nb_of_families Current.nb_of_families

  let bname = Util.wrap_base Legacy.bname Current.bname
  let patch_person = Util.wrap_base Legacy.patch_person Current.patch_person
  let patch_ascend = Util.wrap_base Legacy.patch_ascend Current.patch_ascend
  let patch_union = Util.wrap_base Legacy.patch_union Current.patch_union
  let patch_family = Util.wrap_base Legacy.patch_family Current.patch_family
  let patch_descend = Util.wrap_base Legacy.patch_descend Current.patch_descend
  let patch_couple = Util.wrap_base Legacy.patch_couple Current.patch_couple
  let insert_string = Util.wrap_base Legacy.insert_string Current.insert_string

  let commit_patches =
    Util.wrap_base Legacy.commit_patches Current.commit_patches

  let commit_notes = Util.wrap_base Legacy.commit_notes Current.commit_notes
  let new_iper = Util.wrap_base Legacy.new_iper Current.new_iper
  let new_ifam = Util.wrap_base Legacy.new_ifam Current.new_ifam
  let insert_person = Util.wrap_base Legacy.insert_person Current.insert_person
  let insert_ascend = Util.wrap_base Legacy.insert_ascend Current.insert_ascend
  let insert_union = Util.wrap_base Legacy.insert_union Current.insert_union
  let insert_family = Util.wrap_base Legacy.insert_family Current.insert_family

  let insert_descend =
    Util.wrap_base Legacy.insert_descend Current.insert_descend

  let insert_couple = Util.wrap_base Legacy.insert_couple Current.insert_couple
  let delete_person = Util.wrap_base Legacy.delete_person Current.delete_person
  let delete_ascend = Util.wrap_base Legacy.delete_ascend Current.delete_ascend
  let delete_union = Util.wrap_base Legacy.delete_union Current.delete_union
  let delete_family = Util.wrap_base Legacy.delete_family Current.delete_family

  let delete_descend =
    Util.wrap_base Legacy.delete_descend Current.delete_descend

  let delete_couple = Util.wrap_base Legacy.delete_couple Current.delete_couple
  let person_of_key = Util.wrap_base Legacy.person_of_key Current.person_of_key

  let persons_of_name =
    Util.wrap_base Legacy.persons_of_name Current.persons_of_name

  let persons_of_first_name = function
    | Legacy_base base ->
        let spi = Legacy.persons_of_first_name base in
        Legacy_string_person_index spi
    | Current_base base ->
        let spi = Current.persons_of_first_name base in
        Current_string_person_index spi

  let persons_of_surname = function
    | Legacy_base base ->
        let spi = Legacy.persons_of_surname base in
        Legacy_string_person_index spi
    | Current_base base ->
        let spi = Current.persons_of_surname base in
        Current_string_person_index spi

  let persons_of_alias = function
    | Legacy_base base ->
        let spi = Legacy.persons_of_alias base in
        Legacy_string_person_index spi
    | Current_base base ->
        let spi = Current.persons_of_alias base in
        Current_string_person_index spi

  let spi_first = Util.wrap_spi Legacy.spi_first Current.spi_first
  let spi_next = Util.wrap_spi Legacy.spi_next Current.spi_next
  let spi_find = Util.wrap_spi Legacy.spi_find Current.spi_find

  let base_visible_get base pf iper =
    let pfl p =
      let p = Legacy_person p in
      pf p
    in
    let curr p =
      let p = Current_person p in
      pf p
    in
    match base with
    | Legacy_base base -> Legacy.base_visible_get base pfl iper
    | Current_base base -> Current.base_visible_get base curr iper

  let base_visible_write =
    Util.wrap_base Legacy.base_visible_write Current.base_visible_write

  let base_particles =
    Util.wrap_base Legacy.base_particles Current.base_particles

  let base_strings_of_first_name =
    Util.wrap_base Legacy.base_strings_of_first_name
      Current.base_strings_of_first_name

  let base_strings_of_surname =
    Util.wrap_base Legacy.base_strings_of_surname
      Current.base_strings_of_surname

  let base_strings_of_alias =
    Util.wrap_base Legacy.base_strings_of_alias Current.base_strings_of_alias

  let load_ascends_array =
    Util.wrap_base Legacy.load_ascends_array Current.load_ascends_array

  let load_unions_array =
    Util.wrap_base Legacy.load_unions_array Current.load_unions_array

  let load_couples_array =
    Util.wrap_base Legacy.load_couples_array Current.load_couples_array

  let load_descends_array =
    Util.wrap_base Legacy.load_descends_array Current.load_descends_array

  let load_strings_array =
    Util.wrap_base Legacy.load_strings_array Current.load_strings_array

  let load_persons_array =
    Util.wrap_base Legacy.load_persons_array Current.load_persons_array

  let load_families_array =
    Util.wrap_base Legacy.load_families_array Current.load_families_array

  let clear_ascends_array =
    Util.wrap_base Legacy.clear_ascends_array Current.clear_ascends_array

  let clear_unions_array =
    Util.wrap_base Legacy.clear_unions_array Current.clear_unions_array

  let clear_couples_array =
    Util.wrap_base Legacy.clear_couples_array Current.clear_couples_array

  let clear_descends_array =
    Util.wrap_base Legacy.clear_descends_array Current.clear_descends_array

  let clear_strings_array =
    Util.wrap_base Legacy.clear_strings_array Current.clear_strings_array

  let clear_persons_array =
    Util.wrap_base Legacy.clear_persons_array Current.clear_persons_array

  let clear_families_array =
    Util.wrap_base Legacy.clear_families_array Current.clear_families_array

  let base_notes_read =
    Util.wrap_base Legacy.base_notes_read Current.base_notes_read

  let base_notes_read_first_line =
    Util.wrap_base Legacy.base_notes_read_first_line
      Current.base_notes_read_first_line

  let base_notes_are_empty =
    Util.wrap_base Legacy.base_notes_are_empty Current.base_notes_are_empty

  let base_notes_origin_file =
    Util.wrap_base Legacy.base_notes_origin_file Current.base_notes_origin_file

  let base_notes_dir =
    Util.wrap_base Legacy.base_notes_dir Current.base_notes_dir

  let base_wiznotes_dir =
    Util.wrap_base Legacy.base_wiznotes_dir Current.base_wiznotes_dir

  let date_of_last_change =
    Util.wrap_base Legacy.date_of_last_change Current.date_of_last_change

  module Collection : sig
    type 'a t =
      | Legacy_collection of 'a Legacy.Collection.t
      | Current_collection of 'a Current.Collection.t
      | Dummy_collection

    val length : 'a t -> int
    val map : ('a -> 'b) -> 'a t -> 'b t
    val iter : ('a -> unit) -> 'a t -> unit
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
    val fold : ?from:int -> ?until:int -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_until : ('a -> bool) -> ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val iterator : 'a t -> unit -> 'a option
  end = struct
    type 'a t =
      | Legacy_collection of 'a Legacy.Collection.t
      | Current_collection of 'a Current.Collection.t
      | Dummy_collection

    let length = function
      | Legacy_collection c -> Legacy.Collection.length c
      | Current_collection c -> Current.Collection.length c
      | Dummy_collection -> -1

    let map f = function
      | Legacy_collection c ->
          let c = Legacy.Collection.map f c in
          Legacy_collection c
      | Current_collection c ->
          let c = Current.Collection.map f c in
          Current_collection c
      | Dummy_collection as c -> c

    let iter f = function
      | Legacy_collection c -> Legacy.Collection.iter f c
      | Current_collection c -> Current.Collection.iter f c
      | Dummy_collection -> ()

    let iteri f = function
      | Legacy_collection c -> Legacy.Collection.iteri f c
      | Current_collection c -> Current.Collection.iteri f c
      | Dummy_collection -> ()

    (* DO SOMETHING *)
    let fold ?from ?until f acc coll =
      log "FOLDING";

      let res =
        match (from, until) with
        | Some from, Some until -> (
            match coll with
            | Legacy_collection c -> Legacy.Collection.fold ~from ~until f acc c
            | Current_collection c ->
                Current.Collection.fold ~from ~until f acc c
            | Dummy_collection -> acc)
        | None, None -> (
            match coll with
            | Legacy_collection c -> Legacy.Collection.fold f acc c
            | Current_collection c -> Current.Collection.fold f acc c
            | Dummy_collection -> acc)
        | Some from, None -> (
            match coll with
            | Legacy_collection c -> Legacy.Collection.fold ~from f acc c
            | Current_collection c -> Current.Collection.fold ~from f acc c
            | Dummy_collection -> acc)
        | None, Some until -> (
            match coll with
            | Legacy_collection c -> Legacy.Collection.fold ~until f acc c
            | Current_collection c -> Current.Collection.fold ~until f acc c
            | Dummy_collection -> acc)
      in
      log "FOLDED";
      res

    let fold_until continue fn acc coll =
      match coll with
      | Legacy_collection c -> Legacy.Collection.fold_until continue fn acc c
      | Current_collection c -> Current.Collection.fold_until continue fn acc c
      | Dummy_collection -> acc

    let iterator = function
      | Legacy_collection c -> Legacy.Collection.iterator c
      | Current_collection c -> Current.Collection.iterator c
      | Dummy_collection -> fun () -> None
  end

  module Marker : sig
    type ('k, 'v) t =
      | Legacy_marker of ('k, 'v) Legacy.Marker.t
      | Current_marker of ('k, 'v) Current.Marker.t
      | Dummy_marker of 'v

    val get : ('k, 'v) t -> 'k -> 'v
    val set : ('k, 'v) t -> 'k -> 'v -> unit
  end = struct
    type ('k, 'v) t =
      | Legacy_marker of ('k, 'v) Legacy.Marker.t
      | Current_marker of ('k, 'v) Current.Marker.t
      | Dummy_marker of 'v

    let get = function
      | Legacy_marker m -> Legacy.Marker.get m
      | Current_marker m -> Current.Marker.get m
      | Dummy_marker v -> fun _ -> v

    let set = function
      | Legacy_marker m -> Legacy.Marker.set m
      | Current_marker m -> Current.Marker.set m
      | Dummy_marker _ -> fun _ _ -> ()
  end

  let ipers =
    Util.wrap_base
      (fun base -> Collection.Legacy_collection (Legacy.ipers base))
      (fun base -> Collection.Current_collection (Current.ipers base))

  let persons =
    Util.wrap_base
      (fun base ->
        let coll = Legacy.persons base in
        let coll = Legacy.Collection.map (fun p -> Legacy_person p) coll in
        Collection.Legacy_collection coll)
      (fun base ->
        let coll = Current.persons base in
        let coll = Current.Collection.map (fun p -> Current_person p) coll in
        Collection.Current_collection coll)

  let ifams ?(select = fun _ -> true) =
    Util.wrap_base
      (fun base ->
        let coll = Legacy.ifams ~select base in
        Collection.Legacy_collection coll)
      (fun base ->
        let coll = Current.ifams ~select base in
        Collection.Current_collection coll)

  let families ?(select = fun _ -> true) =
    let lselect f =
      let f = Legacy_family f in
      select f
    in
    let cselect f =
      let f = Current_family f in
      select f
    in
    Util.wrap_base
      (fun base ->
        let coll = Legacy.families ~select:lselect base in
        let coll = Legacy.Collection.map (fun f -> Legacy_family f) coll in
        Collection.Legacy_collection coll)
      (fun base ->
        let coll = Current.families ~select:cselect base in
        let coll = Current.Collection.map (fun f -> Current_family f) coll in
        Current_collection coll)

  let dummy_collection _x = Collection.Dummy_collection

  let iper_marker collection v =
    match collection with
    | Collection.Legacy_collection c ->
        let marker = Legacy.iper_marker c v in
        Marker.Legacy_marker marker
    | Collection.Current_collection c ->
        let marker = Current.iper_marker c v in
        Marker.Current_marker marker
    | Collection.Dummy_collection -> Marker.Dummy_marker v

  let ifam_marker collection v =
    match collection with
    | Collection.Legacy_collection c ->
        let marker = Legacy.ifam_marker c v in
        Marker.Legacy_marker marker
    | Collection.Current_collection c ->
        let marker = Current.ifam_marker c v in
        Marker.Current_marker marker
    | Collection.Dummy_collection -> Marker.Dummy_marker v

  let dummy_marker _k v = Marker.Dummy_marker v
  let read_nldb = Util.wrap_base Legacy.read_nldb Current.read_nldb
  let write_nldb = Util.wrap_base Legacy.write_nldb Current.write_nldb

  let sync ?(scratch = false) ~save_mem =
    Util.wrap_base
      (Legacy.sync ~scratch ~save_mem)
      (Current.sync ~scratch ~save_mem)

  let make bname particles arrays =
    let base = Current.make bname particles arrays in
    Current_base base

  let gc ?(dry_run = false) ~save_mem =
    Util.wrap_base
      (Legacy.gc ~dry_run ~save_mem)
      (Current.gc ~dry_run ~save_mem)

  let set_fpoi_cache =
    Util.wrap_base Legacy.set_fpoi_cache Current.set_fpoi_cache
end
