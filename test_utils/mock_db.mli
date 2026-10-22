(** Builder and helpers for constructing mock [Gwdb] databases in tests. *)

type t
(** The mutable builder state representing an in-memory database under construction. *)

val create : unit -> t
(** [create ()] initializes a new empty database builder with standard default
    strings interned: index 0 is [""], index 1 is ["?"]. *)

val add_string : t -> string -> int
(** [add_string db s] interns string [s] if not present and returns its integer index. *)

val add_person :
  t ->
  ?first_name:string ->
  ?surname:string ->
  ?occ:int ->
  ?sex:Def.sex ->
  ?access:Def.access ->
  ?titles:(string Def.gen_title_name * string * string) list ->
  ?parents:int ->
  unit ->
  int
(** [add_person db ?first_name ?surname ?occ ?sex ?access ?titles ?parents ()] registers a person
    with the given attributes and returns their integer identifier.
    - [access] defaults to [Def.Public].
    - [titles] is a list of [(t_name, t_ident, t_place)]. *)

val add_family :
  t ->
  ?father:int ->
  ?mother:int ->
  ?children:int list ->
  ?fsources:string ->
  unit ->
  int
(** [add_family db ?father ?mother ?children ?fsources ()] registers a family
    with the given parents, children, and source string, updating parent union
    lists and child parent links, and returns its integer identifier.
    - [father] defaults to [-1] (no father).
    - [mother] defaults to [-1] (no mother).
    - [children] defaults to [[]].
    - [fsources] defaults to [""]. *)

val set_unions : t -> person:int -> int list -> unit
(** [set_unions db ~person unions] replaces the list of family indices for
    person [person] with [unions]. Does nothing if [person] is not registered. *)

val set_parents : t -> person:int -> int option -> unit
(** [set_parents db ~person parents] sets or clears the parent family index for
    person [person]. Does nothing if [person] is not registered. *)

val set_children : t -> family:int -> int list -> unit
(** [set_children db ~family children] replaces the list of child person indices
    for family [family] with [children]. Does nothing if [family] is not registered. *)

val build : ?bname:string -> t -> Gwdb.base
(** [build ?bname db] finalizes the mock database and constructs an in-memory
    [Gwdb.base] instance with all data tables loaded.
    - [bname] is the database name, defaulting to [""]. *)

val iper : int -> Gwdb.iper
(** [iper i] converts integer person index [i] into a [Gwdb.iper] identifier. *)

val ifam : int -> Gwdb.ifam
(** [ifam i] converts integer family index [i] into a [Gwdb.ifam] identifier. *)

val istr : int -> Gwdb.istr
(** [istr i] converts integer string index [i] into a [Gwdb.istr] identifier. *)

val poi : Gwdb.base -> int -> Gwdb.person
(** [poi base i] retrieves the [Gwdb.person] record for person index [i] from [base].
    Shorthand for [Gwdb.poi base (iper i)]. *)

val foi : Gwdb.base -> int -> Gwdb.family
(** [foi base i] retrieves the [Gwdb.family] record for family index [i] from [base].
    Shorthand for [Gwdb.foi base (ifam i)]. *)

val sou : Gwdb.base -> int -> string
(** [sou base i] resolves the string at index [i] in [base].
    Shorthand for [Gwdb.sou base (istr i)]. *)
