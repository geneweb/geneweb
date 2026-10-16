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
  ?parents:int ->
  unit ->
  int
(** [add_person db ?first_name ?surname ?occ ?sex ?parents ()] registers a person
    with the given attributes and returns their integer identifier. If [parents]
    is specified (as a family index), the person is linked to that family as child. *)

val add_family :
  t ->
  ?father:int ->
  ?mother:int ->
  ?children:int list ->
  ?fsources:string ->
  unit ->
  int
(** [add_family db ?father ?mother ?children ?fsources ()] registers a family
    with the given father, mother, and children indices, updates the spouses'
    unions and children's parent pointers automatically, and returns the family index. *)

val set_unions : t -> person:int -> int list -> unit
(** [set_unions db ~person unions] overrides the list of family indices for a person. *)

val set_parents : t -> person:int -> int option -> unit
(** [set_parents db ~person parents] overrides the parents family index for a person. *)

val set_children : t -> family:int -> int list -> unit
(** [set_children db ~family children] overrides the children person indices for a family. *)

val build : ?bname:string -> t -> Gwdb.base
(** [build ?bname db] compiles the builder into a fully initialized [Gwdb.base],
    loading arrays into memory so that the base is immediately ready for queries. *)

val iper : int -> Gwdb.iper
(** [iper i] converts an integer person identifier into a [Gwdb.iper]. *)

val ifam : int -> Gwdb.ifam
(** [ifam i] converts an integer family identifier into a [Gwdb.ifam]. *)

val istr : int -> Gwdb.istr
(** [istr i] converts an integer string identifier into a [Gwdb.istr]. *)

val poi : Gwdb.base -> int -> Gwdb.person
(** [poi base i] retrieves the [Gwdb.person] at index [i]. *)

val foi : Gwdb.base -> int -> Gwdb.family
(** [foi base i] retrieves the [Gwdb.family] at index [i]. *)

val sou : Gwdb.base -> int -> string
(** [sou base i] retrieves the string value at index [i]. *)
