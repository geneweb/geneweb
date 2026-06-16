(** This module provides type-aware functions for encoding and decoding JSON
    messages. *)

(* The type [!'a t] would be more precise, but OCaml 4.08 does not support
   the injectivity annotation. *)
type 'a t
(** Type of encoding of the 'a value. *)

type error = [ `Json_error of string | `Invalid_type of string ]

val pp_error : error Fmt.t
(** [pp_error ppf e] prints the error message for logs. *)

type 'a res = ('a, error) result

val val_of_json : 'a t -> Yojson.Safe.t -> 'a res
(** [val_of_json e j] returns the value of type 'a that is encoded in [j] or a
    string error if [j] does not encode such a value. *)

val val_to_json : 'a t -> 'a -> Yojson.Safe.t
(** [val_to_json e v j] returns the JSON representation of the value [v]. *)

val enum : name:string -> ('a * string) list -> 'a t
(** [enum ~name l] returns the encoding of an enum type. [l] is an associative
    list between constructors of ['a] and their JSON representation.

    The [name] string is used for printing.

    The underlying encoding will raise {!exception:Failwith} if it is called on
    a constructor of type ['a] that is not present in [l]. *)

val generic :
  to_json:('a -> Yojson.Safe.t) ->
  of_json:(Yojson.Safe.t -> 'a res) ->
  pp:unit Fmt.t ->
  'a t
(** [generic ~to_json ~of_jon ~pp] creates a encoding for the type ['a] using
    the encoder and decoder given as argument. [pp] is used to print this
    encoding in [pp] and [pp_desc]. *)

val pp : 'a t Fmt.t
(** [pp ppf e] prints the encoding [e] for debugging purposes. *)

module Syntax : sig
  val unit : unit t
  (** Encoding of unit. *)

  val bool : bool t
  (** Encoding of boolean. *)

  val float : float t
  (** Encoding of floating number. *)

  val int : int t
  (** Encoding of an integer. *)

  val option : 'a t -> 'a option t
  (** Encoding of an option. *)

  val list : 'a t -> 'a list t
  (** Encoding of a homogeneous list. *)

  val array : 'a t -> 'a array t
  (** Encoding of an array. *)

  val string : string t
  (** Encoding of a string. *)

  val tup2 : 'a t -> 'b t -> ('a * 'b) t
  (** Encoding of a pair. *)

  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  (** Encoding of a triple. *)

  val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  (** Encoding of a quadruple. *)
end
