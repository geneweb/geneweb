type error = [ Encoding.error | `Bad_arity ]

val pp_error : error Fmt.t

type 'a res = ('a, error) Lwt_result.t

module Desc : sig
  (* The type [(!'a, !'r) t] would be more precise, but OCaml 4.08 does
     not support the injectivity annotation. *)
  type ('a, 'r) t
  (** Type of the encoding of an arrow of type 'a. The parametric type [r] is
      the return type of the arrow. *)

  val eval : ('a, _) t -> 'a -> Yojson.Safe.t list -> Yojson.Safe.t res
  val arity : ('a, _) t -> int

  val pp_desc : ('a, 'r) t Fmt.t
  (** [pp_desc ppf e] printst the encoding [e] on the formatter [ppf] for
      debugging purposes. *)

  module Syntax : sig
    include module type of Encoding.Syntax

    val ret : 'a Encoding.t -> ('a res, 'a) t
    val ( @-> ) : 'a Encoding.t -> ('b, 'r) t -> ('a -> 'b, 'r) t
  end
end

type ('a, 'r) meth = private { name : string; desc : ('a, 'r) Desc.t; f : 'a }
(** Type of method. *)

val decl : string -> ('a, 'r) Desc.t -> 'a -> ('a, 'r) meth
(** [decl n desc f] declares the method [n] with description [desc] and the
    underlying function [f]. *)

type t
(** Type of a service. A service is just a heterogeneous set of methods. *)

type binding = private
  | Binding : ('a, 'r) Desc.t * 'a -> binding
      (** Internal binding used to store the methods. *)

val empty : t
(** An empty service. *)

val add : ('a, 'r) meth -> t -> t
(** [add meth s] adds the method [meth] in the service [s]. *)

val find : string -> t -> binding option
(** [find name s] finds the method [name] in the service [s]. *)

val fold : (string -> binding -> 'a -> 'a) -> t -> 'a -> 'a

module PingPong : sig
  val ping : (string res, string) meth
  val echo : (string -> string res, string) meth
  val srv : t
end
