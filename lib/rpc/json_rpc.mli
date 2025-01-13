(** Implementation of the JSON-RPC 2.0 Specification.
    See https://www.jsonrpc.org/specification for details.

    The API is partially inspired by the JSON-RPC implementation
    in ocaml-lsp: https://github.com/ocaml/ocaml-lsp/tree/master *)

type json = Yojson.Safe.t

module Id : sig
  type t = [ `String of string | `Int of int ]

  val hash : t -> int
  val equal : t -> t -> bool
  val to_json : t -> json
  val pp : t Fmt.t
end

module Structured : sig
  type t = [ `Assoc of (string * json) list | `List of json list ]

  val to_json : t -> json
  val pp : t Fmt.t
end

module Notification : sig
  type t = private { meth : string; params : Structured.t option }

  val make : ?params:Structured.t -> string -> t
  val to_json : t -> json
  val of_json : json -> t option
  val pp : t Fmt.t
end

module Request : sig
  type t = private { id : Id.t; meth : string; params : Structured.t option }

  val make : ?params:Structured.t -> Id.t -> string -> t
  val to_json : t -> json
  val of_json : json -> t option
  val pp : t Fmt.t
end

module Response : sig
  module Error : sig
    type t = private { code : int; message : string; data : json option }

    val parse_error : ?data:json -> unit -> t
    (** Invalid JSON was received by the server. An error occurred on the
      server while parsing the JSON text. *)

    val invalid_request : ?data:json -> unit -> t
    (** The JSON sent is not a valid Request object. *)

    val method_not_found : ?data:json -> unit -> t
    (** The method does not exist / is not available. *)

    val invalid_params : ?data:json -> unit -> t
    (** Invalid method parameter(s). *)

    val internal_error : ?data:json -> unit -> t
    (** Internal JSON-RPC error. *)

    val server_error : ?data:json -> code:int -> string -> t
    (** Reserved for implementation-defined server-errors.

        @raise Failwith if the code is not between -32099 and -32000. *)

    val to_json : t -> json
    val of_json : json -> t option
    val pp : t Fmt.t
  end

  type t = private { id : Id.t option; result : (json, Error.t) Result.t }

  val ok : id:Id.t -> json -> t
  val error : ?id:Id.t -> Error.t -> t
  val to_json : t -> json
  val of_json : json -> t option
  val pp : t Fmt.t
end
