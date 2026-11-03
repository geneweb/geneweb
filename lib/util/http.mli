(** HTTP request decomposition and typed domain representations. *)

(** HTTP header extraction utilities. *)
module Header : sig
  val get : request:string list -> string -> string option
  (** [get ~request name] looks up the first header value matching [name]
      case-insensitively in the provided raw HTTP [request] headers. *)
end

type meth =
  | GET
  | POST
  | HEAD
  | PUT
  | DELETE
  | OPTIONS
  | TRACE
  | CONNECT
  | PATCH
  | OTHER of string  (** HTTP request method. *)

val meth_of_string : string -> meth
(** [meth_of_string s] converts an HTTP method string to a typed [meth]. *)

val string_of_meth : meth -> string
(** [string_of_meth m] returns the standard uppercase HTTP method string. *)

type request_line = {
  meth : meth;
  raw_target : string;
  path : string;
  query : string option;
  version : string option;
}
(** Decomposed HTTP request line.
    [path] is the request path stripped of query string and leading slash (e.g. ["base"]).
    [query] is the optional query string following [?]. *)

val parse_request_line : string list -> request_line option
(** [parse_request_line request] extracts and decomposes the HTTP request line
    from the first line of the raw [request] list. Returns [None] if the request
    is empty or malformed. *)

type content_type =
  | UrlEncoded
  | MultipartForm of { boundary : string }
  | Other of string  (** Categorized HTTP request content type. *)

val parse_content_type : string -> content_type
(** [parse_content_type header] parses a [Content-Type] header value into a typed
    representation, extracting the boundary parameter for multipart forms. *)

val extract_boundary_opt : Adef.encoded_string -> string option
(** [extract_boundary_opt content_type] extracts the boundary parameter string
    specified in the multipart [content_type] parameter. Returns [Some boundary]
    if present, or [None] if [content_type] is not a multipart form or does not
    contain a boundary parameter. *)

val extract_boundary : Adef.encoded_string -> string
(** [extract_boundary content_type] extracts the boundary string specified in
    the multipart [content_type] parameter.
    @deprecated Use [extract_boundary_opt] instead.
    @raise Not_found if no boundary parameter is present. *)

val extract_multipart :
  string ->
  Adef.encoded_string ->
  Adef.encoded_string * (string * Adef.encoded_string) list
(** [extract_multipart boundary str] parses a multipart form body [str] separated
    by [boundary], returning the reconstructed query string and parsed environment
    association list. *)

val create_env : Adef.encoded_string -> (string * Adef.encoded_string) list
(** [create_env s] parses a query string or form body into an association list
    of key-value pairs separated by '&' or ';'. *)

val p_getenv : (string * Adef.encoded_string) list -> string -> string option
(** [p_getenv env label] returns the decoded value associated with [label] in [env]. *)

val parse_query_params : string -> (string * Adef.encoded_string) list
(** [parse_query_params q] parses a URL query string (e.g. ["m=NG&b=base"]) into
    an environment association list. *)

val merge_env :
  (string * Adef.encoded_string) list ->
  (string * Adef.encoded_string) list ->
  (string * Adef.encoded_string) list
(** [merge_env url_params body_params] merges URL query parameters with body
    parameters, placing URL parameters first. *)
