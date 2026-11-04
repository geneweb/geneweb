(** HTTP request decomposition and typed domain representations. *)

(** Strongly typed HTTP multipart boundary. *)
module Boundary : sig
  type t = private string
  (** A boundary delimiter string used to separate parts in a multipart HTTP body. *)

  val of_string : string -> t
  (** [of_string s] converts raw string [s] into a typed multipart boundary {!t}. *)

  val to_string : t -> string
  (** [to_string b] returns the raw string representation of boundary [b]. *)
end

(** Strongly typed HTTP header lines and case-insensitive header lookup. *)
module Headers : sig
  type t
  (** Collection of HTTP header lines. *)

  val of_lines : string list -> t
  (** [of_lines lines] creates a headers collection {!t} from a list of raw HTTP lines. *)

  val get : t -> string -> string option
  (** [get t name] searches headers [t] case-insensitively for a header matching
      [name] and returns [Some value] (trimmed of whitespace) if found, or [None]
      otherwise. *)

  val to_lines : t -> string list
  (** [to_lines t] returns the raw HTTP lines contained in headers collection [t]. *)
end

(** Legacy HTTP header extraction utilities. *)
module Header : sig
  val get : request:string list -> string -> string option
  (** [get ~request name] looks up the first header value matching [name]
      case-insensitively in the provided raw HTTP [request] headers. *)
end

(** Supported HTTP request methods. *)
type meth =
  | GET  (** HTTP GET method. *)
  | POST  (** HTTP POST method. *)
  | HEAD  (** HTTP HEAD method. *)
  | PUT  (** HTTP PUT method. *)
  | DELETE  (** HTTP DELETE method. *)
  | OPTIONS  (** HTTP OPTIONS method. *)
  | TRACE  (** HTTP TRACE method. *)
  | CONNECT  (** HTTP CONNECT method. *)
  | PATCH  (** HTTP PATCH method. *)
  | OTHER of string  (** Unrecognized or custom HTTP method token. *)

val meth_of_string : string -> meth
(** [meth_of_string s] converts an HTTP method token string to a typed {!meth}.
    The conversion is case-insensitive for standard methods. *)

val string_of_meth : meth -> string
(** [string_of_meth m] returns the standard uppercase HTTP method string
    representation for [m]. *)

(** Supported HTTP protocol versions. *)
type http_version =
  | Http_1_0  (** HTTP/1.0 protocol. *)
  | Http_1_1  (** HTTP/1.1 protocol. *)
  | Http_2_0  (** HTTP/2 protocol. *)
  | Unknown_version of string
      (** Unrecognized or custom protocol version string. *)

val string_of_http_version : http_version -> string
(** [string_of_http_version v] returns the string representation of [v]
    (e.g. ["HTTP/1.0"], ["HTTP/1.1"], ["HTTP/2.0"]). *)

val http_version_of_string : string -> http_version
(** [http_version_of_string s] parses a protocol version string into {!http_version}. *)

type path = private string
(** Strongly typed HTTP request path, stripped of leading slash and query string. *)

val path_of_string : string -> path
(** [path_of_string s] creates a typed {!path} from raw string [s]. *)

val string_of_path : path -> string
(** [string_of_path p] returns the raw string representation of [p]. *)

type query_string = private string
(** Strongly typed HTTP URL query string. *)

val query_of_string : string -> query_string
(** [query_of_string s] creates a typed {!query_string} from raw string [s]. *)

val string_of_query : query_string -> string
(** [string_of_query q] returns the raw string representation of [q]. *)

type request_line = {
  meth : meth;  (** The HTTP method. *)
  raw_target : string;  (** The raw request target URI or path as received. *)
  path : path;
      (** Request path stripped of query string and leading slash (e.g. ["base"]). *)
  query : query_string option;
      (** Optional query string following ['?'] if present. *)
  version : http_version option;
      (** Optional HTTP version if specified in the request line. *)
}
(** Decomposed HTTP request line. *)

val parse_request_line : string list -> request_line option
(** [parse_request_line request] extracts and decomposes the HTTP request line
    from the first non-empty line of the raw [request] list. Returns [None] if
    the request is empty, malformed, or missing a valid method/target. *)

(** Categorized HTTP request content type. *)
type content_type =
  | UrlEncoded  (** Standard application/x-www-form-urlencoded form data. *)
  | MultipartForm of { boundary : Boundary.t }
      (** Multipart form data (multipart/form-data) with extracted {!Boundary.t}. *)
  | Other of string  (** Any other content type (e.g. ["application/json"]). *)

val parse_content_type : string -> content_type
(** [parse_content_type header] parses a [Content-Type] header value into a typed
    representation, extracting the boundary parameter for multipart forms. *)

val extract_boundary_opt : Adef.encoded_string -> Boundary.t option
(** [extract_boundary_opt content_type] extracts the boundary parameter
    specified in the multipart [content_type] parameter. Returns [Some boundary]
    if present, or [None] if [content_type] is not a multipart form or does not
    contain a boundary parameter. *)

val extract_boundary : Adef.encoded_string -> Boundary.t
(** [extract_boundary content_type] extracts the boundary specified in
    the multipart [content_type] parameter.
    @deprecated Use {!extract_boundary_opt} instead.
    @raise Not_found if no boundary parameter is present. *)

(** Encapsulated HTTP request parameters / environment. *)
module Env : sig
  type t
  (** Encapsulated key-value environment mapping parameter names to encoded values. *)

  val empty : t
  (** The empty environment containing no key-value pairs. *)

  val of_encoded_string : Adef.encoded_string -> t
  (** [of_encoded_string s] parses a query string or form body into an environment. *)

  val of_query_string : query_string -> t
  (** [of_query_string q] converts a typed {!query_string} into an environment.
      Empty query strings yield {!empty}. *)

  val get : t -> string -> string option
  (** [get env name] retrieves the decoded value associated with key [name] in [env],
      or [None] if [name] is not present. *)

  val merge : url_env:t -> body_env:t -> t
  (** [merge ~url_env ~body_env] merges [url_env] and [body_env]. Values in
      [url_env] take precedence over duplicate keys in [body_env]. *)

  val to_list : t -> (string * Adef.encoded_string) list
  (** [to_list env] converts [env] into an association list of key-value pairs. *)

  val of_list : (string * Adef.encoded_string) list -> t
  (** [of_list pairs] creates an environment from an association list of key-value pairs. *)
end

val extract_multipart :
  Boundary.t -> Adef.encoded_string -> Adef.encoded_string * Env.t
(** [extract_multipart boundary str] parses a multipart form body [str] separated
    by [boundary], returning the reconstructed query string and parsed environment. *)

val create_env : Adef.encoded_string -> (string * Adef.encoded_string) list
(** [create_env s] parses a query string or form body into an association list
    of key-value pairs separated by '&' or ';'. *)

val p_getenv : (string * Adef.encoded_string) list -> string -> string option
(** [p_getenv env label] returns the decoded value associated with [label] in [env]. *)

val parse_query_params : query_string -> Env.t
(** [parse_query_params q] parses a URL query string into an environment. *)

val merge_env :
  (string * Adef.encoded_string) list ->
  (string * Adef.encoded_string) list ->
  (string * Adef.encoded_string) list
(** [merge_env url_params body_params] merges URL query parameters with body
    parameters, placing URL parameters first. *)
