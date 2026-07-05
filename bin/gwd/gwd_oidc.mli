open Geneweb

val cookie_access : string list -> string -> (char * string * string) option
(** [cookie_access request base_name] returns the access rights (['w'] wizard or
    ['f'] friend, together with the user and username) granted by a valid OIDC
    session cookie found in [request] for [base_name], or [None]. *)

val handle_mode :
  Config.config ->
  (string * string) list ->
  string ->
  string ->
  float ->
  string option ->
  bool
(** [handle_mode conf base_env from_addr base_file utm mode] intercepts the OIDC
    request modes (login, callback, logout) and auto-detects OIDC callbacks.
    Returns [true] when the request was an OIDC one and has been handled. *)
