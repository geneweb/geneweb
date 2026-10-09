val enabled : (string * string) list -> bool
(** [enabled base_env] is [true] when [oidc_provider_url] is set for the base,
    i.e. OIDC is turned on (even if other required keys are still missing). *)

val cookie_access :
  secret:string -> string list -> string -> (char * string * string) option
(** Access ([w]/[f], user, username) from a valid signed OIDC session cookie in
    [request] for the base, or [None]. [secret] keys the cookie's HMAC. *)

val session_timeout : (string * string) list -> int
(** OIDC session lifetime in seconds: [oidc_session_timeout] from the base
    environment when set to a positive integer, otherwise the global
    [login_timeout]. *)

val renew_session :
  Geneweb.Config.config ->
  base_file:string ->
  acc:char ->
  user:string ->
  username:string ->
  unit
(** Re-issue the OIDC session cookie with a renewed expiry (sliding session), so
    the timeout applies to inactivity rather than to the time elapsed since
    login. *)

val handle_mode :
  Geneweb_http.Connection.t -> Geneweb.Config.config -> string option -> bool
(** Handle the OIDC modes (login, callback, logout) and auto-detected callbacks.
    Returns [true] if the request was OIDC and has been handled. *)
