val cookie_access : string list -> string -> (char * string * string) option
(** Access ([w]/[f], user, username) from a valid OIDC session cookie in
    [request] for the base, or [None]. *)

val handle_mode : Geneweb.Config.config -> string option -> bool
(** Handle the OIDC modes (login, callback, logout) and auto-detected callbacks.
    Returns [true] if the request was OIDC and has been handled. *)
