(* This module is intended to implement version 0.8 of the XDG Base Directory
   specification for UNIX-like systems.

   Note that this module does not perform any checks regarding directory
   permissions or existence. Such sanity checks must be handled by the
   calling binary.

   See: https://specifications.freedesktop.org/basedir/0.8/ *)

type t
(** Type of XDG environment. *)

val make : ?runtime_dir:string -> getenv:(string -> string option) -> unit -> t
(** [make ?runtime_dir ~getenv ()] creates a XDG environment using [getenv] to
    read the environment variables.

    If [runtime_dir] is specified, it must be owned by the user with 700
    permissions. *)

val data_home : t -> Var.one Var.t
(** [data_home] defines the base directory relative to which user-specific data
    files should be stored. *)

val config_home : t -> Var.one Var.t
(** [config_home] defines the base directory relative to which user-specific
    configuration files should be stored. *)

val state_home : t -> Var.one Var.t
(** [state_home] defines the base directory relative to which user-specific
    files should be stored. *)

val cache_home : t -> Var.one Var.t
(** [cache_home] defines the base directory relative to which user-specific
    non-essential data files should be stored. *)

val runtime_dir : t -> Var.one Var.t option
(** [runtime_dir] defines the base directory relative to which user-specific
    non-essential runtime files and other file objects (such as sockets, named
    pipes, ...) should be stored. *)

val data_dirs : t -> Var.many Var.t
(** [data_dirs] defines the preference-ordered set of base directories to search
    for data files in addition to the [data_home] base directory. *)

val config_dirs : t -> Var.many Var.t
(** [config_dirs] defines the preference-ordered set of base directories to
    search for configuration files in addition to the [config_home] base
    directory. *)
