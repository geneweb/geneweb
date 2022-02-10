(* $Id: history.mli,v 5.4 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

(** Returns path to the file where history of current base updates is stored *)
val file_name : config -> string

(** [record conf change action] records new modification in the history files
    (global file and specific for each concerned by modification person).
    Additionally it does:

    - Updates [conf.default_sosa_ref] if concered by modification person is referenced by default_sosa_ref
    - Notify foreign {i notify_change} about modification on the base
      (doesn't notify if multiple modifications are done succesively) *)
val record : config -> base -> (iper, iper, ifam, string) base_changed -> string -> unit

(** [notify conf base action]
    Explicit notification of foreign script {i notify_change}
    that modification action [action] was executed on the database.
    Since [record] already does notify script about unary modification on the base,
    this function is used exclusively to send notification about multiple
    modifications and avoid creating indefinite amount of processes for each modification
    (for example for each concerned person in the list of modified persons). *)
val notify : config -> base -> string -> unit

(** Displays an history of updates *)
val print : config -> base -> unit

(** Same as `print`, but simultaneously searches for text inside the history and higlhight all found matches.
    Search pattern is available with {i s} variable in environement [conf.env]. *)
val print_search : config -> base -> unit

(* Useful stuff for who (i.e. plugins) wants to read history file. *)

(** Parses one line of history file that delimits one modification record. *)
val line_fields : string -> (string * string * string * string option) option
