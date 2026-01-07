(* $Id: history.mli,v 5.4 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val file_name : Config.config -> string
(** Returns path to the file where history of current base updates is stored *)

val record :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.ifam, Gwdb.istr) Def.base_changed ->
  string ->
  unit
(** [record conf change action] records new modification in the history files
    (global file and specific for each concerned by modification person).
    Additionally it does:

    - Updates [conf.default_sosa_ref] if concered by modification person is
      referenced by default_sosa_ref
    - Notify foreign {i notify_change} about modification on the base (doesn't
      notify if multiple modifications are done succesively) *)

val notify : Config.config -> Gwdb.base -> string -> unit
(** [notify conf base action] Explicit notification of foreign script
    {i notify_change} that modification action [action] was executed on the
    database. Since [record] already does notify script about unary modification
    on the base, this function is used exclusively to send notification about
    multiple modifications and avoid creating indefinite amount of processes for
    each modification (for example for each concerned person in the list of
    modified persons). *)

val print : Config.config -> Gwdb.base -> unit
(** Displays an history of updates *)

val print_search : Config.config -> Gwdb.base -> unit
(** Same as `print`, but simultaneously searches for text inside the history and
    higlhight all found matches. Search pattern is available with {i s} variable
    in environement [conf.env]. *)

(* Useful stuff for who (i.e. plugins) wants to read history file. *)

val line_fields : string -> (string * string * string * string option) option
(** Parses one line of history file that delimits one modification record. *)

val filter_map_history :
  conf:Config.config ->
  skip:int ->
  n:int ->
  filter:
    (time:string -> user:string -> action:string -> keyo:string option -> bool) ->
  f:(time:string -> user:string -> action:string -> keyo:string option -> 'a) ->
  'a list
(** [filter_map_history conf skip n filter f] returns
    [f(h[skip]); ...; f(h[skip+n])] such that h is the history of updates in
    reverse chronological order filtered by the [filter] function. *)

val total_entries :
  conf:Config.config ->
  filter:
    (time:string -> user:string -> action:string -> keyo:string option -> bool) ->
  int
