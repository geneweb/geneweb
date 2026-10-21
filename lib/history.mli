(* $Id: history.mli v7.1 16/10/2026 00:19:10 $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

val file_name : config -> string
(** Returns path to the file where history of current base updates is stored *)

val record :
  config ->
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.ifam,
    string )
  base_changed ->
  string ->
  unit
(** [record conf base changed action] records the update [changed], of code
    [action], after a base modification:
    - updates the default Sosa reference in the .gwf file when the modified
      person is that reference;
    - appends a line to the history file when [history=yes] and the user is not
      manitou, and records the person diff ({!HistoryDiff.record_diff});
    - calls the external [notify_change] script, except for [U_Multi] updates,
      which are notified once by {!notify}. *)

val notify : config -> Geneweb_db.Driver.base -> string -> unit
(** [notify conf base action] Explicit notification of foreign script
    {i notify_change} that modification action [action] was executed on the
    database. Since [record] already does notify script about unary modification
    on the base, this function is used exclusively to send notification about
    multiple modifications and avoid creating indefinite amount of processes for
    each modification (for example for each concerned person in the list of
    modified persons). *)

val print : config -> Geneweb_db.Driver.base -> unit
(** Displays [k] lines of the history of updates, read backwards from position
    [pos], restricted to the wizard [wiz] when set. When [s] is set, only the
    lines whose date, user or key contain [s] (case sensitive if [c=on]) are
    displayed, with the matches highlighted. *)

val line_fields : string -> (string * string * string * string option) option
(** [line_fields line] parses one line of the history file into
    [(time, user, action, key)], [key] being the person key or the note page
    when present; [None] for a line too short to be a record. *)
