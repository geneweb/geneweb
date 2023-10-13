(* $Id: wiznotes.mli,v 5.3 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Config
open Gwdb

val dir : config -> base -> string
(** Returns the path to the wizard notes files associated to the base. *)

val print : config -> base -> unit
(** Prints the HTML page displaying the wizard notes.
    Fails if wizard authentification is incorrect *)

val print_mod : config -> base -> unit
(** Prints the HTML page displaying editable wizard notes.
    Fails if wizard authentification is incorrect or if current user cannot
    edit. *)

val print_mod_ok : config -> base -> unit
(** Commits the modification and displays the `OK` page.
    Fails if wizard authentification is incorrect *)

val print_view : config -> base -> unit
(** Same as `print_mod`, but works even if user cannot edit.
    It still fails in case of wrong authentification. *)

val print_search : config -> base -> unit
(** Same as `print` but highlights HTML with the speficied
    string searched (environment key of search is `s`).
    If no search is specified, just prints the wizard notes. *)

val connected_wizards : config -> base -> unit
(** Displays the connected wizards. *)

val change_wizard_visibility : config -> base -> unit
(** Same as `connected_wizards`, but starts by updating the wizard
    visibility. *)
