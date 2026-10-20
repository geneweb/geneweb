(* $Id: wiznotes.mli,v 5.3 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Config

val wiz_dir : config -> Geneweb_db.Driver.base -> string
(** Returns the path to the wizard notes files associated to the base. *)

val wizard_auth_file_name : config -> string
(** The wizard description (or password) file of the base, [""] if none: wizard
    notes are only available with one. *)

val wizards : config -> Geneweb_db.Driver.base -> string list
(** The wizards having a notes file, sorted. *)

val wizard_note : config -> Geneweb_db.Driver.base -> string -> string
(** [wizard_note conf base wz] is the text of the notes of wizard [wz]. *)

val wizard_display_name : config -> string -> string
(** [wizard_display_name conf wz] is the name of wizard [wz] as given in the
    wizard description file, [wz] if not found. *)

val print : config -> Geneweb_db.Driver.base -> unit
(** Prints the HTML page displaying the wizard notes. Fails if wizard
    authentification is incorrect *)

val print_mod : config -> Geneweb_db.Driver.base -> unit
(** Prints the HTML page displaying editable wizard notes. Fails if wizard
    authentification is incorrect or if current user cannot edit. *)

val print_mod_ok : config -> Geneweb_db.Driver.base -> unit
(** Commits the modification and displays the `OK` page. Fails if wizard
    authentification is incorrect *)

val print_view : config -> Geneweb_db.Driver.base -> unit
(** Same as `print_mod`, but works even if user cannot edit. It still fails in
    case of wrong authentification. *)

val print_search : config -> Geneweb_db.Driver.base -> unit
(** Same as `print` but highlights HTML with the speficied string searched
    (environment key of search is `s`). If no search is specified, just prints
    the wizard notes. *)

val connected_wizards : config -> Geneweb_db.Driver.base -> unit
(** Displays the connected wizards. *)

val change_wizard_visibility : config -> Geneweb_db.Driver.base -> unit
(** Same as `connected_wizards`, but starts by updating the wizard visibility.
*)
