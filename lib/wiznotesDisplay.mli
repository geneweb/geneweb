(* $Id: wiznotes.mli,v 5.3 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

val dir : Config.config -> Gwdb.base -> string
(** Returns the path to the wizard notes files associated to the base. *)

val print : Config.config -> Gwdb.base -> unit
(** Prints the HTML page displaying the wizard notes. Fails if wizard
    authentification is incorrect *)

val print_mod : Config.config -> Gwdb.base -> unit
(** Prints the HTML page displaying editable wizard notes. Fails if wizard
    authentification is incorrect or if current user cannot edit. *)

val print_mod_ok : Config.config -> Gwdb.base -> unit
(** Commits the modification and displays the `OK` page. Fails if wizard
    authentification is incorrect *)

val print_view : Config.config -> Gwdb.base -> unit
(** Same as `print_mod`, but works even if user cannot edit. It still fails in
    case of wrong authentification. *)

val print_search : Config.config -> Gwdb.base -> unit
(** Same as `print` but highlights HTML with the speficied string searched
    (environment key of search is `s`). If no search is specified, just prints
    the wizard notes. *)

val connected_wizards : Config.config -> Gwdb.base -> unit
(** Displays the connected wizards. *)

val change_wizard_visibility : Config.config -> Gwdb.base -> unit
(** Same as `connected_wizards`, but starts by updating the wizard visibility.
*)
