(* $Id: merge.mli,v 5.5 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val print_someone : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Prints person's key on the socket *)

val print : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** Displays a menu for merging two persons *)

val print_possible_continue_merging : Config.config -> Gwdb.base -> unit
(** Prints link on the page to continue merging two persons (or two
    duplications). *)
