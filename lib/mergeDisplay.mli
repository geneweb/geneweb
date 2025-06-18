(* $Id: merge.mli,v 5.5 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

val print_someone :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Prints person's key on the socket *)

val print : config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Displays a menu for merging two persons *)

val print_possible_continue_merging : config -> Geneweb_db.Driver.base -> unit
(** Prints link on the page to continue merging two persons (or two
    duplications). *)
