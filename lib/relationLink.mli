(* $Id: relationLink.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

type info = {
  ip : Geneweb_db.Driver.iper;
  sp : sex;
  ip1 : Geneweb_db.Driver.iper;
  ip2 : Geneweb_db.Driver.iper;
  b1 : (Geneweb_db.Driver.iper * sex) list;
  b2 : (Geneweb_db.Driver.iper * sex) list;
  c1 : int;
  c2 : int;
  pb1 : (Geneweb_db.Driver.iper * sex) list option;
  pb2 : (Geneweb_db.Driver.iper * sex) list option;
  nb1 : (Geneweb_db.Driver.iper * sex) list option;
  nb2 : (Geneweb_db.Driver.iper * sex) list option;
  sp1 : Geneweb_db.Driver.person option;
  sp2 : Geneweb_db.Driver.person option;
  bd : int;
  td_prop : Adef.safe_string;
}

val threshold : int ref

val make_dist_tab :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  int ->
  (Geneweb_db.Driver.iper -> int) * (Geneweb_db.Driver.iper -> int)

val find_first_branch :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper -> int) * (Geneweb_db.Driver.iper -> int) ->
  Geneweb_db.Driver.iper ->
  int ->
  Geneweb_db.Driver.iper ->
  sex ->
  (Geneweb_db.Driver.iper * sex) list option

val print_relation_path : config -> Geneweb_db.Driver.base -> info -> unit
val print : config -> Geneweb_db.Driver.base -> unit
