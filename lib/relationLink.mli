(* $Id: relationLink.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type info = {
  ip : Gwdb.iper;
  sp : Def.sex;
  ip1 : Gwdb.iper;
  ip2 : Gwdb.iper;
  b1 : (Gwdb.iper * Def.sex) list;
  b2 : (Gwdb.iper * Def.sex) list;
  c1 : int;
  c2 : int;
  pb1 : (Gwdb.iper * Def.sex) list option;
  pb2 : (Gwdb.iper * Def.sex) list option;
  nb1 : (Gwdb.iper * Def.sex) list option;
  nb2 : (Gwdb.iper * Def.sex) list option;
  sp1 : Gwdb.person option;
  sp2 : Gwdb.person option;
  bd : int;
  td_prop : Adef.safe_string;
}

val threshold : int ref

val make_dist_tab :
  Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  int ->
  (Gwdb.iper -> int) * (Gwdb.iper -> int)

val find_first_branch :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper -> int) * (Gwdb.iper -> int) ->
  Gwdb.iper ->
  int ->
  Gwdb.iper ->
  Def.sex ->
  (Gwdb.iper * Def.sex) list option

val print_relation_path : Config.config -> Gwdb.base -> info -> unit
val print : Config.config -> Gwdb.base -> unit
