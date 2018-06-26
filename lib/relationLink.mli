(* $Id: relationLink.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

type info =
  { ip : iper;
    sp : sex;
    ip1 : iper;
    ip2 : iper;
    b1 : (iper * sex) list;
    b2 : (iper * sex) list;
    c1 : int;
    c2 : int;
    pb1 : (iper * sex) list option;
    pb2 : (iper * sex) list option;
    nb1 : (iper * sex) list option;
    nb2 : (iper * sex) list option;
    sp1 : person option;
    sp2 : person option;
    bd : int;
    td_prop : string }

val threshold : int ref
val make_dist_tab :
  config -> base -> iper -> int -> (iper -> int) * (iper -> int)
val find_first_branch :
  config -> base -> (iper -> int) * (iper -> int) -> iper -> int -> iper ->
    sex -> (iper * sex) list option
val print_relation_path : config -> base -> info -> unit

val print : config -> base -> unit
