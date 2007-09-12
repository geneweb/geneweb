(* camlp5r *)
(* $Id: relationLink.mli,v 5.4 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;

type info =
  { ip : iper;
    sp : sex;
    ip1 : iper;
    ip2 : iper;
    b1 : list (iper * sex);
    b2 : list (iper * sex);
    c1 : int;
    c2 : int;
    pb1 : option (list (iper * sex));
    pb2 : option (list (iper * sex));
    nb1 : option (list (iper * sex));
    nb2 : option (list (iper * sex));
    sp1 : option person;
    sp2 : option person;
    bd : int;
    td_prop : string }
;

value threshold : ref int;
value make_dist_tab :
  config -> base -> iper -> int -> (iper -> int * iper -> int);
value find_first_branch :
  config -> base -> (iper -> int * iper -> int) ->
    iper -> int -> iper -> sex -> option (list (iper * sex));
value print_relation_path : config -> base -> info -> unit;

value print : config -> base -> unit;
