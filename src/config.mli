(* $Id: config.mli,v 1.7 1999-02-02 10:24:01 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    cgi : bool;
    command : string;
    lang : string;
    bname : string;
    env : list (string * string);
    senv : mutable list (string * string);
    henv : mutable list (string * string);
    base_env : list (string * string);
    request : list string;
    lexicon : Hashtbl.t string string;
    charset : string;
    today : date;
    today_wd : int }
;
