(* $Id: config.mli,v 1.3 1998-11-21 10:54:09 ddr Exp $ *)

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
    today_d : int;
    today_m : int;
    today_y : int;
    today_wd : int }
;
