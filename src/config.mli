(* $Id: config.mli,v 1.2 1998-09-29 16:12:15 ddr Exp $ *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    cgi : bool;
    command : string;
    lang : string;
    bname : string;
    env : list (string * string);
    senv : mutable string;
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
