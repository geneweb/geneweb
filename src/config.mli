(* $Id: config.mli,v 1.6 1999-01-08 10:22:54 roglo Exp $ *)

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
