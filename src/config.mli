(* $Id: config.mli,v 1.5 1999-01-06 10:47:43 ddr Exp $ *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    has_wizard_passwd : bool;
    has_friend_passwd : bool;
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
