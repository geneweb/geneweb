(* $Id: config.mli,v 2.2 1999-03-30 10:46:07 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    cgi : bool;
    command : string;
    lang : string;
    can_send_image : bool;
    bname : string;
    env : list (string * string);
    senv : mutable list (string * string);
    henv : mutable list (string * string);
    base_env : list (string * string);
    request : list string;
    lexicon : Hashtbl.t string string;
    charset : string;
    is_rtl : bool;
    today : date;
    today_wd : int }
;
