(* $Id: config.mli,v 2.1 1999-03-08 11:18:28 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    cgi : bool;
    command : string;
    lang : string;
    can_send_photo : bool;
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
