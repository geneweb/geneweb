(* $Id: config.mli,v 3.4 2000-01-10 02:14:37 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;

type config =
  { wizard : bool;
    friend : bool;
    just_friend_wizard : bool;
    user : string;
    passwd : string;
    cgi : bool;
    command : string;
    indep_command : string;
    highlight : string;
    lang : string;
    default_lang : string;
    can_send_image : bool;
    cancel_links : mutable bool;
    access_by_key : bool;
    bname : string;
    env : list (string * string);
    senv : mutable list (string * string);
    henv : mutable list (string * string);
    base_env : list (string * string);
    request : list string;
    lexicon : Hashtbl.t string string;
    charset : string;
    is_rtl : bool;
    auth_file : string;
    border : int;
    today : dmy;
    today_wd : int;
    time : (int * int * int) }
;
