(* $Id: config.mli,v 4.0 2001-03-16 19:34:29 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

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
    public_if_titles : bool;
    cancel_links : mutable bool;
    access_by_key : bool;
    private_years : int;
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
