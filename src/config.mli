(* $Id: config.mli,v 4.11 2004-07-18 14:26:38 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;

type config =
  { from : string;
    wizard : bool;
    friend : bool;
    just_friend_wizard : bool;
    user : string;
    passwd : string;
    username : string;
    cgi : bool;
    command : string;
    indep_command : string;
    highlight : string;
    lang : string;
    default_lang : string;
    multi_parents : bool;
    can_send_image : bool;
    public_if_titles : bool;
    public_if_no_date : bool;
    cancel_links : mutable bool;
    setup_link : mutable bool;
    access_by_key : bool;
    private_years : int;
    hide_names : bool;
    use_restrict : bool;
    no_image : bool;
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
    time : (int * int * int);
    ctime : float }
;
