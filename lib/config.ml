(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb

type auth_scheme_kind =
    NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme
and token_auth_scheme = { ts_user : string; ts_pass : string }
and http_auth_scheme =
    Basic of basic_auth_scheme
  | Digest of digest_auth_scheme
and basic_auth_scheme =
  { bs_realm : string; bs_user : string; bs_pass : string }
and digest_auth_scheme =
  { ds_username : string;
    ds_realm : string;
    ds_nonce : string;
    ds_meth : string;
    ds_uri : string;
    ds_qop : string;
    ds_nc : string;
    ds_cnonce : string;
    ds_response : string }

type output_conf =
  { status : Def.httpStatus -> unit
  ; header : string -> unit
  ; body : string -> unit
  ; file : string -> string -> bool -> bool
  ; flush : unit -> unit
  }

type config =
  { from : string;
#ifdef API
    api_host : string;
    api_port : int;
#endif
    manitou : bool;
    supervisor : bool;
    wizard : bool;
    is_printed_by_template : bool;
    trace_templ : bool;
    friend : bool;
    just_friend_wizard : bool;
    user : string;
    username : string;
    auth_scheme : auth_scheme_kind;
    command : string;
    indep_command : string;
    highlight : string;
    lang : string;
    default_lang : string;
    default_sosa_ref : iper * Gwdb.person option;
    multi_parents : bool;
    can_send_image : bool;
    authorized_wizards_notes : bool;
    public_if_titles : bool;
    public_if_no_date : bool;
    mutable setup_link : bool;
    access_by_key : bool;
    private_years : int;
    hide_names : bool;
    use_restrict : bool;
    no_image : bool;
    no_note : bool;
    bname : string;
    cgi_passwd : string;
    env : (string * string) list;
    mutable senv : (string * string) list;
    mutable henv : (string * string) list;
    base_env : (string * string) list;
    allowed_titles : string list Lazy.t;
    denied_titles : string list Lazy.t;
    request : string list;
    lexicon : (string, string) Hashtbl.t;
    mutable charset : string;
    is_rtl : bool;
    left : string;
    right : string;
    auth_file : string;
    border : int;
    mutable n_connect : (int * int * int * (string * float) list) option;
    today : dmy;
    today_wd : int;
    time : int * int * int;
    ctime : float;
    mutable output_conf : output_conf ;
    (* prefix for image urls:
       the value of argument -images_url if specified, otherwise
       command ^ "?m=IM&v=" in CGI mode
       "images" otherwise *)
    image_prefix : string;

    (* if true, the base name is in the b argument of the query string: ?b=BASE&...
       if false, the base name is the last element of the uri path: .../base?... *)
    cgi : bool }

(**/**)
(**  A dummy {!type:config} value, with uninitialized fields.
     Used for testing purpose *)
let empty =
  { from = ""
  ; manitou = false
  ; supervisor = false
  ; wizard = false
#ifdef API
  ; api_host = ""
  ; api_port = 0
#endif
  ; is_printed_by_template = false
  ; trace_templ = false
  ; friend = false
  ; just_friend_wizard = false
  ; user = ""
  ; username = ""
  ; auth_scheme = NoAuth
  ; command = ""
  ; indep_command = ""
  ; highlight = ""
  ; lang = ""
  ; default_lang = ""
  ; default_sosa_ref = Gwdb.dummy_iper, None
  ; multi_parents = false
  ; can_send_image = false
  ; authorized_wizards_notes = false
  ; public_if_titles = false
  ; public_if_no_date = false
  ; setup_link = false
  ; access_by_key = false
  ; private_years = 0
  ; hide_names = false
  ; use_restrict = false
  ; no_image = false
  ; no_note = false
  ; bname = ""
  ; cgi_passwd = ""
  ; env = []
  ; senv = []
  ; henv = []
  ; base_env = []
  ; allowed_titles = lazy []
  ; denied_titles = lazy []
  ; request = []
  ; lexicon = Hashtbl.create 16
  ; charset = ""
  ; is_rtl = false
  ; left = ""
  ; right = ""
  ; auth_file = ""
  ; border = 0
  ; n_connect = None
  ; today = { Def.day = 0 ; month = 0 ; year = 0 ; delta = 0 ; prec = Def.Sure }
  ; today_wd = 0
  ; time = 0,0,0
  ; ctime = 0.
  ; image_prefix=""
  ; cgi = false
  ; output_conf =
      { status = ignore
      ; header = ignore
      ; body = ignore
      ; file = (fun _ _ _ -> true)
      ; flush = ignore
      }
  }
(**/**)
