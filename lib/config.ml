(* Copyright (c) 1998-2007 INRIA *)

open Def

type auth_scheme_kind =
  | NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme

and token_auth_scheme = { ts_user : string; ts_pass : string }

and http_auth_scheme =
  | Basic of basic_auth_scheme
  | Digest of digest_auth_scheme

and basic_auth_scheme = {
  bs_realm : string;
  bs_user : string;
  bs_pass : string;
}

and digest_auth_scheme = {
  ds_username : string;
  ds_realm : string;
  ds_nonce : string;
  ds_meth : string;
  ds_uri : string;
  ds_qop : string;
  ds_nc : string;
  ds_cnonce : string;
  ds_response : string;
}

type output_conf = {
  status : Def.httpStatus -> unit;
  header : string -> unit;
  body : string -> unit;
  flush : unit -> unit;
}

type env = (string * Adef.encoded_string) list

type config = {
  from : string;
  api_mode : bool;
  manitou : bool;
  supervisor : bool;
  wizard : bool;
  is_printed_by_template : bool;
  debug : bool;
  query_start : float;
  friend : bool;
  semi_public : bool;
  just_friend_wizard : bool;
  user : string;
  username : string;
  userkey : string;
  user_iper : Geneweb_db.Driver.iper option;
  auth_scheme : auth_scheme_kind;
  command : string;
  indep_command : string;
  highlight : string;
  lang : string;
  vowels : string list;
  default_lang : string;
  browser_lang : string;
  default_sosa_ref : Geneweb_db.Driver.iper * Geneweb_db.Driver.person option;
  multi_parents : bool;
  authorized_wizards_notes : bool;
  public_if_titles : bool;
  public_if_no_date : bool;
  mutable setup_link : bool;
  access_by_key : bool;
  private_years : int;
  private_years_death : int;
  private_years_marriage : int;
  hide_names : bool;
  use_restrict : bool;
  no_image : bool;
  no_note : bool;
  bname : string;
  nb_of_persons : int;
  nb_of_families : int;
  cgi_passwd : string;
  env : env;
  mutable senv : env;
  mutable henv : env;
  base_env : (string * string) list (* content of .gwf file *);
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
  ctime : float; (* TODO verify usefulness *)
  mutable output_conf : output_conf;
  (* HTTP printer *)
  (* prefix for image urls:
     the value of argument -images_url if specified, otherwise
     command ^ "?m=IM&v=" in CGI mode
     "images" otherwise *)
  gw_prefix : string;
  images_prefix : string;
  etc_prefix : string;
  (* in CGI mode, provides location of etc files to Apache for direct loading *)
  (* if true, the base name is in the b argument of the query string: ?b=BASE&...
     if false, the base name is the last element of the uri path: .../base?... *)
  cgi : bool;
  forced_plugins : string list;
  plugins : string list;
  secret_salt : string option;
  predictable_mode : bool;
}

(**/**)

(** A dummy {!type:config} value, with uninitialized fields. Used for testing
    purpose *)
let empty =
  {
    from = "";
    manitou = false;
    supervisor = false;
    wizard = false;
    api_mode = false;
    is_printed_by_template = false;
    debug = false;
    query_start = 0.;
    friend = false;
    semi_public = false;
    just_friend_wizard = false;
    user = "";
    username = "";
    userkey = "";
    user_iper = None;
    auth_scheme = NoAuth;
    command = "";
    indep_command = "";
    highlight = "";
    lang = "";
    vowels = [];
    default_lang = "";
    browser_lang = "";
    default_sosa_ref = (Geneweb_db.Driver.Iper.dummy, None);
    multi_parents = false;
    authorized_wizards_notes = false;
    public_if_titles = false;
    public_if_no_date = false;
    setup_link = false;
    access_by_key = false;
    private_years = 0;
    private_years_death = 0;
    private_years_marriage = 0;
    hide_names = false;
    use_restrict = false;
    no_image = false;
    no_note = false;
    bname = "";
    nb_of_persons = 0;
    nb_of_families = 0;
    cgi_passwd = "";
    env = [];
    senv = [];
    henv = [];
    base_env = [];
    allowed_titles = lazy [];
    denied_titles = lazy [];
    request = [];
    lexicon = Hashtbl.create 16;
    charset = "";
    is_rtl = false;
    left = "";
    right = "";
    auth_file = "";
    border = 0;
    n_connect = None;
    today = { Def.day = 0; month = 0; year = 0; delta = 0; prec = Def.Sure };
    today_wd = 0;
    time = (0, 0, 0);
    ctime = 0.;
    gw_prefix = "";
    images_prefix = "";
    etc_prefix = "";
    cgi = false;
    output_conf =
      { status = ignore; header = ignore; body = ignore; flush = ignore };
    forced_plugins = [];
    plugins = [];
    secret_salt = None;
    predictable_mode = false;
  }

(**/**)
