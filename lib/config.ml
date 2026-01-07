(* Copyright (c) 1998-2007 INRIA *)

type token_auth_scheme = { ts_user : string; ts_pass : string }

type basic_auth_scheme = {
  bs_realm : string;
  bs_user : string;
  bs_pass : string;
}

type digest_auth_scheme = {
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

type http_auth_scheme =
  | Basic of basic_auth_scheme
  | Digest of digest_auth_scheme

type auth_scheme_kind =
  | NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme

type output_conf = {
  status : Def.httpStatus -> unit;
  header : string -> unit;
  body : string -> unit;
  flush : unit -> unit;
}

type env = (string * Adef.encoded_string) list
type dates_format = DMY | MDY

type config = {
  from : string;
  api_mode : bool;
  manitou : bool;
  supervisor : bool;
  wizard : bool;
  is_printed_by_template : bool;
  debug : bool;
  friend : bool;
  just_friend_wizard : bool;
  user : string;
  username : string;
  auth_scheme : auth_scheme_kind;
  command : string;
  indep_command : string;
  lang : string;
  default_lang : string;
  default_sosa_ref : Gwdb.iper * Gwdb.person option;
  authorized_wizards_notes : bool;
  public_if_titles : bool;
  public_if_no_date : bool;
  mutable setup_link : bool;
  access_by_key : bool;
  private_years : int;
  default_contemporary_private_years : int;
  hide_private_names : bool;
  use_restrict : bool;
  no_image : bool;
  no_note : bool;
  bname : string;
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
  today : Date.dmy;
  today_wd : int;
  time : int * int * int;
  ctime : float;
  mutable output_conf : output_conf;
  (* HTTP printer *)
  (* prefix for image urls:
     the value of argument -images_url if specified, otherwise
     command ^ "?m=IM&v=" in CGI mode
     "images" otherwise *)
  image_prefix : string;
  static_path : string;
  (* in CGI mode, provides location of etc files to Apache for direct loading *)
  (* if true, the base name is in the b argument of the query string: ?b=BASE&...
     if false, the base name is the last element of the uri path: .../base?... *)
  cgi : bool;
  forced_plugins : string list;
  plugins : string list;
  notify_change : string option;
  preferred_countries : string list option;
  dates_format : dates_format;
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
    friend = false;
    just_friend_wizard = false;
    user = "";
    username = "";
    auth_scheme = NoAuth;
    command = "";
    indep_command = "";
    lang = "";
    default_lang = "";
    default_sosa_ref = (Gwdb.dummy_iper, None);
    authorized_wizards_notes = false;
    public_if_titles = false;
    public_if_no_date = false;
    setup_link = false;
    access_by_key = false;
    private_years = 0;
    default_contemporary_private_years = 100;
    hide_private_names = false;
    use_restrict = false;
    no_image = false;
    no_note = false;
    bname = "";
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
    today = { Date.day = 0; month = 0; year = 0; delta = 0; prec = Date.Sure };
    today_wd = 0;
    time = (0, 0, 0);
    ctime = 0.;
    image_prefix = "";
    static_path = "";
    cgi = false;
    output_conf =
      { status = ignore; header = ignore; body = ignore; flush = ignore };
    forced_plugins = [];
    plugins = [];
    notify_change = None;
    preferred_countries = None;
    dates_format = DMY;
  }

(**/**)

module Trimmed = struct
  type t = {
    from : string;
    api_mode : bool;
    manitou : bool;
    supervisor : bool;
    wizard : bool;
    is_printed_by_template : bool;
    debug : bool;
    friend : bool;
    just_friend_wizard : bool;
    user : string;
    username : string;
    auth_scheme : auth_scheme_kind;
    command : string;
    indep_command : string;
    lang : string;
    default_lang : string;
    default_sosa_ref : Gwdb.iper * Gwdb.person option;
    authorized_wizards_notes : bool;
    public_if_titles : bool;
    public_if_no_date : bool;
    setup_link : bool;
    access_by_key : bool;
    private_years : int;
    default_contemporary_private_years : int;
    hide_private_names : bool;
    use_restrict : bool;
    no_image : bool;
    no_note : bool;
    bname : string;
    cgi_passwd : string;
    allowed_titles : string list Lazy.t;
    denied_titles : string list Lazy.t;
    request : string list;
    lexicon : (string, string) Hashtbl.t;
    charset : string;
    is_rtl : bool;
    left : string;
    right : string;
    auth_file : string;
    border : int;
    n_connect : (int * int * int * (string * float) list) option;
    today : Date.dmy;
    today_wd : int;
    time : int * int * int;
    ctime : float;
    output_conf : output_conf;
    image_prefix : string;
    static_path : string;
    cgi : bool;
    forced_plugins : string list;
    plugins : string list;
    notify_change : string option;
    preferred_countries : string list option;
    dates_format : dates_format;
  }

  let to_config
      ({
         from;
         api_mode;
         manitou;
         supervisor;
         wizard;
         is_printed_by_template;
         debug;
         friend;
         just_friend_wizard;
         user;
         username;
         auth_scheme;
         command;
         indep_command;
         lang;
         default_lang;
         default_sosa_ref;
         authorized_wizards_notes;
         public_if_titles;
         public_if_no_date;
         setup_link;
         access_by_key;
         private_years;
         default_contemporary_private_years;
         hide_private_names;
         use_restrict;
         no_image;
         no_note;
         bname;
         cgi_passwd;
         allowed_titles;
         denied_titles;
         request;
         lexicon;
         charset;
         is_rtl;
         left;
         right;
         auth_file;
         border;
         n_connect;
         today;
         today_wd;
         time;
         ctime;
         output_conf;
         image_prefix;
         static_path;
         cgi;
         forced_plugins;
         plugins;
         notify_change;
         preferred_countries;
         dates_format;
       } :
        t) =
    {
      from;
      api_mode;
      manitou;
      supervisor;
      wizard;
      is_printed_by_template;
      debug;
      friend;
      just_friend_wizard;
      user;
      username;
      auth_scheme;
      command;
      indep_command;
      lang;
      default_lang;
      default_sosa_ref;
      authorized_wizards_notes;
      public_if_titles;
      public_if_no_date;
      setup_link;
      access_by_key;
      private_years;
      default_contemporary_private_years;
      hide_private_names;
      use_restrict;
      no_image;
      no_note;
      bname;
      cgi_passwd;
      env = [];
      senv = [];
      henv = [];
      base_env = [];
      allowed_titles;
      denied_titles;
      request;
      lexicon;
      charset;
      is_rtl;
      left;
      right;
      auth_file;
      border;
      n_connect;
      today;
      today_wd;
      time;
      ctime;
      output_conf;
      image_prefix;
      static_path : string;
      cgi;
      forced_plugins;
      plugins;
      notify_change;
      preferred_countries;
      dates_format;
    }

  let from_config
      ({
         from;
         api_mode;
         manitou;
         supervisor;
         wizard;
         is_printed_by_template;
         debug;
         friend;
         just_friend_wizard;
         user;
         username;
         auth_scheme;
         command;
         indep_command;
         lang;
         default_lang;
         default_sosa_ref;
         authorized_wizards_notes;
         public_if_titles;
         public_if_no_date;
         setup_link;
         access_by_key;
         private_years;
         default_contemporary_private_years;
         hide_private_names;
         use_restrict;
         no_image;
         no_note;
         bname;
         cgi_passwd;
         allowed_titles;
         denied_titles;
         request;
         lexicon;
         charset;
         is_rtl;
         left;
         right;
         auth_file;
         border;
         n_connect;
         today;
         today_wd;
         time;
         ctime;
         output_conf;
         image_prefix;
         static_path;
         cgi;
         forced_plugins;
         plugins;
         notify_change;
         preferred_countries;
         dates_format;
       } :
        config) =
    {
      from;
      api_mode;
      manitou;
      supervisor;
      wizard;
      is_printed_by_template;
      debug;
      friend;
      just_friend_wizard;
      user;
      username;
      auth_scheme;
      command;
      indep_command;
      lang;
      default_lang;
      default_sosa_ref;
      authorized_wizards_notes;
      public_if_titles;
      public_if_no_date;
      setup_link;
      access_by_key;
      private_years;
      default_contemporary_private_years;
      hide_private_names;
      use_restrict;
      no_image;
      no_note;
      bname;
      cgi_passwd;
      allowed_titles;
      denied_titles;
      request;
      lexicon;
      charset;
      is_rtl;
      left;
      right;
      auth_file;
      border;
      n_connect;
      today;
      today_wd;
      time;
      ctime;
      output_conf;
      image_prefix;
      static_path : string;
      cgi;
      forced_plugins;
      plugins;
      notify_change;
      preferred_countries;
      dates_format;
    }
end
