open Def

(** Authentication scheme data type *)
type auth_scheme_kind =
  | NoAuth
  | TokenAuth of token_auth_scheme
  | HttpAuth of http_auth_scheme

and token_auth_scheme = { ts_user : string; ts_pass : string }
(** Authentication via security token *)

(** Authentication via HTTP *)
and http_auth_scheme =
  | Basic of basic_auth_scheme
  | Digest of digest_auth_scheme

and basic_auth_scheme = {
  bs_realm : string;
  bs_user : string;
  bs_pass : string;
}
(** Basic authentication scheme inside {i Autorization} HTTP header *)

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
(** Digest authentication scheme inside {i Autorization} HTTP header *)

type output_conf = {
  status : Def.httpStatus -> unit;
  header : string -> unit;
  body : string -> unit;
  flush : unit -> unit;
}
(** HTTP printer, that prints and sends requests on the user's socket *)

type env = (string * Adef.encoded_string) list

(** Geneweb configuration data type *)
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
  ctime : float;
  mutable output_conf : output_conf;
  (* HTTP printer *)
  (* prefix for image urls:
     the value of argument -images_url if specified, otherwise
     command ^ "?m=IM&v=" in CGI mode
     "images" otherwise *)
  gw_prefix : string;
  images_prefix : string;
      (* if true, the base name is in the b argument of the query string: ?b=BASE&...
         if false, the base name is the last element of the uri path: .../base?... *)
  etc_prefix : string;
      (* in CGI mode, provides location of etc files to Apache for direct loading *)
  cgi : bool;
  forced_plugins : string list;
  plugins : string list;
  secret_salt : string option;
      (** Secret salt generated at the server startup. The salt is used in
          form's digests to enhance security. *)
  predictable_mode : bool;
      (** Determine if we are in predictable mode. In this mode, output must not
          depend on random state. *)
}
(** Geneweb configuration data type *)

val empty : config
(** A dummy {!type:config} value, with uninitialized fields. Used for testing
    purpose *)
