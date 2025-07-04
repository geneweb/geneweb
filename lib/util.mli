(* Copyright (c) 1998-2007 INRIA *)
open Config
open Def

val hash_file : string -> string option
(** [hash_file path] Compute the MD5 hash of the file at [path]. Returns
    [Some hex] on success or [None] if the file couldn’t be read. *)

val hash_file_cached : string -> string option
(** [hash_file_cached path] Like [hash_file], but memoizes by file modification
    time to avoid recomputing the hash on repeated calls. *)

val resolve_asset_file : Config.config -> string -> string
(** [resolve_asset_file conf name] Find and return the full filesystem path for
    asset [name] according to base directory and Secure.assets(). *)

val is_welcome : bool ref

val print_default_gwf_file : string -> unit
(** print default config file bname.gwf or bname.gwb/etc/mybase.gwf *)

val read_base_env : string -> string -> bool -> (string * string) list
(** read base environment bname.gwf or bname.gwb/etc/bname.gwf *)

val time_debug :
  config -> float -> int -> string list -> string list -> string list -> unit
(** prints the query duration and reports it in the "home" section *)

val bpath : string -> string
(** Alias for !GWPARAM.bpath *)

val search_in_assets : string -> string
(** Checks that the file in argument belong to one of the asserts dir (defined
    in the Secure module) *)

val etc_file_name : Config.config -> string -> string

val open_etc_file : Config.config -> string -> (in_channel * string) option
(** [open_etc_file conf fname] search for template {i etc/fname.txt} inside the
    base directory or inside one of assets directories. Returns input channel
    and the path to given template. *)

val is_full_html_template : config -> string -> bool
(** [is_full_html_template conf fname] returns [true] if the template file
    [fname] contains <!DOCTYPE html> in its first 3 lines, indicating it's a
    complete HTML page rather than a fragment. This is used to determine whether
    HTTP headers should be sent before the template content. *)

val escache_value : Geneweb_db.Driver.base -> Adef.encoded_string
(** Returns the date of the base directory last update *)

val commit_patches : config -> Geneweb_db.Driver.base -> unit
(** Commits the patches and logs the modification *)

val update_wf_trace : config -> string -> unit

val get_referer : config -> Adef.escaped_string
(** Get referer (the page you came from to the current page) page from HTTP
    request *)

val clean_html_tags : string -> string
val clean_comment_tags : string -> string
val uri_encode : string -> string
val uri_decode : string -> string

val html : ?content_type:string -> config -> unit
(** Prints HTTP response headers with giving content type (default :
    {i text/html}) on the socket. *)

val unauthorized : config -> string -> unit
(** Prints HTTP response with code 401 (Unauthorized) and error page with giving
    message *)

val string_of_ctime : config -> string

val commd :
  ?excl:string list ->
  ?trim:bool ->
  ?pwd:bool ->
  ?henv:bool ->
  ?senv:bool ->
  config ->
  Adef.escaped_string
(** Returns link to the current command (database name after domain name and
    port in url) with query string that containts bindings from [conf.henv] and
    [conf.senv]. Doesn't add binding [(k,v)] when:
    - k = "oc" or "ocz" and v = "0"
    - v = "" *)

val prefix_base : config -> Adef.escaped_string
val prefix_base_password : config -> Adef.escaped_string

val hidden_env_aux : config -> (string * Adef.encoded_string) list -> unit
(** [hidden_env_aux env] Creates a hidden HTML input for every key and value in
    [env]. *)

val hidden_env : config -> unit
(** Creates a hidden HTML input for every key and value in [conf.henv] and
    [conf.senv]. Used to include immutable environement bindings in the HTML
    form. *)

val hidden_textarea : config -> string -> Adef.encoded_string -> unit

val hidden_input : config -> string -> Adef.encoded_string -> unit
(** [hidden_input conf k v] *)

val hidden_input_s : config -> string -> string -> unit
(** [hidden_input_s conf k v] *)

val submit_input : config -> string -> Adef.encoded_string -> unit
(** [submit_input conf k v] *)

val nobtit :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.title list
(** [nobtit conf base p] returns list of titles of [p] from the [base] that
    respects constraints imposed by [conf.allowed_titles] and
    [conf.denied_titles] *)

val strictly_after_private_years : dmy -> int -> bool

val authorized_age :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** Alias to !GWPARAM.p_auth *)

val is_old_person :
  config ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  gen_person ->
  bool

val start_with : string -> int -> string -> bool
val start_with_vowel : config -> string -> bool
val access_status : Geneweb_db.Driver.person -> string

val acces_n :
  config ->
  Geneweb_db.Driver.base ->
  Adef.escaped_string ->
  Geneweb_db.Driver.person ->
  Adef.escaped_string
(** Returns URL query string to access nth person (e.g. for person 2 in url:
    p2=foo&n2=bar&oc2=1 *)

val acces :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.escaped_string

val accessible_by_key :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  string ->
  bool
(** [accessible_by_key conf base p fn sn] Tells if Geneweb_db.Driver.person
    could be accessed by his first name and surname

    i.e. current base configuration and user rights allow this and person's name
    is suitable for searching by key (e.g. `? ?` is not) *)

val geneweb_link :
  config -> Adef.escaped_string -> Adef.safe_string -> Adef.safe_string
(** [geneweb_link conf href s] Returns HTML link to actual geneweb's command
    (database name) with additional (to those defind by [commd]) argument [href]
    and [s] as textual content of the link. *)

val wprint_geneweb_link :
  config -> Adef.escaped_string -> Adef.safe_string -> unit
(** Prints on the socket link created by [geneweb_link]. *)

val is_restricted :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> bool
(** Tells if person is restrited to acccess. If mode `use_restrict` is disabled
    returns always [false]. *)

val is_hidden : Geneweb_db.Driver.person -> bool
(** Tells if person is hiden (if his surname is empty) *)

val is_public :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** Tells if person is public
    - access = Public or
    - IfTitle and has titles or
    - is_old_person) *)

val private_txt : config -> string -> string

val pget_opt :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.person option
(** Returns person option with giving id from the base. Wrapper around
    `Geneweb_db.poi` defined such as:
    - Some ip: if user have permissions or `use_restrict` disabled.
    - None: if `conf.use_restrict` (option defined in .gwf file): checks that
      the user has enought rights to see corresponding person (see
      `authorized_age`). If the user does not have enought permissions, returns
      None. *)

val pget :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.person
(** Value of [pget_opt], map None to empty_person *)

val string_gen_person :
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  gen_person ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) gen_person
(** Remplaces string ids inside person's entry by their actual string value. *)

val string_gen_family :
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.ifam,
    Geneweb_db.Driver.istr )
  gen_family ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam, string) gen_family
(** Remplaces string ids inside family's entry by their actual string value. *)

val gen_person_text :
  ?escape:bool ->
  ?html:bool ->
  ?sn:bool ->
  ?p_first_name:(Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string) ->
  ?p_surname:(Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string) ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Returns person's first name and surname HTML description depending on :
    - his public name
    - his qualifiers If person is hiden returns ".....". If person's names are
      hiden or access to them is denied returns "x x"
    - if [html=false], doesn't encapsulates description in HTML tag <em>.
    - if [sn=false], doesn't display surname *)

val gen_person_title_text :
  (config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string ->
  Adef.safe_string) ->
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** [gen_person_title_text reference paccess conf base p] returns HTML structure
    of person that describes person's first name surname and main title.
    [reference] is used to either encapsulate structure in the link (or other
    type of maniplations). *)

val person_text_without_title :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Makes call to [gen_person_text_without_title] with [std_access] *)

val main_title :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.title option
(** Returns main person's title. If person doesn't have it, then returns first
    title from the list. *)

val titled_person_text :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.title ->
  Adef.safe_string
(** Returns person's first name and surname text description depending on
    person's title *)

val one_title_text :
  Geneweb_db.Driver.base -> Geneweb_db.Driver.title -> Adef.safe_string
(** Returns HTML representation of title's identifier with its place (if exists)
*)

val person_title_text :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Returns HTML structure of person that describes person's first name surname
    and main title. Calls [gen_person_title_text] with [no_reference]. *)

val person_title :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Returns HTML representation of person's main title (or first title if main
    doesn't exists). If person doesn't have a title or if access to person isn't
    granted returns empty string *)

val child_of_parent :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string

val mod_ind_link :
  config -> Geneweb_db.Driver.person -> Adef.safe_string -> Adef.safe_string
(** [mod_ind_link conf base p s] creates a hyperlink with the URL
    "?m=MOD_IND&i=\{iper\}" where '\{iper\}' is the index of the person [p], and
    the text [s]. If [s] is empty, it defaults to print a wrench icon. *)

val reference :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string ->
  Adef.safe_string
(** [reference conf base p desc] returns HTML link to the person where [desc] is
    content of the link (generaly his first name and surname description). If
    person is hidden returns [desc] (do not create link). *)

val reference_noid :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string ->
  Adef.safe_string
(** Same as [reference] but link doesn't has "id" field *)

val no_reference :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string ->
  Adef.safe_string
(** [reference conf Geneweb_db.Driver.base p desc] returns [desc] without
    creating a link *)

val referenced_person_title_text :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Retruns HTML link to the person that contains its first name, surname and
    person's nobility title. Calls [gen_person_title_text] with [reference]. *)

val referenced_person_text :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Returns HTML link to the person that contains its first name and surname. *)

val referenced_person_text_without_surname :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string
(** Returns HTML link to the person that contains its first name. *)

val update_family_loop :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Adef.safe_string ->
  Adef.safe_string

val p_getenv : Config.env -> string -> string option
(** Returns value associated to the label in environnement *)

val p_getint : Config.env -> string -> int option
(** Returns integer value associated to the label in environnement *)

val create_env : Adef.encoded_string -> Config.env
(** Create association list from the query part of a URL. (i.e. a list of
    key-value separated by `&` or `;`) *)

val string_of_place : config -> string -> Adef.escaped_string
val raw_string_of_place : config -> string -> string
val place_of_string : config -> string -> place option
val allowed_tags_file : string ref

val body_prop : config -> string
(** Returns additional attributes for <body> tag from [config] (defined in .gwf
    file). *)

val message_to_wizard : config -> unit
(** Prints all messages sent to wizard (or friend) on the socket. Messages are
    located in {i <basename>/etc/mess_wizzard.txt} (messages destinated to all
    wizards) and in {i <basename>/etc/mess_wizzard_<user>.txt} (messages
    destinated to considered wizard). *)

val of_course_died : config -> Geneweb_db.Driver.person -> bool
val hexa_string : string -> string

val surname_particle : Geneweb_db.Driver.base -> string -> string
(** [surname_particle base sn] Extract the particle of [sn] if there is one. The
    list of particles to use is defined in [base]. *)

val surname_without_particle : Geneweb_db.Driver.base -> string -> string
(** [surname_without_particle base sn] Remove the particle of [sn] if there is
    one. The list of particles to use is defined in [Geneweb_db.Driver.base]. *)

val specify_homonymous :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool -> unit

val get_approx_birth_date_place :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  date option * Adef.safe_string

val get_approx_death_date_place :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  date option * Adef.safe_string

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

val check_format : ('a, 'b) format2 -> string -> ('a, 'b) format2 option
val valid_format : ('a, 'b) format2 -> string -> ('a, 'b) format2

val transl : config -> string -> string
(** Find translation of given keyword in [conf.lexicon]. Keywords used to be its
    english translation but can be any string. *)

val transl_nth : config -> string -> int -> string
(** [transl_nth conf w n] translate word [w] and returns [n]'th field of its
    translation (with [nth_field]). *)

val simple_decline : config -> string -> string
val transl_decline : config -> string -> string -> string
val ftransl : config -> ('a, 'b) format2 -> ('a, 'b) format2
val ftransl_nth : config -> ('a, 'b) format2 -> int -> ('a, 'b) format2
val fdecline : ('a, 'b) format2 -> string -> ('a, 'b) format2
val fcapitale : ('a, 'b) format2 -> ('a, 'b) format2

val nth_field : string -> int -> string
(** [nth_field str n] gets [n]'th field of string that separate its fields with
    "/". Example :
    - nth_field "a/b/</c>/d" 0 = a
    - nth_field "a/b/</c>/d" 1 = b
    - nth_field "a/b/</c>/d" 2 = </c>
    - nth_field "a/b/</c>/d" 3 = d *)

val cftransl : config -> string -> string list -> string
val translate_eval : string -> string

val transl_a_of_b : config -> string -> string -> string -> string
(** [transl_a_of_b conf a b b_raw] Translate "a of b" using [b_raw] for
    declension. i.e. if [b] is wrapped in html, [b_raw] should be that texte
    with no html, and [b_raw] should be [b] otherwise. *)

val transl_a_of_gr_eq_gen_lev : config -> string -> string -> string -> string

val std_color : config -> Adef.safe_string -> Adef.safe_string
(** Colorise HTML element with [conf.highlight] color (wrap text in <span> with
    inline style). *)

val index_of_sex : sex -> int
(** Sex index used in translations (0 for male, 1 for female, 2 for neuter) *)

val string_of_pevent_name :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.istr gen_pers_event_name ->
  Adef.safe_string

val string_of_fevent_name :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.istr gen_fam_event_name ->
  Adef.safe_string
(** [string_of_fevent_name conf base fevent_name] *)

val string_of_witness_kind : config -> sex -> witness_kind -> Adef.safe_string
(** [string_of_witness_kind conf sex wk] Return the string corresponding to wk
    according to [sex] and [conf]. *)

val string_of_witness_kind_raw : witness_kind -> Adef.safe_string
(** [string_of_witness_kind_raw conf wk] Return the string corresponding to
    generic coding of wk . *)

val relation_txt :
  config -> sex -> Geneweb_db.Driver.family -> (('a -> 'b) -> 'b, 'a, 'b) format

val string_of_decimal_num : config -> float -> string

val person_exists :
  config -> Geneweb_db.Driver.base -> string * string * int -> bool
(** test if person exists using fn, sn, oc as arguments *)

val mark_if_not_public :
  config -> Geneweb_db.Driver.base -> string * string * int -> bool
(** used by Roglo to flag persons that should be public and are not. Active only
    if url contains red_if_not_public=on *)

val husband_wife :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool ->
  Adef.safe_string
(** returns a string listing the spouses of a person [bool] if true return all
    spouses otherwise the first one only *)

val find_person_in_env :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person option
(** [find_person_in_env conf base suff] Reconstitutes the key of a person from
    [conf.env], using ["i" ^ suff] or ["n" ^ suff] + ["p" ^ suff] +
    ["oc" ^ suff] *)

val find_person_in_env_pref :
  config -> Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.person option
(** [find_person_in_env_pref conf base prefix] Same as [find_person_in_env]
    except that it uses a prefix instead of a suffix. *)

(* Recherche le sosa uniquement dans le fichier gwf *)
val default_sosa_ref :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person option

val find_sosa_ref :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person option

val update_gwf_sosa :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper * (string * string * int) ->
  unit

val get_server_string : config -> string
(** Returns server host name with its port number (if different from 80). *)

val get_request_string : config -> string
(** Returns request string. Request string has format {i scriptname?querystring}
    where scriptname is a path to the script in URI. *)

val create_topological_sort :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, int) Geneweb_db.Collection.Marker.t

val p_of_sosa :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_sosa.t ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person option
(** [p_of_sosa conf base sosa p0] Get the sosa [sosa] of [p0] if it exists *)

val branch_of_sosa :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_sosa.t ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person list option
(** [branch_of_sosa conf base sosa p0] Get all the lineage to go from [p0]'s
    ancestor with sosa number [sosa] to [p0] *)

val sosa_of_branch : Geneweb_db.Driver.person list -> Geneweb_sosa.t
(** [sosa_of_branch branch] Given a path of person to follow [branch], return
    the sosa number of the last person of this list. No check is done to ensure
    that given persons are actually parents. *)

val old_branch_of_sosa :
  config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_sosa.t ->
  (Geneweb_db.Driver.iper * sex) list option
(** @deprecated Use [branch_of_sosa] instead *)

val old_sosa_of_branch :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * sex) list ->
  Geneweb_sosa.t
(** @deprecated Use [sosa_of_branch] instead *)

val only_printable : string -> string
(** Trims and remplaces all non-printable characters by spaces in the given
    string. *)

val only_printable_or_nl : string -> string
(** Same as [only_printable] but also accepts '\n'. *)

val relation_type_text : config -> relation_type -> int -> Adef.safe_string
val rchild_type_text : config -> relation_type -> int -> Adef.safe_string

val has_nephews_or_nieces :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool

val browser_doesnt_have_tables : config -> bool
val doctype : Adef.safe_string

val begin_centered : config -> unit
(** Prints on the socket beginning of the <table> tag untill first opened <td>
    where the text is centred *)

val end_centered : config -> unit
(** Prints on the socket end of the column and table opened by [begin_centered]
*)

val print_alphab_list :
  config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

val short_f_month : int -> string

(* Reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }
(** Authenticated user from from authorization file. *)

val read_gen_auth_file : string -> string -> auth_user list
(** Read all authenticated users with their passwords from authorization file
    (associated to {i "wizard_passwd_file"} in [conf.base_env]) *)

val is_that_user_and_password : auth_scheme_kind -> string -> string -> bool
(** [is_that_user_and_password auth_sheme user paswd] verify if given user with
    his password correspond to the authentication scheme. *)

(* Searching *)

val in_text : bool -> string -> string -> bool
val html_highlight : bool -> string -> string -> string

(* Print list in columns with Gutil.alphabetic order *)

val wprint_in_columns :
  config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

val is_hide_names : config -> Geneweb_db.Driver.person -> bool
(** Tells if person's names are hiden (if person's access is [Private] or if
    mode [conf.hide_names] is enabled). *)

val reduce_list : int -> 'a list -> 'a list
(** [reduce_list n l] takes [n] first elements from the list [l] *)

val gen_print_tips : config -> Adef.safe_string -> unit
(** Print a tip with the specified text *)

val print_tips_relationship : config -> unit
(** Print a tip that tells to
    {i Click an individual below to calculate the family link.} *)

val images_prefix : config -> string
(** get value of images_prefix *)

val get_opt : config -> string -> bool -> bool
(** get option value for evar "im", "sp", "ma". Default value is defined by
    third param *)

val display_options : config -> Adef.escaped_string

type cache_visited_t =
  (string, (Geneweb_db.Driver.iper * string) list) Hashtbl.t

val cache_visited : config -> string
val read_visited : config -> cache_visited_t
val record_visited : config -> Geneweb_db.Driver.iper -> unit

val array_mem_witn :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (Geneweb_db.Driver.iper * Def.witness_kind) array ->
  Adef.safe_string option
(** [array_mem_witn conf base ip array] checks if [ip] is in [array] and returns
    corresponding [string_of_witness_kind] if so. *)

val make_key :
  Geneweb_db.Driver.base ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  Def.gen_person ->
  string * string * int
(** make a tuple (first_name, surname, occ) apply Name.lower *)

val name_key : Geneweb_db.Driver.base -> string -> string
(** [name_key base name] is [name], with particles put at the end of the string
    instead of the beginning. *)

val nb_char_occ : char -> string -> int
(** [nb_char_occ c s] return the number of times [c] appears in [s]. *)

val escape_html : string -> Adef.escaped_string
(** [escape_html str] replaces '&', '"', '\'', '<' and '>' with their
    corresponding character entities (using entity number) *)

val safe_html : string -> Adef.safe_string
(** [safe_html s] sanitizes [s] element in order to fix ill-formed HTML input
    and to prevent XSS injection

    It removes any tag which is not allowed by geneweb. It removes all
    attributes starting with ["on"]. It removes any attribute when the value
    starts with ["javascript"]. Text is escaped using [escape_html]. *)

val string_with_macros :
  config -> (char * (unit -> string)) list -> string -> string
(** [string_with_macros conf env s] Return a string with "%xxx" macro replaced
    by their value. *)

val is_empty_name : Geneweb_db.Driver.person -> bool
(** [is_empty_name p] [false] if we knwon the first name or the last name of
    [p]. *)

val select_masc :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * int) list ->
  (Geneweb_db.Driver.iper, int * Geneweb_db.Driver.person) Hashtbl.t
(** [select_masc conf base ips] From [ips], a list matching ipers to a number of
    maximum generations, get maximum ascendants of ipers up to these
    corresponding generations.

    A person is maximum ascendant if their generation matches the maximum, or if
    they do not have ancestors.

    The result is a Hashtbl matching an iper to the corresponding person and
    their generation. *)

val select_desc :
  config ->
  Geneweb_db.Driver.base ->
  int ->
  (Geneweb_db.Driver.iper * int) list ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.person) Hashtbl.t
(** [select_desc conf base gen_desc ips] From [ips], a list matching ipers to a
    number of maximum generations, get spouses and descendants of ipers up to
    these corresponding generations. *)

val select_mascdesc :
  config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * int) list ->
  int ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.person) Hashtbl.t
(** [select_ascdesc conf base ips gen_desc] Get maximum ascendants with
    {!val:select_masc}, and get their desc with {!val:select_desc} *)

val sprintf_today : Config.config -> Adef.safe_string
(** [sprintf_today confo] Uses {!val:Mutil.sprintf_date} in order to print
    datetime defined in [conf]. *)

val auth_warning :
  config ->
  Geneweb_db.Driver.base ->
  ('a, Geneweb_db.Driver.person, Geneweb_db.Driver.ifam, 'b, 'c, 'd, 'e) warning ->
  bool
(** [auth_warning conf base w] Check if current user has enough right in order
    to see [w] *)

val name_with_roman_number : string -> string option
(** Convert arabic numerals to roman numerals. [Some result] is returned if
    there are numerals, [None] if not. *)

val cut_words : string -> string list
(** [cut_words str] Same output as
    [String.split_on_char ' ' s |> List.map String.trim |> List.filter ((<>)
     "")] *)

val designation :
  Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> Adef.escaped_string
(** [designation base p] is [Gutil.designation base p |> escape_html] *)

val has_children : Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
val get_bases_list : ?format_fun:(string -> string) -> unit -> string list

val test_cnt_d : config -> string
(** tests if cnt_d exists and creaets it if needed *)

val extract_value : char -> string -> string
(** [extract_value delimiter s] Assuming that the string [s] is of the form
    [key=value], where = stands for a one char [delimiter], this function
    extracts the value.
    @raise Not_found if [s] does not contain exactly one delimiter. *)

val sys_to_note_link : string -> string
(** convert system path (a/b/c) to note_link format (a:b:c) *)

val note_link_to_sys : string -> string
(** convert note_link path (a:b:c) to system format (a/b/c) *)
