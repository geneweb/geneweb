(* Copyright (c) 1998-2007 INRIA *)

val cnt_dir : string ref
(** The directory where counters (e.g. number page displayed) are stored. *)

val base_path : string list -> string -> string
(** Alias for GWPARAM.base_path *)

val bpath : string -> string
(** Alias for GWPARAM.bpath *)

val search_in_assets : string -> string
(** Checks that the file in argument belong to one of the asserts dir
    (defined in the Secure module) *)

val include_begin : Config.config -> Adef.safe_string -> unit
val include_end : Config.config -> Adef.safe_string -> unit

val etc_file_name : string -> string
(** Returns the path to the template file in parameter *)

val escache_value : Gwdb.base -> Adef.encoded_string
(** Returns the date of the base directory last update *)

val commit_patches : Config.config -> Gwdb.base -> unit
(** Commits the patches and logs the modification *)

val update_wf_trace : Config.config -> string -> unit

val get_referer : Config.config -> Adef.escaped_string
(** Get referer (the page you came from to the current page) page from HTTP request *)

val clean_html_tags : string -> string list -> string

val html : ?content_type:string -> Config.config -> unit
(** Prints HTTP response headers with giving content type (default : {i text/html}) on the socket. *)

val unauthorized : Config.config -> string -> unit
(** Prints HTTP response with code 401 (Unauthorized) and error page with giving message *)

val string_of_ctime : Config.config -> string

val commd :
  ?excl:string list ->
  ?trim:bool ->
  ?henv:bool ->
  ?senv:bool ->
  Config.config ->
  Adef.escaped_string
(** Returns link to the current command (database name after domain name and port in url) with query string
    that containts bindings from [conf.henv] and [conf.senv]. Doesn't add binding [(k,v)] when:
    - k = "oc" or "ocz" and v = "0"
    - v = "" *)

val prefix_base : Config.config -> Adef.escaped_string
val prefix_base_password : Config.config -> Adef.escaped_string
val prefix_base_password_2 : Config.config -> Adef.escaped_string

val hidden_env_aux :
  Config.config -> (string * Adef.encoded_string) list -> unit
(** [hidden_env_aux env]
    Creates a hidden HTML input for every key and value in [env].
*)

val hidden_env : Config.config -> unit
(** Creates a hidden HTML input for every key and value in [conf.henv] and [conf.senv].
    Used to include immutable environement bindings in the HTML form. *)

val hidden_textarea : Config.config -> string -> Adef.encoded_string -> unit

val hidden_input : Config.config -> string -> Adef.encoded_string -> unit
(** [hidden_input conf k v] *)

val hidden_input_s : Config.config -> string -> string -> unit
(** [hidden_input_s conf k v] *)

val submit_input : Config.config -> string -> Adef.encoded_string -> unit
(** [submit_input conf k v] *)

val nobtit : Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title list
(** [nobtit conf base p] returns list of titles of [p] from the [base]
    that respects constraints imposed by [conf.allowed_titles] and
    [conf.denied_titles] *)

val strictly_after_private_years : Config.config -> Date.dmy -> bool

val authorized_age : Config.config -> Gwdb.base -> Gwdb.person -> bool
(** Alias to GWPARAM.p_auth *)

(* TODO see if it can be removed from mli; it is used in geneanet's geneweb-plugin-api *)
val is_old_person :
  Config.config -> (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person -> bool

val start_with_vowel : string -> bool

val acces_n :
  Config.config ->
  Gwdb.base ->
  Adef.escaped_string ->
  Gwdb.person ->
  Adef.escaped_string
(** Returns URL query string to access nth person
    (e.g. for person 2 in url: p2=foo&n2=bar&oc2=1 *)

val acces : Config.config -> Gwdb.base -> Gwdb.person -> Adef.escaped_string

val accessible_by_key :
  Config.config -> Gwdb.base -> Gwdb.person -> string -> string -> bool
(** [accessible_by_key conf base p fn sn]
    Tells if person could be accessed by his first name and surname

    i.e. current base configuration and user rights allow this and
    person's name is suitable for searching by key (e.g. `? ?` is not)
*)

val geneweb_link :
  Config.config -> Adef.escaped_string -> Adef.safe_string -> Adef.safe_string
(** [geneweb_link conf href s] Returns HTML link to actual geneweb's command (database name) with additional (to those defind by [commd])
    argument [href] and [s] as textual content of the link. *)

val wprint_geneweb_link :
  Config.config -> Adef.escaped_string -> Adef.safe_string -> unit
(** Prints on the socket link created by [geneweb_link]. *)

val is_restricted : Config.config -> Gwdb.base -> Gwdb.iper -> bool
(** Tells if person is restrited to acccess. If mode `use_restrict` is
    disabled returns always [false]. *)

val is_empty_person : Gwdb.person -> bool
(** Tells if person is an empty person (a placeholder: his surname is empty) *)

val pget_opt : Config.config -> Gwdb.base -> Gwdb.iper -> Gwdb.person option
(** Returns person option with giving id from the base.
    Wrapper around `Gwdb.poi` defined such as:
    - Some ip: if user have permissions or `use_restrict` disabled.
    - None: if `conf.use_restrict` (option defined in .gwf file):
      checks that the user has enought rights to see
      corresponding person (see `authorized_age`).
      If the user does not have enought permissions, returns
      None.
*)

val pget : Config.config -> Gwdb.base -> Gwdb.iper -> Gwdb.person
(** Value of [pget_opt], map None to empty_person *)

val string_gen_person :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  (Gwdb.iper, Gwdb.iper, string) Def.gen_person
(** Remplaces string ids inside person's entry by their actual string value. *)

val string_gen_family :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.ifam, Gwdb.istr) Def.gen_family ->
  (Gwdb.iper, Gwdb.ifam, string) Def.gen_family
(** Remplaces string ids inside family's entry by their actual string value. *)

val gen_person_title_text :
  (Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string ->
  Adef.safe_string) ->
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string
(** [gen_person_title_text reference paccess conf base p] returns HTML structure
    of person that describes person's first name surname and main title. [reference]
    is used to either encapsulate structure in the link (or other type
    of maniplations). *)

val person_text_without_title :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Makes call to [gen_person_text_without_title] with [std_access] *)

val main_title : Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title option
(** Returns main person's title. If person doesn't have it, then returns first title
    from the list. *)

val max_ancestor_level : Config.config -> Gwdb.base -> Gwdb.iper -> int -> int

val titled_person_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title -> Adef.safe_string
(** Returns person's first name and surname text description depending on
    person's title *)

val one_title_text : Gwdb.base -> Gwdb.title -> Adef.safe_string
(** Returns HTML representation of title's identifier with its place (if exists) *)

val person_title_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns HTML structure of person that describes person's first name surname
    and main title. Calls [gen_person_title_text] with [no_reference]. *)

val person_title : Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns HTML representation of person's main title (or first title if
    main doesn't exists). If person doesn't have a title or if access to
    person isn't granted returns empty string *)

val child_of_parent :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val reference :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string ->
  Adef.safe_string
(** [reference conf base p desc] returns HTML link to the person
    where [desc] is content of the link (generaly his first name and
    surname description). If person is hidden returns [desc] (do not
    create link). *)

val reference_noid :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string ->
  Adef.safe_string
(** Same as [reference] but link doesn't has "id" field *)

val no_reference :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string ->
  Adef.safe_string
(** [reference conf base p desc] returns [desc] without creating a link *)

val referenced_person_title_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Retruns HTML link to the person that contains its first name, surname and person's
    nobility title. Calls [gen_person_title_text] with [reference]. *)

val referenced_person_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns HTML link to the person that contains its first name and surname. *)

val referenced_person_text_without_surname :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns HTML link to the person that contains its first name. *)

val update_family_loop :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string ->
  Adef.safe_string

val p_getenv : Config.env -> string -> string option
(** Returns value associated to the label in environnement *)

val p_getint : Config.env -> string -> int option
(** Returns integer value associated to the label in environnement *)

val create_env : Adef.encoded_string -> Config.env
(** Create association list from the query part of a URL.
    (i.e. a list of key-value separated by `&` or `;`)
*)

val open_etc_file : string -> (in_channel * string) option
(** [open_etc_file fname] search for template {i etc/fname.txt}
    inside the base directory or inside one of assets directories.
    Returns input channel and the path to given template. *)

val string_of_place : string -> Adef.escaped_string
val trimmed_string_of_place : string -> Adef.escaped_string
val raw_string_of_place : string -> string
val allowed_tags_file : string ref

val body_prop : Config.config -> string
(** Returns additional attributes for <body> tag from [config] (defined in .gwf file). *)

val message_to_wizard : Config.config -> unit
(** Prints all messages sent to wizard (or friend) on the socket. Messages are located in
    {i <basename>/etc/mess_wizzard.txt} (messages destinated to all wizards) and in
    {i <basename>/etc/mess_wizzard_<user>.txt} (messages destinated to considered wizard). *)

val of_course_died : Config.config -> Gwdb.person -> bool

val surname_particle : Gwdb.base -> string -> string
(** [surname_particle base sn]
    Extract the particle of [sn] if there is one.
    The list of particles to use is defined in [base]. *)

val surname_without_particle : Gwdb.base -> string -> string
(** [surname_without_particle base sn]
    Remove the particle of [sn] if there is one.
    The list of particles to use is defined in [base]. *)

val specify_homonymous :
  Config.config -> Gwdb.base -> Gwdb.person -> bool -> unit

val get_approx_birth_date_place :
  Gwdb.base -> Gwdb.person -> Date.date option * Adef.safe_string

val get_approx_death_date_place :
  Gwdb.base -> Gwdb.person -> Date.date option * Adef.safe_string

type ('a, 'b) format2 = ('a, unit, string, 'b) format4

val check_format : ('a, 'b) format2 -> string -> ('a, 'b) format2 option
val valid_format : ('a, 'b) format2 -> string -> ('a, 'b) format2

val transl : Config.config -> string -> string
(** Find translation of given keyword in [conf.lexicon].
    Keywords used to be its english translation but can be any string. *)

val transl_nth : Config.config -> string -> int -> string
(** [transl_nth conf w n] translate word [w] and returns [n]'th field of its translation (with [nth_field]). *)

val transl_decline : Config.config -> string -> string -> string
val ftransl : Config.config -> ('a, 'b) format2 -> ('a, 'b) format2
val ftransl_nth : Config.config -> ('a, 'b) format2 -> int -> ('a, 'b) format2
val fdecline : ('a, 'b) format2 -> string -> ('a, 'b) format2
val fcapitale : ('a, 'b) format2 -> ('a, 'b) format2

val nth_field : string -> int -> string
(** [nth_field str n] gets [n]'th field of string that separate its fields with "/".
    Example :
    - nth_field "a/b/</c>/d" 0 = a
    - nth_field "a/b/</c>/d" 1 = b
    - nth_field "a/b/</c>/d" 2 = </c>
    - nth_field "a/b/</c>/d" 3 = d *)

val cftransl : Config.config -> string -> string list -> string
val translate_eval : string -> string

val transl_a_of_b : Config.config -> string -> string -> string -> string
(** [transl_a_of_b conf a b b_raw]
    Translate "a of b" using [b_raw] for declension.
    i.e. if [b] is wrapped in html, [b_raw] should be that texte with no html,
    and [b_raw] should be [b] otherwise.
*)

val transl_a_of_gr_eq_gen_lev :
  Config.config -> string -> string -> string -> string

val std_color : Config.config -> Adef.safe_string -> Adef.safe_string
(** Colorise HTML element with [conf.highlight] color
    (wrap text in <span> with inline style). *)

val index_of_sex : Def.sex -> int
(** Sex index used in translations (0 for male, 1 for female, 2 for neuter) *)

val string_of_pevent_name_without_base :
  Config.config -> 'a Def.gen_pers_event_name -> Adef.safe_string

val string_of_fevent_name_without_base :
  Config.config -> 'a Def.gen_fam_event_name -> Adef.safe_string

val string_of_pevent_name :
  Config.config ->
  Gwdb.base ->
  Gwdb.istr Def.gen_pers_event_name ->
  Adef.safe_string

val string_of_pevent_name' :
  Config.config -> string Def.gen_pers_event_name -> Adef.safe_string

val string_of_fevent_name :
  Config.config ->
  Gwdb.base ->
  Gwdb.istr Def.gen_fam_event_name ->
  Adef.safe_string
(** [string_of_fevent_name conf base fevent_name]
*)

val string_of_fevent_name' :
  Config.config -> string Def.gen_fam_event_name -> Adef.safe_string

val string_of_witness_kind :
  Config.config -> Def.sex -> Def.witness_kind -> Adef.safe_string
(** [string_of_witness_kind conf sex wk]
    Return the string corresponding to wk according to [sex] and [conf].
*)

val string_of_access : Config.config -> Def.access -> Adef.safe_string

val relation_txt :
  Config.config -> Def.sex -> Gwdb.family -> (('a -> 'b) -> 'b, 'a, 'b) format

val string_of_decimal_num : Config.config -> float -> string
val person_exists : Config.config -> Gwdb.base -> string * string * int -> bool

val husband_wife :
  Config.config -> Gwdb.base -> Gwdb.person -> bool -> Adef.safe_string

val find_person_in_env :
  Config.config -> Gwdb.base -> string -> Gwdb.person option
(** [find_person_in_env conf base suff]
    Reconstitutes the key of a person from [conf.env],
    using ["i" ^ suff] or ["n" ^ suff] + ["p" ^ suff] + ["oc" ^ suff]
*)

val find_person_in_env_pref :
  Config.config -> Gwdb.base -> string -> Gwdb.person option
(** [find_person_in_env_pref conf base prefix]
    Same as [find_person_in_env] except that it uses a prefix
    instead of a suffix.
*)

(* Recherche le sosa uniquement dans le fichier gwf *)
val default_sosa_ref : Config.config -> Gwdb.base -> Gwdb.person option
val find_sosa_ref : Config.config -> Gwdb.base -> Gwdb.person option

val update_gwf_sosa :
  Config.config -> Gwdb.base -> Gwdb.iper * (string * string * int) -> unit

val get_server_string : Config.config -> string
(** Returns server host name with its port number (if different from 80). *)

val get_request_string : Config.config -> string
(** Returns request string. Request string has format {i scriptname?querystring} where
    scriptname is a path to the script in URI. *)

val create_topological_sort :
  Config.config -> Gwdb.base -> (Gwdb.iper, int) Gwdb.Marker.t

val p_of_sosa :
  Config.config -> Gwdb.base -> Sosa.t -> Gwdb.person -> Gwdb.person option
(** [p_of_sosa conf base sosa p0]
    Get the sosa [sosa] of [p0] if it exists
*)

val branch_of_sosa :
  Config.config -> Gwdb.base -> Sosa.t -> Gwdb.person -> Gwdb.person list option
(** [branch_of_sosa conf base sosa p0]
    Get all the lineage to go from [p0]'s ancestor with sosa number [sosa] to [p0]
*)

val sosa_of_branch : Gwdb.person list -> Sosa.t
(** [sosa_of_branch branch]
    Given a path of person to follow [branch], return the sosa number
    of the last person of this list. No check is done to ensure that
    given persons are actually parents.
*)

val old_branch_of_sosa :
  Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  Sosa.t ->
  (Gwdb.iper * Def.sex) list option
(** @deprecated Use [branch_of_sosa] instead *)

val old_sosa_of_branch :
  Config.config -> Gwdb.base -> (Gwdb.iper * Def.sex) list -> Sosa.t
(** @deprecated Use [sosa_of_branch] instead *)

val relation_type_text :
  Config.config -> Def.relation_type -> Def.sex -> Adef.safe_string

val rchild_type_text :
  Config.config -> Def.relation_type -> Def.sex -> Adef.safe_string

val has_nephews_or_nieces : Config.config -> Gwdb.base -> Gwdb.person -> bool
val browser_doesnt_have_tables : Config.config -> bool
val doctype : Adef.safe_string

val begin_centered : Config.config -> unit
(** Prints on the socket beginning of the <table> tag untill first opened <td> where the text is centred *)

val end_centered : Config.config -> unit
(** Prints on the socket end of the column and table opened by [begin_centered] *)

val print_alphab_list :
  Config.config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

val short_f_month : int -> string
(** 2 letters to represent the month in the french republican calendar *)

(* Reading password file *)

type auth_user = { au_user : string; au_passwd : string; au_info : string }
(** Authenticated user from from authorization file. *)

val read_gen_auth_file : string -> auth_user list
(** Read all authenticated users with their passwords from authorization file (associated to {i "wizard_passwd_file"} in [conf.base_env]) *)

val is_that_user_and_password :
  Config.auth_scheme_kind -> string -> string -> bool
(** [is_that_user_and_password auth_sheme user paswd] verify if given user with his password correspond to the authentication scheme. *)

(* Searching *)

val in_text : bool -> string -> string -> bool
val html_highlight : bool -> string -> string -> string

(* Print list in columns with Gutil.alphabetic order *)

val wprint_in_columns :
  Config.config -> ('a -> string) -> ('a -> unit) -> 'a list -> unit

val is_hide_names : Config.config -> Gwdb.person -> bool
(** Tells if person's names are hiden (if person's access is [Private] or if mode [conf.hide_names] is enabled). *)

val print_reference : Config.config -> string -> int -> string -> unit

val gen_print_tips : Config.config -> Adef.safe_string -> unit
(** Print a tip with the specified text *)

val print_tips_relationship : Config.config -> unit
(** Print a tip that tells to {i Click an individual below to calculate the family link.} *)

val display_options : Config.config -> Adef.escaped_string

type cache_visited_t = (string, (Gwdb.iper * string) list) Hashtbl.t

val cache_visited : Config.config -> string
val read_visited : Config.config -> cache_visited_t
val record_visited : Config.config -> Gwdb.iper -> unit

val array_mem_witn :
  Gwdb.base ->
  Gwdb.iper ->
  (Gwdb.iper * Def.witness_kind) array ->
  Gwdb.istr array ->
  (Def.witness_kind * string) option
(** [array_mem_witn conf base ip array] checks if [ip] is in [array]
    and returns corresponding [string_of_witness_kind] and witness note if so.
*)

val name_key : Gwdb.base -> string -> string
(** [name_key base name] is [name],
    with particles put at the end of the string instead of the beginning.
*)

val escape_html : string -> Adef.escaped_string
(** [escape_html str] replaces '&', '"', '\'', '<' and '>'
    with their corresponding character entities (using entity number) *)

val safe_html : string -> Adef.safe_string
(**
   [safe_html s] sanitizes [s] element in order to fix ill-formed
   HTML input and to prevent XSS injection

   It removes any tag which is not allowed by geneweb.
   It removes all attributes starting with ["on"].
   It removes any attribute when the value starts with ["javascript"].
   Text is escaped using [escape_html].
 *)

val string_with_macros :
  Config.config -> (char * (unit -> string)) list -> string -> string
(** [string_with_macros conf env s]
    Return a string with "%xxx" macro replaced by their value.
*)

val is_empty_name : Gwdb.person -> bool
(** [is_empty_name p]
    [false] if we knwon the first name or the last name of [p].
*)

module IperSet : sig
  include Set.S with type elt = Gwdb.iper
end

module IfamSet : sig
  include Set.S with type elt = Gwdb.ifam
end

(**/**)

val copy_from_templ_ref :
  (Config.config -> (string * Adef.encoded_string) list -> in_channel -> unit)
  ref
(** Reference by default [Templ.copy_from_templ] *)
(* [copy_from_templ_ref] is for internal usage only. Use copy_from_templ *)

(**/**)

val include_template :
  Config.config ->
  (string * Adef.encoded_string) list ->
  string ->
  (unit -> unit) ->
  unit
(** [include_template conf env fname failure]
    Search [fname] in templates path and interpret it with global environnement [env] provided.
    Interpretation of template write directly its results in the socket.
    If the file can not be found, [failure] is called.
*)

val select_masc :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper * int) list ->
  (Gwdb.iper, int * Gwdb.person) Hashtbl.t
(** [select_masc conf base ips]
    From [ips], a list matching ipers to a number of maximum generations,
    get maximum ascendants of ipers up to these corresponding generations.

    A person is maximum ascendant if their generation matches the maximum, or
    if they do not have ancestors.

    The result is a Hashtbl matching an iper to the corresponding person and
    their generation.
*)

val select_desc :
  Config.config ->
  Gwdb.base ->
  int ->
  (Gwdb.iper * int) list ->
  (Gwdb.iper, Gwdb.person) Hashtbl.t
(** [select_desc conf base gen_desc ips]
    From [ips], a list matching ipers to a number of maximum generations,
    get spouses and descendants of ipers up to these corresponding generations.
*)

val select_mascdesc :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper * int) list ->
  int ->
  (Gwdb.iper, Gwdb.person) Hashtbl.t
(** [select_ascdesc conf base ips gen_desc]
    Get maximum ascendants with {!val:select_masc}, and get their desc with
    {!val:select_desc}
 *)

val sprintf_today : Config.config -> Adef.safe_string
(** [sprintf_today confo]
    Uses {!val:Mutil.sprintf_date} in order to print datetime defined in [conf]. *)

val auth_warning :
  Config.config ->
  Gwdb.base ->
  ('a, Gwdb.person, Gwdb.ifam, 'b, 'c, 'd, 'e) Warning.warning ->
  bool
(** [auth_warning conf base w]
    Check if current user has enough right in order to see [w] *)

val name_with_roman_number : string -> string option
(** Convert arabic numerals to roman numerals.
    [Some result] is returned if there are numerals, [None] if not.
*)

val designation : Gwdb.base -> Gwdb.person -> Adef.escaped_string
(** [designation base p] is [Gutil.designation base p |> escape_html] *)

val has_children : Gwdb.base -> Gwdb.person -> bool

val is_fully_visible_to_visitors :
  Config.config -> Gwdb.base -> Gwdb.person -> bool
(** [is_fully_visible_to_visitors conf base p] is true iff [p] is fully visible for a visitor *)

val is_public : Config.config -> Gwdb.base -> Gwdb.person -> bool
