val hidden_first_name_txt : Adef.safe_string
val hidden_surname_txt : Adef.safe_string
val is_hidden : Config.config -> Gwdb.base -> Gwdb.person -> bool
val is_restricted : Config.config -> Gwdb.base -> Gwdb.person -> bool
val hidden_or_restricted_fullname_string : Adef.safe_string

val map_person_name_visibility :
  ?on_hidden_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  ?on_restricted_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  on_visible_name:
    (Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string) ->
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Adef.safe_string

val first_name_html_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val fullname_html_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val first_name_str_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> string

val fullname_str_of_person : Config.config -> Gwdb.base -> Gwdb.person -> string

val title_html_of_person :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.title -> Adef.safe_string
(** Returns person's first name and surname text description depending on
    person's title *)

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

val person_title_text :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Returns HTML structure of person that describes person's first name surname
    and main title. Calls [gen_person_title_text] with [no_reference]. *)

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

val person_text_without_title :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string
(** Makes call to [gen_person_text_without_title] with [std_access] *)

val child_of_parent :
  Config.config -> Gwdb.base -> Gwdb.person -> Adef.safe_string

val specify_homonymous :
  Config.config -> Gwdb.base -> Gwdb.person -> bool -> unit

val husband_wife :
  Config.config -> Gwdb.base -> Gwdb.person -> bool -> Adef.safe_string
