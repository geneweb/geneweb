type dict_type =
  | Fnames
  | Snames
  | Fnames_alias
  | Snames_alias
  | Places
  | PubNames
  | Qualifiers
  | Aliases
  | Occupation
  | Estates
  | Titles
  | Sources

type error_type =
  | InvisibleCharacters
  | BadCapitalization
  | MultipleSpaces
  | NonBreakingSpace
  | MiscTypographicErrors
  | MixedScripts
  | Mojibake
  | HtmlEntities
  | SuspiciousCharacters
  | FrenchOrdinals
  | RepeatedWords
  | NoLetters

val invisible_html : Config.config -> string -> string
(** [invisible_html conf s] is [s] escaped for HTML, with its invisible
    characters, its non-breaking spaces and its spaces at both ends wrapped in
    the spans of m=CHK_DATA (classes [ic], [zw], titled with the code point and
    its name), so that they can be seen. *)

val applies : error_type -> dict_type -> bool
(** [applies error_type dict] tells whether [error_type] is checked in the
    entries of [dict]. *)

val find_dict_type_for_istr :
  Geneweb_db.Driver.base -> Geneweb_db.Driver.istr -> dict_type option
(** [find_dict_type_for_istr base istr] A dictionary whose index has records
    using [istr], first names and surnames first: an istr may belong to several
    dictionaries. Fallback only: prefer passing the dictionary explicitly via
    the [d] parameter. *)

val make_error_html :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.istr ->
  string ->
  error_type ->
  string * string * string * string * bool
(** Generate HTML markup for displaying an error with highlighting. *)

val collect_all_errors :
  ?max_results:int ->
  ?sel_err_types:error_type list ->
  Geneweb_db.Driver.base ->
  dict_type ->
  (Geneweb_db.Driver.istr * string * error_type list) list
(** [collect_all_errors ?max_results ?sel_err_types base dict] The entries of
    [dict] with errors of [sel_err_types], at most [max_results], in the order
    of the index of the dictionaries, corrected by the committed patches. *)
