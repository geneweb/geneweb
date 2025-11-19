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

type checkdata_entry = Geneweb_db.Driver.istr * string

val update_cache_entry :
  Config.config -> dict_type -> Geneweb_db.Driver.istr -> string -> bool
(** Update a single entry in the cache file for the specified dictionary. *)

val find_dict_type_for_istr :
  Config.config -> Geneweb_db.Driver.istr -> dict_type option
(** Find which dictionary type contains the given istr. *)

val cache_file_exists : Config.config -> dict_type -> bool
(** Check if a cache file exists for the specified dictionary type. *)

val make_error_html :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.istr ->
  string ->
  error_type ->
  string * string * string * string * bool
(** Generate HTML markup for displaying an error with highlighting. *)

val collect_all_errors_with_cache :
  ?max_results:int option ->
  ?sel_err_types:error_type list ->
  Config.config ->
  Geneweb_db.Driver.base ->
  dict_type ->
  (Geneweb_db.Driver.istr * string * error_type list) list
(** Smart error collection using cache when available. *)

val dict_to_cache_name : dict_type -> string
(** Convert dictionary type to its cache filename. *)
