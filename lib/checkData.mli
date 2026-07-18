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
  Config.config ->
  dict_type ->
  old_istr:Geneweb_db.Driver.istr ->
  new_istr:Geneweb_db.Driver.istr ->
  string ->
  bool
(** [update_cache_entry conf dict ~old_istr ~new_istr v] replaces the cache
    entry keyed by [old_istr] with [(new_istr, v)], atomically (write to a
    temporary file, then rename). After a modification the persons reference a
    new istr while the old one keeps its former string, so the entry must be
    re-keyed to stay valid. Returns [false] if the cache file or the entry is
    missing, or on write failure. *)

val find_dict_type_for_istr :
  Config.config -> Geneweb_db.Driver.istr -> dict_type option
(** Find which dictionary cache contains the given istr. Linear scan of all
    cache files; first match wins even though an istr may belong to several
    dictionaries. Fallback only: prefer passing the dictionary explicitly via
    the [d] parameter. *)

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
