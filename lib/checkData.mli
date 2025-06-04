type dict_type =
  | Fnames
  | Snames
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

val find_error_positions : error_type -> string -> int list
val fix_error : error_type -> string -> string
val make_error_html : Config.config -> string -> string -> error_type -> string

val collect_all_errors :
  Geneweb_db.Driver.base -> dict_type -> (string * error_type list) list
