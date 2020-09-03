open Config

val print_sub_part
  : config
  -> Gwdb.base
  -> bool
  -> string
  -> string
  -> int
  -> Wiki.document
  -> unit

val print_mod_view_page
  : config
  -> Gwdb.base
  -> bool
  -> string
  -> string
  -> (bool -> unit)
  -> (string * string) list
  -> string
  -> unit

val print_mod_ok
  : config
  -> Gwdb.base
  -> (string -> string option)
  -> (string option -> string)
  -> (string -> (string * string) list * string)
  -> (string -> string -> unit) -> (string -> string)
  -> bool
  -> unit
