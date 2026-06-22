type t

val make :
  ?path:string ->
  conf:Config.Trimmed.t ->
  lang:Lang.t ->
  query:(string * string list) list ->
  unit ->
  t

val make' :
  ?path:string ->
  conf:Config.Trimmed.t ->
  lang:Lang.t ->
  query:(string * string) list ->
  unit ->
  t

val lang : t -> Lang.t
val to_string : t -> string
val query : t -> (string * string list) list
val with_fragment : t -> string option -> t
