type t

val make :
  conf:Config.Trimmed.t -> lang:Lang.t -> query:(string * string) list -> t

val lang : t -> Lang.t
val to_string : t -> string
