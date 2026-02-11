type t

val make : conf:Config.Trimmed.t -> query:(string * string) list -> t
val to_string : t -> string
