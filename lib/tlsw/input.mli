type t

val of_string : string -> t
val of_file : string -> t
val of_channel : in_channel -> t
val offset : t -> int
val eof : t -> bool
val peak : t -> char option
val next : t -> t
val to_source : t -> Loc.source
val sub : t -> int -> int -> string
