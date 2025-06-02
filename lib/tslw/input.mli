type t

val of_string : string -> t
val offset : t -> int
val eof : t -> bool
val peak : t -> char option
val next : t -> t
