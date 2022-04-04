
type t

exception Not_a_geneweb_base
exception Unsupported_base

val string_of_version : t -> string

val check_version : string -> t option

val eq_version : t -> t -> bool

val gnwb20 : t
val gnwb21 : t
val gnwb22 : t
val gnwb23 : t
val gnwb24 : t
val gnwb25 : t
