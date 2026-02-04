type t

val english : t
val from_tag : string -> t option
val tag : t -> string
val all : t list
