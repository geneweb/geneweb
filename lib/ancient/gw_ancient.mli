val is_available : bool

(* Trimmed ocaml-ancient library signature *)

type 'a ancient

val mark : 'a -> 'a ancient
val follow : 'a ancient -> 'a
val delete : 'a ancient -> unit
