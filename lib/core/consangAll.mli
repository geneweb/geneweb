(* Copyright (c) 2006-2007 INRIA *)

val compute : ?verbosity:int -> Geneweb_db.Driver.base -> bool -> bool
(** [compute base from_scratch] [?verbosity] may be 0, 1 or 2 (default is 2)
    Compute consanguinity for each person in the base. If [from_scratch] is set
    then recompute consanguinity for entire database. Return [true] if base has
    been patched, [false] otherwise. *)
