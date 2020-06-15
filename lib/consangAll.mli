(* Copyright (c) 2006-2007 INRIA *)

open Gwdb

(** [compute base from_scratch]
    [?verbosity] may be 0, 1 or 2 (default is 2)

    Return [true] if base has been patched, [false] otherwise.
*)
val compute : ?verbosity:int -> base -> bool -> bool
