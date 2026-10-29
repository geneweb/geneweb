val return_if : bool -> (unit -> 'a) -> 'a option
(** [return_if false return] is [None] and [return_if true return] is
    [Some (return ())].  *)

module Infix : sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
end
