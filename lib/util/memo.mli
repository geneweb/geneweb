(* Each function should use a different module instance (cache is shared in a given module instance) *)
module Make
    (Key : Hashtbl.HashedType) (F : sig
      type res

      val f : Key.t -> res
    end) : sig
  val init : unit -> unit
  val memoized : Key.t -> F.res
end
