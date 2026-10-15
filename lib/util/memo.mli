(** Generic memoization module. *)

module Make
    (Key : Hashtbl.HashedType) (F : sig
      type res

      val f : Key.t -> res
    end) : sig
  val memoized : Key.t -> F.res
  (** [memoized k] returns the cached result of [F.f k], or computes, caches, and returns it. *)

  val clear : unit -> unit
  (** [clear ()] flushes all cached entries from the memoization table. *)
end
