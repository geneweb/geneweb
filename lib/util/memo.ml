module Make
    (Key : Hashtbl.HashedType) (F : sig
      type res

      val f : Key.t -> res
    end) =
struct
  module Cache = Hashtbl.Make (Key)

  let cache = Cache.create 16

  let memoized x =
    match Cache.find_opt cache x with
    | Some result -> result
    | None ->
        let result = F.f x in
        Cache.add cache x result;
        result

  let clear () = Cache.clear cache
end
