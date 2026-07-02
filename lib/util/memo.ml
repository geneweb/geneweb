module Make
    (Key : Hashtbl.HashedType) (F : sig
      type res

      val f : Key.t -> res
    end) =
struct
  module Cache = Hashtbl.Make (Key)

  let table = ref None
  let init () = table := Some (Cache.create 16)

  let memoized x =
    match !table with
    | Some table -> (
        match Cache.find_opt table x with
        | Some result -> result
        | None ->
            let result = F.f x in
            Cache.add table x result;
            result)
    | None -> F.f x
end
