module type OrderedType = Map.OrderedType

module type S =
  sig
    include Map.S
    val key_after : (key -> key -> int) -> 'a t -> key
    val next : (key -> key -> int) -> 'a t -> key
  end

module Make (Ord: OrderedType) = struct

  include Map.Make (Ord)

  type 'a internal =
    | Empty
    | Node of {l:'a internal; v:key; d:'a; r:'a internal; h:int}

  let rec key_after f_compare = function
    | Empty -> raise Not_found
    | Node {l; v; r; _} ->
      let c = f_compare v in
      if c < 0 then try key_after f_compare l with Not_found -> v
      else if c > 0 then key_after f_compare r
      else v

  let key_after f_compare (x : 'a t) =
    key_after f_compare (Obj.magic x)

  let rec next x = function
    | Empty -> raise Not_found
    | Node {l; v; r; _} ->
      let c = Ord.compare x v in
      if c < 0 then try next x l with Not_found -> v else next x r

  let next k (x : 'a t) =
    next k (Obj.magic x)

end
