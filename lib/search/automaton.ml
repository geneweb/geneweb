module type X = sig
  type word

  val pattern : word
  val max_dist : int
end

module type S = sig
  type char_
  type word
  type state

  val init : state
  val next : char_ -> state -> state
  val accept : state -> bool
  val can_match : state -> bool
  val recognize : word -> bool
end

module Make (W : Word.S) (X : X with type word = W.t) = struct
  type char_ = W.char_
  type word = W.t

  let pattern = X.pattern
  let len = W.length pattern
  let max_dist = X.max_dist

  type state = int list

  let init =
    let rec loop st i = if i >= 0 then loop (i :: st) (i - 1) else st in
    loop [] len

  let next c st =
    let rec loop st nst i =
      match (st, nst) with
      | sti :: stj :: st, nsti :: _ ->
          let cost = if W.get pattern i = c then 0 else 1 in
          let u = nsti + 1 |> min (sti + cost) |> min (stj + 1) in
          loop (stj :: st) (u :: nst) (i + 1)
      | _ -> List.rev nst
    in
    loop st [ List.hd st + 1 ] 0

  let accept st =
    let rec last st =
      match st with [] -> assert false | [ x ] -> x | _ :: xs -> last xs
    in
    last st <= max_dist

  let can_match st =
    let x = List.hd st in
    let min = List.fold_left min x st in
    min <= max_dist

  let recognize s =
    let len = W.length s in
    let rec loop st i =
      if i < len && can_match st then loop (next (W.get s i) st) (i + 1)
      else accept st
    in
    loop init 0
end
