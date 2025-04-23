module type S = sig
  type 'a t
  type char_
  type word

  val cardinal : 'a t -> int
  val mem : word -> 'a t -> bool
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : word -> 'a -> 'a t -> 'a t
  val remove : word -> 'a t -> 'a t
  val update : word -> ('a option -> 'a option) -> 'a t -> 'a t
  val fold : (word -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_subtries : (char_ -> 'a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (word -> 'a -> unit) -> 'a t -> unit
  val step : char_ -> 'a t -> 'a t
  val data : 'a t -> 'a option
  val of_seq : (word * 'a) Seq.t -> 'a t
  val to_seq : 'a t -> (word * 'a) Seq.t
  val pp : 'a Fmt.t -> 'a t Fmt.t
  val pp_statistics : 'a t Fmt.t
end

module Make (W : Word.S) = struct
  module M = Map.Make (struct
    type t = W.char_

    (* The alphabet order is reversed to minimize slow reversals in the trie
       iterators. *)
    let compare x y = -W.compare_char x y
  end)

  type char_ = W.char_
  type word = W.t
  type 'a t = { subtries : 'a t M.t; data : 'a option; cardinal : int }

  let empty = { subtries = M.empty; data = None; cardinal = 0 }
  let[@inline always] cardinal { cardinal; _ } = cardinal
  let[@inline always] is_empty { cardinal; _ } = cardinal = 0

  let update w f t =
    let len = W.length w in
    let rec loop t i =
      let { subtries; data; _ } = t in
      if i = len then
        let fdata = f data in
        let diff =
          match (data, fdata) with
          | Some _, Some _ | None, None -> 0
          | Some _, None -> -1
          | None, Some _ -> 1
        in
        let cardinal = t.cardinal + diff in
        if cardinal = 0 then (diff, empty)
        else (diff, { t with data = fdata; cardinal })
      else
        let c = W.get w i in
        let child =
          match M.find c t.subtries with
          | exception Not_found -> empty
          | child -> child
        in
        let diff, nchild = loop child (i + 1) in
        let subtries =
          if is_empty nchild then M.remove c subtries
          else M.add c nchild subtries
        in
        (diff, { t with subtries; cardinal = t.cardinal + diff })
    in
    loop t 0 |> snd

  let add w v t = update w (fun _ -> Some v) t
  let remove w t = update w (fun _ -> None) t
  let step c t = M.find c t.subtries
  let[@inline always] data t = t.data

  let mem w t =
    let len = W.length w in
    let rec loop i t =
      if i = len then Option.is_some (data t)
      else
        match step (W.get w i) t with
        | exception Not_found -> false
        | t -> loop (i + 1) t
    in
    loop 0 t

  let of_seq s =
    let rec loop s acc =
      match s () with
      | Seq.Nil -> acc
      | Cons ((w, v), s) -> loop s (add w v acc)
    in
    loop s empty

  let to_seq t =
    let rec loop stack () =
      match stack with
      | [] -> Seq.Nil
      | (rev_pfx, t) :: stack -> (
          let { subtries; data; _ } = t in
          let stack =
            M.fold
              (fun c tc stack -> (c :: rev_pfx, tc) :: stack)
              subtries stack
          in
          match data with
          | Some v ->
              let w = W.of_rev_list rev_pfx in
              Seq.Cons ((w, v), loop stack)
          | None -> loop stack ())
    in
    loop [ ([], t) ]

  let pp_human_size ppf i =
    let eucl u v = (u / v, u mod v) in
    let qm, rm = eucl i (1 lsl 20) in
    let qk, rk = eucl rm (1 lsl 10) in
    if qm > 0 then Fmt.pf ppf "%d Mio" qm
    else if qk > 0 then Fmt.pf ppf "%d Kio" rk
    else Fmt.pf ppf "%d o" rk

  let pp_statistics ppf t =
    Fmt.pf ppf "%d words (size %a)" t.cardinal pp_human_size
      (Obj.reachable_words (Obj.repr t))

  let fold f t acc =
    let rec loop stack acc =
      match stack with
      | [] -> acc
      | (rev_pfx, t) :: stack ->
          let { subtries; data; _ } = t in
          let stack =
            M.fold
              (fun c tc stack -> (c :: rev_pfx, tc) :: stack)
              subtries stack
          in
          let acc =
            match data with
            | Some v -> f (W.of_rev_list rev_pfx) v acc
            | None -> acc
          in
          loop stack acc
    in
    loop [ ([], t) ] acc

  let fold_subtries f { subtries; _ } acc = M.fold f subtries acc
  let iter f t = fold (fun w v () -> f w v) t ()

  let pp pp_val =
    Fmt.box
    @@ Fmt.iter_bindings ~sep:Fmt.sp iter
    @@ Fmt.parens
    @@ Fmt.pair ~sep:Fmt.comma W.pp pp_val
end

module Default = Make (Word.Default)
