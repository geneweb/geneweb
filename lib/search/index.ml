module type S = sig
  type t
  type char_
  type word
  type entry

  val of_seq : (word * entry) Seq.t -> t
  val mem : word -> t -> bool
  val search : word list -> t -> entry Seq.t
  val search_prefix : word list -> t -> entry Seq.t
  val fuzzy_search : max_dist:int -> word list -> t -> entry Seq.t
end

module type Entry = sig
  type t

  val dummy : t
  val compare : t -> t -> int
  val hash : t -> int
  val pp : t Fmt.t
end

module Make (W : Word.S) (E : Entry) = struct
  type char_ = W.char_
  type word = W.t
  type entry = E.t

  module Trie = Trie.Make (W)

  (* Hash consed entries *)
  module HE = struct
    type t = { e : E.t; mutable tag : int }

    module W = Weak.Make (struct
      type nonrec t = t

      let hash { e; _ } = E.hash e
      let equal { e = e1; _ } { e = e2; _ } = E.compare e1 e2 = 0
    end)

    let compare { tag = t1; _ } { tag = t2; _ } = t1 - t2
    let dummy = { e = E.dummy; tag = -1 }
    let[@inline always] to_entry { e; _ } = e
    let pp ppf { e; _ } = E.pp ppf e

    let add =
      let tbl = W.create 200 in
      let ctr = ref 0 in
      fun e ->
        let he = W.merge tbl { e; tag = -1 } in
        if he.tag = -1 then (
          he.tag <- !ctr;
          incr ctr);
        he
  end

  module Flatset = Flatset.Make (HE)

  type t = Flatset.t Trie.t

  let of_seq =
    let module SE = Set.Make (HE) in
    fun s ->
      let t : SE.t Trie.t = Trie.empty in
      let t =
        Seq.fold_left
          (fun t (w, e) ->
            let he = HE.add e in
            Trie.update w
              (fun so ->
                match so with
                | None -> Some (SE.singleton he)
                | Some s -> Some (SE.add he s))
              t)
          t s
      in
      let t =
        Trie.fold
          (fun w se t -> Trie.add w (Flatset.of_seq @@ SE.to_seq se) t)
          t Trie.empty
      in
      t

  let mem = Trie.mem

  let intersection l =
    match l with
    | [] -> Seq.empty
    | _ :: _ ->
        Seq.map HE.to_entry @@ Iterator.to_seq
        @@ Iterator.join (module Flatset.Comparator) l

  let ( let* ) = Option.bind

  let rec try_fold f acc l =
    match l with
    | [] -> Some acc
    | x :: xs ->
        let* acc = f acc x in
        try_fold f acc xs

  let search ws t =
    let rec loop w =
      (* Produce the iterator of entries associated with the key [w] in
         the index [t]. Returns [None] if there is no entries associated
         with [w]. *)
      let len = W.length w in
      fun i t ->
        if i = len then
          let* s = Trie.data t in
          Some (Flatset.iterator s)
        else
          match Trie.step (W.get w i) t with
          | exception Not_found -> None
          | t -> loop w (i + 1) t
    in
    try_fold
      (fun acc w ->
        let* u = loop w 0 t in
        Some (u :: acc))
      [] ws
    |> Option.value ~default:[]
    |> intersection

  let search_prefix ps t =
    let rec loop acc pfx =
      (* Accumulate in [acc] all the iterators of flatsets in [t] whose the
         key starts by [pfx]. *)
      let len = W.length pfx in
      fun i t ->
        if i = len then
          Trie.fold (fun _ se acc -> Flatset.iterator se :: acc) t acc
        else
          match Trie.step (W.get pfx i) t with
          | exception Not_found -> acc
          | t -> loop acc pfx (i + 1) t
    in
    try_fold
      (fun acc pfx ->
        match loop [] pfx 0 t with
        | [] -> None
        | l -> Some ((Iterator.union (module Flatset.Comparator) l) :: acc))
      [] ps
    |> Option.value ~default:[]
    |> intersection

  type automaton =
    | A :
        (module Automaton.S
           with type char_ = char_
            and type word = word
            and type state = 's)
        * 's
        -> automaton

  let fuzzy_search ~max_dist ps t =
    let atms =
      List.map
        (fun p : automaton ->
          let module A =
            Automaton.Make
              (W)
              (struct
                type nonrec word = word

                let pattern = p
                let max_dist = max_dist
              end)
          in
          A ((module A), A.init))
        ps
    in
    let rec loop acc (A ((module A), st)) t =
      (* Accumulate in [acc] all the iterators of flatsets in [t] whose the
         key matches the pattern represented by the automaton A. *)
      if A.accept st then
        Trie.fold (fun _ se acc -> Flatset.iterator se :: acc) t acc
      else if A.can_match st then
        Trie.fold_subtries
          (fun c t acc -> loop acc (A ((module A), A.next c st)) t)
          t acc
      else acc
    in
    try_fold
      (fun acc atm ->
        match loop [] atm t with
        | [] -> None
        | l -> Some ((Iterator.union (module Flatset.Comparator) l) :: acc))
      [] atms
    |> Option.value ~default:[]
    |> intersection
end

module Default =
  Make
    (Word.Default)
    (struct
      include Word.Default

      let dummy = ""
    end)
