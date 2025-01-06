module type S = sig
  type t
  type char_
  type word
  type entry

  val of_seq : (word * entry) Seq.t -> t
  val mem : word -> t -> bool
  val search : word list -> t -> entry Seq.t
  val search_prefix : word list -> t -> entry Seq.t
  val fuzzy_search: max_dist:int -> word list -> t -> entry Seq.t
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
        Seq.map HE.to_entry @@ Flatset.Iterator.to_seq
        @@ Flatset.Iterator.join l

  let search ws t =
    let rec loop w =
      let len = W.length w in
      fun i t ->
        if i = len then
          Option.bind (Trie.data t) @@ fun s -> Some (Flatset.iterator s)
        else
          match Trie.step (W.get w i) t with
          | exception Not_found -> None
          | t -> loop w (i + 1) t
    in
    List.fold_left
      (fun acc w -> match loop w 0 t with Some it -> it :: acc | None -> acc)
      [] ws
    |> intersection

  let search_prefix ps t =
    (* Accumulate in [acc] all the iterators of flatsets in [t] whose the
       key starts by [pfx]. *)
    let rec loop acc pfx =
      let len = W.length pfx in
      fun i t ->
        if i = len then
          Trie.fold (fun _ se acc -> Flatset.iterator se :: acc) t acc
        else
          match Trie.step (W.get pfx i) t with
          | exception Not_found -> acc
          | t -> loop acc pfx (i + 1) t
    in
    List.fold_left
      (fun acc pfx ->
        match loop [] pfx 0 t with
        | [] -> acc
        | l -> Flatset.Iterator.union l :: acc)
      [] ps
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
    (* Accumulate in [acc] all the iterators of flatsets in [t] whose the
       key matches the pattern represented by the automaton A. *)
    let rec loop acc (A ((module A), st)) t =
      if A.accept st then
        Trie.fold (fun _ se acc -> Flatset.iterator se :: acc) t acc
      else if A.can_match st then
        Trie.fold_subtries
          (fun c t acc -> loop acc (A ((module A), A.next c st)) t)
          t acc
      else acc
    in
    List.fold_left
      (fun acc atm ->
        match loop [] atm t with
        | [] -> acc
        | l -> Flatset.Iterator.union l :: acc)
      [] atms
    |> intersection
end

module Default =
  Make
    (Word.Default)
    (struct
      include Word.Default

      let dummy = ""
    end)
