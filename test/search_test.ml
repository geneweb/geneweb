module A = Alcotest
module Compat = Geneweb_compat
module Trie = Geneweb_search.Trie.Default
module Cursor = Geneweb_search.Cursor
module Comparator = Geneweb_search.Comparator
module Seq = Geneweb_compat.Seq

(* Compute the Levenshtein distance of [s1] and [s2]. *)
(* Unused --
let distance s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let l1, l2, s1, s2 = if l2 < l1 then (l2, l1, s2, s1) else (l1, l2, s1, s2) in
  let prev = Array.init (l2 + 1) (fun j -> j) in
  let dp = Array.init (l2 + 1) (fun _ -> 0 (* dummy *)) in
  for i = 1 to l1 do
    dp.(0) <- i;
    for j = 1 to l2 do
      if String.get s1 (i - 1) = String.get s2 (j - 1) then
        dp.(j) <- prev.(j - 1)
      else
        let u = min prev.(j) dp.(j - 1) in
        dp.(j) <- 1 + min u prev.(j - 1)
    done;
    Array.blit dp 0 prev 0 (l2 + 1)
  done;
  prev.(l2) *)

module Cursor_tests = struct
  type wit

  let cmp =
    (module struct
      type nonrec wit = wit
      type t = int

      let compare = Int.compare
      let dummy = 0
    end : Comparator.S
      with type t = int
       and type wit = wit)

  let cursor_of_list l =
    let l = ref l in
    let curr () = match !l with [] -> raise Cursor.End | x :: _ -> (x, ()) in
    let next () = match !l with [] -> () | _ :: xs -> l := xs in
    let seek _ = assert false in
    Cursor.make cmp ~curr ~next ~seek

  let test_union_1 () =
    let c1 = cursor_of_list [ 1; 3; 5; 10 ] in
    let c2 = cursor_of_list [ 2; 6; 11 ] in
    let expected = cursor_of_list [ 1; 2; 3; 5; 6; 10; 11 ] in
    let result = Cursor.union cmp [ c1; c2 ] in
    A.(check bool) "foo" true (Cursor.equal cmp result expected)

  let test_union_2 () =
    let c1 = cursor_of_list [ 2; 4; 5; 7 ] in
    let c2 = cursor_of_list [ 3; 4; 6; 11 ] in
    let expected = cursor_of_list [ 2; 3; 4; 5; 6; 7; 11 ] in
    let result = Cursor.union cmp [ c1; c2 ] in
    A.(check bool) "foo" true (Cursor.equal cmp result expected)

  let test_union_3 () =
    let c1 = cursor_of_list [ 1 ] in
    let c2 = cursor_of_list [ 1; 4; 6; 11 ] in
    let expected = cursor_of_list [ 1; 4; 6; 11 ] in
    let result = Cursor.union cmp [ c1; c2 ] in
    A.(check bool) "foo" true (Cursor.equal cmp result expected)

  let all =
    let quick_test s tst = A.test_case s `Quick tst in
    ( "Cursor tests",
      [
        quick_test "union 1" test_union_1;
        quick_test "union 2" test_union_2;
        quick_test "union 3" test_union_3;
      ] )
end

module Trie_tests = struct
  let test_cardinal _trie _a () =
    let trie =
      Trie.of_seq @@ List.to_seq [ ("foo", ()); ("bar", ()); ("saucisse", ()) ]
    in
    A.(check int) "cardinal after insertion" 3 (Trie.cardinal trie)

  let test_mem trie _a () =
    A.(check bool) "mem Auxais" true (Trie.mem "Auxais" trie);
    A.(check bool) "mem Paris" false (Trie.mem "Paris" trie);
    A.(check bool) "mem Borde" false (Trie.mem "Borde" trie)

  let test_remove trie _a () =
    A.(check bool) "remove Auxais (before)" true (Trie.mem "Auxais" trie);
    let trie = Trie.remove "Auxais" trie in
    A.(check bool) "remove Auxais (after)" false (Trie.mem "Auxais" trie)

  let test_update trie _a () =
    let c = Trie.cardinal trie in
    let trie =
      Trie.update "Auxais" (function Some _ -> None | None -> None) trie
    in
    A.(check bool) "update Auxais" true (Trie.cardinal trie = c - 1)

  let test_lexicographic_order _trie _a () =
    let trie =
      Trie.of_seq
      @@ List.to_seq
           [ ("abe", ()); ("ab", ()); ("a", ()); ("bcd", ()); ("abcd", ()) ]
    in
    let expected =
      [ ("a", ()); ("ab", ()); ("abcd", ()); ("abe", ()); ("bcd", ()) ]
    in
    A.(check (list (pair string unit)))
      "order of_seq" expected
      (List.of_seq @@ Trie.to_seq trie);
    let l = Trie.fold (fun w () acc -> (w, ()) :: acc) trie [] |> List.rev in
    A.(check (list (pair string unit))) "order fold" expected l

  let test_random_mem trie a =
    let sz = Array.length a in
    QCheck.Test.make ~count:1000 ~name:"random mem" QCheck.(int_bound (sz - 1))
    @@ fun i -> Trie.mem a.(i) trie

  let test_random_remove trie a =
    let sz = Array.length a in
    QCheck.Test.make ~count:1000 ~name:"random remove"
      QCheck.(int_bound (sz - 1))
    @@ fun i ->
    let trie = Trie.remove a.(i) trie in
    not @@ Trie.mem a.(i) trie

  let create_trie path =
    Compat.In_channel.with_open_text path @@ fun ic ->
    let rec loop t l i =
      match Compat.In_channel.input_line ic with
      | None -> (t, Array.of_list l)
      | Some line -> loop (Trie.add line i t) (line :: l) (i + 1)
    in
    loop Trie.empty [] 1

  let generate dict =
    let trie, a = create_trie dict in
    let quick_test s tst = A.test_case s `Quick (tst trie a) in
    let qcheck_test tst = QCheck_alcotest.to_alcotest (tst trie a) in
    ( "Trie tests",
      [
        quick_test "cardinal" test_cardinal;
        quick_test "mem" test_mem;
        quick_test "remove" test_remove;
        quick_test "update" test_update;
        quick_test "lexicography_order" test_lexicographic_order;
        qcheck_test test_random_mem;
        qcheck_test test_random_remove;
      ] )
end

module Flatset_tests = struct
  module Entry = struct
    type t = int

    let dummy = 0
    let compare = Int.compare
    let pp = Fmt.int
  end

  module Flatset = Geneweb_search.Flatset.Make (Entry)

  module Naive = struct
    let of_seq s =
      let arr = Array.of_seq s in
      Array.fast_sort Int.compare arr;
      arr

    let mem = Array.mem
    let empty = [||]

    let union a1 a2 =
      let l1 = Array.to_list a1 in
      let l2 = Array.to_list a2 in
      List.rev_append l1 l2 |> List.sort_uniq Int.compare |> Array.of_list

    let inter a1 a2 =
      let l1 = Array.to_list a1 in
      let l2 = Array.to_list a2 in
      let rec loop acc l1 l2 =
        match (l1, l2) with
        | x :: xs, y :: ys ->
            if x = y then loop (x :: acc) xs ys
            else if x < y then loop acc xs l2
            else loop acc l1 ys
        | _ -> acc
      in
      loop [] l1 l2 |> List.sort Int.compare |> Array.of_list

    let cursor t =
      let idx = ref 0 in
      let curr () =
        if !idx < Array.length t then (t.(!idx), ()) else raise Cursor.End
      in
      let next () = if !idx < Array.length t then incr idx in
      let seek w =
        let rec loop () =
          match curr () with
          | exception Cursor.End -> ()
          | v, () when w <= v -> ()
          | _ ->
              next ();
              loop ()
        in
        loop ()
      in
      Cursor.make Flatset.cmp ~curr ~next ~seek
  end

  let nonempty_array = QCheck.Gen.(range_subset ~size:5 0 50)

  let index_array =
    QCheck.Gen.(
      nonempty_array >>= fun a ->
      int_range 0 (Array.length a - 1) >>= fun i -> pure (a, i))

  let test_empty () =
    let s = Flatset.of_seq Seq.empty in
    let it = Flatset.cursor s in
    Cursor.next it;
    Cursor.seek it 10;
    let b =
      match Cursor.curr it with exception Cursor.End -> true | _ -> false
    in
    A.(check bool) "end cursor" true b

  let test_seek_advance () =
    let s = Flatset.of_seq (List.to_seq [ 1; 3; 5; 9 ]) in
    let it = Flatset.cursor s in
    Cursor.seek it 4;
    A.(check int) "first seek" 5 (fst @@ Cursor.curr it);
    Cursor.seek it 4;
    A.(check int) "second seek" 5 (fst @@ Cursor.curr it)

  let test_random_mem =
    QCheck.Test.make ~count:1000 ~name:"random mem" (QCheck.make index_array)
    @@ fun (a, i) ->
    let seq = Array.to_seq a in
    let s1 = Naive.of_seq seq in
    let s2 = Flatset.of_seq seq in
    Naive.mem a.(i) s1 = Flatset.mem a.(i) s2

  let test_random_cursor_next =
    QCheck.Test.make ~count:1000 ~name:"random cursor next"
      (QCheck.make nonempty_array)
    @@ fun a ->
    let seq = Array.to_seq a in
    let it1 = Naive.cursor @@ Naive.of_seq seq in
    let it2 = Flatset.cursor @@ Flatset.of_seq seq in
    (* Cursor.equal (module C) it1 it2 *)
    let seq1 = Cursor.to_seq it1 in
    let seq2 = Cursor.to_seq it2 in
    let b = Seq.equal (fun (k1, ()) (k2, ()) -> Int.equal k1 k2) seq1 seq2 in
    if not b then
      Fmt.pr "it1 = %a@. it2 = %a@."
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq1
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq2;
    b

  let test_random_cursor_seek =
    QCheck.Test.make ~count:1000 ~name:"random cursor seek"
      (QCheck.make index_array)
    @@ fun (a, i) ->
    let seq = Array.to_seq a in
    let it1 = Naive.cursor @@ Naive.of_seq seq in
    let it2 = Flatset.cursor @@ Flatset.of_seq seq in
    let probe = a.(i) + 5 in
    Cursor.seek it1 probe;
    Cursor.seek it2 probe;
    let v1 = try Some (Cursor.curr it1) with Cursor.End -> None in
    let v2 = try Some (Cursor.curr it2) with Cursor.End -> None in
    Option.equal (fun (k1, ()) (k2, ()) -> Int.equal k1 k2) v1 v2

  let test_random_cursor_union =
    QCheck.Test.make ~count:1000 ~name:"random cursor union"
      QCheck.(make (Gen.list_size (Gen.int_range 1 100) nonempty_array))
    @@ fun l ->
    let it1 =
      let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
      List.fold_left Naive.union Naive.empty l1 |> Naive.cursor
    in
    let it2 =
      let l2 =
        List.map (fun a -> Array.to_seq a |> Flatset.of_seq |> Flatset.cursor) l
      in
      Cursor.union Flatset.cmp l2
    in
    (* Cursor.equal (module C) it1 it2 *)
    let seq1 = Cursor.to_seq it1 in
    let seq2 = Cursor.to_seq it2 in
    let b = Seq.equal (fun (k1, ()) (k2, ()) -> Int.equal k1 k2) seq1 seq2 in
    if not b then
      Fmt.pr "it1 = %a@. it2 = %a@."
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq1
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq2;
    b

  let test_random_cursor_join =
    QCheck.Test.make ~count:1000 ~name:"random cursor join"
      QCheck.(make (Gen.list_size (Gen.int_range 1 3) nonempty_array))
    @@ fun l ->
    let it1 =
      let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
      let x = List.hd l1 in
      List.fold_left Naive.inter x l1 |> Naive.cursor
    in
    let it2 =
      let l2 =
        List.map (fun a -> Array.to_seq a |> Flatset.of_seq |> Flatset.cursor) l
      in
      Cursor.join Flatset.cmp l2
    in
    (* Cursor.equal (module C) it1 it2 *)
    let seq1 = Cursor.to_seq it1 in
    let seq2 = Cursor.to_seq it2 in
    let b = Seq.equal (fun (k1, ()) (k2, ()) -> Int.equal k1 k2) seq1 seq2 in
    if not b then
      Fmt.pr "it1 = %a@. it2 = %a@."
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq1
        Fmt.(seq ~sep:comma @@ pair int nop)
        seq2;
    b

  let all =
    let quick_test s tst = A.test_case s `Quick tst in
    let qcheck_test tst = QCheck_alcotest.to_alcotest tst in
    ( "Flatset tests",
      [
        quick_test "empty" test_empty;
        quick_test "seek advance" test_seek_advance;
        qcheck_test test_random_mem;
        qcheck_test test_random_cursor_next;
        qcheck_test test_random_cursor_seek;
        qcheck_test test_random_cursor_union;
        qcheck_test test_random_cursor_join;
      ] )
end

(* TODO: add tests for Index. *)

let () =
  match Array.to_list Sys.argv with
  | x :: path :: xs ->
      let argv = Array.of_list (x :: xs) in
      A.run ~argv __FILE__
        [ Cursor_tests.all; Trie_tests.generate path; Flatset_tests.all ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
