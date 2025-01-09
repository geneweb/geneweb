module A = Alcotest
module Index = Geneweb_search.Index.Default
module Trie = Geneweb_search.Trie.Default
module Word = Geneweb_search.Word.Default
module Analyze = Geneweb_search.Analyze
module Iterator = Geneweb_search.Iterator

(* Compute the Levenshtein distance of [s1] and [s2]. *)
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
  prev.(l2)

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
      match In_channel.input_line ic with
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
  module C = Flatset.Comparator

  module Naive = struct
    type t = int array

    let of_seq = Array.of_seq
    let to_seq = Array.to_seq
    let mem = Array.mem
    let cardinal = List.length
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

    let iterator t =
      let st = ref (to_seq t) in
      let curr () =
        match !st () with
        | Seq.Nil -> raise Iterator.End
        | Seq.Cons (hd, _) -> hd
      in
      let next () =
        match !st () with Seq.Nil -> () | Seq.Cons (_, tl) -> st := tl
      in
      let seek w =
        let rec loop () =
          match curr () with
          | exception Iterator.End -> ()
          | v when w <= v -> ()
          | _ ->
              next ();
              loop ()
        in
        loop ()
      in
      Iterator.make (module C) ~curr ~next ~seek
  end

  let nonempty_array = QCheck.Gen.(array_size (int_range 1 100) int)

  let index_array =
    QCheck.Gen.(
      nonempty_array >>= fun a ->
      int_range 0 (Array.length a - 1) >>= fun i -> pure (a, i))

  let test_empty () =
    let s = Flatset.of_seq Seq.empty in
    let it = Flatset.iterator s in
    Iterator.next it;
    Iterator.seek it 10;
    let b =
      match Iterator.curr it with exception Iterator.End -> true | _ -> false
    in
    A.(check bool) "end iterator" true b

  let test_seek_advance () =
    let s = Flatset.of_seq (List.to_seq [ 1; 3; 5; 9 ]) in
    let it = Flatset.iterator s in
    Iterator.seek it 4;
    A.(check int) "first seek" 5 (Iterator.curr it);
    Iterator.seek it 4;
    A.(check int) "second seek" 5 (Iterator.curr it)

  let test_random_mem =
    QCheck.Test.make ~count:1000 ~name:"random mem" (QCheck.make index_array)
    @@ fun (a, i) ->
    let seq = Array.to_seq a in
    let s1 = Naive.of_seq seq in
    let s2 = Flatset.of_seq seq in
    Naive.mem a.(i) s1 = Flatset.mem a.(i) s2

  let test_random_iterator_next =
    QCheck.Test.make ~count:1000 ~name:"random iterator next"
      (QCheck.make nonempty_array)
    @@ fun a ->
    let seq = Array.to_seq a in
    let it1 = Naive.iterator @@ Naive.of_seq seq in
    let it2 = Flatset.iterator @@ Flatset.of_seq seq in
    Iterator.equal (module C) it1 it2

  let test_random_iterator_seek =
    QCheck.Test.make ~count:1000 ~name:"random iterator seek"
      (QCheck.make index_array)
    @@ fun (a, i) ->
    let seq = Array.to_seq a in
    let it1 = Naive.iterator @@ Naive.of_seq seq in
    let it2 = Flatset.iterator @@ Flatset.of_seq seq in
    let probe = a.(i) + 5 in
    Iterator.seek it1 probe;
    Iterator.seek it2 probe;
    let v1 = try Some (Iterator.curr it1) with Iterator.End -> None in
    let v2 = try Some (Iterator.curr it2) with Iterator.End -> None in
    Option.equal Int.equal v1 v2

  let test_random_iterator_union =
    QCheck.Test.make ~count:1000 ~name:"random iterator union"
      QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
    @@ fun l ->
    let it1 =
      let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
      List.fold_left Naive.union Naive.empty l1 |> Naive.iterator
    in
    let it2 =
      let l2 =
        List.map
          (fun a -> Array.to_seq a |> Flatset.of_seq |> Flatset.iterator)
          l
      in
      Iterator.union (module C) l2
    in
    Iterator.equal (module C) it1 it2

  let test_random_iterator_join =
    QCheck.Test.make ~count:1000 ~name:"random iterator join"
      QCheck.(make Gen.(list_size (int_range 1 100) nonempty_array))
    @@ fun l ->
    let it1 =
      let l1 = List.map (fun a -> Array.to_seq a |> Naive.of_seq) l in
      let x = List.hd l1 in
      List.fold_left Naive.inter x l1 |> Naive.iterator
    in
    let it2 =
      let l2 =
        List.map
          (fun a -> Array.to_seq a |> Flatset.of_seq |> Flatset.iterator)
          l
      in
      Iterator.join (module C) l2
    in
    Iterator.equal (module C) it1 it2

  let all =
    let quick_test s tst = A.test_case s `Quick tst in
    let qcheck_test tst = QCheck_alcotest.to_alcotest tst in
    ( "flatset tests",
      [
        quick_test "empty" test_empty;
        quick_test "seek advance" test_seek_advance;
        qcheck_test test_random_mem;
        qcheck_test test_random_iterator_next;
        qcheck_test test_random_iterator_seek;
        qcheck_test test_random_iterator_union;
        qcheck_test test_random_iterator_join;
      ] )
end

(* TODO: add tests for Index. *)

let () =
  match Array.to_list Sys.argv with
  | x :: path :: xs ->
      let argv = Array.of_list (x :: xs) in
      A.run ~argv __FILE__ [ Trie_tests.generate path; Flatset_tests.all ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
