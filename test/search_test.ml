module A = Alcotest
module Index = Geneweb_search.Index.Default
module Trie = Geneweb_search.Trie.Default
module Word = Geneweb_search.Word.Default
module Analyze = Geneweb_search.Analyze

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

(* TODO: add tests for Index and flat sets. *)

let () =
  match Array.to_list Sys.argv with
  | x :: path :: xs ->
      let argv = Array.of_list (x :: xs) in
      A.run ~argv __FILE__
        [ Trie_tests.generate path ]
  | _ -> failwith "expected a dictionary file in txt format as first argument"
