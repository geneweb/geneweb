(* TODO Fmt *)
let testable_sosa = Alcotest.testable Fmt.nop Sosa.eq

let sosa_eq () =
  (Alcotest.check testable_sosa) "0 = 0" Sosa.zero Sosa.zero;
  (Alcotest.check testable_sosa) "1 = 1" Sosa.one Sosa.one;
  (Alcotest.check @@ Alcotest.neg @@ testable_sosa) "0 <> 1" Sosa.zero Sosa.one;
  (Alcotest.check @@ Alcotest.neg @@ testable_sosa) "1 <> 0" Sosa.one Sosa.zero;
  ()

let sosa_int () =
  (Alcotest.check testable_sosa) "of_int 0" Sosa.zero (Sosa.of_int 0);
  (Alcotest.check testable_sosa) "of_int 1" Sosa.one (Sosa.of_int 1);
  ()

let sosa_string () =
  (Alcotest.check testable_sosa)
    {|of_string "0"|} Sosa.zero (Sosa.of_string "0");
  (Alcotest.check testable_sosa) {|of_string "1"|} Sosa.one (Sosa.of_string "1");
  (Alcotest.check Alcotest.string)
    "to_string zero" "0" (Sosa.to_string Sosa.zero);
  (Alcotest.check Alcotest.string) "to_string one" "1" (Sosa.to_string Sosa.one);
  (Alcotest.check_raises "of_string (-1)") (Failure "Sosa.of_string") (fun () ->
      ignore (Sosa.of_string "-1"));
  (Alcotest.check_raises "inc Sosa.zero (-1)") (Invalid_argument "Sosa.of_int")
    (fun () -> ignore (Sosa.inc Sosa.zero (-1)));
  (Alcotest.check_raises "sub Sosa.zero Sosa.one") (Invalid_argument "Sosa.sub")
    (fun () -> ignore (Sosa.sub Sosa.zero Sosa.one));
  (Alcotest.check_raises "mul Sosa.one (-1)") (Invalid_argument "Sosa.of_int")
    (fun () -> ignore (Sosa.mul Sosa.one (-1)));
  (Alcotest.check_raises "div Sosa.one (-1)") (Invalid_argument "Sosa.of_int")
    (fun () -> ignore (Sosa.div Sosa.one (-1)));
  ()

let sosa_pp () =
  let ints = [ 1; 10; 100; 1000; 10000; 100000; 1000000 ] in
  let strings =
    [ "1"; "10"; "100"; "1,000"; "10,000"; "100,000"; "1,000,000" ]
  in
  let l = List.combine ints strings in
  List.iter
    (fun (i, s) ->
      (Alcotest.check Alcotest.string)
        "" s
        (Sosa.to_string_sep "," (Sosa.of_int i)))
    l

let sosa_gen () =
  let sosas = List.init 15 (fun i -> i + 1) in
  let generations = [ 1; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 4; 4; 4; 4 ] in
  let l = List.combine sosas generations in
  List.iter
    (fun (i, gen) ->
      (Alcotest.check Alcotest.int) "" gen (Sosa.gen (Sosa.of_int i)))
    l

let sosa_branches () =
  let l = [ 0; 0; 1; 1; 0 ] in
  (Alcotest.check (Alcotest.list Alcotest.int))
    "branch 38" l
    (Sosa.branches @@ Sosa.of_int 38)

let v =
  [
    ("sosa-eq", [ Alcotest.test_case "Sosa equality" `Quick sosa_eq ]);
    ("sosa-int", [ Alcotest.test_case "Sosa <-> int" `Quick sosa_int ]);
    ("sosa-string", [ Alcotest.test_case "Sosa <-> string" `Quick sosa_string ]);
    ("sosa-pp", [ Alcotest.test_case "Sosa pretty print" `Quick sosa_pp ]);
    ("sosa-gen", [ Alcotest.test_case "Sosa generation" `Quick sosa_gen ]);
    ( "sosa-branches",
      [ Alcotest.test_case "Sosa branches" `Quick sosa_branches ] );
  ]
