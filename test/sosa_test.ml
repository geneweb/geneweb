open Alcotest
module Sosa = Geneweb_sosa

(* TODO Fmt *)
let testable_sosa = testable Fmt.nop Sosa.eq

let sosa_eq () =
  (check testable_sosa) "0 = 0" Sosa.zero Sosa.zero;
  (check testable_sosa) "1 = 1" Sosa.one Sosa.one;
  (check @@ neg @@ testable_sosa) "0 <> 1" Sosa.zero Sosa.one;
  (check @@ neg @@ testable_sosa) "1 <> 0" Sosa.one Sosa.zero;
  ()

let sosa_int () =
  (check testable_sosa) "of_int 0" Sosa.zero (Sosa.of_int 0);
  (check testable_sosa) "of_int 1" Sosa.one (Sosa.of_int 1);
  ()

let sosa_string () =
  (check testable_sosa) {|of_string "0"|} Sosa.zero (Sosa.of_string "0");
  (check testable_sosa) {|of_string "1"|} Sosa.one (Sosa.of_string "1");
  (check string) "to_string zero" "0" (Sosa.to_string Sosa.zero);
  (check string) "to_string one" "1" (Sosa.to_string Sosa.one);
  (check string) "test sosa 1" "1"
    (Sosa.to_string (Sosa.div (Sosa.of_int 1000) 1000));
  (check string) "test sosa 2" "2"
    (Sosa.to_string (Sosa.div (Sosa.of_int 2000) 1000));
  (check string) "test sosa div" "234"
    (Sosa.to_string (Sosa.div (Sosa.of_int 234000) 1000));
  (*   %let;tmp;%expr(xxx-((xxx/1000)*1000))%in;  *)
  (check string) "test sosa sub/div" "234"
    (Sosa.to_string
       (Sosa.sub (Sosa.of_int 1234)
          (Sosa.mul (Sosa.div (Sosa.of_int 1234) 1000) 1000)));
  (check string) "test sosa div/10/10/10" "234"
    (Sosa.to_string
       (Sosa.sub (Sosa.of_int 1234)
          (Sosa.mul
             (Sosa.div (Sosa.div (Sosa.div (Sosa.of_int 1234) 10) 10) 10)
             1000)));
  ()

let sosa_pp () =
  let ints = [ 1; 10; 100; 1000; 10000; 100000; 1000000 ] in
  let strings =
    [ "1"; "10"; "100"; "1,000"; "10,000"; "100,000"; "1,000,000" ]
  in
  let l = List.combine ints strings in
  List.iter
    (fun (i, s) -> (check string) "" s (Sosa.to_string_sep "," (Sosa.of_int i)))
    l

let sosa_gen () =
  let sosas = List.init 15 (fun i -> i + 1) in
  let generations = [ 1; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 4; 4; 4; 4 ] in
  let l = List.combine sosas generations in
  List.iter (fun (i, gen) -> (check int) "" gen (Sosa.gen (Sosa.of_int i))) l

let sosa_branches () =
  let l = [ 0; 0; 1; 1; 0 ] in
  (check (list int)) "branch 38" l (Sosa.branches @@ Sosa.of_int 38)

let v =
  [
    ("sosa-eq", [ test_case "Sosa equality" `Quick sosa_eq ]);
    ("sosa-int", [ test_case "Sosa <-> int" `Quick sosa_int ]);
    ("sosa-string", [ test_case "Sosa <-> string" `Quick sosa_string ]);
    ("sosa-pp", [ test_case "Sosa pretty print" `Quick sosa_pp ]);
    ("sosa-gen", [ test_case "Sosa generation" `Quick sosa_gen ]);
    ("sosa-branches", [ test_case "Sosa branches" `Quick sosa_branches ]);
  ]
