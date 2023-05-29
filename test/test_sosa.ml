open OUnit2
open Geneweb

let suite =
  [
    "Sosa"
    >::: [
           ( "eq" >:: fun _ ->
             assert_equal true (Sosa.eq Sosa.zero Sosa.zero);
             assert_equal false (Sosa.eq Sosa.zero Sosa.one);
             assert_equal true (Sosa.eq Sosa.one Sosa.one);
             assert_equal false (Sosa.eq Sosa.one Sosa.zero) );
           ( "int conversion" >:: fun _ ->
             assert_equal ~cmp:Sosa.eq Sosa.zero (Sosa.of_int 0);
             assert_equal ~cmp:Sosa.eq Sosa.one (Sosa.of_int 1) );
           ( "string conversion" >:: fun _ ->
             assert_equal ~cmp:Sosa.eq Sosa.zero (Sosa.of_string "0");
             assert_equal ~cmp:Sosa.eq Sosa.one (Sosa.of_string "1");
             assert_equal "0" (Sosa.to_string Sosa.zero);
             assert_equal "1" (Sosa.to_string Sosa.one) );
           ( "pretty print" >:: fun _ ->
             let test a b =
               assert_equal
                 ~printer:(fun x -> x)
                 a
                 (Sosa.to_string_sep "," (Sosa.of_int b))
             in
             test "1" 1;
             test "10" 10;
             test "100" 100;
             test "1,000" 1000;
             test "10,000" 10000;
             test "100,000" 100000;
             test "1,000,000" 1000000 );
           ( "gen" >:: fun _ ->
             let test exp x =
               assert_equal ~printer:string_of_int exp
               @@ Sosa.gen (Sosa.of_int x)
             in
             test 1 1;
             test 2 2;
             test 2 3;
             test 3 4;
             test 3 5;
             test 3 6;
             test 3 7;
             test 4 8;
             test 4 9;
             test 4 10;
             test 4 11;
             test 4 12;
             test 4 13;
             test 4 14;
             test 4 15 );
           ( "branches" >:: fun _ ->
             assert_equal
               ~printer:(fun list ->
                 "[" ^ String.concat ";" (List.map string_of_int list) ^ "]")
               [ 0; 0; 1; 1; 0 ]
               (Sosa.branches @@ Sosa.of_int 38) );
         ];
  ]
