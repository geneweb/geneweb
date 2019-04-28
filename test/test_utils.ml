open Geneweb
open OUnit2

let name_unaccent _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Some.name_unaccent b)
  in
  test "etienne" "étienne"
  ; test "Etienne" "Étienne"
  ; test "yvette" "ÿvette"
  ; test "Yvette" "Ÿvette"
  ; test "Etienne" "Ĕtienne"
  (* unaccent performs cyrillic to latin translation! *)
  ; test "Genri" "Генри"

(* Name.lower performs lower and unaccent *)
let name_lower _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Name.lower b)
  in
  test "abcdef" "ABCdEF"
  ; test "abcdef" "ÂBÇdĘF"
  ; test "etienne" "Ĕtienne"
  ; test "andre" "André"
  ; test "andrea" "Andréá"
  ; test "ellenika" "ελληνικά"
  ; test "ellnhiya" "ΈΛΛΝΉΊΎΆ"
  ; test "yvette" "Ÿvette"
  (* cyrillic to latin and lower *)
  ; test "genri" "Генри"
  ; test "genri" "ГЕНРИ"
  (* Latin supplemental, vietnameese *)
  ; test "mien dinh nguyen phuc" "Miên Định Nguyễn Phúc"

(*
    %1 of %2
de: %1 von :d:%2
fr: %1 d[e |’]%2
*)
(*
let util_decline _ =
  let test s wt s1 s2 s2_raw =
    assert_equal ~printer:(fun x -> x) s (Util.gen_decline wt s1 s2 s2_raw)
  in
  test "abc xxx def yyy ghi" "abc %1 def %2 ghi" "xxx" "<xyz>yyy" "yyy"
  test "abc xxx def yyy ghi" "abc %1 def %2 ghi" "xxx" "<xyz>yyy" "yyy"
*)

let util_capitale_full _ =
  let array_to_string c =
    Array.fold_left
      (fun acc chr -> acc ^ (String.make 1 (Char.chr (chr)))) "" c 
  in
  let test_n_bytes c =
  let n = Array.length c in
    Printf.printf "\n\nLine: %X;" (Array.get c 0) ;
    if n > 1 then
      let c1 = Array.get c 1 in
      let s1 = Printf.sprintf " %X" c1 in
      Printf.printf "%s;" (if c1 = 0 then " xx [80-BF]" else s1) else ();
    if n > 2 then 
      let c1 = Array.get c 2 in
      let s1 = Printf.sprintf " %X" c1 in
      Printf.printf "%s;" (if c1 = 0 then " xx [80-BF]" else s1) else ();
    Printf.printf "\n           " ;
    Printf.printf "80                  8A          " ;
    Printf.printf "90                  9A          " ;
    Printf.printf "A0                  AA          " ;
    Printf.printf "B0                  BA          " ;
    Printf.printf "\n standard: " ;
    let rec loop1 i =
      Array.set c (n-1) i ;
      let s1 = array_to_string c in
      Printf.printf "%s " s1 ;
      if i < 0xBF then loop1 (i + 1)
    in loop1 0x80 ;
    Printf.printf "\nuppercase: ";
    let rec loop1 i =
      Array.set c (n-1) i ;
      let s2 = array_to_string c in
      let s2 = Util.capitale s2 in
      Printf.printf "%s " s2 ;
      if i < 0xBF then loop1 (i + 1)
    in loop1 0x80 ;
  "done"
  in
  let test c =
    assert_equal "done" (test_n_bytes c)
  in
  test [|0xC3; 0|]
  ; test [|0xC4; 0|]
  ; test [|0xC5; 0|]
  ; test [|0xC6; 0|]
  ; test [|0xC7; 0|]
  ; test [|0xC8; 0|]
  ; test [|0xCE; 0|]
  ; test [|0xCF; 0|]
  ; test [|0xD0; 0|]
  ; test [|0xD1; 0|]
  ; test [|0xD2; 0|]
  ; test [|0xE1; 0xB8; 0|]
  ; test [|0xE1; 0xB9; 0|]
  ; test [|0xE1; 0xBA; 0|]
  ; test [|0xE1; 0xBB; 0|]
  ; test [|0xE1; 0xBC; 0|]
  
let util_capitale _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Util.capitale b)
  in
  test "Abc" "abc"
  ; test "Abc" "Abc"
  ; test "Étienne" "étienne"
  ; test "Étienne" "Étienne"
  ; test "Étienne" "Étienne"
  ; test "Ńoemie" "ńoemie"
  ; test "Ńoemie" "Ńoemie"
  ; test "Ĕtienne" "ĕtienne"
  ; test "Ĕtienne" "Ĕtienne"
  ; test "Ÿvette" "ÿvette"
  ; test "Ÿvette" "Ÿvette"
  (* greek *)
  ; test "Δ λληνικά" "δ λληνικά"
  ; test "Δ λληνικά" "Δ λληνικά"
  ; test "Ελληνικά" "ελληνικά"
  ; test "Ελληνικά" "Ελληνικά"
  ; test "Ζ λληνικά" "ζ λληνικά"
  ; test "Ζ λληνικά" "Ζ λληνικά"

  ; test "Ϊ λληνικ" "ϊ λληνικ"
  ; test "Ϊ λληνικ" "Ϊ λληνικ"
  ; test "Ϋ λληνικ" "ϋ λληνικ"
  ; test "Ϋ λληνικ" "Ϋ λληνικ"
  ; test "Ό λληνικ" "ό λληνικ"
  ; test "Ό λληνικ" "Ό λληνικ"
  ; test "Ύ λληνικ" "ύ λληνικ"
  ; test "Ύ λληνικ" "Ύ λληνικ"
  ; test "Ώ λληνικ" "ώ λληνικ"
  ; test "Ώ λληνικ" "Ώ λληνικ"

  ; test "Έλληνικά" "έλληνικά"
  ; test "Έλληνικά" "Έλληνικά"
  ; test "Π λληνικ" "π λληνικ"
  ; test "Π λληνικ" "Π λληνικ"
  ; test "Ω λληνικ" "ω λληνικ"
  ; test "Ω λληνικ" "Ω λληνικ"
  (* cyrillic *)
  ; test "Генри" "генри"
  ; test "Генри" "Генри"
  ; test "Б енри" "б енри"
  ; test "Б енри" "Б енри"
  ; test "Я енри" "я енри"
  ; test "Я енри" "Я енри"
  ; test "Ѿ енри" "ѿ енри"
  ; test "Ѿ енри" "Ѿ енри"
  ; test "Ѡ енри" "ѡ енри"
  ; test "Ѡ енри" "Ѡ енри"
  ; test "Ҁ енри" "ҁ енри"
  ; test "Ҁ енри" "Ҁ енри"
  (* Latin extended, Vietnameese *)
  ; test "Nguyễn" "nguyễn"
  ; test "Nguyễn" "Nguyễn"
  ; test "Đình" "đình"
  ; test "Đình" "Đình"
  ; test "Ḕtienne" "ḕtienne"
  ; test "Ḕtienne" "Ḕtienne"
(*
    %1 of %2
de: %1 von :d:%2
fr: %1 d[e |’]%2
*)
(*
let mutil_nominative _ =
  let test s1 s2 =
    assert_equal ~printer:(fun x -> x) s1 (Mutil.nominative s2)
  in
  test "abcdef" "abc:d:def"
  test "abcdef" "abc d[def|ghi]i"
*)

let mutil_contains _ =
  let str = "foo bar Ĕtienne Έλληνικά Генри bar" in
  let test t b1 b2 =
    assert_equal b1 (Mutil.contains ~wildcard:false str t)
  ; assert_equal b2 (Mutil.contains ~wildcard:true str t)
  in
  test "foo" true true
; test "foo bar" true true
; test "baz" false false
; test "foo_b" false true
; test "foo b" true true
; test "foo__b" false false
; test "bar__" false true
; test "r" true true
; test "" true true
; test "Ĕtienne" true true
; test "Έλληνικά" true true
; test "Генри" true true

(* in this test, second argument is the byte number, not the char number 
   at which to start the test.
   Transforming chars into bytes at the beginning of start_with breaks
   the behaviour of contains! TODO fix this!!
*)

let mutil_start_with _ =
  assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" (-1) "foo")
; assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" 4 "foo")
; assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" 4 "Ĕoo") 
; assert_bool "Mutil.start_with \"foo\" 0 \"foo\""
    (Mutil.start_with "foo" 0 "foo")
; assert_bool "not (Mutil.start_with \"bar\" 0 \"foo\")"
    (not @@ Mutil.start_with "bar" 0 "foo")
; assert_bool "not (Mutil.start_with \"bar\" 3 \"foo\")"
    (not @@ Mutil.start_with "bar" 3 "foo")
; assert_bool "Mutil.start_with \"\" 0 \"foo\""
    (Mutil.start_with "" 0 "foo")
; assert_bool "Mutil.start_with \"Ĕtien\" 0 \"Ĕtienne\""
    (Mutil.start_with "Ĕtien" 0 "Ĕtienne")
; assert_bool "not (Mutil.start_with \"Ĕtien\" 5 \"Ĕtienne\")"
    (not @@ Mutil.start_with "Ĕtien" 5 "Ĕtienne")
; assert_bool "not (Mutil.start_with \"Ĕtien\" 7 \"Ĕtienne\")"
    (not @@ Mutil.start_with "Ĕtien" 7 "Ĕtienne")
; assert_bool "Mutil.start_with \"Ĕtien\" 1 \"aĔtienne\""
    (Mutil.start_with "Ĕtien" 1 "aĔtienne")
; assert_bool "Mutil.start_with \"Ĕtien\" 2 \"ĔaĔtienne\""
    (Mutil.start_with "Ĕtien" 2 "ĔaĔtienne")

let mutil_arabian_romian _ =
  let test a r =
    assert_equal a (Mutil.arabian_of_roman r) ;
    assert_equal r (Mutil.roman_of_arabian a)
  in
  test 39 "XXXIX" ;
  test 246 "CCXLVI" ;
  test 421 "CDXXI" ;
  test 160 "CLX" ;
  test 207 "CCVII" ;
  test 1066 "MLXVI"

let mutil_compare_after_particle _ =
  let particles =
    [ "da " ; "dal " ; "de la " ; "de " ; "del " ; "della " ; "des " ; "du "
    ; "d'" ; "van " ; "von " ]
  in
  let test a b =
    let test exp a b =
      let cmp = Mutil.compare_after_particle particles in
      let assert_equal =
        assert_equal ~printer:(fun i -> Printf.sprintf "%i (%s / %s)" i a b)
      in
      assert_equal exp (cmp a b)
    in
    test (-1) a b ;
    test 1 b a ;
    test 0 a a ;
    test 0 b b
  in
  test "de la fontaine" "de musset" ;
  test "de sade" "de sévigné" ;
  test "de lattre de tassigny" "de montgolfier" ;
  test "des cars" "du guesclin" ;
  test "d'aboville" "d'artagnan" ;
  test "descartes" "dupont"

let util_str_sub _ =
  let test ?pad e s i j =
    let i = Util.str_nth_pos s i in
    assert_equal e ~printer:(fun x -> x) (Util.str_sub ?pad s i j)
  in
  test "日" "日本語" 0 1 ;
  test "日本語" "日本語" 0 3 ;
  test "語" "日本語" 2 1 ;
  test "ε" "ελληνικά" 0 1 ;
  test "ελληνικά" "ελληνικά" 0 8 ;
  test "λ" "ελληνικά" 1 1 ;
  test "ά" "ελληνικά" 7 1 ;
  test "š" "švédčina" 0 1 ;
  test "švédčina" "švédčina" 0 8 ;
  test "a" "švédčina" 7 1

let util_safe_html _ =
  assert_equal
    ~printer:(fun x -> x)
    {|<a href="localhost:2318/foo_w?lang=fr&#38;acte=123">foo</a>|}
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&acte=123">foo</a>|}) ;
  assert_equal
    ~printer:(fun x -> x)
    {|<a href="localhost:2318/foo_w?lang=fr&#38;image=on">foo</a>|}
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&image=on">foo</a>|})

let suite =
  [ "Mutil" >:::
    [ "mutil_contains" >:: mutil_contains
    (*; "mutil_nominative" >:: mutil_nominative *)
    ; "mutil_start_with" >:: mutil_start_with
    ; "mutil_arabian_romian" >:: mutil_arabian_romian
    ; "mutil_compare_after_particle" >:: mutil_compare_after_particle
    ]
  ; "Util" >:::
    [ "util_str_sub" >:: util_str_sub
    (*; "util_safe_html" >:: util_safe_html *)
    ; "util_capitale" >:: util_capitale
    ; "util_capitale_full" >:: util_capitale_full
    (*; "util_decline" >:: util_decline *)
    ; "name_unaccent" >:: name_unaccent
    ; "name_lower" >:: name_lower
    ]
  ]

