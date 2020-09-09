open Geneweb
open OUnit2
open Config
open Def

let mutil_contains _ =
  let str = "foo bar" in
  let test t b1 b2 =
    assert_equal b1 (Mutil.contains ~wildcard:false str t)
  ; assert_equal b2 (Mutil.contains ~wildcard:true str t)
  in
  test "foo" true true
; test "baz" false false
; test "foo_b" false true
; test "foo b" true true
; test "foo__b" false false
; test "bar__" false true
; test "r" true true
; test "" true true

let mutil_start_with _ =
  assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" (-1) "foo")
; assert_raises (Invalid_argument "start_with")
    (fun () -> Mutil.start_with "foo" 4 "foo")
; assert_bool "Mutil.start_with \"foo\" 0 \"foo\""
    (Mutil.start_with "foo" 0 "foo")
; assert_bool "not (Mutil.start_with \"bar\" 0 \"foo\")"
    (not @@ Mutil.start_with "bar" 0 "foo")
; assert_bool "Mutil.start_with \"\" 0 \"foo\""
    (Mutil.start_with "" 0 "foo")

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

let mutil_string_of_int_sep _ =
  let test sep exp int =
    assert_equal ~printer:(fun s -> s) exp (Mutil.string_of_int_sep sep int)
  in
  test "," "1" 1 ;
  test "," "10" 10 ;
  test "," "100" 100 ;
  test "," "1,000" 1000 ;
  test "," "10,000" 10000 ;
  test "," "100,000" 100000 ;
  test "," "1,000,000" 1000000

let utf8_sub _ =
  let test ?pad e s i j =
    let i = Utf8.get s i in
    assert_equal e ~printer:(fun x -> x) (Utf8.sub ?pad s i j)
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

let util_transl_a_of_b _ =
  let conf = Config.empty in
  let conf = {conf with env = ("lang", "fr") :: conf.env} in
  let _ = Hashtbl.add conf.lexicon "%1 of %2" "%1 d[e |']%2" in
  let test aaa (s1, s2, s2_raw) =
    let bbb = Util.transl_a_of_b conf s1 s2 s2_raw in
    assert_equal aaa bbb
  in
    test "naissance de <b>Jean</b>" ("naissance", "<b>Jean</b>", "Jean")
  ; test "naissance d'<b>André</b>" ("naissance", "<b>André</b>", "André")

let util_string_with_macros _ =
  let conf = Config.empty in
  assert_equal
    ~printer:(fun s -> s)
    {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|}
    (Util.string_with_macros conf [] {|jean@dupond.net - le 1 &amp; 2|})

let util_no_html_tags _ =
  assert_equal
    ~printer:(fun s -> s)
    {|&lt;a href="mailto:jean@dupond.net"&gt;jean@dupond.net&lt;/a&gt; - le 1 &amp; 2|}
    (Util.no_html_tags {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|})

let datedisplay_string_of_date _ =
  let conf = Config.empty in
  let conf = {conf with env = ("lang", "co") :: conf.env} in
  let _ = Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i |']%m %y/%d d[i |']%m %y/d[i |']%m %y/in u %y"
  in
  let _ = Hashtbl.add conf.lexicon "(month)"
    "ghjennaghju/ferraghju/marzu/aprile/maghju/ghjugnu/lugliu/aostu/sittembre/uttobre/nuvembre/dicembre"
  in
 let test aaa cal (d, m, y) =
    let date = Dgreg ({day = d; month = m; year = y; prec = Sure; delta = 0}, cal) in
    let bbb = DateDisplay.string_of_date conf date in
    let _ = if not (aaa = bbb) then Printf.eprintf "\n%s\n" bbb else () in
    assert_equal aaa bbb
  in
    test "4 d'aostu 1974" Dgregorian (4, 8, 1974)
  ; test "4 di sittembre 1974" Dgregorian (4, 9, 1974)
  ; test "1<sup>u</sup> di ferraghju 1974" Dgregorian (1, 2, 1974)
  ; test "di marzu 1974" Dgregorian (0, 3, 1974)
  ; test "d'aprile 1974" Dgregorian (0, 4, 1974)
  ; test "in u 1974" Dgregorian (0, 0, 1974)
  ; let _ = Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i']%m %y/%d d[i %m %y/d[i |'%m %y/in u %y"
  in
    test "1<sup>u</sup> d[i']ferraghju 1974" Dgregorian (1, 2, 1974)
  ; test "d[i |'marzu 1975" Dgregorian (0, 3, 1975)
  ; test "4 d[i sittembre 1974" Dgregorian (4, 9, 1974)

let suite =
  [ "Mutil" >:::
    [ "mutil_contains" >:: mutil_contains
    ; "mutil_start_with" >:: mutil_start_with
    ; "mutil_arabian_romian" >:: mutil_arabian_romian
    ; "mutil_compare_after_particle" >:: mutil_compare_after_particle
    ; "mutil_string_of_int_sep" >:: mutil_string_of_int_sep
    ]
  ; "Utf8" >:::
    [ "utf8_sub" >:: utf8_sub
    ]
  ; "Util" >:::
    [ "util_safe_html" >:: util_safe_html
    ; "util_transl_a_of_b" >:: util_transl_a_of_b
    ; "util_string_with_macros" >:: util_string_with_macros
    ; "datedisplay_string_of_date" >:: datedisplay_string_of_date
    ; "util_no_html_tags" >:: util_no_html_tags
    ]
  ]
