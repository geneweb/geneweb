open Geneweb
open OUnit2
open Def

let mutil_contains _ =
  let str = "foo bar" in
  let test t b1 = assert_equal b1 (Mutil.contains str t) in
  test "foo" true
; test "baz" false
; test "foo_b" false
; test "foo b" true
; test "foo__b" false
; test "bar__" false
; test "r" true
; test "" true

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

let test_particles =
  [ "da_" ; "dal_" ; "de_la_" ; "de_" ; "del_" ; "della_" ; "des_" ; "du_"
  ; "d'" ; "van_" ; "von_" ]

let mutil_compare_after_particle _ =
  let particles = Mutil.compile_particles test_particles in
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
  test "de montaine" "de la nusset"  ;
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

let name_title _ =
  let test exp = List.iter (fun s -> assert_equal ~printer:(fun s -> s) exp (Name.title s)) in
  test "Jean-Baptiste" [ "jean-baptiste" ; "JEAN-baptiste" ; "Jean-Baptiste" ; "jeaN-baptistE" ]

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

let util_name_with_roman_number _ =
  let test r a =
    let printer = function Some x -> "Some \"" ^ String.escaped x ^ "\"" | None -> "None" in
    assert_equal ~printer r (Util.name_with_roman_number a) in
  test (Some "XXXIX XXXIX") "39 39" ;
  test (Some "XXXIX x XXXIX") "39 x 39" ;
  test (Some "foo CCXLVI") "foo 246" ;
  test (Some "bar CDXXI baz") "bar 421 baz" ;
  test (Some "bar CLX baz CCVII") "bar 160 baz 207" ;
  test None "foo bar baz"

let printer_safe x = (x : Adef.safe_string :> string)
let printer_encoded x = (x : Adef.encoded_string :> string)
let printer_escaped x = (x : Adef.escaped_string :> string)

let util_safe_html _ =
  assert_equal
    ~printer:printer_safe
    (Adef.safe {|<a href="localhost:2318/foo_w?lang=fr&#38;acte=123">foo</a>|})
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&acte=123">foo</a>|}) ;
  assert_equal
    ~printer:printer_safe
    (Adef.safe {|<a href="localhost:2318/foo_w?lang=fr&#38;image=on">foo</a>|})
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&image=on">foo</a>|})

let util_transl_a_of_b _ =
  let conf = Config.empty in
  let conf = {conf with env = ("lang", Adef.encoded "fr") :: conf.env} in
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

let util_escape_html _ =
  assert_equal
    ~printer:printer_escaped
    (Adef.escaped {|&#60;a href=&#34;mailto:jean@dupond.net&#34;&#62;jean@dupond.net&#60;/a&#62; - le 1 &#38;amp; 2|})
    (Util.escape_html {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|})

let datedisplay_string_of_date _ =
  let conf = Config.empty in
  let conf = {conf with env = ("lang", Adef.encoded "co") :: conf.env} in
  let _ = Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i |']%m %y/%d d[i |']%m %y/d[i |']%m %y/in u %y"
  in
  let _ = Hashtbl.add conf.lexicon "(month)"
    "ghjennaghju/ferraghju/marzu/aprile/maghju/ghjugnu/lugliu/aostu/sittembre/uttobre/nuvembre/dicembre"
  in
 let test aaa cal (d, m, y) =
    let date = Dgreg ({day = d; month = m; year = y; prec = Sure; delta = 0}, cal) in
    let bbb = DateDisplay.string_of_date conf date in
    assert_equal aaa bbb
  in
    test (Adef.safe "4 d'aostu 1974") Dgregorian (4, 8, 1974)
  ; test (Adef.safe "4 di sittembre 1974") Dgregorian (4, 9, 1974)
  ; test (Adef.safe "1<sup>u</sup> di ferraghju 1974") Dgregorian (1, 2, 1974)
  ; test (Adef.safe "di marzu 1974") Dgregorian (0, 3, 1974)
  ; test (Adef.safe "d'aprile 1974") Dgregorian (0, 4, 1974)
  ; test (Adef.safe "in u 1974") Dgregorian (0, 0, 1974)
  ; let _ = Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i']%m %y/%d d[i %m %y/d[i |'%m %y/in u %y"
  in
    test (Adef.safe "1<sup>u</sup> d[i']ferraghju 1974") Dgregorian (1, 2, 1974)
  ; test (Adef.safe "d[i |'marzu 1975") Dgregorian (0, 3, 1975)
  ; test (Adef.safe "4 d[i sittembre 1974") Dgregorian (4, 9, 1974)


let name_unaccent _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Name.string_unaccent false b)
  in
  let test_l a b =
    assert_equal ~printer:(fun x -> x) a (Name.string_unaccent true b)
  in
  let test_ls a b =
    assert_equal ~printer:(fun x -> x) a (Name.string_unaccent ~special:true true b)
  in
  let test_nl a b =
    assert_equal ~printer:(fun x -> x) a (Name.lower b)
  in
  test "etienne" "étienne"
  ; test "Etienne" "Étienne"
  ; test "yvette" "ÿvette"
  ; test "Yvette" "Ÿvette"
  ; test "Etienne" "Ĕtienne"
(* apostrophes *)
  ; test    "L'homme" "L'homme"
  ; test_l  "l'homme" "L'homme"
  ; test_ls "l homme" "L'homme"
  ; test_ls "l homme" "L’homme"
  ; test_l  "l‘homme" "L‘homme" (* Unidecode does not handle ‘ *)
  ; test_ls "l homme" "L‘homme" 
  ; test_nl "l homme" "L‘homme" (* Name.lower replaces special chars by a space *)
  ; test_nl "l.homme" "L.homme" (* | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' *)
  ; test_nl "l homme" "L$homme"
(* unaccent performs cyrillic to latin translation! *)
  ; test   "Genri" "Генри"
  ; test_l "genri" "ГЕНРИ"
(* Latin supplemental, vietnameese 
  ; test    "Mien Dinh Nguyen Phuc" "Miên Định Nguyễn Phúc"
  ; test_l  "mien dinh nguyen phuc" "Miên Định Nguyễn Phúc"
  ; test_nl "mien dinh nguyen phuc" "Miên Định Nguyễn Phúc"
  
  ; test "aaaaaaaeceeeeiiiinooooouuuuyy"
         "àáâãäåæçèéêëìíîïñòóôõöùúûüýÿ"
  ; test "llnnnnooooerrrsssstttuuuuuuwyyzzz"
         "ŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżž"
  ; test_nl "abcdefghijklmnopqrstuvwxyz"
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
(  * Latin-1 supplement. C3 80 - "àáâãäåæçèéêëìíîïñòóôõöùúûüýÿ" *  )
  ; test    "AAAAAAAECEEEEIIIINOOOOOUUUUYY"
            "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
  ; test_l  "aaaaaaaeceeeeiiiinooooouuuuyy"
            "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
  ; test_nl "aaaaaaaeceeeeiiiinooooouuuuyy"
            "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
(  * Latin-1 supplement. C4 80 - "āăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĺļľ" *  )
  ; test_nl "aaaccccddeeeeegggghhiiiiiijjklll"
            "ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽ"
(  * Latin-1 supplement. C5 80 - "ŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżž" *  )
  ; test_nl "llnnnnooooerrrsssstttuuuuuuwyyzzz"
            "ĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"

(  * Latin-1 supplement. Greek, CE 80 - "αβγδεζηθικλμνξοπρςστυφχψωάέήίΰϊϋόύώ" *  )
(  *     "abgde dz e th iklmnxopr ss tu ph kh ps o aeniaiyoyo" *  )
(  *     "ΑΒΓΔΕ Ζ  Η Θ  ΙΚΛΜΝΞΟΠΡ ςΣ ΤΥ Φ  Χ  Ψ  Ω ΆΈήΊΰΪΫΌΎΏ" *  )
  ; test_nl "abgdedzethiklmnxoprsstuphkhpsoaeniaiyoyo"
            "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡςΣΤΥΦΧΨΩΆΈήΊΰΪΫΌΎΏ"

*)

let suite =
  [ "Mutil" >:::
    [ "mutil_contains" >:: mutil_contains
    ; "mutil_start_with" >:: mutil_start_with
    ; "mutil_arabian_romian" >:: mutil_arabian_romian
    ; "mutil_compare_after_particle" >:: mutil_compare_after_particle
    ; "mutil_string_of_int_sep" >:: mutil_string_of_int_sep
    ]
  ; "Name" >:::
    [ "name_title" >:: name_title
    ]
  ; "Utf8" >:::
    [ "utf8_sub" >:: utf8_sub
    ]
  ; "Util" >:::
    [ "datedisplay_string_of_date" >:: datedisplay_string_of_date
    ; "util_name_with_roman_number" >:: util_name_with_roman_number
    ; "util_escape_html" >:: util_escape_html
    ; "util_safe_html" >:: util_safe_html
    ; "util_string_with_macros" >:: util_string_with_macros
    ; "util_transl_a_of_b" >:: util_transl_a_of_b
    ; "name_unaccent" >:: name_unaccent
    ]
  ]
