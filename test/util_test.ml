open Alcotest
open Geneweb

let mutil_contains () =
  let str = "foo bar" in
  let str2 = "/notes_d/albums/test-images-alb" in
  let test t b = (check bool) t b (Mutil.contains str t) in
  let test2 t b = (check bool) t b (Mutil.contains str2 t) in
  test "foo" true;
  test "baz" false;
  test "foo_b" false;
  test "foo b" true;
  test "foo__b" false;
  test "bar__" false;
  test "r" true;
  test "" true;
  test2 "albums/test-images-alb" true;
  test2 "albums/test" true

let mutil_start_with () =
  check_raises "" (Invalid_argument "start_with") (fun () ->
      ignore @@ Mutil.start_with "foo" (-1) "foo");
  check_raises "" (Invalid_argument "start_with") (fun () ->
      ignore @@ Mutil.start_with "foo" 4 "foo");
  (check bool) "Mutil.start_with \"foo\" 0 \"foo\"" true
    (Mutil.start_with "foo" 0 "foo");
  (check bool) "not (Mutil.start_with \"bar\" 0 \"foo\")" true
    (not @@ Mutil.start_with "bar" 0 "foo");
  (check bool) "Mutil.start_with \"\" 0 \"foo\"" true
    (Mutil.start_with "" 0 "foo");
  ()

let mutil_arabian_romian _ =
  let test a r =
    (check int) "arabian_of_roman" a (Mutil.arabian_of_roman r);
    (check string) "roman_of_arabian" r (Mutil.roman_of_arabian a)
  in
  test 39 "XXXIX";
  test 246 "CCXLVI";
  test 421 "CDXXI";
  test 160 "CLX";
  test 207 "CCVII";
  test 1066 "MLXVI"

let test_particles =
  [
    "da_";
    "dal_";
    "de_la_";
    "de_";
    "del_";
    "della_";
    "des_";
    "du_";
    "d'";
    "van_";
    "von_";
  ]

let mutil_compare_after_particle _ =
  let particles = Mutil.compile_particles test_particles in
  let test a b =
    let test exp a b =
      let cmp = Mutil.compare_after_particle particles in
      (check int) "" exp (cmp a b)
    in
    test (-1) a b;
    test 1 b a;
    test 0 a a;
    test 0 b b
  in
  test "de la fontaine" "de musset";
  test "de montaine" "de la nusset";
  test "de sade" "de sévigné";
  test "de lattre de tassigny" "de montgolfier";
  test "des cars" "du guesclin";
  test "d'aboville" "d'artagnan";
  test "descartes" "dupont"

let mutil_string_of_int_sep _ =
  let test sep exp int =
    (check string) "" exp (Mutil.string_of_int_sep sep int)
  in
  test "," "1" 1;
  test "," "10" 10;
  test "," "100" 100;
  test "," "1,000" 1000;
  test "," "10,000" 10000;
  test "," "100,000" 100000;
  test "," "1,000,000" 1000000

let name_title _ =
  let test exp = List.iter (fun s -> (check string) "" exp (Name.title s)) in
  test "Jean-Baptiste"
    [ "jean-baptiste"; "JEAN-baptiste"; "Jean-Baptiste"; "jeaN-baptistE" ]

let utf8_sub _ =
  let test ?pad e s i j =
    let i = Utf8.get s i in
    (check string) "" e (Utf8.sub ?pad s i j)
  in
  test "日" "日本語" 0 1;
  test "日本語" "日本語" 0 3;
  test "語" "日本語" 2 1;
  test "ε" "ελληνικά" 0 1;
  test "ελληνικά" "ελληνικά" 0 8;
  test "λ" "ελληνικά" 1 1;
  test "ά" "ελληνικά" 7 1;
  test "š" "švédčina" 0 1;
  test "švédčina" "švédčina" 0 8;
  test "a" "švédčina" 7 1

let util_name_with_roman_number _ =
  let test r a = (check (option string)) "" r (Util.name_with_roman_number a) in
  test (Some "XXXIX XXXIX") "39 39";
  test (Some "XXXIX x XXXIX") "39 x 39";
  test (Some "foo CCXLVI") "foo 246";
  test (Some "bar CDXXI baz") "bar 421 baz";
  test (Some "bar CLX baz CCVII") "bar 160 baz 207";
  test None "foo bar baz"

let printer_safe x = (x : Adef.safe_string :> string)
let printer_encoded x = (x : Adef.encoded_string :> string)
let printer_escaped x = (x : Adef.escaped_string :> string)

let util_safe_html _ =
  (check string) ""
    {|<a href="localhost:2318/foo_w?lang=fr&#38;acte=123">foo</a>|}
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&acte=123">foo</a>|}
      :> string);
  (check string) ""
    {|<a href="localhost:2318/foo_w?lang=fr&#38;image=on">foo</a>|}
    (Util.safe_html {|<a href="localhost:2318/foo_w?lang=fr&image=on">foo</a>|}
      :> string)

let util_transl_a_of_b _ =
  let conf = Config.empty in
  let conf =
    {
      conf with
      env = ("lang", Adef.encoded "fr") :: conf.env;
      vowels = [ "a"; "e"; "i"; "o"; "u"; "y" ];
    }
  in
  Hashtbl.add conf.lexicon "%1 of %2" "%1 d[e |']%2";
  let test aaa (s1, s2, s2_raw) =
    let bbb = Util.transl_a_of_b conf s1 s2 s2_raw in
    (check string) "" aaa bbb
  in
  test "naissance de <b>Jean</b>" ("naissance", "<b>Jean</b>", "Jean");
  test "naissance d'<b>André</b>" ("naissance", "<b>André</b>", "André")

let util_string_with_macros _ =
  let conf = Config.empty in
  (check string) ""
    {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|}
    (Util.string_with_macros conf [] {|jean@dupond.net - le 1 &amp; 2|})

let utf8_capitalize_fst _ =
  let test a r = (check string) a r (Utf8.capitalize_fst a) in
  test "" "";
  test "abcdef" "Abcdef";
  test " ghiljl" " Ghiljl";
  test "<i>mnopqr1" "<i>Mnopqr1";
  test "<i><b>mnopqr2" "<i><b>Mnopqr2";
  test "<i> <b>mnopqr3" "<i> <b>Mnopqr3";
  test "<i>, <b>mnopqr4" "<i>, <b>mnopqr4"

let util_escape_html _ =
  (check string) ""
    {|&#60;a href=&#34;mailto:jean@dupond.net&#34;&#62;jean@dupond.net&#60;/a&#62; - le 1 &#38;amp; 2|}
    (Util.escape_html
       {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|}
      :> string)

let datedisplay_string_of_date _ =
  let open Def in
  let conf = Config.empty in
  let conf =
    {
      conf with
      env = ("lang", Adef.encoded "co") :: conf.env;
      vowels = [ "a"; "e"; "i"; "o"; "u"; "y" ];
    }
  in
  Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i |']%m %y/%d d[i |']%m %y/d[i |']%m %y/in u %y";
  Hashtbl.add conf.lexicon "(month)"
    "ghjennaghju/ferraghju/marzu/aprile/maghju/ghjugnu/lugliu/aostu/sittembre/uttobre/nuvembre/dicembre";
  let test aaa cal (d, m, y) =
    let date =
      Dgreg ({ day = d; month = m; year = y; prec = Sure; delta = 0 }, cal)
    in
    let bbb :> string = DateDisplay.string_of_date conf date in
    (check string) "" aaa bbb
  in
  test "4 d'aostu 1974" Dgregorian (4, 8, 1974);
  test "4 di sittembre 1974" Dgregorian (4, 9, 1974);
  test "1<sup>u</sup> di ferraghju 1974" Dgregorian (1, 2, 1974);
  test "di marzu 1974" Dgregorian (0, 3, 1974);
  test "d'aprile 1974" Dgregorian (0, 4, 1974);
  test "in u 1974" Dgregorian (0, 0, 1974);
  Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i']%m %y/%d d[i %m %y/d[i |'%m %y/in u %y";
  test "1<sup>u</sup> d[i']ferraghju 1974" Dgregorian (1, 2, 1974);
  test "d[i |'marzu 1975" Dgregorian (0, 3, 1975);
  test "4 d[i sittembre 1974" Dgregorian (4, 9, 1974)

let start_with_vowel _ =
  let conf = Config.empty in
  let conf =
    { conf with vowels = [ "a"; "e"; "i"; "o"; "u"; "y"; "ae"; "oe" ] }
  in
  (check bool) "Start with vowel abc" true (Util.start_with_vowel conf "abc");
  (check bool) "Start with vowel Abc" true (Util.start_with_vowel conf "Abc");
  (check bool) "Start with vowel Æbc" true (Util.start_with_vowel conf "Æbc");
  (check bool) "Start with vowel Ébc" true (Util.start_with_vowel conf "Ébc");
  (check bool) "Start with vowel Ÿbc" true (Util.start_with_vowel conf "Ÿbc");
  (check bool) "Start with vowel øbc" true (Util.start_with_vowel conf "øbc");
  (check bool) "Start with vowel def" false (Util.start_with_vowel conf "def");
  ()

let v =
  [
    ( "mutil",
      [
        test_case "Mutil.contains" `Quick mutil_contains;
        test_case "Mutil.start_with" `Quick mutil_start_with;
        test_case "Mutil arabian-roman" `Quick mutil_arabian_romian;
        test_case "Mutil particule" `Quick mutil_compare_after_particle;
        test_case "Mutil.string_of_int_sep" `Quick mutil_compare_after_particle;
      ] );
    ("name", [ test_case "Name.title" `Quick name_title ]);
    ( "utf8",
      [
        test_case "Utf8.sub" `Quick utf8_sub;
        test_case "Utf8.name_with_roman_number" `Quick
          util_name_with_roman_number;
        test_case "Utf8.capitalize_fst" `Quick utf8_capitalize_fst;
      ] );
    ( "util",
      [
        test_case "Util.safe_html" `Quick util_safe_html;
        test_case "Util.transl_a_of_b" `Quick util_transl_a_of_b;
        test_case "Util.string_with_macros" `Quick util_string_with_macros;
        test_case "Util.escape_html" `Quick util_escape_html;
        test_case "Util.start_with_vowel" `Quick start_with_vowel;
      ] );
    ( "date-display",
      [
        test_case "DateDisplay.string_of_date" `Quick datedisplay_string_of_date;
      ] );
  ]
