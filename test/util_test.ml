let ext_string_contains () =
  let str = "foo bar" in
  let test t b =
    (Alcotest.check Alcotest.bool) t b (Ext_string.contains str t)
  in
  test "foo" true;
  test "baz" false;
  test "foo_b" false;
  test "foo b" true;
  test "foo__b" false;
  test "bar__" false;
  test "r" true;
  test "" true

let ext_string_start_with () =
  Alcotest.check_raises "" (Invalid_argument "start_with") (fun () ->
      ignore @@ Ext_string.start_with "foo" (-1) "foo");
  Alcotest.check_raises "" (Invalid_argument "start_with") (fun () ->
      ignore @@ Ext_string.start_with "foo" 4 "foo");
  (Alcotest.check Alcotest.bool)
    "Ext_string.start_with \"foo\" 0 \"foo\"" true
    (Ext_string.start_with "foo" 0 "foo");
  (Alcotest.check Alcotest.bool)
    "not (Ext_string.start_with \"bar\" 0 \"foo\")" true
    (not @@ Ext_string.start_with "bar" 0 "foo");
  (Alcotest.check Alcotest.bool)
    "Ext_string.start_with \"\" 0 \"foo\"" true
    (Ext_string.start_with "" 0 "foo");
  ()

let ext_string_split_on_char () =
  let test ~__POS__ ~expected ~separator s =
    Alcotest.check' ~pos:__POS__
      (Alcotest.list Alcotest.string)
      ~msg:"" ~expected
      ~actual:(Ext_string.split_on_char separator s)
  in
  let space = ' ' in
  let comma = ',' in
  test ~__POS__ ~expected:[] ~separator:space "";
  test ~__POS__ ~expected:[ "foo" ] ~separator:space "foo";
  test ~__POS__ ~expected:[ "foo," ] ~separator:space "foo,";
  test ~__POS__ ~expected:[ "foo" ] ~separator:space " foo ";
  test ~__POS__ ~expected:[ "foo," ] ~separator:space "foo, ";
  test ~__POS__ ~expected:[ "foo"; "bar" ] ~separator:space "foo bar";
  test ~__POS__ ~expected:[ "foo,bar" ] ~separator:space "foo,bar";
  test ~__POS__ ~expected:[ "foo,"; "bar,baz" ] ~separator:space "foo,  bar,baz";
  test ~__POS__ ~expected:[] ~separator:comma "";
  test ~__POS__ ~expected:[ "foo" ] ~separator:comma "foo";
  test ~__POS__ ~expected:[ "foo" ] ~separator:comma "foo,";
  test ~__POS__ ~expected:[ "foo" ] ~separator:comma " foo ";
  test ~__POS__ ~expected:[ "foo" ] ~separator:comma "foo, ";
  test ~__POS__ ~expected:[ "foo bar" ] ~separator:comma "foo bar";
  test ~__POS__ ~expected:[ "foo"; "bar" ] ~separator:comma "foo,bar";
  test ~__POS__ ~expected:[ "foo"; "bar"; "baz" ] ~separator:comma
    "foo,  bar,baz"

let mutil_arabian_romian () =
  let test a r =
    (Alcotest.check Alcotest.int)
      "arabian_of_roman" a (Mutil.arabian_of_roman r);
    (Alcotest.check Alcotest.string)
      "roman_of_arabian" r (Mutil.roman_of_arabian a)
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

let mutil_compare_after_particle () =
  let particles = Mutil.compile_particles test_particles in
  let test a b =
    let test exp a b =
      let cmp = Mutil.compare_after_particle particles in
      (Alcotest.check Alcotest.int) "" exp (cmp a b)
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

let mutil_string_of_int_sep () =
  let test sep exp int =
    (Alcotest.check Alcotest.string) "" exp (Mutil.string_of_int_sep sep int)
  in
  test "," "1" 1;
  test "," "10" 10;
  test "," "100" 100;
  test "," "1,000" 1000;
  test "," "10,000" 10000;
  test "," "100,000" 100000;
  test "," "1,000,000" 1000000

let name_title () =
  let test exp =
    List.iter (fun s -> (Alcotest.check Alcotest.string) "" exp (Name.title s))
  in
  test "Jean-Baptiste"
    [ "jean-baptiste"; "JEAN-baptiste"; "Jean-Baptiste"; "jeaN-baptistE" ]

let utf8_sub () =
  let test ?pad e s i j =
    let i = Utf8.get s i in
    (Alcotest.check Alcotest.string) "" e (Utf8.sub ?pad s i j)
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

let utf8_alphabetical_order () =
  let test ~__POS__ ~expected s s' =
    let testable =
      let same_sign x y = x = y || (x < 0 && y < 0) || (x > 0 && y > 0) in
      Alcotest.testable Format.pp_print_int same_sign
    in
    let expected =
      match expected with `Before -> -1 | `Same -> 0 | `After -> 1
    in
    Alcotest.check' ~pos:__POS__ testable ~msg:"same sign" ~expected
      ~actual:(Utf8.alphabetic_order s s')
  in
  let test_printable_ascii_characters () =
    let first_ascii_character = String.make 1 (Char.chr 0) in
    let last_ascii_character = String.make 1 (Char.chr 128) in
    let printable_ascii_characters =
      List.init 95 (fun i -> String.make 1 (Char.chr (i + 32)))
    in
    ignore
      (List.fold_left
         (fun previous_character current_character ->
           test ~__POS__ ~expected:`Before previous_character current_character;
           current_character)
         first_ascii_character printable_ascii_characters);
    ignore
      (List.fold_right
         (fun current_character next_character ->
           test ~__POS__ ~expected:`After next_character current_character;
           current_character)
         printable_ascii_characters last_ascii_character)
  in
  test ~__POS__ ~expected:`Same "" "";
  test ~__POS__ ~expected:`Before "" "a";
  test ~__POS__ ~expected:`After "a" "";
  test ~__POS__ ~expected:`Same "a" "a";
  test ~__POS__ ~expected:`After "a" "A";
  test ~__POS__ ~expected:`After "prefix" "Suffix";
  test ~__POS__ ~expected:`Before "Pre" "Prefix";
  test ~__POS__ ~expected:`After "suf" "prefix";
  test_printable_ascii_characters ();
  test ~__POS__ ~expected:`Before "a" "à";
  test ~__POS__ ~expected:`Before "A" "À";
  test ~__POS__ ~expected:`Before "À" "a";
  test ~__POS__ ~expected:`After "VANÉAU" "VANEAU";
  test ~__POS__ ~expected:`Before "Saint-Lô" "Saint-Lomer";
  test ~__POS__ ~expected:`Before "VANÉAU" "VANEBU"

let utf8_initial () =
  let test ~__POS__ ~expected s =
    Alcotest.check' ~pos:__POS__
      (Alcotest.option Alcotest.int)
      ~msg:"" ~expected ~actual:(Utf8.initial s)
  in
  test ~__POS__ ~expected:None "";
  test ~__POS__ ~expected:None " ";
  test ~__POS__ ~expected:None "a";
  test ~__POS__ ~expected:(Some 0) "A";
  test ~__POS__ ~expected:None "à";
  test ~__POS__ ~expected:(Some 0) "À";
  test ~__POS__ ~expected:None "û";
  test ~__POS__ ~expected:(Some 0) "Û";
  test ~__POS__ ~expected:None "ù";
  test ~__POS__ ~expected:(Some 0) "Ù";
  test ~__POS__ ~expected:(Some 0) "Foo";
  test ~__POS__ ~expected:None "foo";
  test ~__POS__ ~expected:(Some 0) "FOO";
  test ~__POS__ ~expected:(Some 8) "van der Poel";
  test ~__POS__ ~expected:None "élise";
  test ~__POS__ ~expected:(Some 0) "Élise";
  test ~__POS__ ~expected:None "à l'envers";
  test ~__POS__ ~expected:(Some 0) "À l'envers";
  test ~__POS__ ~expected:None "ève";
  test ~__POS__ ~expected:(Some 0) "Ève";
  test ~__POS__ ~expected:None "être";
  test ~__POS__ ~expected:(Some 0) "Être";
  test ~__POS__ ~expected:None "ô combien";
  test ~__POS__ ~expected:(Some 0) "Ô combien";
  test ~__POS__ ~expected:None "âtre";
  test ~__POS__ ~expected:(Some 0) "Âtre";
  test ~__POS__ ~expected:None "d'œuf";
  test ~__POS__ ~expected:(Some 2) "d'Œuf";
  test ~__POS__ ~expected:None "álvarez";
  test ~__POS__ ~expected:(Some 0) "Álvarez"

let util_name_with_roman_number () =
  let test r a =
    (Alcotest.check (Alcotest.option Alcotest.string))
      "" r
      (Geneweb.Util.name_with_roman_number a)
  in
  test (Some "XXXIX XXXIX") "39 39";
  test (Some "XXXIX x XXXIX") "39 x 39";
  test (Some "foo CCXLVI") "foo 246";
  test (Some "bar CDXXI baz") "bar 421 baz";
  test (Some "bar CLX baz CCVII") "bar 160 baz 207";
  test None "foo bar baz"

let util_safe_html () =
  (Alcotest.check Alcotest.string)
    "" {|<a href="localhost:2318/foo_w?lang=fr&#38;acte=123">foo</a>|}
    (Geneweb.Util.safe_html
       {|<a href="localhost:2318/foo_w?lang=fr&acte=123">foo</a>|}
      :> string);
  (Alcotest.check Alcotest.string)
    "" {|<a href="localhost:2318/foo_w?lang=fr&#38;image=on">foo</a>|}
    (Geneweb.Util.safe_html
       {|<a href="localhost:2318/foo_w?lang=fr&image=on">foo</a>|}
      :> string)

let util_transl_a_of_b () =
  let conf = Geneweb.Config.empty in
  let conf = { conf with env = ("lang", Adef.encoded "fr") :: conf.env } in
  Hashtbl.add conf.lexicon "%1 of %2" "%1 d[e |']%2";
  let test aaa (s1, s2, s2_raw) =
    let bbb = Geneweb.Util.transl_a_of_b conf s1 s2 s2_raw in
    (Alcotest.check Alcotest.string) "" aaa bbb
  in
  test "naissance de <b>Jean</b>" ("naissance", "<b>Jean</b>", "Jean");
  test "naissance d'<b>André</b>" ("naissance", "<b>André</b>", "André")

let util_string_with_macros () =
  let conf = Geneweb.Config.empty in
  let test ~expected actual =
    (Alcotest.check Alcotest.string)
      "" expected
      (Geneweb.Util.string_with_macros ~conf ~env:[] actual)
  in
  List.iter
    (fun actual -> test ~expected:actual actual)
    [
      {|a@.|};
      {|a@b|};
      {|a@b.|};
      {|a@.b|};
      {|a@b..|};
      {|<a href="#foo">foo</a>|};
    ];
  test
    ~expected:
      {|<a href="https://foo.com#foo" target="_blank">https://foo.com#foo</a>|}
    {|https://foo.com#foo|};
  test ~expected:{|<a target="_blank" href="https://foo.com#foo">foo</a>|}
    {|<a href="https://foo.com#foo">foo</a>|};
  test ~expected:{|<a target="_blank" href="mailto:a@b.c">a@b.c</a>|}
    {|<a href="mailto:a@b.c">a@b.c</a>|};
  test ~expected:{|abc <a href="mailto:a@b.c" target="_blank">a@b.c</a>|}
    {|abc a@b.c|};
  test ~expected:{|<a href="mailto:a@b.." target="_blank">a@b..</a>.|}
    {|a@b...|};
  test ~expected:{|<a href="mailto:a@b.c" target="_blank">a@b.c</a>.|}
    {|a@b.c.|};
  test
    ~expected:
      {|<a href="mailto:jean@dupond.net" target="_blank">jean@dupond.net</a> - le 1 &amp; 2|}
    {|jean@dupond.net - le 1 &amp; 2|}

let util_escape_html () =
  (Alcotest.check Alcotest.string)
    ""
    {|&#60;a href=&#34;mailto:jean@dupond.net&#34;&#62;jean@dupond.net&#60;/a&#62; - le 1 &#38;amp; 2|}
    (Geneweb.Util.escape_html
       {|<a href="mailto:jean@dupond.net">jean@dupond.net</a> - le 1 &amp; 2|}
      :> string)

let privacy_of_dummy_person () =
  let test ~__POS__ ~public_if_no_date predicate =
    let empty_base =
      Gwdb.make "" []
        ( ([||], [||], [||]),
          ([||], [||], [||]),
          [||],
          {
            Def.nread = (fun _ _ -> "");
            norigin_file = "";
            efiles = (fun () -> []);
          } )
    in
    let predicate, expected =
      match predicate with
      | `Is_contemporary -> (Geneweb.Person.is_contemporary, true)
      | `Is_visible -> (Geneweb.Person.is_visible, false)
    in
    Alcotest.check' ~pos:__POS__ Alcotest.bool ~msg:"" ~expected
      ~actual:
        (predicate
           { Geneweb.Config.empty with public_if_no_date }
           empty_base
           (Gwdb.empty_person empty_base Gwdb.dummy_iper))
  in
  test ~__POS__ ~public_if_no_date:false `Is_contemporary;
  test ~__POS__ ~public_if_no_date:false `Is_visible;
  test ~__POS__ ~public_if_no_date:true `Is_contemporary;
  test ~__POS__ ~public_if_no_date:true `Is_visible

let datedisplay_string_of_date () =
  let conf = Geneweb.Config.empty in
  let conf = { conf with env = ("lang", Adef.encoded "co") :: conf.env } in
  Hashtbl.add conf.lexicon "(date)"
    "1<sup>u</sup> d[i |']%m %y/%d d[i |']%m %y/d[i |']%m %y/in u %y";
  Hashtbl.add conf.lexicon "(month)"
    "ghjennaghju/ferraghju/marzu/aprile/maghju/ghjugnu/lugliu/aostu/sittembre/uttobre/nuvembre/dicembre";
  let test aaa cal (d, m, y) =
    let date =
      Date.Dgreg ({ day = d; month = m; year = y; prec = Sure; delta = 0 }, cal)
    in
    let bbb :> string = Geneweb.DateDisplay.string_of_date conf date in
    (Alcotest.check Alcotest.string) "" aaa bbb
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

let html_text_content () =
  let module Expected : sig
    type t

    val plain_text : string -> t
    val make : text_content:string -> is_plain_text:bool -> t

    val test :
      __POS__:Alcotest.Source_code_position.pos -> expected:t -> string -> unit
  end = struct
    type t = { text_content : string; is_plain_text : bool }

    let make ~text_content ~is_plain_text = { text_content; is_plain_text }
    let plain_text s = make ~text_content:s ~is_plain_text:true

    let test ~__POS__ ~expected s =
      Alcotest.check' ~pos:__POS__
        (Alcotest.pair Alcotest.string Alcotest.bool)
        ~msg:""
        ~expected:(expected.text_content, expected.is_plain_text)
        ~actual:(Html.text_content s, Html.is_plain_text s)
  end in
  let test_cases =
    let test_case ~__POS__ ~expected s = (__POS__, expected, s) in
    let plain_text_test_case ~__POS__ s =
      test_case ~__POS__ ~expected:(Expected.plain_text s) s
    in
    [
      plain_text_test_case ~__POS__ "";
      plain_text_test_case ~__POS__ " ";
      plain_text_test_case ~__POS__ "foo";
      plain_text_test_case ~__POS__ "foo\nbar";
      plain_text_test_case ~__POS__ "Paris > France";
      plain_text_test_case ~__POS__ {|"Ferdinand" Henri|};
      plain_text_test_case ~__POS__ "< foo >";
      plain_text_test_case ~__POS__ ">foo";
      plain_text_test_case ~__POS__ ">foo>";
      plain_text_test_case ~__POS__ "/>foo>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo<bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "</foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<!-- Comment -->";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<!-- <span>foo</span> -->";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foobar" ~is_plain_text:false)
        "foo<br />bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<span></span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<html></html>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<foo></foo>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<foo></bar>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:" " ~is_plain_text:false)
        "<span> </span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<html> </html>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:" " ~is_plain_text:false)
        "<foo> </foo>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:" " ~is_plain_text:false)
        "<foo> </bar>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<span<foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</span<";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<html<foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</html<";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"" ~is_plain_text:false)
        "<foo<bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</bar<";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "<span>foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "</span>foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo<span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "<span>foo</span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "<html>foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "</html>foo";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo<html>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</html>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "<html>foo</html>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"bar" ~is_plain_text:false)
        "<foo>bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"bar" ~is_plain_text:false)
        "</foo>bar";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo<bar>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        "foo</bar>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"bar" ~is_plain_text:false)
        "<foo>bar</foo>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foobar" ~is_plain_text:false)
        "<span>foo</span><span>bar</span>";
      test_case ~__POS__
        ~expected:(Expected.make ~text_content:"foo" ~is_plain_text:false)
        {|<div class="main-content"><span>foo</span></div>|};
      test_case ~__POS__
        ~expected:
          (Expected.make ~text_content:{|>alert("XsS")|} ~is_plain_text:false)
        {|><sCrIpt>alert("XsS")</scRiPt>|};
      test_case ~__POS__
        ~expected:
          (Expected.make ~text_content:"My test page" ~is_plain_text:false)
        {|<!doctype html><html lang="en-US"><head><meta charset="utf-8" /><meta name="viewport" content="width=device-width" /><title>My test page</title></head><body><img src="" alt="My test image" /></body></html>|};
    ]
  in
  List.iter
    (fun (__POS__, expected, s) -> Expected.test ~__POS__ ~expected s)
    test_cases

let v =
  [
    ( "ext-string",
      [
        Alcotest.test_case "Ext_string.contains" `Quick ext_string_contains;
        Alcotest.test_case "Ext_string.start_with" `Quick ext_string_start_with;
        Alcotest.test_case "Ext_string.split_on_char" `Quick
          ext_string_split_on_char;
      ] );
    ( "mutil",
      [
        Alcotest.test_case "Mutil arabian-roman" `Quick mutil_arabian_romian;
        Alcotest.test_case "Mutil particule" `Quick mutil_compare_after_particle;
        Alcotest.test_case "Mutil.string_of_int_sep" `Quick
          mutil_string_of_int_sep;
      ] );
    ("name", [ Alcotest.test_case "Name.title" `Quick name_title ]);
    ( "utf8",
      [
        Alcotest.test_case "Utf8.sub" `Quick utf8_sub;
        Alcotest.test_case "alphabetical-order" `Quick utf8_alphabetical_order;
        Alcotest.test_case "initial-letter" `Quick utf8_initial;
      ] );
    ( "util",
      [
        Alcotest.test_case "Util.safe_html" `Quick util_safe_html;
        Alcotest.test_case "Util.transl_a_of_b" `Quick util_transl_a_of_b;
        Alcotest.test_case "Util.string_with_macros" `Quick
          util_string_with_macros;
        Alcotest.test_case "Util.escape_html" `Quick util_escape_html;
        Alcotest.test_case "Util.name_with_roman_number" `Quick
          util_name_with_roman_number;
        Alcotest.test_case "privacy-of-dummy-person" `Quick
          privacy_of_dummy_person;
      ] );
    ( "date-display",
      [
        Alcotest.test_case "DateDisplay.string_of_date" `Quick
          datedisplay_string_of_date;
      ] );
    ("html", [ Alcotest.test_case "text-content" `Quick html_text_content ]);
  ]
