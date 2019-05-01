open Geneweb
open OUnit2

let name_unaccent _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Some.name_unaccent b)
  in
  let test_a a b =
    assert_equal ~printer:(fun x -> x) a (Some.name_unaccent ~apostr:true b)
  in
  test "etienne" "étienne"
  ; test "Etienne" "Étienne"
  ; test "yvette" "ÿvette"
  ; test "Yvette" "Ÿvette"
  ; test "Etienne" "Ĕtienne"
  (* apostrophes *)
  ; test "L'homme" "L'homme"
  ; test_a "L homme" "L’homme"
  (* unaccent performs cyrillic to latin translation! *)
  ; test "Genri" "Генри"

(* Name.lower performs lower and unaccent *)
let name_lower _ =
  let test a b =
    assert_equal ~printer:(fun x -> x) a (Name.lower b)
  in
  let test_a a b =
    assert_equal ~printer:(fun x -> x) a (Name.lower ~apostr:true b)
  in
  let test_v a b =
    assert_equal ~printer:(fun x -> x) a (Name.lower ~viet:true b)
  in
  test "abcdef" "ABCdEF"
  ; test "abcdef" "ÂBÇdĘF"
  ; test "etienne" "Ĕtienne"
  ; test "andre" "André"
  ; test "andrea" "Andréá"
  ; test "ellenika" "ελληνικά"
  ; test "ellnhiya" "ΈΛΛΝΉΊΎΆ"
  ; test "yvette" "Ÿvette"
  ; test "l homme" "L'homme"
  ; test "l’homme" "L’homme"
  ; test_a "l homme" "L’homme"
  (* cyrillic to latin and lower *)
  ; test "genri" "Генри"
  ; test "genri" "ГЕНРИ"
  (* Latin supplemental, vietnameese *)
  ; test "mien dịnh nguyễn phuc" "Miên Định Nguyễn Phúc"
  ; test_v "mien dinh nguyen phuc" "Miên Định Nguyễn Phúc"

let capitalize str =
  let strlen = String.length str in
  let rec loop s i =
    if i < strlen then
      let char = String.sub str i (Name.nbc str.[i]) in
      loop (s ^ (Util.capitale char)) (i + (Name.nbc str.[i]))
    else s
  in
  loop "" 0

let util_capitale_full _ =
  let test_s a b =
    assert_equal ~printer:(fun x -> x) a (capitalize b)
  in
(* ASCII *)
  test_s "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
         "abcdefghijklmnopqrstuvwxyz"
  ; test_s "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
(* Latin-1 supplement. C3 80 - *)
  ; test_s "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
           "àáâãäåæçèéêëìíîïñòóôõöùúûüýÿ"
  ; test_s "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
           "ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝŸ"
(* Latin-1 supplement. C4 80 - *)
  ; test_s "ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽ"
           "āăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĺļľ"
  ; test_s "ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽ"
           "ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽ"
(* Latin-1 supplement. C5 80 - *)
  ; test_s "ĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
           "ŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷÿźżž"
  ; test_s "ĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
           "ĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽ"
(* Latin-1 supplement. C5 80 - *)

(* Latin-1 supplement. Greek, CE 80 - *)
  ; test_s "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡςΣΤΥΦΧΨΩΆΈήΊΰΪΫΌΎΏ"
           "αβγδεζηθικλμνξοπρςστυφχψωάέήίΰϊϋόύώ"
  ; test_s "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡςΣΤΥΦΧΨΩΆΈήΊΰΪΫΌΎΏ"
           "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡςΣΤΥΦΧΨΩΆΈήΊΰΪΫΌΎΏ"
           
(* Latin-1 supplement. Cyrillic, - *)
  ; test_s "ƂƄƇƋƑƘƠƢƤƧƪƯƵƼЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ"
           "ƃƅƈƌƒƙơƣƥƨƪưƶƽѐёђѓєѕіїјљњћќѝўџ"
  ; test_s "ƂƄƇƋƑƘƠƢƤƧƪƯƵƼЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ"
           "ƂƄƇƋƑƘƠƢƤƧƪƯƵƼЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏ"

  ; test_s 
    "ІУАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏѠѢѤѦѨѪѬѮѰѲѴѶѸѺѼѾ"
    "іуабвгдежзийклмнопрстуфхцчшщъыьэюяѐёђѓєѕіїјљњћќѝўџѡѣѥѧѩѫѭѯѱѳѵѷѹѻѽѿ"
  ; test_s 
    "ІУАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏѠѢѤѦѨѪѬѮѰѲѴѶѸѺѼѾ"
    "ІУАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏѠѢѤѦѨѪѬѮѰѲѴѶѸѺѼѾ"

  ; test_s 
    "ҀҊҌҎҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾ"
    "ҁҋҍҏґғҕҗҙқҝҟҡңҥҧҩҫҭүұҳҵҷҹһҽҿ"
  ; test_s 
    "ҀҊҌҎҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾ"
    "ҀҊҌҎҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾ"

  ; test_s "ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿ"
           "ἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷ"
  ; test_s "ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿ"
           "ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿ"

  ; test_s "ẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸ"
           "ạảấầẩẫậắằẳẵặẹẻẽếềểễệỉịọỏốồổỗộớờởỡợụủứừửữựỳỵỷỹ"
  ; test_s "ẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸ"
           "ẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲỴỶỸ"

  ; test_s "ǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǸǺǼǾ"
           "ǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯǲǵǹǻǽǿ"
  ; test_s "ǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǸǺǼǾ"
           "ǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǸǺǼǾ"

  ; test_s "ȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȤȦȨȪȬȮȰȲȻ"
           "ȁȃȅȇȉȋȍȏȑȓȕȗșțȝȟȥȧȩȫȭȯȱȳȼ"
  ; test_s "ȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȤȦȨȪȬȮȰȲȻ"
           "ȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȤȦȨȪȬȮȰȲȻ"

  ; test_s "ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾ" (* E1 B8 80 *)
           "ḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿ"
  ; test_s "ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾ"
           "ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾ"

  ; test_s "ṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾẀẂẄẆẈẊẌẎẐẒẔ"
           "ṁṃṅṇṉṋṍṏṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿẁẃẅẇẉẋẍẏẑẓẕ"
  ; test_s "ṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾẀẂẄẆẈẊẌẎẐẒẔ"
           "ṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾẀẂẄẆẈẊẌẎẐẒẔ"

(*
  ; test_s 
    "ӐӒӔӖӘӚӜӞӠӢӤӦӨӪӬӮӰӲӴӶӸӺӼӾԀԂԄԆԈԊԌԎԐԒ"
    "ӑӓӕӗәӛӝӟӡӣӥӧөӫӭӯӱӳӵӷӹӻӽӿԁԃԅԇԉԋԍԏԑԓ"
  ; test_s 
    "ӁӃӅӇӉӋӍӀ"
    "ӂӄӆӈӊӌӎӏ"

  ; test_s 
    "ԔԖԘԚԜԞԠԢԤԦԨԪԬԮᠠᠡᠢᠣᠤᠥᠦᠨᠪᠫᠬᠭᠮᠯᠰᠱᠲᠳᠴᠵᠶᠷᠸᠹᠺᠻᠼᠽᠾᠿ"
    "ԕԗԙԛԝԟԡԣԥԧԩԫԭԯᠠᠡᠢᠣᠤᠥᠦᠨᠪᠫᠬᠭᠮᠯᠰᠱᠲᠳᠴᠵᠶᠷᠸᠹᠺᠻᠼᠽᠾᠿ"
  ; test_s 
    "ᴫℏⰊꙀꙂꙄꙆꙈꙊꙌꙎꙐꙒꙔꙖꙘꙚꙜꙞꙠꙢꙤꙦꙨꙪꙬꚀꚂꚄꚆꚈꚊꚌꚎꚐꚒꚔꚖꚘꚚ"
    "ᴫℏⰺꙁꙃꙅꙇꙉꙋꙍꙏꙑꙓꙕꙗꙙꙛꙝꙟꙡꙣꙥꙧꙩꙫꙭꚁꚃꚅꚇꚉꚋꚍꚏꚑꚓꚕꚗꚙꚛ"
*)







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

(* in this test, second argument the char number at which to start the test.*)
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
    ; "mutil_start_with" >:: mutil_start_with
    ; "mutil_arabian_romian" >:: mutil_arabian_romian
    ; "mutil_compare_after_particle" >:: mutil_compare_after_particle
    ]
  ; "Util" >:::
    [ "util_str_sub" >:: util_str_sub
    ; "util_safe_html" >:: util_safe_html
    ; "util_capitale" >:: util_capitale
    ; "util_capitale_full" >:: util_capitale_full
    ; "name_unaccent" >:: name_unaccent
    ; "name_lower" >:: name_lower
    ]
  ]

