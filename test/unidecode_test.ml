open Alcotest

let lower expected s () = (check string) s expected (Name.lower s)
let former expected s () = (check string) s expected (Name.former_lower s)

let restored () =
  ignore (Name.former_lower "Weiß");
  (check string) "Weiß" "weiss" (Name.lower "Weiß")

let v =
  [
    ( "unidecode",
      [
        test_case "Ḣ" `Quick (lower "hugh" "Ḣugh");
        test_case "Ή" `Quick (lower "era" "Ήρα");
        test_case "ΰ" `Quick (lower "u" "ΰ");
        test_case "ẞ" `Quick (lower "weiss" "WEIẞ");
        test_case "ß" `Quick (lower "weiss" "Weiß");
        test_case "ю" `Quick (lower "kryouger" "Крюгер");
        test_case "Љ" `Quick (lower "ljoubitsa" "Љубица");
        test_case "Ș" `Quick (lower "stefan" "Ștefan");
        test_case "Ş" `Quick (lower "stefan" "Ştefan");
        test_case "ї" `Quick (lower "i" "ї");
        test_case "ſ" `Quick (lower "christian" "Chriſtian");
        test_case "ʉ" `Quick (lower "u" "ʉ");
        test_case "ἀ" `Quick (lower "a" "ἀ");
        test_case "kasra" `Quick (lower "al bin ali" "\u{0650}Al Bin Ali");
        test_case "東京" `Quick (lower "東京" "東京");
        test_case "が" `Quick (lower "が" "が");
        test_case "no-break space" `Quick
          (lower "jean\u{00A0}ii" "Jean\u{00A0}II");
      ] );
    ( "former",
      [
        test_case "ß" `Quick (former "weisz" "Weiß");
        test_case "ẞ" `Quick (former "weis" "WEIẞ");
        test_case "Ḣ" `Quick (former "gugh" "Ḣugh");
        test_case "Ή" `Quick (former "hra" "Ήρα");
        test_case "ю" `Quick (former "kryuger" "Крюгер");
        test_case "Ș" `Quick (former "Ștefan" "Ștefan");
        test_case "unchanged" `Quick (former "muller" "Müller");
        test_case "restored" `Quick restored;
      ] );
  ]
