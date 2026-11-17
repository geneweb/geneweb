open Alcotest

let rec ordered = function
  | a :: (b :: _ as l) ->
      (check bool)
        (a ^ " < " ^ b)
        true
        (Collation.compare a b < 0 && Collation.compare b a > 0);
      ordered l
  | [ _ ] | [] -> ()

let root () =
  List.iter ordered
    [
      [
        "Gaël";
        "Gael Elizabeth";
        "Gaelle";
        "Gaëlle";
        "Gaetan";
        "Gaétan";
        "Gaëtan";
        "Gaetano";
      ];
      [ "nuss"; "Nuss"; "NUSS"; "nuß" ];
      [ "cote"; "coté"; "côte"; "côté" ];
      [ "role"; "Role"; "rôle"; "Rôle" ];
      [ "e"; "é"; "è"; "ĕ"; "ê"; "ě"; "ë" ];
      [ "ae"; "æ"; "af" ];
      [ "oe"; "œ"; "of" ];
      [ "ss"; "ß"; "st" ];
      [ "d"; "ð"; "e" ];
      [ "n"; "ŋ"; "o" ];
      [ "o"; "ø"; "p" ];
      [ "z"; "þ" ];
      [ "Иван"; "Иуда"; "Йоган"; "Кирилл" ];
      [ "Zola"; "Ωμέγα"; "Иван"; "Աշոտ"; "東京" ];
      [ "1789"; "Abel" ];
      [ "Saint Martin"; "Saint-Martin"; "Sainte"; "Saintes" ];
      [ "d’Arc"; "Dame" ];
    ];
  (check bool) "é <> e + U+0301" true (Collation.compare "é" "e\u{301}" <> 0)

let tailorings () =
  let tailored lang l =
    Collation.set_lang lang;
    Fun.protect ~finally:(fun () -> Collation.set_lang "") (fun () -> ordered l)
  in
  ordered [ "Nuñez"; "Nuno" ];
  tailored "es" [ "Nuno"; "Nuñez" ];
  ordered [ "Nuñez"; "Nuno" ];
  tailored "sv-SE"
    [
      "Aaberg";
      "Olsen";
      "Yngve";
      "Ütter";
      "Zander";
      "Åberg";
      "Ärlig";
      "Ærø";
      "Ödman";
      "Øster";
    ];
  tailored "da" [ "NUSS"; "Nuss"; "nuss" ];
  tailored "da" [ "Andersen"; "Zander"; "Ærø"; "Øster"; "Aaberg" ];
  tailored "cs"
    [ "Cibulka"; "Čapek"; "Hrubý"; "Chalupa"; "Ivan"; "Rybář"; "Řezník" ];
  tailored "pl" [ "Lwów"; "Łódź"; "Mazur"; "Szymon"; "Śliwa" ];
  tailored "tr"
    [ "Iğdır"; "Ilgaz"; "ılık"; "Irmak"; "Ismail"; "Işık"; "İsmail" ];
  tailored "ru" [ "Иван"; "Ivan" ];
  tailored "br" [ "Cudennec"; "Chever"; "C’hoarzh"; "Daniel" ]

(* The letters followed by "a" of CollationTest_NON_IGNORABLE_SHORT.txt
   (UCA 17.0.0) made of the letters and marks of the table, in its order,
   without the variants told apart by the third level of the DUCET alone
   (ª º µ ς ϐ…), the titlecase digraphs (ǅ) and the overlay mark U+0334 *)
let ducet =
  {|a A á Á à À ă Ă ắ Ắ ằ Ằ ẵ Ẵ ẳ Ẳ â Â ấ Ấ ầ Ầ ẫ Ẫ ẩ Ẩ ǎ Ǎ å Å ǻ Ǻ ä Ä ǟ Ǟ ã Ã
ȧ Ȧ ǡ Ǡ ą Ą ā Ā ả Ả ȁ Ȁ ȃ Ȃ ạ Ạ ặ Ặ ậ Ậ ḁ Ḁ æ Æ ǽ Ǽ ǣ Ǣ ɐ Ɐ ɑ Ɑ ɒ Ɒ b B ḃ Ḃ
ḅ Ḅ ḇ Ḇ ʙ ƀ Ƀ ɓ Ɓ ƃ Ƃ c C ć Ć ĉ Ĉ č Č ċ Ċ ç Ç ḉ Ḉ ȼ Ȼ ƈ Ƈ ɕ d D ď Ď ḋ Ḋ ḑ Ḑ
đ Đ ḍ Ḍ ḓ Ḓ ḏ Ḏ ð Ð ȸ ʣ ʥ ʤ ɖ Ɖ ɗ Ɗ ƌ Ƌ ȡ ẟ e E é É è È ĕ Ĕ ê Ê ế Ế ề Ề ễ Ễ
ể Ể ě Ě ë Ë ẽ Ẽ ė Ė ȩ Ȩ ḝ Ḝ ę Ę ē Ē ḗ Ḗ ḕ Ḕ ẻ Ẻ ȅ Ȅ ȇ Ȇ ẹ Ẹ ệ Ệ ḙ Ḙ ḛ Ḛ ɇ Ɇ
ǝ Ǝ ə Ə ɛ Ɛ ɘ ɚ ɜ Ɜ ɝ ɞ ʚ ɤ Ɤ f F ḟ Ḟ ʩ ƒ Ƒ g G ǵ Ǵ ğ Ğ ĝ Ĝ ǧ Ǧ ġ Ġ ģ Ģ ḡ Ḡ
ɡ Ɡ ɢ ǥ Ǥ ɠ Ɠ ʛ ɣ Ɣ ƣ Ƣ h H ĥ Ĥ ȟ Ȟ ḧ Ḧ ḣ Ḣ ḩ Ḩ ħ Ħ ḥ Ḥ ḫ Ḫ ẖ ʜ ƕ Ƕ ɦ Ɦ ɧ i
I í Í ì Ì ĭ Ĭ î Î ǐ Ǐ ï Ï ḯ Ḯ ĩ Ĩ İ į Į ī Ī ỉ Ỉ ȉ Ȉ ȋ Ȋ ị Ị ḭ Ḭ ı ɪ Ɪ ɨ Ɨ ɩ
Ɩ j J ĵ Ĵ ǰ ȷ ɉ Ɉ ʝ Ʝ ɟ ʄ k K ḱ Ḱ ǩ Ǩ ķ Ķ ḳ Ḳ ḵ Ḵ ƙ Ƙ ʞ Ʞ l L ĺ Ĺ ľ Ľ ļ Ļ ł
Ł ḷ Ḷ ḹ Ḹ ḽ Ḽ ḻ Ḻ ỻ Ỻ ʪ ʫ ʟ ƚ Ƚ ɫ Ɫ ɬ Ɬ ɭ ȴ ɮ ƛ Ƛ ʎ m M ḿ Ḿ ṁ Ṁ ṃ Ṃ ɱ Ɱ n N
ń Ń ǹ Ǹ ň Ň ñ Ñ ṅ Ṅ ņ Ņ ṇ Ṇ ṋ Ṋ ṉ Ṉ ɴ ɲ Ɲ ƞ Ƞ ɳ ȵ ŋ Ŋ o O ó Ó ò Ò ŏ Ŏ ô Ô ố
Ố ồ Ồ ỗ Ỗ ổ Ổ ǒ Ǒ ö Ö ȫ Ȫ ő Ő õ Õ ṍ Ṍ ṏ Ṏ ȭ Ȭ ȯ Ȯ ȱ Ȱ ø Ø ǿ Ǿ ǫ Ǫ ǭ Ǭ ō Ō ṓ
Ṓ ṑ Ṑ ỏ Ỏ ȍ Ȍ ȏ Ȏ ơ Ơ ớ Ớ ờ Ờ ỡ Ỡ ở Ở ợ Ợ ọ Ọ ộ Ộ œ Œ ɶ ɔ Ɔ ɵ Ɵ ɷ ȣ Ȣ p P ṕ
Ṕ ṗ Ṗ ƥ Ƥ ɸ q Q ȹ ʠ ɋ Ɋ ĸ r R ŕ Ŕ ř Ř ṙ Ṙ ŗ Ŗ ȑ Ȑ ȓ Ȓ ṛ Ṛ ṝ Ṝ ṟ Ṟ ʀ Ʀ ɍ Ɍ ɹ
ɺ ɻ ɼ ɽ Ɽ ɾ ɿ ʁ s S ś Ś ṥ Ṥ ŝ Ŝ š Š ṧ Ṧ ṡ Ṡ ş Ş ṣ Ṣ ṩ Ṩ ș Ș ß ẞ ʂ Ʂ ȿ Ȿ ẜ ẝ
ʃ Ʃ ƪ ʅ ʆ t T ť Ť ẗ ṫ Ṫ ţ Ţ ṭ Ṭ ț Ț ṱ Ṱ ṯ Ṯ ʨ ƾ ʦ ʧ ŧ Ŧ ƫ ƭ Ƭ ʈ Ʈ ȶ ʇ Ʇ u U
ú Ú ù Ù ŭ Ŭ û Û ǔ Ǔ ů Ů ü Ü ǘ Ǘ ǜ Ǜ ǚ Ǚ ǖ Ǖ ű Ű ũ Ũ ṹ Ṹ ų Ų ū Ū ṻ Ṻ ủ Ủ ȕ Ȕ
ȗ Ȗ ư Ư ứ Ứ ừ Ừ ữ Ữ ử Ử ự Ự ụ Ụ ṳ Ṳ ṷ Ṷ ṵ Ṵ ʉ Ʉ ɥ Ɥ ʮ ʯ ɯ Ɯ ɰ ʊ Ʊ v V ṽ Ṽ ṿ
Ṿ ʋ Ʋ ỽ Ỽ ʌ Ʌ w W ẃ Ẃ ẁ Ẁ ŵ Ŵ ẘ ẅ Ẅ ẇ Ẇ ẉ Ẉ ʍ x X ẍ Ẍ ẋ Ẋ y Y ý Ý ỳ Ỳ ŷ Ŷ ẙ
ÿ Ÿ ỹ Ỹ ẏ Ẏ ȳ Ȳ ỷ Ỷ ỵ Ỵ ʏ ɏ Ɏ ƴ Ƴ ỿ Ỿ ȝ Ȝ z Z ź Ź ẑ Ẑ ž Ž ż Ż ẓ Ẓ ẕ Ẕ ƍ ƶ Ƶ
ȥ Ȥ ʐ ʑ ɀ Ɀ ʒ Ʒ ǯ Ǯ ƹ Ƹ ƺ ʓ þ Þ ƿ Ƿ ƻ ƨ Ƨ ƽ Ƽ ƅ Ƅ ʔ ɂ Ɂ ʕ ʡ ʢ ʖ ǀ ǁ ǂ ǃ ʗ ʘ
ʬ ʭ α Α ἀ Ἀ ἄ Ἄ ᾄ ἂ Ἂ ᾂ ἆ Ἆ ᾆ ᾀ ἁ Ἁ ἅ Ἅ ᾅ ἃ Ἃ ᾃ ἇ Ἇ ᾇ ᾁ ά Ά ᾴ ὰ Ὰ ᾲ ᾰ Ᾰ ᾶ ᾷ
ᾱ Ᾱ ᾳ β Β γ Γ δ Δ ε Ε ἐ Ἐ ἔ Ἔ ἒ Ἒ ἑ Ἑ ἕ Ἕ ἓ Ἓ έ Έ ὲ Ὲ ϝ Ϝ ͷ Ͷ ϛ Ϛ ζ Ζ ͱ Ͱ η
Η ἠ Ἠ ἤ Ἤ ᾔ ἢ Ἢ ᾒ ἦ Ἦ ᾖ ᾐ ἡ Ἡ ἥ Ἥ ᾕ ἣ Ἣ ᾓ ἧ Ἧ ᾗ ᾑ ή Ή ῄ ὴ Ὴ ῂ ῆ ῇ ῃ θ Θ ι Ι
ἰ Ἰ ἴ Ἴ ἲ Ἲ ἶ Ἶ ἱ Ἱ ἵ Ἵ ἳ Ἳ ἷ Ἷ ί Ί ὶ Ὶ ῐ Ῐ ῖ ϊ Ϊ ΐ ῒ ῗ ῑ Ῑ ϳ Ϳ κ Κ ϗ Ϗ λ Λ
μ Μ ν Ν ξ Ξ ο Ο ὀ Ὀ ὄ Ὄ ὂ Ὂ ὁ Ὁ ὅ Ὅ ὃ Ὃ ό Ό ὸ Ὸ π Π ϻ Ϻ ϟ Ϟ ϙ Ϙ ρ Ρ ῤ ῥ Ῥ ϼ
σ Σ ͼ Ͼ ͻ Ͻ ͽ Ͽ τ Τ υ Υ ὐ ὔ ὒ ὖ ὑ Ὑ ὕ Ὕ ὓ Ὓ ὗ Ὗ ύ Ύ ὺ Ὺ ῠ Ῠ ῦ ϋ Ϋ ΰ ῢ ῧ ῡ Ῡ
φ Φ χ Χ ψ Ψ ω Ω ὠ Ὠ ὤ Ὤ ᾤ ὢ Ὢ ᾢ ὦ Ὦ ᾦ ᾠ ὡ Ὡ ὥ Ὥ ᾥ ὣ Ὣ ᾣ ὧ Ὧ ᾧ ᾡ ώ Ώ ῴ ὼ Ὼ ῲ
ῶ ῷ ῳ ϡ Ϡ ͳ Ͳ ϸ Ϸ ϣ Ϣ ϥ Ϥ ϧ Ϧ ϩ Ϩ ϫ Ϫ ϭ Ϭ ϯ Ϯ а А ӑ Ӑ ӓ Ӓ ә Ә ӛ Ӛ ӕ Ӕ б Б в
В г Г ѓ Ѓ ґ Ґ ғ Ғ ӻ Ӻ ҕ Ҕ ӷ Ӷ д Д ԁ Ԁ ђ Ђ ԃ Ԃ ҙ Ҙ е Е ѐ Ѐ ӗ Ӗ ё Ё є Є ж Ж ӂ
Ӂ ӝ Ӝ ԫ Ԫ җ Җ з З ӟ Ӟ ԅ Ԅ ԑ Ԑ ѕ Ѕ ӡ Ӡ ԇ Ԇ и И ѝ Ѝ ӥ Ӥ ӣ Ӣ ҋ Ҋ і І ї Ї й Й ј
Ј к К ќ Ќ қ Қ ӄ Ӄ ҡ Ҡ ҟ Ҟ ҝ Ҝ ԟ Ԟ ԛ Ԛ л Л ӆ Ӆ ԯ Ԯ ԓ Ԓ ԡ Ԡ љ Љ ԉ Ԉ ԕ Ԕ м М ӎ
Ӎ н Н ԩ Ԩ ӊ Ӊ ң Ң ӈ Ӈ ԣ Ԣ ҥ Ҥ њ Њ ԋ Ԋ о О ӧ Ӧ ө Ө ӫ Ӫ п П ԥ Ԥ ҧ Ҧ ҁ Ҁ р Р ҏ
Ҏ ԗ Ԗ с С ԍ Ԍ ҫ Ҫ т Т ԏ Ԏ ҭ Ҭ ћ Ћ у У ў Ў ӱ Ӱ ӳ Ӳ ӯ Ӯ ү Ү ұ Ұ ѹ Ѹ ф Ф х Х ӽ
Ӽ ӿ Ӿ ҳ Ҳ һ Һ ԧ Ԧ ѡ Ѡ ѿ Ѿ ѽ Ѽ ѻ Ѻ ц Ц ҵ Ҵ ч Ч ӵ Ӵ ԭ Ԭ ҷ Ҷ ӌ Ӌ ҹ Ҹ ҽ Ҽ ҿ Ҿ џ
Џ ш Ш щ Щ ъ Ъ ы Ы ӹ Ӹ ь Ь ҍ Ҍ ѣ Ѣ э Э ӭ Ӭ ю Ю я Я ԙ Ԙ ѥ Ѥ ѧ Ѧ ѫ Ѫ ѩ Ѩ ѭ Ѭ ѯ
Ѯ ѱ Ѱ ѳ Ѳ ѵ Ѵ ѷ Ѷ ҩ Ҩ ԝ Ԝ ӏ Ӏ|}

let unicode () =
  String.split_on_char '\n' ducet
  |> List.concat_map (String.split_on_char ' ')
  |> List.map (fun s -> s ^ "a")
  |> ordered

let v =
  [
    ( "collation",
      [
        test_case "root" `Quick root;
        test_case "tailorings" `Quick tailorings;
        test_case "Unicode conformance" `Quick unicode;
      ] );
  ]
