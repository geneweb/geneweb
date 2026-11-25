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
      [ "† Dupont"; "✠ ambassadeur"; "1789"; "Abel" ];
      [ "Saint Martin"; "Saint-Martin"; "Sainte"; "Saintes" ];
      [ "d’Arc"; "Dame" ];
      [ "Abd"; "ʿAbd"; "Abda" ];
      [ "Ahomee"; "ʻAhomeʻe"; "Ahomef" ];
      [ "Ali"; "ʾAli"; "Alia" ];
      [ "ê"; "ẹ"; "é\u{323}"; "ẹ\u{301}"; "f" ];
      [ "dža"; "ǆa"; "Dža"; "ǅa"; "Ǆa" ];
      [ "fils"; "ﬁls"; "Fils" ];
      [ "Me"; "Mᵉ"; "Mf" ];
      [ "p"; "ꝑ"; "q" ];
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
  tailored "cs" [ "ch"; "cH"; "Ch"; "CH" ];
  tailored "pl" [ "Lwów"; "Łódź"; "Mazur"; "Szymon"; "Śliwa" ];
  tailored "tr"
    [ "Iğdır"; "Ilgaz"; "ılık"; "Irmak"; "Ismail"; "Işık"; "İsmail" ];
  tailored "ru" [ "Иван"; "Ivan" ];
  tailored "br" [ "Cudennec"; "Chever"; "C’hoarzh"; "Daniel" ];
  tailored "hu" [ "Czuczor"; "Csaba"; "Dózsa"; "Dzsida"; "Esterházy" ];
  tailored "hu" [ "kasza"; "kassza"; "kaszt" ];
  tailored "hu" [ "dzs"; "Dzs"; "DZS" ];
  tailored "hr"
    [ "Drašković"; "Džamonja"; "Đurić"; "Lovro"; "Ljubica"; "Marko" ];
  tailored "cy" [ "Lyn"; "Lloyd"; "Roberts"; "Rhys" ];
  tailored "sq" [ "Cani"; "Çela"; "Dushi"; "Dhimitri"; "Elena" ];
  tailored "mt" [ "Ġorġ"; "Grech"; "Għajn"; "Hamrun"; "Ħal"; "Ilma" ];
  tailored "hsb" [ "Hanka"; "Chróst"; "Ivan" ];
  tailored "fo" [ "Zakarias"; "Ærting"; "Øster"; "Aaberg" ]

(* The characters followed by "a" of CollationTest_NON_IGNORABLE_SHORT.txt
   (UCA 17.0.0) that are in the table, in its order, without the spaces and
   invisible characters, ʻ ʾ ʿ (ignored by the table), l· (a contraction of the DUCET, as ŀ) and the
   characters whose third weight in the DUCET marks a variant (ª, ᵉ, ｅ, ς,
   ϐ…), digraphs and ligatures excepted (ǅ, ﬁ) *)
let ducet =
  {|‾ _ ‗ - ‐ ‒ – — ― ⁓ , ; ⁏ : ‼ ⁉ ! ¡ ⁈ ⁇ ? ¿ ‽ … ‥ . ⁕ ⁖ ⁘ ⁙ ⁚ ⁛ ⁜ ⁝ ⁞ ' ‹ ›
" « » ( ) [ ] { } ⁅ ⁆ ‖ § ¶ ⁋ @ * ⁎ ⁑ / \ & ⁊ # % ‰ ‱ † ‡ • ‣ ‧ ⁃ ⁌ ⁍ ⁗ ‴ ″
′ ‷ ‶ ‵ ‸ ※ ‿ ⁔ ⁀ ⁐ ⁁ ⁂ ` ´ ΄ ˜ ^ ¯ ˘ ˙ ¨ ΅ ῭ ῁ ˚ ˝ ¸ ˛ ʹ ͵ ʺ ˂ ˃ ˄ ˅ ˆ ˇ ˈ
ˉ ˊ ˋ ˌ ˍ ˎ ˏ ˒ ˓ ˔ ˕ ˖ ˗ ˞ ˟ ˥ ˦ ˧ ˨ ˩ ˪ ˫ ˬ ˭ ˯ ˰ ˱ ˲ ˳ ˴ ˵ ˶ ˷ ˸ ˹ ˺ ˻ ˼
˽ ˾ ˿ ꞈ ° ℃ ℉ ҂ © ® ℄ ℈ ℔ ℗ ℘ ℞ ℟ ℣ ℥ ℧ ℩ ℮ ℺ ⅁ ⅂ ⅃ ⅄ ⅊ ⅌ ⅏ ↊ ↋ ϶ + ± ÷ × <
≮ = ≠ > ≯ ¬ | ¦ ~ ⁒ ⁄ ⅋ ː ˑ ¤ ¢ $ £ ¥ ₠ ₡ ₢ ₣ ₤ ₥ ₦ ₩ ₪ ₫ € ₭ ₮ ₯ ₰ ₱ ₲ ₳ ₴
₵ ₶ ₷ ₸ ₹ ₺ ₻ ₼ ₽ ₾ ₿ ⃀ ⃁ 0 1 2 3 4 5 6 ↅ 7 8 9 ↀ ↁ ↂ ↆ ↇ ↈ ̲ ̓ ҆ ̔ ҅ ́ ̀ ̆
̂ ̌ ̊ ͂ ̈ ̋ ̃ ̇ ̸ ̧ ̨ ̄ ̍ ̎ ̒ ̕ ̚ ̽ ̾ ̿ ͆ ͊ ͋ ͌ ͐ ͑ ͒ ͗ ͛ ͝ ͞ ҄ ҇ ᪰ ᪱ ᪲ ᪳ ᪴
᪻ ᪼ ᫁ ᫂ ᫅ ᫆ ᫇ ᫈ ᫉ ᫋ ᫏ ᫐ ᫑ ᫒ ᫓ ᫔ ᫕ ᫖ ᫗ ᫘ ᫙ ᫚ ᫛ ᫜ ᫠ ᫡ ᫢ ᫣ ᫤ ᫥ ᫧ ᫨ ᫩ ᫪ ᫫ ᷀ ᷁ ᷃
᷄ ᷅ ᷆ ᷇ ᷈ ᷉ ᷋ ᷌ ᷍ ᷎ ᷑ ᷵ ᷶ ᷷ ᷸ ᷻ ᷾ ⃰ ꙼ ꙽ ̖ ̗ ̘ ̙ ̜ ̝ ̞ ̟ ̠ ̩ ̪ ̫ ̬ ̯ ̳ ̺ ̻ ̼
͇ ͈ ͉ ͍ ͎ ͓ ͔ ͕ ͖ ͙ ͚ ͜ ͟ ͢ ᪵ ᪶ ᪷ ᪸ ᪹ ᪺ ᪽ ᫃ ᫄ ᫊ ᫝ ᫦ ᷂ ᷏ ᷐ ᷹ ᷺ ᷼ ᷽ ᷿ ⃬ ⃭ ⃮ ⃯
︧ ̶ ̷ ⃘ ⃙ ⃚ ⃥ ⃪ ⃫ ᪾ ⃝ ⃞ ⃟ ⃠ ⃢ ⃣ ⃤ ̵ ̅ ̉ ̏ ̐ ̑ ̛ ̡ ̢ ̣ ̤ ̥ ̦ ̭ ̮ ̰ ̱ ̴ ̹ ͅ ͘
͠ ︢ ︩ ͡ ︠ ҃ ︮ ꙯ ⃐ ⃑ ⃒ ⃓ ⃔ ⃕ ⃖ ⃗ ⃛ ⃜ ⃡ ⃦ ⃧ ⃨ ⃩ ℀ ℁ ⅍ a A á Á à À ă Ă ắ Ắ ằ Ằ
ẵ Ẵ ẳ Ẳ â Â ấ Ấ ầ Ầ ẫ Ẫ ẩ Ẩ ǎ Ǎ å Å ǻ Ǻ ä Ä ǟ Ǟ ã Ã ȧ Ȧ ǡ Ǡ ą Ą ā Ā ả Ả ȁ Ȁ
ȃ Ȃ ạ Ạ ặ Ặ ậ Ậ ḁ Ḁ ꜳ Ꜳ æ ᷔ Æ ǽ Ǽ ǣ Ǣ ᷕ ꜵ Ꜵ ꜷ Ꜷ ᷖ ꜹ Ꜹ ꜻ Ꜻ ꜽ Ꜽ ẚ ᴀ ⱥ Ⱥ ᶏ ꞻ Ꞻ
ᴁ ᴂ ꬱ ɐ Ɐ ɑ Ɑ ꬰ ᶐ ɒ Ɒ ꭤ b B ḃ Ḃ ḅ Ḅ ḇ Ḇ ʙ ƀ Ƀ ᴯ ᴃ ᵬ ꞗ Ꞗ ᶀ ɓ Ɓ ƃ Ƃ ꞵ Ꞵ ℅ ℆ c
C ć Ć ĉ Ĉ č Č ċ Ċ ç Ç ḉ Ḉ ᴄ ȼ Ȼ ꞓ Ꞓ ꞔ Ꞔ ƈ Ƈ ɕ ꜿ Ꜿ d D ď Ď ḋ Ḋ ḑ Ḑ đ Đ ḍ Ḍ ḓ
Ḓ ḏ Ḏ ȸ ǳ ʣ ǲ Ǳ ǆ ǅ Ǆ ꭦ ʥ ʤ ᴅ ᴆ ꟈ Ꟈ ᵭ ᶁ ɖ Ɖ ɗ Ɗ ᶑ ƌ Ƌ ȡ ꝱ ẟ e E é É è È ĕ Ĕ
ê Ê ế Ế ề Ề ễ Ễ ể Ể ě Ě ë Ë ẽ Ẽ ė Ė ȩ Ȩ ḝ Ḝ ę Ę ē Ē ḗ Ḗ ḕ Ḕ ẻ Ẻ ȅ Ȅ ȇ Ȇ ẹ Ẹ
ệ Ệ ḙ Ḙ ḛ Ḛ ᴇ ꬲ ꬳ ɇ Ɇ ᶒ ꬴ ⱸ ǝ Ǝ ⱻ ə Ə ᶕ ɛ Ɛ ᶓ ɘ ɚ ɜ Ɜ ᶔ ᴈ ɝ ɞ ʚ ɤ Ɤ f F ḟ Ḟ
℻ ﬀ ﬃ ﬄ ﬁ ﬂ ʩ ꜰ ꬵ ꞙ Ꞙ ᵮ ᶂ ƒ Ƒ ꟻ g G ǵ Ǵ ğ Ğ ĝ Ĝ ǧ Ǧ ġ Ġ ģ Ģ ḡ Ḡ ꟑ Ꟑ ɡ Ɡ ꬶ ɢ
ǥ Ǥ ᶃ ɠ Ɠ ʛ ᵷ ꝿ Ꝿ ɣ Ɣ ƣ Ƣ h H ĥ Ĥ ȟ Ȟ ḧ Ḧ ḣ Ḣ ḩ Ḩ ħ Ħ ḥ Ḥ ḫ Ḫ ẖ ʜ ƕ Ƕ ꞕ ɦ Ɦ
ⱨ Ⱨ ⱶ Ⱶ ꟶ Ꟶ ꜧ Ꜧ ɧ ʽ i I í Í ì Ì ĭ Ĭ î Î ǐ Ǐ ï Ï ḯ Ḯ ĩ Ĩ İ į Į ī Ī ỉ Ỉ ȉ Ȉ
ȋ Ȋ ị Ị ḭ Ḭ ⅱ Ⅱ ⅲ Ⅲ ĳ Ĳ ⅳ Ⅳ ⅸ Ⅸ ı ɪ Ɪ ꟾ ꟷ ᴉ ɨ Ɨ ᵻ ᶖ ꞽ Ꞽ ɩ Ɩ ᵼ j J ĵ Ĵ ǰ ȷ ᴊ
ɉ Ɉ ʝ Ʝ ɟ ʄ k K ḱ Ḱ ǩ Ǩ ķ Ķ ḳ Ḳ ḵ Ḵ ᴋ ᶄ ƙ Ƙ ⱪ Ⱪ ꝁ Ꝁ ꝃ Ꝃ ꝅ Ꝅ ʞ Ʞ l L ĺ Ĺ ľ Ľ
ļ Ļ ł Ł ḷ Ḷ ḹ Ḹ ḽ Ḽ ḻ Ḻ ŀ Ŀ ǉ ǈ Ǉ ỻ Ỻ ʪ ʫ ʟ ꝇ Ꝇ ᴌ ꝉ Ꝉ ƚ Ƚ ⱡ Ⱡ ɫ Ɫ ꬸ ꬹ ɬ Ɬ ꬷ
ᶅ ɭ ꞎ ȴ ꝲ ɮ ꞁ Ꞁ ꟛ Ꟛ ƛ Ƛ ʎ m M ḿ Ḿ ṁ Ṁ ṃ Ṃ ᴍ ᵯ ᶆ ɱ Ɱ ꬺ ꟽ ꟿ ꝳ n N ń Ń ǹ Ǹ ň Ň
ñ Ñ ṅ Ṅ ņ Ņ ṇ Ṇ ṋ Ṋ ṉ Ṉ ǌ ǋ Ǌ № ɴ ᴻ ᴎ ᵰ ɲ Ɲ ƞ Ƞ ꞑ Ꞑ ᶇ ɳ ȵ ꬻ ꝴ ŋ Ŋ ꬼ o O ó Ó
ò Ò ŏ Ŏ ô Ô ố Ố ồ Ồ ỗ Ỗ ổ Ổ ǒ Ǒ ö Ö ȫ Ȫ ő Ő õ Õ ṍ Ṍ ṏ Ṏ ȭ Ȭ ȯ Ȯ ȱ Ȱ ø Ø ǿ Ǿ
ǫ Ǫ ǭ Ǭ ō Ō ṓ Ṓ ṑ Ṑ ỏ Ỏ ȍ Ȍ ȏ Ȏ ơ Ơ ớ Ớ ờ Ờ ỡ Ỡ ở Ở ợ Ợ ọ Ọ ộ Ộ œ Œ ꝏ Ꝏ ᴏ ᴑ
ꬽ ɶ ᴔ ꭁ ꭂ ꭀ ꭃ ꭄ ᴓ ꬾ ɔ Ɔ ᴐ ᴒ ꬿ ᶗ ꭢ ꝍ Ꝍ ᴖ ᴗ ⱺ ɵ Ɵ ꝋ Ꝋ ɷ ꞷ Ꞷ ȣ Ȣ ᴕ p P ṕ Ṕ ṗ Ṗ
₧ ᴘ ᵽ Ᵽ ꝑ Ꝑ ᵱ ᶈ ƥ Ƥ ꝓ Ꝓ ꝕ Ꝕ ꟼ ɸ ⱷ q Q ȹ ꞯ ꝗ Ꝗ ꝙ Ꝙ ʠ ɋ Ɋ ĸ r R ŕ Ŕ ř Ř ṙ Ṙ ŗ
Ŗ ȑ Ȑ ȓ Ȓ ṛ Ṛ ṝ Ṝ ṟ Ṟ ₨ ꭅ ʀ Ʀ ꭆ ᴙ ɍ Ɍ ᵲ ɹ ᴚ ɺ ɻ ⱹ ᶉ ɼ ɽ Ɽ ꭉ ɾ ᵳ ɿ ꭇ ꭈ ꭊ ꭋ ꭌ
ʁ ꭨ ꝵ ꝶ ꝝ Ꝝ s S ś Ś ṥ Ṥ ŝ Ŝ š Š ṧ Ṧ ṡ Ṡ ş Ş ṣ Ṣ ṩ Ṩ ș Ș ß ẞ ﬆ ﬅ ꜱ ꟊ Ꟊ ꟍ Ꟍ ᵴ
ᶊ ʂ Ʂ ȿ Ȿ ꟗ Ꟗ ẜ ẝ ʃ Ʃ ꭍ ᶋ ᶘ ƪ ʅ ʆ t T ť Ť ẗ ṫ Ṫ ţ Ţ ṭ Ṭ ț Ț ṱ Ṱ ṯ Ṯ ʨ ℡ ᵺ ƾ
ʦ ꭧ ʧ ꜩ Ꜩ ᴛ ŧ Ŧ ⱦ Ⱦ ᵵ ƫ ƭ Ƭ ʈ Ʈ ȶ ꝷ ʇ Ʇ u U ú Ú ù Ù ŭ Ŭ û Û ǔ Ǔ ů Ů ü Ü ǘ Ǘ
ǜ Ǜ ǚ Ǚ ǖ Ǖ ű Ű ũ Ũ ṹ Ṹ ų Ų ū Ū ṻ Ṻ ủ Ủ ȕ Ȕ ȗ Ȗ ư Ư ứ Ứ ừ Ừ ữ Ữ ử Ử ự Ự ụ Ụ
ṳ Ṳ ṷ Ṷ ṵ Ṵ ᴜ ꭎ ᴝ ᴞ ᵫ ꭐ ꭑ ʉ Ʉ ꭏ ꞹ Ꞹ ᵾ ᶙ ꭒ ꞿ Ꞿ ɥ Ɥ ʮ ʯ ɯ Ɯ ꟺ ᴟ ɰ ʊ Ʊ ᵿ v V ṽ
Ṽ ṿ Ṿ ⅵ Ⅵ ⅶ Ⅶ ⅷ Ⅷ ꝡ Ꝡ ᴠ ꝟ Ꝟ ᶌ ʋ Ʋ ⱱ ⱴ ỽ Ỽ ʌ Ʌ w W ẃ Ẃ ẁ Ẁ ŵ Ŵ ẘ ẅ Ẅ ẇ Ẇ ẉ Ẉ
ᴡ ⱳ Ⱳ ʍ x X ẍ Ẍ ẋ Ẋ ⅺ Ⅺ ⅻ Ⅻ ᶍ ꭖ ꭗ ꭘ ꭙ ꭓ Ꭓ ꭔ ꭕ y Y ý Ý ỳ Ỳ ŷ Ŷ ẙ ÿ Ÿ ỹ Ỹ ẏ Ẏ
ȳ Ȳ ỷ Ỷ ỵ Ỵ ʏ ɏ Ɏ ƴ Ƴ ỿ Ỿ ꭚ ȝ Ȝ z Z ź Ź ẑ Ẑ ž Ž ż Ż ẓ Ẓ ẕ Ẕ ƍ ᴢ ƶ Ƶ ᵶ ᶎ Ᶎ ȥ
Ȥ ʐ ʑ ɀ Ɀ ⱬ Ⱬ ꝣ Ꝣ ʒ Ʒ ǯ Ǯ ᴣ ƹ Ƹ ᶚ ƺ ʓ þ Þ ꟓ ꟒ ꝥ Ꝥ ꝧ Ꝧ ƿ Ƿ ꟕ ꟔ ꝩ Ꝩ ꭠ ꭡ ꭣ ꝫ Ꝫ
ꝭ Ꝭ ꝯ Ꝯ ꝸ ƻ ꜫ Ꜫ ꜭ Ꜭ ꜯ Ꜯ ƨ Ƨ ƽ Ƽ ƅ Ƅ ʔ ɂ Ɂ ˀ ʼ ŉ ˮ ꜣ Ꜣ ꞌ Ꞌ ꞏ ʕ ꟏ ꟎ ˁ ᴤ ᴥ
ꜥ Ꜥ ʡ ʢ ʖ ǀ ǁ ǂ ǃ ʗ ʘ ʬ ʭ α Α ἀ Ἀ ἄ Ἄ ᾄ ᾌ ἂ Ἂ ᾂ ᾊ ἆ Ἆ ᾆ ᾎ ᾀ ᾈ ἁ Ἁ ἅ Ἅ ᾅ ᾍ ἃ
Ἃ ᾃ ᾋ ἇ Ἇ ᾇ ᾏ ᾁ ᾉ ά Ά ᾴ ὰ Ὰ ᾲ ᾰ Ᾰ ᾶ ᾷ ᾱ Ᾱ ᾳ ᾼ β Β γ Γ ᴦ δ Δ ε Ε ἐ Ἐ ἔ Ἔ ἒ Ἒ
ἑ Ἑ ἕ Ἕ ἓ Ἓ έ Έ ὲ Ὲ ϝ Ϝ ͷ Ͷ ϛ Ϛ ζ Ζ ͱ Ͱ η Η ἠ Ἠ ἤ Ἤ ᾔ ᾜ ἢ Ἢ ᾒ ᾚ ἦ Ἦ ᾖ ᾞ ᾐ ᾘ
ἡ Ἡ ἥ Ἥ ᾕ ᾝ ἣ Ἣ ᾓ ᾛ ἧ Ἧ ᾗ ᾟ ᾑ ᾙ ή Ή ῄ ὴ Ὴ ῂ ῆ ῇ ῃ ῌ θ Θ ι Ι ἰ Ἰ ἴ Ἴ ἲ Ἲ ἶ Ἶ
ἱ Ἱ ἵ Ἵ ἳ Ἳ ἷ Ἷ ί Ί ὶ Ὶ ῐ Ῐ ῖ ϊ Ϊ ΐ ῒ ῗ ῑ Ῑ ϳ Ϳ κ Κ ϗ Ϗ λ Λ ᴧ μ Μ ν Ν ξ Ξ ο
Ο ὀ Ὀ ὄ Ὄ ὂ Ὂ ὁ Ὁ ὅ Ὅ ὃ Ὃ ό Ό ὸ Ὸ π Π ᴨ ϻ Ϻ ϟ Ϟ ϙ Ϙ ρ Ρ ῤ ῥ Ῥ ᴩ ϼ σ Σ ͼ Ͼ ͻ
Ͻ ͽ Ͽ τ Τ υ Υ ὐ ὔ ὒ ὖ ὑ Ὑ ὕ Ὕ ὓ Ὓ ὗ Ὗ ύ Ύ ὺ Ὺ ῠ Ῠ ῦ ϋ Ϋ ΰ ῢ ῧ ῡ Ῡ φ Φ χ Χ ψ
Ψ ᴪ ω Ω ὠ Ὠ ὤ Ὤ ᾤ ᾬ ὢ Ὢ ᾢ ᾪ ὦ Ὦ ᾦ ᾮ ᾠ ᾨ ὡ Ὡ ὥ Ὥ ᾥ ᾭ ὣ Ὣ ᾣ ᾫ ὧ Ὧ ᾧ ᾯ ᾡ ᾩ ώ Ώ
ῴ ὼ Ὼ ῲ ῶ ῷ ῳ ῼ ꭥ ϡ Ϡ ͳ Ͳ ϸ Ϸ ϣ Ϣ ϥ Ϥ ϧ Ϧ ϩ Ϩ ϫ Ϫ ϭ Ϭ ϯ Ϯ а А ӑ Ӑ ӓ Ӓ ә Ә ӛ
Ӛ ӕ Ӕ б Б в В г Г ѓ Ѓ ғ Ғ ӻ Ӻ ҕ Ҕ ӷ Ӷ д Д ԁ Ԁ ꚁ Ꚁ ђ Ђ ꙣ Ꙣ ԃ Ԃ ҙ Ҙ е Е ѐ Ѐ ӗ
Ӗ ё Ё є Є ж Ж ӂ Ӂ ӝ Ӝ ԫ Ԫ ꚅ Ꚅ җ Җ з З ӟ Ӟ ꙁ Ꙁ ԅ Ԅ ԑ Ԑ ꙃ Ꙃ ѕ Ѕ ꙅ Ꙅ ӡ Ӡ ꚉ Ꚉ ԇ
Ԇ ꚃ Ꚃ и И ѝ Ѝ ӥ Ӥ ӣ Ӣ ҋ Ҋ і І ї Ї ꙇ Ꙇ й Й ј Ј ꙉ Ꙉ к К ќ Ќ қ Қ ӄ Ӄ ҡ Ҡ ҟ Ҟ ҝ
Ҝ ԟ Ԟ ԛ Ԛ л Л ᴫ ӆ Ӆ ԯ Ԯ ԓ Ԓ ԡ Ԡ љ Љ ꙥ Ꙥ ԉ Ԉ ԕ Ԕ м М ӎ Ӎ ꙧ Ꙧ н Н ԩ Ԩ ӊ Ӊ ң Ң
ӈ Ӈ ԣ Ԣ ҥ Ҥ њ Њ ԋ Ԋ о О ӧ Ӧ ө Ө ӫ Ӫ п П ԥ Ԥ ҧ Ҧ ҁ Ҁ р Р ҏ Ҏ ԗ Ԗ с С ⷵ ԍ Ԍ ҫ
Ҫ т Т ꚍ Ꚍ ᲊ Ᲊ ԏ Ԏ ҭ Ҭ ꚋ Ꚋ ћ Ћ у У ў Ў ӱ Ӱ ӳ Ӳ ӯ Ӯ ү Ү ұ Ұ ꙋ Ꙋ ѹ Ѹ ф Ф х Х ӽ
Ӽ ӿ Ӿ ҳ Ҳ һ Һ ԧ Ԧ ꚕ Ꚕ ѡ Ѡ ѿ Ѿ ꙍ Ꙍ ѽ Ѽ ѻ Ѻ ц Ц ꙡ Ꙡ ꚏ Ꚏ ҵ Ҵ ꚑ Ꚑ ч Ч ӵ Ӵ ԭ Ԭ ꚓ
Ꚓ ҷ Ҷ ӌ Ӌ ҹ Ҹ ꚇ Ꚇ ҽ Ҽ ҿ Ҿ џ Џ ш Ш ꚗ Ꚗ щ Щ ꙏ Ꙏ ꙿ ъ Ъ ꙑ Ꙑ ы Ы ӹ Ӹ ь Ь ҍ Ҍ ѣ Ѣ
ꙓ Ꙓ э Э ӭ Ӭ ю Ю ꙕ Ꙕ ꙗ Ꙗ я Я ԙ Ԙ ѥ Ѥ ѧ Ѧ ꙙ Ꙙ ѫ Ѫ ꙛ Ꙛ ѩ Ѩ ꙝ Ꙝ ѭ Ѭ ѯ Ѯ ѱ Ѱ ѳ Ѳ
ѵ Ѵ ѷ Ѷ ꙟ Ꙟ ҩ Ҩ ԝ Ԝ ӏ Ӏ|}

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
