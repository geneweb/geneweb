(* Test suite for GeneWeb declension mechanism *)
open Geneweb
open Alcotest

(* Helper function to create a test config with Czech language *)
let make_czech_conf () =
  let lexicon = Hashtbl.create 100 in
  (* Add some basic Czech translations with declensions *)
  Hashtbl.add lexicon "add" "přidat :a:";
  Hashtbl.add lexicon "with" "s :i:";
  Hashtbl.add lexicon "person/persons" "osoba/osob";
  Hashtbl.add lexicon "on %s's side" "ze strany :g:%s";
  Hashtbl.add lexicon "%1 of %2" "%1 :g:%2";
  Hashtbl.add lexicon "marriage between %s and %s"
    "manželství mezi :a:%s a :g:%s";
  Config.{ empty with lang = "cs"; lexicon }

let make_english_conf () =
  let lexicon = Hashtbl.create 100 in
  Hashtbl.add lexicon "add" "adde";
  Hashtbl.add lexicon "with" "with";
  Hashtbl.add lexicon "test before" "test before +before";
  Hashtbl.add lexicon "person/persons" "eperson/epersons";
  Hashtbl.add lexicon "on %s's side" "on %s's side";
  Hashtbl.add lexicon "%1 of %2" "%1 of %2";
  Hashtbl.add lexicon "married%t to" "married%t to";
  Config.{ empty with lang = "en"; lexicon }

(* ============================================ *)
(* Phase 1: Unit tests for basic functions     *)
(* ============================================ *)

(* ============================================ *)
(* Phase 1: Direct Mutil.decline tests         *)
(* Testing with Translate.eval                 *)
(* ============================================ *)

(* Test 1.1: Mutil.decline + Translate.eval *)
let test_mutil_decline_accusative () =
  let name = "Vladana:a:-u:g:-y" in
  let declined = Mutil.decline 'a' name in
  let result = Translate.eval declined in
  (check string) "accusative of Vladana" "Vladanu" result

let test_mutil_decline_genitive () =
  let name = "Vladana:a:-u:g:-y" in
  let declined = Mutil.decline 'g' name in
  let result = Translate.eval declined in
  (check string) "genitive of Vladana" "Vladany" result

let test_mutil_decline_instrumental () =
  let name = "Jan:i:+em:g:+a" in
  let declined = Mutil.decline 'i' name in
  let result = Translate.eval declined in
  (check string) "instrumental of Jan" "Janem" result

let test_mutil_decline_genitive_with_subtraction () =
  let name = "Novák:g:-ka" in
  let declined = Mutil.decline 'g' name in
  let result = Translate.eval declined in
  (check string) "genitive of Novák" "Nováka" result

let test_mutil_decline_unknown_case () =
  let name = "Vladana:a:-u:g:-y" in
  let declined = Mutil.decline 'x' name in
  let result = Translate.eval declined in
  (* Unknown case should return nominative *)
  (check string) "unknown case returns nominative" "Vladana" result

let test_mutil_decline_no_declension () =
  let name = "Marie" in
  let declined = Mutil.decline 'g' name in
  let result = Translate.eval declined in
  (check string) "name without declension" "Marie" result

(* Test 1.2: gen_decline_basic - Pattern :X: at the end *)
let test_gen_decline_basic_accusative () =
  let wt = "přidat :a:" in
  let name = "Vladana:a:-u:g:-y" in
  let result = Util.gen_decline_basic wt name in
  (check string) "přidat + accusative" "přidat Vladanu" (Translate.eval result)

let test_gen_decline_basic_genitive () =
  let wt = "ze strany :g:" in
  let name = "Jan:g:+a" in
  let result = Util.gen_decline_basic wt name in
  (check string) "ze strany + genitive" "ze strany Jana" (Translate.eval result)

let test_gen_decline_basic_instrumental () =
  let wt = "s :i:" in
  let name = "Marie:i:-í" in
  let result = Util.gen_decline_basic wt name in
  (check string) "s + instrumental" "s Marií" (Translate.eval result)

let test_gen_decline_basic_no_pattern () =
  let wt = "some text" in
  let name = "Vladana:a:-u:g:-y" in
  let result = Util.gen_decline_basic wt name in
  (check string) "no pattern should add nominative" "some text Vladana"
    (Translate.eval result)

let test_gen_decline_basic_empty_name () =
  let wt = "přidat :a:" in
  let result = Util.gen_decline_basic wt "" in
  (* When name is empty, the space before :a: remains *)
  (check string) "empty name returns template with trailing space" "přidat "
    (Translate.eval result)

(* Test 1.3: gen_decline - Pattern :X:%Y with parameters *)
let test_gen_decline_simple_two_params () =
  let conf = make_czech_conf () in
  let wt = "%1 :g:%2" in
  let s1 = "syn" in
  let s2 = "Vladana:a:-u:g:-y" in
  let result = Util.gen_decline conf wt s1 s2 s2 in
  (check string) "%1 of %2 in Czech" "syn Vladany" (Translate.eval result)

let test_gen_decline_two_genitive_patterns () =
  let conf = make_czech_conf () in
  let wt = ":g:%1 a :g:%2" in
  let s1 = "Petr:g:+a" in
  let s2 = "Jan:g:+a" in
  let result = Util.gen_decline conf wt s1 s2 s2 in
  (check string) "two genitive patterns" "Petra a Jana" (Translate.eval result)

let test_gen_decline_only_param1 () =
  let conf = make_czech_conf () in
  let wt = "%1" in
  let s1 = "syn" in
  let s2 = "Vladana" in
  let result = Util.gen_decline conf wt s1 s2 s2 in
  (check string) "only %1 parameter" "syn" (Translate.eval result)

(* ============================================ *)
(* Phase 2: Integration tests with lexicon     *)
(* ============================================ *)

(* Test 2.1: transl - Lexicon lookup *)
let test_transl_czech () =
  let conf = make_czech_conf () in
  (check string) "translate 'add' to Czech" "přidat :a:"
    (Util.transl conf "add")

let test_transl_czech_with_param () =
  let conf = make_czech_conf () in
  (check string) "translate with parameter" "ze strany :g:%s"
    (Util.transl conf "on %s's side")

let test_transl_unknown_key () =
  let conf = make_czech_conf () in
  let result = Util.transl conf "unknown_key" in
  (* Should return the key itself or a "not found" marker *)
  (check bool) "unknown key handled" true (String.length result > 0)

let test_transl_english_no_declension () =
  let conf = make_english_conf () in
  (check string) "English has no declension codes" "adde"
    (Util.transl conf "add");
  let result = Util.transl_decline conf "test before" "name" in
  (check string) "English test before " "name test before" result

(* Test 2.2: transl_decline - Translation + declension *)
let test_transl_decline_czech () =
  let conf = make_czech_conf () in
  let name = "rodina:a:-u" in
  (check string) "add family in Czech" "přidat rodinu"
    (Util.transl_decline conf "add" name)

let test_transl_decline_no_declension_in_name () =
  let conf = make_czech_conf () in
  let name = "Maria" in
  (check string) "name without declension" "přidat Maria"
    (Util.transl_decline conf "add" name)

let test_transl_decline_english () =
  let conf = make_english_conf () in
  let name = "rodina:a:-u" in
  (* English should ignore declension codes and use nominative *)
  (check string) "English ignores declension" "adde rodina"
    (Util.transl_decline conf "add" name)

(* Test 2.3: transl_a_of_b - Composed expression *)
let test_transl_a_of_b_czech () =
  let conf = make_czech_conf () in
  let s1 = "syn" in
  let s2_nominative = "Vladana" in
  let s2_with_decl = "Vladana:a:-u:g:-y" in
  (* y1 should have declensions, y2 is the raw form *)
  let result = Util.transl_a_of_b conf s1 s2_with_decl s2_nominative in
  (check string) "son of Vladana in Czech" "syn Vladany" (Translate.eval result)

let test_transl_a_of_b_with_declensions () =
  let conf = make_czech_conf () in
  let s1 = "bratr" in
  let s2 = "Jan" in
  let s2_decl = "Jan:g:+a" in
  (* In "%1 of %2", only %2 is declined (genitive), %1 stays nominative *)
  let result = Util.transl_a_of_b conf s1 s2_decl s2 in
  (check string) "second name declined to genitive" "bratr Jana"
    (Translate.eval result)

(* ============================================ *)
(* Phase 3: Edge cases and robustness          *)
(* ============================================ *)

(* Test 4.1: Missing declensions *)
let test_missing_declension_graceful () =
  let name = "John" in
  let result = Translate.eval (Mutil.decline 'g' name) in
  (* Should not crash, return nominative *)
  (check string) "missing declension returns nominative" "John" result

(* Test 4.2: Malformed patterns *)
let test_malformed_pattern_no_crash () =
  let wt = "přidat :a" in
  (* Missing final colon *)
  let name = "Vladana" in
  (* Should not crash *)
  let result = Util.gen_decline_basic wt name in
  (check bool) "malformed pattern doesn't crash" true (String.length result > 0)

(* Test 4.3: Unicode characters *)
let test_unicode_in_names () =
  let name = "Lukáš:g:--še" in
  let result = Translate.eval (Mutil.decline 'g' name) in
  (check string) "Unicode characters preserved" "Lukáše" result

let test_unicode_czech_chars () =
  let name = "Václav:g:-va" in
  let result = Translate.eval (Mutil.decline 'g' name) in
  (check string) "Czech characters with háček" "Václava" result

(* Test 4.4: Empty strings *)
let test_empty_string_decline () =
  let result = Translate.eval (Mutil.decline 'g' "") in
  (check string) "empty string returns empty" "" result

let test_gen_decline_empty_template () =
  let conf = make_czech_conf () in
  let result = Util.gen_decline conf "" "a" "b" "b" in
  (check string) "empty template" "" (Translate.eval result)

(* Test 4.5: Multiple cases in same expression *)
let test_multiple_cases_in_expression () =
  let conf = make_czech_conf () in
  let wt = ":i:%1 a :g:%2" in
  let s1 = "Jan:i:+em:g:+a" in
  let s2 = "Petr:i:+em:g:+a" in
  let result = Util.gen_decline conf wt s1 s2 s2 in
  (check string) "instrumental and genitive" "Janem a Petra"
    (Translate.eval result)

(* Test 4.6: Long declension chains *)
let test_long_declension_chain () =
  let name = "Vladana:a:-u:g:-y:d:-ě:i:-ou:l:+ě:v:+o" in
  (check string) "accusative from long chain" "Vladanu"
    (Translate.eval (Mutil.decline 'a' name));
  (check string) "genitive from long chain" "Vladany"
    (Translate.eval (Mutil.decline 'g' name));
  (check string) "instrumental from long chain" "Vladanou"
    (Translate.eval (Mutil.decline 'i' name))

(* ============================================ *)
(* Phase 4: Multilingual tests                 *)
(* ============================================ *)

let test_same_name_different_languages () =
  let name = "Vladana:a:-u:g:-y" in
  let conf_cs = make_czech_conf () in
  let conf_en = make_english_conf () in
  (* Czech should apply declension *)
  (check string) "Czech with declension" "přidat Vladanu"
    (Util.transl_decline conf_cs "add" name);
  (* English should use nominative *)
  (check string) "English without declension" "adde Vladana"
    (Util.transl_decline conf_en "add" name)

(* ============================================ *)
(* Phase 5: Tests for apply_decline function  *)
(* ============================================ *)

(* Note: These tests assume the existence of a function:
   val apply_decline : string -> string -> string
   that applies Mutil.decline followed by Translate.eval
   
   This simulates the %apply;decline(case, string) template function *)

let apply_decline case_str name =
  let case_char = if String.length case_str > 0 then case_str.[0] else 'n' in
  Translate.eval (Mutil.decline case_char name)

let test_apply_decline_accusative () =
  let name = "Vladana:a:-u:g:-y" in
  let result = apply_decline "a" name in
  (check string) "apply_decline accusative" "Vladanu" result

let test_apply_decline_genitive () =
  let name = "Vladana:a:-u:g:-y" in
  let result = apply_decline "g" name in
  (check string) "apply_decline genitive" "Vladany" result

let test_apply_decline_all_cases () =
  let name = "Vladana:a:-u:g:-y:d:-ě:i:-ou:l:+ě:v:+o" in
  let result_a = apply_decline "a" name in
  let result_g = apply_decline "g" name in
  let result_d = apply_decline "d" name in
  let result_i = apply_decline "i" name in
  (check string) "all cases accusative" "Vladanu" result_a;
  (check string) "all cases genitive" "Vladany" result_g;
  (check string) "all cases dative" "Vladaně" result_d;
  (check string) "all cases instrumental" "Vladanou" result_i

let test_apply_decline_no_declensions () =
  let name = "Marie" in
  let result = apply_decline "g" name in
  (* Without declensions, should return nominative unchanged *)
  (check string) "apply_decline without declensions" "Marie" result

let test_apply_decline_invalid_case () =
  let name = "Vladana:a:-u:g:-y" in
  let result = apply_decline "z" name in
  (* Invalid case should return nominative *)
  (check string) "apply_decline invalid case" "Vladana" result

let test_apply_decline_utf8 () =
  let name = "Lukáš:g:--še" in
  let result = apply_decline "g" name in
  (check string) "apply_decline UTF-8 characters" "Lukáše" result

let test_apply_decline_multiple_transforms () =
  let name = "Jan:g:+a:i:+em" in
  let result_g = apply_decline "g" name in
  let result_i = apply_decline "i" name in
  (check string) "multiple transforms genitive" "Jana" result_g;
  (check string) "multiple transforms instrumental" "Janem" result_i

(* Test du remplacement *)
let test_replacement () =
  let name = "Henri:a:Aaaa:i:Iiii" in
  let result_a = apply_decline "a" name in
  let result_i = apply_decline "i" name in
  (check string) "replacement: accusative" "Aaaa" result_a;
  (check string) "replacement: instrumental" "Iiii" result_i

(* Test du remplacement *)
let test_mixing () =
  let name = "Henri:a:Aaaa:i:+Iiii" in
  let result_a = apply_decline "a" name in
  let result_i = apply_decline "i" name in
  (check string) "replacement: accusative" "Aaaa" result_a;
  (check string) "replacement: instrumental" "Iiii" result_i

(* Test de la traduction/declinaison *)
let test_decline () =
  let _name = "Jan:g:+a:i:+em" in
  let conf_cs = make_czech_conf () in
  let conf_en = make_english_conf () in
  let s = "on %s's side:::Jan:g:+a:i:+em" in
  let result_a = Templ.eval_transl conf_cs true s "0" in
  (* "ze strany :g:%s"  *)
  (check string) "translate decline" "Ze strany Jana" result_a;

  let s = "on %s's side:::[person/persons]" in
  let result_b = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate ::: translate 1" "Ze strany osob" result_b;

  let s = "on %s's side:::[person/persons]1" in
  let result_b = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate :: translate 2" "Ze strany osob" result_b;

  let s = "on %s's side:::[person/persons]0" in
  let result_b = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate :: translate 3" "Ze strany osoba" result_b;

  let s = "on %s's side::person/persons" in
  let result_b = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate :: translate 4" "Ze strany osob" result_b;

  let s = "on %s's side:::aaa [person/persons] bbb" in
  let result_c = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate ::: aaa translate bbb 5" "Ze strany aaa osob bbb"
    result_c;

  let s = "on %s's side:::aaa [person/persons]0 bbb" in
  let result_d = Templ.eval_transl conf_cs true s "1" in
  (check string) "translate ::: aaa translate/0 bbb 6" "Ze strany aaa osoba bbb"
    result_d;

  let s = "add:::[person/persons]" in
  let result_d = Templ.eval_transl conf_en true s "1" in
  (check string) "translate ::: translate 7" "Adde epersons" result_d;

  let s = "add::person/persons" in
  let result_d = Templ.eval_transl conf_en true s "1" in
  (check string) "translate :: translate 8" "Adde epersons" result_d;

  let s = "marriage between %s and %s:::Vladana:a:-u:g:-y:Lukáš:g:--še" in
  let result_d = Templ.eval_transl conf_cs true s "0" in
  (check string) "translate :: translate 9" "Manželství mezi Vladanu a Lukáše"
    result_d;

  let s = "married%t to::" in
  let result_d = Templ.eval_transl conf_en true s "0" in
  (check string) "translate :: empty param 10" "Married to" result_d

(* ============================================ *)
(* Test suite definition                        *)
(* ============================================ *)

let v =
  [
    (* Phase 1: Direct Mutil.decline tests *)
    ( "mutil-decline-basic",
      [
        test_case "Decline accusative" `Quick test_mutil_decline_accusative;
        test_case "Decline genitive" `Quick test_mutil_decline_genitive;
        test_case "Decline instrumental" `Quick test_mutil_decline_instrumental;
        test_case "Decline with subtraction" `Quick
          test_mutil_decline_genitive_with_subtraction;
        test_case "Unknown case returns nominative" `Quick
          test_mutil_decline_unknown_case;
        test_case "No declension defined" `Quick
          test_mutil_decline_no_declension;
      ] );
    ( "gen-decline-basic",
      [
        test_case "Pattern :a: accusative" `Quick
          test_gen_decline_basic_accusative;
        test_case "Pattern :g: genitive" `Quick test_gen_decline_basic_genitive;
        test_case "Pattern :i: instrumental" `Quick
          test_gen_decline_basic_instrumental;
        test_case "No pattern in template" `Quick
          test_gen_decline_basic_no_pattern;
        test_case "Empty name" `Quick test_gen_decline_basic_empty_name;
      ] );
    ( "gen-decline-params",
      [
        test_case "Simple two parameters" `Quick
          test_gen_decline_simple_two_params;
        test_case "Two genitive patterns" `Quick
          test_gen_decline_two_genitive_patterns;
        test_case "Only param1" `Quick test_gen_decline_only_param1;
      ] );
    (* Phase 2: Integration with lexicon *)
    ( "transl-lexicon",
      [
        test_case "Translate to Czech" `Quick test_transl_czech;
        test_case "Translate with parameter" `Quick test_transl_czech_with_param;
        test_case "Unknown key" `Quick test_transl_unknown_key;
        test_case "English no declension" `Quick
          test_transl_english_no_declension;
      ] );
    ( "transl-decline",
      [
        test_case "Translate and decline Czech" `Quick test_transl_decline_czech;
        test_case "Name without declension" `Quick
          test_transl_decline_no_declension_in_name;
        test_case "English ignores declension" `Quick
          test_transl_decline_english;
      ] );
    ( "transl-composed",
      [
        test_case "A of B in Czech" `Quick test_transl_a_of_b_czech;
        test_case "Second name declined" `Quick
          test_transl_a_of_b_with_declensions;
      ] );
    (* Phase 3: Edge cases *)
    ( "declension-edge-cases",
      [
        test_case "Missing declension graceful" `Quick
          test_missing_declension_graceful;
        test_case "Malformed pattern no crash" `Quick
          test_malformed_pattern_no_crash;
        test_case "Unicode in names" `Quick test_unicode_in_names;
        test_case "Czech characters" `Quick test_unicode_czech_chars;
        test_case "Empty string" `Quick test_empty_string_decline;
        test_case "Empty template" `Quick test_gen_decline_empty_template;
        test_case "Multiple cases" `Quick test_multiple_cases_in_expression;
        test_case "Long declension chain" `Quick test_long_declension_chain;
      ] );
    (* Phase 4: Multilingual *)
    ( "declension-multilingual",
      [
        test_case "Same name different languages" `Quick
          test_same_name_different_languages;
      ] );
    (* Phase 5: apply_decline function *)
    ( "apply-decline",
      [
        test_case "Apply decline accusative" `Quick
          test_apply_decline_accusative;
        test_case "Apply decline genitive" `Quick test_apply_decline_genitive;
        test_case "Apply decline all cases" `Quick test_apply_decline_all_cases;
        test_case "Apply decline without declensions" `Quick
          test_apply_decline_no_declensions;
        test_case "Apply decline invalid case" `Quick
          test_apply_decline_invalid_case;
        test_case "Apply decline UTF-8" `Quick test_apply_decline_utf8;
        test_case "Apply decline multiple transforms" `Quick
          test_apply_decline_multiple_transforms;
        test_case "Apply replacement" `Quick test_replacement;
        test_case "Apply mixing" `Quick test_mixing;
        test_case "Apply translate decline" `Quick test_decline;
      ] );
  ]
