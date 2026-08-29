open Geneweb
open Alcotest

let normalize () =
  (check string) "" "foo-bar, boobar (baz)"
    (Place.normalize "[foo-bar] - boobar (baz)");
  (check string) "" "[foo-bar - boobar (baz)"
    (Place.normalize "[foo-bar - boobar (baz)");
  (check string) "" "[foo-bar] boobar (baz)"
    (Place.normalize "[foo-bar] boobar (baz)");
  ()

let split_suburb () =
  (check (pair string string))
    ""
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] - boobar (baz)");
  (check (pair string string))
    "test split suburb emdash - 93"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] – boobar (baz)");
  (check (pair string string))
    "test split suburb endash - 94"
    ("foo-bar", "boobar (baz)")
    (Place.split_suburb "[foo-bar] — boobar (baz)");
  (check (pair string string))
    "" ("", "boobar (baz)")
    (Place.split_suburb "boobar (baz)");
  ()

let only_suburb () =
  (check string) "" "foo-bar" (Place.only_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "" (Place.only_suburb "boobar (baz)");
  ()

let without_suburb () =
  (check string) "" "boobar (baz)"
    (Place.without_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "boobar (baz)" (Place.without_suburb "boobar (baz)");
  ()

let compare_places () =
  (check int) "" 0 (Place.compare_places "boobar (baz)" "boobar (baz)");
  (check int) "" (-1) (Place.compare_places "baz (boobar)" "boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[bar-foo] - baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz (boobar)" "[bar-foo] - boobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - ebaz (boobar)"
       "[bar-foo] - éboobar (baz)");
  (check int) "" (-1)
    (Place.compare_places "[foo-bar] - baz, boobar, barboo"
       "[foo-bar] - baz, boobar, barboo, bam");
  ()

let normalize_place () =
  (* Canonical leaf-first form; must byte-match m=L p0/p%d and *_place_norm. *)
  (check string) "berlin false" "Berlin, Allemagne"
    (Place.normalize_place false "Berlin, Allemagne");
  (check string) "berlin true" "Allemagne, Berlin"
    (Place.normalize_place true "Berlin, Allemagne");
  (check string) "paren false" "Paris, 75"
    (Place.normalize_place false "Paris (75)");
  (check string) "paren true" "75, Paris"
    (Place.normalize_place true "Paris (75)");
  (check string) "suburb false" "Paris 16e"
    (Place.normalize_place false "[Hameau Boileau] - Paris 16e");
  (check string) "suburb true" "Paris 16e"
    (Place.normalize_place true "[Hameau Boileau] - Paris 16e");
  (check string) "three false" "Paris, \195\142le-de-France, France"
    (Place.normalize_place false "Paris, \195\142le-de-France, France");
  (check string) "three true" "France, \195\142le-de-France, Paris"
    (Place.normalize_place true "Paris, \195\142le-de-France, France");
  ()

let fold_place_long () =
  (check (pair (list string) string))
    "fold non-inverted"
    ([ "France"; "Paris" ], "")
    (Place.fold_place_long false "Paris, France");
  (check (pair (list string) string))
    "fold inverted"
    ([ "Paris"; "France" ], "")
    (Place.fold_place_long true "Paris, France");
  (check (pair (list string) string))
    "fold paren"
    ([ "75"; "Paris" ], "")
    (Place.fold_place_long false "Paris (75)");
  (check (pair (list string) string))
    "fold suburb"
    ([ "Paris 16e" ], "Hameau Boileau")
    (Place.fold_place_long false "[Hameau Boileau] - Paris 16e");
  ()

let v =
  [
    ("place-normalize", [ test_case "Place normalize" `Quick normalize ]);
    ( "place-normalize-place",
      [ test_case "Place normalize_place ordering" `Quick normalize_place ] );
    ( "place-fold-long",
      [ test_case "Place fold_place_long ordering" `Quick fold_place_long ] );
    ( "place-split-suburb",
      [ test_case "Place split suburb" `Quick split_suburb ] );
    ("place-only-suburb", [ test_case "Place only suburb" `Quick only_suburb ]);
    ( "place-without-suburb",
      [ test_case "Place without suburb" `Quick without_suburb ] );
    ("place-compare", [ test_case "Place compare" `Quick compare_places ]);
  ]
