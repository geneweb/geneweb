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

let v =
  [
    ("place-normalize", [ test_case "Place normalize" `Quick normalize ]);
    ( "place-split-suburb",
      [ test_case "Place split suburb" `Quick split_suburb ] );
    ("place-only-suburb", [ test_case "Place only suburb" `Quick only_suburb ]);
    ( "place-without-suburb",
      [ test_case "Place without suburb" `Quick only_suburb ] );
    ("place-compare", [ test_case "Place compare" `Quick compare_places ]);
  ]
