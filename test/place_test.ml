let normalize () =
  let open Alcotest in
  (check string) "" "foo-bar, boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar] - boobar (baz)");
  (check string) "" "[foo-bar - boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar - boobar (baz)");
  (check string) "" "[foo-bar] boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar] boobar (baz)");
  ()

let split_suburb () =
  let open Alcotest in
  (check (pair string string))
    ""
    ("foo-bar", "boobar (baz)")
    (Geneweb.Place.split_suburb "[foo-bar] - boobar (baz)");
  (check (pair string string))
    "" ("", "boobar (baz)")
    (Geneweb.Place.split_suburb "boobar (baz)");
  ()

let only_suburb () =
  let open Alcotest in
  (check string) "" "foo-bar"
    (Geneweb.Place.only_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "" (Geneweb.Place.only_suburb "boobar (baz)");
  ()

let without_suburb () =
  let open Alcotest in
  (check string) "" "boobar (baz)"
    (Geneweb.Place.without_suburb "[foo-bar] - boobar (baz)");
  (check string) "" "boobar (baz)" (Geneweb.Place.without_suburb "boobar (baz)");
  ()

let compare_places () =
  let open Alcotest in
  (check int) "" 0 (Geneweb.Place.compare_places "boobar (baz)" "boobar (baz)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "baz (boobar)" "boobar (baz)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "baz (boobar)" "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "[bar-foo] - baz (boobar)"
       "[foo-bar] - baz (boobar)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - baz (boobar)"
       "[bar-foo] - boobar (baz)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - ebaz (boobar)"
       "[bar-foo] - éboobar (baz)");
  (check int) "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - baz, boobar, barboo"
       "[foo-bar] - baz, boobar, barboo, bam");
  ()

let v =
  let open Alcotest in
  [
    ("place-normalize", [ test_case "Place normalize" `Quick normalize ]);
    ( "place-split-suburb",
      [ test_case "Place split suburb" `Quick split_suburb ] );
    ("place-only-suburb", [ test_case "Place only suburb" `Quick only_suburb ]);
    ( "place-without-suburb",
      [ test_case "Place without suburb" `Quick without_suburb ] );
    ("place-compare", [ test_case "Place compare" `Quick compare_places ]);
  ]
