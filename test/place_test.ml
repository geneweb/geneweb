let normalize () =
  (Alcotest.check Alcotest.string)
    "" "foo-bar, boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar] - boobar (baz)");
  (Alcotest.check Alcotest.string)
    "" "[foo-bar - boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar - boobar (baz)");
  (Alcotest.check Alcotest.string)
    "" "[foo-bar] boobar (baz)"
    (Geneweb.Place.normalize "[foo-bar] boobar (baz)");
  ()

let split_suburb () =
  (Alcotest.check (Alcotest.pair Alcotest.string Alcotest.string))
    ""
    ("foo-bar", "boobar (baz)")
    (Geneweb.Place.split_suburb "[foo-bar] - boobar (baz)");
  (Alcotest.check (Alcotest.pair Alcotest.string Alcotest.string))
    "" ("", "boobar (baz)")
    (Geneweb.Place.split_suburb "boobar (baz)");
  ()

let only_suburb () =
  (Alcotest.check Alcotest.string)
    "" "foo-bar"
    (Geneweb.Place.only_suburb "[foo-bar] - boobar (baz)");
  (Alcotest.check Alcotest.string)
    "" ""
    (Geneweb.Place.only_suburb "boobar (baz)");
  ()

let without_suburb () =
  (Alcotest.check Alcotest.string)
    "" "boobar (baz)"
    (Geneweb.Place.without_suburb "[foo-bar] - boobar (baz)");
  (Alcotest.check Alcotest.string)
    "" "boobar (baz)"
    (Geneweb.Place.without_suburb "boobar (baz)");
  ()

let compare_places () =
  (Alcotest.check Alcotest.int)
    "" 0
    (Geneweb.Place.compare_places "boobar (baz)" "boobar (baz)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "baz (boobar)" "boobar (baz)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "baz (boobar)" "[foo-bar] - baz (boobar)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "[bar-foo] - baz (boobar)"
       "[foo-bar] - baz (boobar)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - baz (boobar)"
       "[bar-foo] - boobar (baz)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - ebaz (boobar)"
       "[bar-foo] - éboobar (baz)");
  (Alcotest.check Alcotest.int)
    "" (-1)
    (Geneweb.Place.compare_places "[foo-bar] - baz, boobar, barboo"
       "[foo-bar] - baz, boobar, barboo, bam");
  ()

let v =
  [
    ( "place-normalize",
      [ Alcotest.test_case "Place normalize" `Quick normalize ] );
    ( "place-split-suburb",
      [ Alcotest.test_case "Place split suburb" `Quick split_suburb ] );
    ( "place-only-suburb",
      [ Alcotest.test_case "Place only suburb" `Quick only_suburb ] );
    ( "place-without-suburb",
      [ Alcotest.test_case "Place without suburb" `Quick without_suburb ] );
    ( "place-compare",
      [ Alcotest.test_case "Place compare" `Quick compare_places ] );
  ]
