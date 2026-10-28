let normalize () =
  let cases =
    [
      ("[foo-bar] - boobar (baz)", "foo-bar, boobar (baz)");
      ("[foo-bar - boobar (baz)", "[foo-bar - boobar (baz)");
      ("[foo-bar] boobar (baz)", "[foo-bar] boobar (baz)");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Geneweb_test_utils.Check.string expected (Geneweb.Place.normalize input))
    cases

let split_suburb () =
  let cases =
    [
      ("[foo-bar] - boobar (baz)", ("foo-bar", "boobar (baz)"));
      ("boobar (baz)", ("", "boobar (baz)"));
    ]
  in
  List.iter
    (fun (input, expected) ->
      Geneweb_test_utils.Check.string_pair expected
        (Geneweb.Place.split_suburb input))
    cases

let only_suburb () =
  let cases =
    [ ("[foo-bar] - boobar (baz)", "foo-bar"); ("boobar (baz)", "") ]
  in
  List.iter
    (fun (input, expected) ->
      Geneweb_test_utils.Check.string expected (Geneweb.Place.only_suburb input))
    cases

let without_suburb () =
  let cases =
    [
      ("[foo-bar] - boobar (baz)", "boobar (baz)");
      ("boobar (baz)", "boobar (baz)");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Geneweb_test_utils.Check.string expected
        (Geneweb.Place.without_suburb input))
    cases

let compare_places () =
  let cases =
    [
      ("boobar (baz)", "boobar (baz)", 0);
      ("baz (boobar)", "boobar (baz)", -1);
      ("baz (boobar)", "[foo-bar] - baz (boobar)", -1);
      ("[bar-foo] - baz (boobar)", "[foo-bar] - baz (boobar)", -1);
      ("[foo-bar] - baz (boobar)", "[bar-foo] - boobar (baz)", -1);
      ("[foo-bar] - ebaz (boobar)", "[bar-foo] - éboobar (baz)", -1);
      ( "[foo-bar] - baz, boobar, barboo",
        "[foo-bar] - baz, boobar, barboo, bam",
        -1 );
    ]
  in
  List.iter
    (fun (p1, p2, expected) ->
      Geneweb_test_utils.Check.int expected (Geneweb.Place.compare_places p1 p2))
    cases

let v =
  [
    Geneweb_test_utils.Check.suite "place-normalize"
      [ Geneweb_test_utils.Check.case "Place normalize" normalize ];
    Geneweb_test_utils.Check.suite "place-split-suburb"
      [ Geneweb_test_utils.Check.case "Place split suburb" split_suburb ];
    Geneweb_test_utils.Check.suite "place-only-suburb"
      [ Geneweb_test_utils.Check.case "Place only suburb" only_suburb ];
    Geneweb_test_utils.Check.suite "place-without-suburb"
      [ Geneweb_test_utils.Check.case "Place without suburb" without_suburb ];
    Geneweb_test_utils.Check.suite "place-compare"
      [ Geneweb_test_utils.Check.case "Place compare" compare_places ];
  ]
