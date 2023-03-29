open OUnit2

let _ =
  run_test_tt_main (
  "Geneweb" >::: List.flatten [
    Test_calendar.suite;
    Test_mergeInd.suite;
    Test_place.suite;
    Test_sosa.suite;
    Test_utils.suite;
    Test_wiki.suite
  ])

