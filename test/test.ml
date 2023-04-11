open OUnit2

let _ =
  run_test_tt_main
    ("Geneweb"
    >::: [] @ Test_calendar.suite @ Test_chronology.suite @ Test_mergeInd.suite
         @ Test_place.suite @ Test_sosa.suite @ Test_utils.suite
         @ Test_wiki.suite)
