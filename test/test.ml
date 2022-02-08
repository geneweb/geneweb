open OUnit2

let _ =
  run_test_tt_main begin
    "Geneweb" >:::
    []
    @ Test_calendar.suite
    @ Test_utils.suite
    @ Test_sosa.suite
    @ Test_place.suite
    @ Test_wiki.suite
  end

