open OUnit2

let _ =
  run_test_tt_main
    ("Geneweb"
    >::: [] @ Test_chronology.suite
         @ Test_utils.suite
         @ Test_wiki.suite)

let () =
  let test_suite =
    Place_test.v @ Date_test.v @ Sosa_test.v
  in
  try
    Alcotest.run ~and_exit:false "Geneweb" test_suite
  with Alcotest.Test_error -> ()
