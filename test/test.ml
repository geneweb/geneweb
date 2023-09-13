let () =
  let open Alcotest in
  try
    run ~and_exit:false "Geneweb"
      (Sosa_test.v @ Place_test.v @ Date_test.v @ Wiki_test.v @ Merge_test.v
     @ Util_test.v)
  with Test_error -> ()
