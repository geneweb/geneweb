open Alcotest

let () =
  let known_failures = [| "gw-royal"; "note-br" |] in
  let is_ci = Option.is_some (Sys.getenv_opt "GENEWEB_CI") in
  let filter ~name ~index:_ =
    if is_ci then if Array.mem name known_failures then `Skip else `Run
    else `Run
  in
  run ~and_exit:false "Geneweb" ~filter
    (Sosa_test.v @ Place_test.v @ Date_test.v @ Wiki_test.v @ Merge_test.v
   @ Util_test.v @ Ports_test.v)
