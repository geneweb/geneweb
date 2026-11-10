open Alcotest

let () =
  let known_failures = [| "wiki-syntax" |] in
  let is_ci = Option.is_some (Sys.getenv_opt "GENEWEB_CI") in
  let filter ~name ~index:_ =
    if is_ci then if Array.mem name known_failures then `Skip else `Run
    else `Run
  in
  run ~and_exit:false "Geneweb" ~filter
    (List.concat_map Fun.id
       [
         Anomaly_test.v;
         Base_index_test.v;
         Blank_name_test.v;
         Cache_test.v;
         Calendar_test.v;
         Dag2html_test.v;
         Dict_test.v;
         Merge_test.v;
         Notes_corpus_test.v;
         Notes_rename_test.v;
         Notes_search_test.v;
         Oidc_test.v;
         Place_rename_test.v;
         Place_test.v;
         Sosa_test.v;
         Source_test.v;
         Surname_index_test.v;
         Templ_test.v;
         Update_order_test.v;
         Userkey_test.v;
         Util_test.v;
         Wiki_test.v;
       ])
