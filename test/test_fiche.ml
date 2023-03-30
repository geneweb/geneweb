
module R = Gwd_lib.Request



let run () =
  Test_page.init_assets ();
  let base_notes =
    let open Def in
    { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let base = Gwdb.make "" [] ( ([||],[||],[||]), ([||],[||],[||]), [||], base_notes) in
  Test_page.test ({Test_page.config with Geneweb.Config.bname = "aaezahezajk"}) base
  (*R.treat_request Test_page.config*)
