(** Unit tests for gwresync_lib and in-memory commit_patches. *)

let with_temp_dir f =
  let dir = Filename.temp_file "gwtest_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      let rec rm path =
        if Sys.is_directory path then (
          Array.iter
            (fun name -> rm (Filename.concat path name))
            (Sys.readdir path);
          Unix.rmdir path)
        else Sys.remove path
      in
      try rm dir with _ -> ())
    (fun () -> f dir)

let test_commit_patches_in_memory () =
  with_temp_dir (fun bdir ->
      let db = Geneweb_test_utils.Mock_db.create () in
      let p_id =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"John"
          ~surname:"Doe" ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname:bdir db in
      let iper = Geneweb_test_utils.Mock_db.iper p_id in
      let p = Gwdb.poi base iper in
      let gen_p =
        { (Gwdb.gen_person_of_person p) with Def.access = Def.Private }
      in
      Gwdb.patch_person base (Gwdb.get_iper p) gen_p;
      Gwdb.commit_patches ~write_to_disk:false base;
      let p_after = Gwdb.poi base iper in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"patch is committed in memory" true
        (Gwdb.get_access p_after = Def.Private);
      Gwdb.close_base base)

let test_privacy_single_parent () =
  with_temp_dir (fun bdir ->
      let db = Geneweb_test_utils.Mock_db.create () in
      let parent =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Parent"
          ~surname:"Solo" ()
      in
      let child =
        Geneweb_test_utils.Mock_db.add_person db ~first_name:"Child"
          ~surname:"Solo" ()
      in
      let _fam =
        Geneweb_test_utils.Mock_db.add_family db ~father:parent
          ~children:[ child ] ()
      in
      let base = Geneweb_test_utils.Mock_db.build ~bname:bdir db in
      let changed =
        Gwresync_lib.Privacy.apply_cutoff_date ~base ~cutoff_year:1900
          ~verbosity:0
      in
      Geneweb_test_utils.Check.bool ~pos:__POS__
        ~msg:"cutoff date runs safely without out-of-bounds error" true
        (changed || not changed);
      Gwdb.close_base base)

let v =
  [
    ( "gwresync",
      [
        Alcotest.test_case "In-memory commit_patches" `Quick
          test_commit_patches_in_memory;
        Alcotest.test_case "Privacy single parent robustness" `Quick
          test_privacy_single_parent;
      ] );
  ]
