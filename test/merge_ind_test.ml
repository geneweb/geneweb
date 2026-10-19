let test_is_ancestor =
  let db = Mock_db.create () in
  let child = Mock_db.add_person db ~occ:0 () in
  let father = Mock_db.add_person db ~occ:1 () in
  let mother = Mock_db.add_person db ~occ:2 () in
  let _fam = Mock_db.add_family db ~father ~mother ~children:[ child ] () in
  let base = Mock_db.build db in
  let child = Mock_db.poi base child in
  let father = Mock_db.poi base father in
  let mother = Mock_db.poi base mother in
  let test ~__POS__ ~msg exp p1 p2 () =
    Check.bool ~pos:__POS__ ~msg exp (Geneweb.Person.is_ancestor base p1 p2)
  in
  [
    test ~__POS__ ~msg:"is_ancestor child father" false child father;
    test ~__POS__ ~msg:"is_ancestor father child" true father child;
    test ~__POS__ ~msg:"is_ancestor mother child" true mother child;
  ]

let test_can_have_unnamed_spouse_duplicate =
  let db = Mock_db.create () in
  let p0 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let p1 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:1 ()
  in
  let p2 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:2 () in
  let p3 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:3 () in
  let p4 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:4 ()
  in
  let f0 = Mock_db.add_family db ~father:p0 ~mother:p1 () in
  let f1 = Mock_db.add_family db ~father:p0 ~mother:p2 () in
  let f2 = Mock_db.add_family db ~father:p0 ~mother:p3 () in
  let f3 = Mock_db.add_family db ~father:p0 ~mother:p4 () in
  let base = Mock_db.build db in
  let p = Mock_db.poi base p0 in
  let test ~__POS__ ~msg exp ifams () =
    Check.bool ~pos:__POS__ ~msg exp
      (Geneweb.CheckItem.can_have_unnamed_spouse_duplicate base p
         (Array.of_list (List.map Mock_db.ifam ifams)))
  in
  [
    test ~__POS__ ~msg:"can_have_unnamed_spouse_duplicate 1 union" false [ f0 ];
    test ~__POS__
      ~msg:"can_have_unnamed_spouse_duplicate 2 unions (named + ? ?)" true
      [ f0; f1 ];
    test ~__POS__ ~msg:"can_have_unnamed_spouse_duplicate 2 unions (? ? + ? ?)"
      false [ f1; f2 ];
    test ~__POS__
      ~msg:"can_have_unnamed_spouse_duplicate 2 unions (named + named)" false
      [ f0; f3 ];
    test ~__POS__
      ~msg:"can_have_unnamed_spouse_duplicate 3 unions (named + ? ? + named)"
      false [ f0; f1; f3 ];
  ]

let test_warning_handle_homonymous =
  let db = Mock_db.create () in
  let p0 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Dupont" ~occ:0 ()
  in
  let p1 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Dupont" ~occ:1 ()
  in
  let p2 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Dupont" ~occ:2 ()
  in
  let p3 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:3 () in
  let p4 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:4 () in
  let f0 = Mock_db.add_family db ~father:p0 ~mother:p1 () in
  let f1 = Mock_db.add_family db ~father:p0 ~mother:p2 () in
  let f2 = Mock_db.add_family db ~father:p0 ~mother:p3 () in
  let f3 = Mock_db.add_family db ~father:p0 ~mother:p4 () in
  let base = Mock_db.build db in
  let p = Mock_db.poi base p0 in
  let w_named =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f0, Mock_db.ifam f1, p)
  in
  let w_quest =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f2, Mock_db.ifam f3, p)
  in
  let test ~__POS__ ~msg exp ws () =
    let res = Geneweb.Warning.handle_homonymous base ws in
    Check.int ~pos:__POS__ ~msg (List.length exp) (List.length res);
    List.iter
      (fun expected_w ->
        Check.bool ~pos:__POS__
          ~msg:(msg ^ " contains expected")
          true
          (List.exists (Geneweb.CheckItem.eq_warning expected_w) res))
      exp
  in
  [
    test ~__POS__
      ~msg:"handle_homonymous suppresses quest string when named exists"
      [ w_named ] [ w_named; w_quest ];
    test ~__POS__
      ~msg:"handle_homonymous keeps quest string when no named exists"
      [ w_quest ] [ w_quest ];
    test ~__POS__ ~msg:"handle_homonymous keeps named homonymous" [ w_named ]
      [ w_named ];
  ]

let test_named_spouse_of_quest_string_warning =
  let db = Mock_db.create () in
  let p0 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let p1 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Martin" ~occ:1 ()
  in
  let p2 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:2 () in
  let f0 = Mock_db.add_family db ~father:p0 ~mother:p1 () in
  let f1 = Mock_db.add_family db ~father:p0 ~mother:p2 () in
  let base = Mock_db.build db in
  let p = Mock_db.poi base p0 in
  let test ~__POS__ ~msg ifam1 ifam2 () =
    let res =
      Geneweb.Warning.named_spouse_of_quest_string_warning base p
        (Mock_db.ifam ifam1) (Mock_db.ifam ifam2)
    in
    Check.int ~pos:__POS__ ~msg 1 (Gwdb.int_of_iper @@ Gwdb.get_iper res)
  in
  [
    test ~__POS__ ~msg:"named_spouse with ifam0 ifam1 (named then quest)" f0 f1;
    test ~__POS__
      ~msg:"named_spouse with ifam1 ifam0 (quest then named / normalized)" f1 f0;
  ]

let test_check_family_unnamed_spouse_duplicate_symmetric =
  let db = Mock_db.create () in
  (* Father with named spouse in f0 and unnamed spouse in f1 *)
  let fa =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let mo_named =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Martin" ~occ:1 ()
  in
  let mo_unnamed =
    Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:2 ()
  in
  let f0 = Mock_db.add_family db ~father:fa ~mother:mo_named () in
  let f1 = Mock_db.add_family db ~father:fa ~mother:mo_unnamed () in
  (* Mother with named spouse in f2 and unnamed spouse in f3 *)
  let mo =
    Mock_db.add_person db ~first_name:"Jeanne" ~surname:"Durand" ~occ:3 ()
  in
  let fa_named =
    Mock_db.add_person db ~first_name:"Pierre" ~surname:"Leroy" ~occ:4 ()
  in
  let fa_unnamed =
    Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:5 ()
  in
  let f2 = Mock_db.add_family db ~father:fa_named ~mother:mo () in
  let f3 = Mock_db.add_family db ~father:fa_unnamed ~mother:mo () in
  (* Person p_cross who is father in f4 (with named mother) and mother in f5 (with unnamed father) *)
  let p_cross =
    Mock_db.add_person db ~first_name:"Alex" ~surname:"Robin" ~occ:6 ()
  in
  let sp_named =
    Mock_db.add_person db ~first_name:"Morgan" ~surname:"Taylor" ~occ:7 ()
  in
  let sp_unnamed =
    Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:8 ()
  in
  let f4 = Mock_db.add_family db ~father:p_cross ~mother:sp_named () in
  let f5 = Mock_db.add_family db ~father:sp_unnamed ~mother:p_cross () in
  let base = Mock_db.build db in
  let check_family fam_idx =
    let wl = ref [] in
    let ifam = Mock_db.ifam fam_idx in
    let fam = Mock_db.foi base fam_idx in
    Geneweb.CheckItem.family ~onchange:false base
      (fun w -> wl := w :: !wl)
      ifam fam;
    !wl
  in
  let expected_fa_warning =
    Geneweb.Warning.PossibleDuplicateFamQuestString
      (Mock_db.ifam f0, Mock_db.ifam f1, Mock_db.poi base fa)
  in
  let expected_mo_warning =
    Geneweb.Warning.PossibleDuplicateFamQuestString
      (Mock_db.ifam f2, Mock_db.ifam f3, Mock_db.poi base mo)
  in
  let expected_cross_warning =
    Geneweb.Warning.PossibleDuplicateFamQuestString
      (Mock_db.ifam f4, Mock_db.ifam f5, Mock_db.poi base p_cross)
  in
  let test ~__POS__ ~msg expected fam_idx () =
    let warnings = check_family fam_idx in
    Check.bool ~pos:__POS__ ~msg true
      (List.exists (Geneweb.CheckItem.eq_warning expected) warnings)
  in
  [
    test ~__POS__ ~msg:"father with named spouse family (f0)"
      expected_fa_warning f0;
    test ~__POS__ ~msg:"father with unnamed spouse family (f1)"
      expected_fa_warning f1;
    test ~__POS__ ~msg:"mother with named spouse family (f2)"
      expected_mo_warning f2;
    test ~__POS__ ~msg:"mother with unnamed spouse family (f3)"
      expected_mo_warning f3;
    test ~__POS__ ~msg:"cross-role parent with named spouse family (f4)"
      expected_cross_warning f4;
    test ~__POS__ ~msg:"cross-role parent with unnamed spouse family (f5)"
      expected_cross_warning f5;
  ]

let test_check_family_unnamed_spouse_homonymous_suppression =
  let db = Mock_db.create () in
  (* Father fa with 2 named homonymous spouses (f_n1, f_n2) and 2 unnamed spouses (f_u1, f_u2) *)
  let fa =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let mo_n1 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Martin" ~occ:1 ()
  in
  let mo_n2 =
    Mock_db.add_person db ~first_name:"Marie" ~surname:"Martin" ~occ:2 ()
  in
  let mo_u1 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:3 () in
  let mo_u2 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:4 () in
  let f_n1 = Mock_db.add_family db ~father:fa ~mother:mo_n1 () in
  let f_n2 = Mock_db.add_family db ~father:fa ~mother:mo_n2 () in
  let f_u1 = Mock_db.add_family db ~father:fa ~mother:mo_u1 () in
  let f_u2 = Mock_db.add_family db ~father:fa ~mother:mo_u2 () in
  (* Father fa_only_unnamed with only 2 unnamed spouses (f_only_u1, f_only_u2) *)
  let fa_only_unnamed =
    Mock_db.add_person db ~first_name:"Pierre" ~surname:"Durand" ~occ:5 ()
  in
  let mo_u3 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:6 () in
  let mo_u4 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:7 () in
  let f_only_u1 =
    Mock_db.add_family db ~father:fa_only_unnamed ~mother:mo_u3 ()
  in
  let f_only_u2 =
    Mock_db.add_family db ~father:fa_only_unnamed ~mother:mo_u4 ()
  in
  (* Mother mo with 2 named homonymous spouses (f_mo_n1, f_mo_n2) and 2 unnamed spouses (f_mo_u1, f_mo_u2) *)
  let mo =
    Mock_db.add_person db ~first_name:"Jeanne" ~surname:"Leroy" ~occ:8 ()
  in
  let fa_n1 =
    Mock_db.add_person db ~first_name:"Paul" ~surname:"Moreau" ~occ:9 ()
  in
  let fa_n2 =
    Mock_db.add_person db ~first_name:"Paul" ~surname:"Moreau" ~occ:10 ()
  in
  let fa_u1 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:11 () in
  let fa_u2 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:12 () in
  let f_mo_n1 = Mock_db.add_family db ~father:fa_n1 ~mother:mo () in
  let f_mo_n2 = Mock_db.add_family db ~father:fa_n2 ~mother:mo () in
  let f_mo_u1 = Mock_db.add_family db ~father:fa_u1 ~mother:mo () in
  let f_mo_u2 = Mock_db.add_family db ~father:fa_u2 ~mother:mo () in
  (* Mother mo_only_unnamed with only 2 unnamed spouses (f_mo_only_u1, f_mo_only_u2) *)
  let mo_only_unnamed =
    Mock_db.add_person db ~first_name:"Claire" ~surname:"Roux" ~occ:13 ()
  in
  let fa_u3 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:14 () in
  let fa_u4 = Mock_db.add_person db ~first_name:"?" ~surname:"?" ~occ:15 () in
  let f_mo_only_u1 =
    Mock_db.add_family db ~father:fa_u3 ~mother:mo_only_unnamed ()
  in
  let f_mo_only_u2 =
    Mock_db.add_family db ~father:fa_u4 ~mother:mo_only_unnamed ()
  in
  let base = Mock_db.build db in
  let check_family fam_idx =
    let wl = ref [] in
    let ifam = Mock_db.ifam fam_idx in
    let fam = Mock_db.foi base fam_idx in
    Geneweb.CheckItem.family ~onchange:false base
      (fun w -> wl := w :: !wl)
      ifam fam;
    !wl
  in
  let expected_named_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f_n1, Mock_db.ifam f_n2, Mock_db.poi base fa)
  in
  let suppressed_unnamed_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f_u1, Mock_db.ifam f_u2, Mock_db.poi base fa)
  in
  let expected_unnamed_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      ( Mock_db.ifam f_only_u1,
        Mock_db.ifam f_only_u2,
        Mock_db.poi base fa_only_unnamed )
  in
  let expected_mo_named_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f_mo_n1, Mock_db.ifam f_mo_n2, Mock_db.poi base mo)
  in
  let suppressed_mo_unnamed_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      (Mock_db.ifam f_mo_u1, Mock_db.ifam f_mo_u2, Mock_db.poi base mo)
  in
  let expected_mo_unnamed_warning =
    Geneweb.Warning.PossibleDuplicateFamHomonymous
      ( Mock_db.ifam f_mo_only_u1,
        Mock_db.ifam f_mo_only_u2,
        Mock_db.poi base mo_only_unnamed )
  in
  let test ~__POS__ ~msg expected_present expected_absent fam_idx () =
    let warnings = check_family fam_idx in
    Option.iter
      (fun exp ->
        Check.bool ~pos:__POS__ ~msg:(msg ^ " has expected") true
          (List.exists (Geneweb.CheckItem.eq_warning exp) warnings))
      expected_present;
    Option.iter
      (fun exp ->
        Check.bool ~pos:__POS__ ~msg:(msg ^ " lacks absent") false
          (List.exists (Geneweb.CheckItem.eq_warning exp) warnings))
      expected_absent
  in
  [
    test ~__POS__ ~msg:"named homonymous present on f_n1"
      (Some expected_named_warning) None f_n1;
    test ~__POS__
      ~msg:"unnamed homonymous suppressed on f_u1 because named exists" None
      (Some suppressed_unnamed_warning) f_u1;
    test ~__POS__
      ~msg:"unnamed homonymous present on f_only_u1 because no named exists"
      (Some expected_unnamed_warning) None f_only_u1;
    test ~__POS__ ~msg:"mother named homonymous present on f_mo_n1"
      (Some expected_mo_named_warning) None f_mo_n1;
    test ~__POS__
      ~msg:
        "mother unnamed homonymous suppressed on f_mo_u1 because named exists"
      None (Some suppressed_mo_unnamed_warning) f_mo_u1;
    test ~__POS__
      ~msg:
        "mother unnamed homonymous present on f_mo_only_u1 because no named \
         exists"
      (Some expected_mo_unnamed_warning) None f_mo_only_u1;
  ]

let test_merge_same_parents =
  let db = Mock_db.create () in
  let p0 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let p1 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:1 ()
  in
  let p2 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let p3 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let _f0 =
    Mock_db.add_family db ~father:p2 ~mother:p3 ~children:[ p0; p1 ] ()
  in
  let base = Mock_db.build db in
  let ind0 = Mock_db.poi base p0 in
  let ind1 = Mock_db.poi base p1 in
  let test ~__POS__ ~msg () =
    let res = Geneweb.MergeInd.merge Geneweb.Config.empty base ind0 ind1 in
    match res with
    | Geneweb.MergeInd.Finished wl ->
        Check.int ~pos:__POS__ ~msg 0 (List.length wl)
    | Geneweb.MergeInd.Stuck _ ->
        Check.fail ~pos:__POS__ (msg ^ ": expected Finished, got Stuck")
  in
  [ test ~__POS__ ~msg:"merge individuals with same parents" ]

let test_merge_same_person =
  let db = Mock_db.create () in
  let p0 =
    Mock_db.add_person db ~first_name:"Jean" ~surname:"Dupont" ~occ:0 ()
  in
  let base = Mock_db.build db in
  let ind0 = Mock_db.poi base p0 in
  let test ~__POS__ ~msg () =
    Check.check_raises ~pos:__POS__ msg Geneweb.Person.Same_person (fun () ->
        ignore (Geneweb.MergeInd.merge Geneweb.Config.empty base ind0 ind0))
  in
  [ test ~__POS__ ~msg:"merge individual with themselves raises Same_person" ]

let test_merge_with_ancestors =
  let db = Mock_db.create () in
  let child0 =
    Mock_db.add_person db ~first_name:"Child" ~surname:"Test" ~occ:0 ()
  in
  let child1 =
    Mock_db.add_person db ~first_name:"Child" ~surname:"Test" ~occ:1 ()
  in
  let fa0 =
    Mock_db.add_person db ~first_name:"Father" ~surname:"Test" ~occ:0 ()
  in
  let mo0 =
    Mock_db.add_person db ~first_name:"Mother" ~surname:"Test" ~occ:0 ()
  in
  let fa1 =
    Mock_db.add_person db ~first_name:"Father" ~surname:"Test" ~occ:0 ()
  in
  let mo1 =
    Mock_db.add_person db ~first_name:"Mother" ~surname:"Test" ~occ:0 ()
  in
  let _f0 =
    Mock_db.add_family db ~father:fa0 ~mother:mo0 ~children:[ child0 ] ()
  in
  let _f1 =
    Mock_db.add_family db ~father:fa1 ~mother:mo1 ~children:[ child1 ] ()
  in
  let base = Mock_db.build db in
  let ind0 = Mock_db.poi base child0 in
  let ind1 = Mock_db.poi base child1 in
  let test ~__POS__ ~msg () =
    let res = Geneweb.MergeInd.merge Geneweb.Config.empty base ind0 ind1 in
    match res with
    | Geneweb.MergeInd.Finished wl ->
        Check.int ~pos:__POS__ ~msg 0 (List.length wl)
    | Geneweb.MergeInd.Stuck _ ->
        Check.fail ~pos:__POS__ (msg ^ ": expected Finished, got Stuck")
  in
  [ test ~__POS__ ~msg:"merge individuals with different compatible parents" ]

let test_merge_stuck_fam =
  let db = Mock_db.create () in
  let child0 =
    Mock_db.add_person db ~first_name:"Child" ~surname:"Test" ~occ:0 ()
  in
  let child1 =
    Mock_db.add_person db ~first_name:"Child" ~surname:"Test" ~occ:1 ()
  in
  let fa0 =
    Mock_db.add_person db ~first_name:"Father" ~surname:"Test" ~occ:0 ()
  in
  let mo0 =
    Mock_db.add_person db ~first_name:"Mother" ~surname:"Test" ~occ:0 ()
  in
  let fa1 =
    Mock_db.add_person db ~first_name:"Father" ~surname:"Test" ~occ:0 ()
  in
  let mo1 =
    Mock_db.add_person db ~first_name:"Mother" ~surname:"Test" ~occ:0 ()
  in
  let _f0 =
    Mock_db.add_family db ~father:fa0 ~mother:mo0 ~children:[ child0 ]
      ~fsources:"SourceA" ()
  in
  let _f1 =
    Mock_db.add_family db ~father:fa1 ~mother:mo1 ~children:[ child1 ]
      ~fsources:"SourceB" ()
  in
  let base = Mock_db.build db in
  let ind0 = Mock_db.poi base child0 in
  let ind1 = Mock_db.poi base child1 in
  let test ~__POS__ ~msg () =
    let res = Geneweb.MergeInd.merge Geneweb.Config.empty base ind0 ind1 in
    match res with
    | Geneweb.MergeInd.Finished _ ->
        Check.fail ~pos:__POS__ (msg ^ ": expected Stuck, got Finished")
    | Geneweb.MergeInd.Stuck (FamJob _, remaining, _) ->
        let branches = Geneweb.MergeInd.person_pairs_of_jobs remaining in
        Check.int ~pos:__POS__ ~msg:(msg ^ " branches length") 1
          (List.length branches);
        let b_p1, b_p2 = List.hd branches in
        Check.int ~pos:__POS__ ~msg:(msg ^ " branch p1") 0
          (Gwdb.int_of_iper b_p1);
        Check.int ~pos:__POS__ ~msg:(msg ^ " branch p2") 1
          (Gwdb.int_of_iper b_p2)
    | Geneweb.MergeInd.Stuck (IndJob _, _, _) ->
        Check.fail ~pos:__POS__ (msg ^ ": expected Stuck FamJob, got IndJob")
  in
  [ test ~__POS__ ~msg:"merge individuals with incompatible parent families" ]

let test_compatible_strings =
  let test ~__POS__ ~msg exp s1 s2 () =
    Check.bool ~pos:__POS__ ~msg exp (Geneweb.MergeInd.compatible_strings s1 s2)
  in
  [
    test ~__POS__ ~msg:"compatible_strings empty and empty" true
      Gwdb.empty_string Gwdb.empty_string;
    test ~__POS__ ~msg:"compatible_strings empty and non-empty" true
      Gwdb.empty_string (Gwdb.istr_of_string "2");
    test ~__POS__ ~msg:"compatible_strings non-empty and empty" true
      (Gwdb.istr_of_string "2") Gwdb.empty_string;
    test ~__POS__ ~msg:"compatible_strings same non-empty" true
      (Gwdb.istr_of_string "2") (Gwdb.istr_of_string "2");
    test ~__POS__ ~msg:"compatible_strings different non-empty" false
      (Gwdb.istr_of_string "2") (Gwdb.istr_of_string "3");
  ]

let v =
  let run_all tests () = List.iter (fun check -> check ()) tests in
  [
    Check.suite "merge-individuals"
      [
        Check.case "Person.is_ancestor" (run_all test_is_ancestor);
        Check.case "CheckItem.can_have_unnamed_spouse_duplicate"
          (run_all test_can_have_unnamed_spouse_duplicate);
        Check.case "Warning.handle_homonymous"
          (run_all test_warning_handle_homonymous);
        Check.case "Warning.named_spouse_of_quest_string_warning"
          (run_all test_named_spouse_of_quest_string_warning);
        Check.case "CheckItem.family symmetric unnamed spouse duplicate"
          (run_all test_check_family_unnamed_spouse_duplicate_symmetric);
        Check.case "CheckItem.family unnamed spouse homonymous suppression"
          (run_all test_check_family_unnamed_spouse_homonymous_suppression);
        Check.case "MergeInd.merge same parents"
          (run_all test_merge_same_parents);
        Check.case "MergeInd.merge same person" (run_all test_merge_same_person);
        Check.case "MergeInd.merge with ancestors"
          (run_all test_merge_with_ancestors);
        Check.case "MergeInd.merge stuck on family"
          (run_all test_merge_stuck_fam);
        Check.case "MergeInd.compatible_strings"
          (run_all test_compatible_strings);
      ];
  ]
