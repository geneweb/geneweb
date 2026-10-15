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
