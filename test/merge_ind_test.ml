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

let v =
  let run_all tests () = List.iter (fun check -> check ()) tests in
  [
    Check.suite "merge-individuals"
      [ Check.case "Person.is_ancestor" (run_all test_is_ancestor) ];
  ]
