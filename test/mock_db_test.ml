let int_of_ifam ifam = int_of_string (Gwdb.string_of_ifam ifam)

let test_children_order () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let c1 = Geneweb_test_utils.Mock_db.add_person db ~first_name:"C1" () in
  let c2 = Geneweb_test_utils.Mock_db.add_person db ~first_name:"C2" () in
  let fam = Geneweb_test_utils.Mock_db.add_family db ~children:[ c1; c2 ] () in
  let base = Geneweb_test_utils.Mock_db.build db in
  let fam_obj = Geneweb_test_utils.Mock_db.foi base fam in
  let children_ipers = Gwdb.get_children fam_obj in
  let children_ints =
    Array.map Gwdb.int_of_iper children_ipers |> Array.to_list
  in
  Geneweb_test_utils.Check.list Alcotest.int ~pos:__POS__
    ~msg:"children order is preserved" [ c1; c2 ] children_ints

let test_string_deduplication () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let id1 = Geneweb_test_utils.Mock_db.add_string db "unique_string" in
  let id2 = Geneweb_test_utils.Mock_db.add_string db "unique_string" in
  let id3 = Geneweb_test_utils.Mock_db.add_string db "another_string" in
  Geneweb_test_utils.Check.int ~pos:__POS__
    ~msg:"add_string deduplicates identical strings" id1 id2;
  Geneweb_test_utils.Check.bool ~pos:__POS__
    ~msg:"add_string assigns distinct index to different string" true
    (id1 <> id3)

let test_union_registration () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let father =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Father" ()
  in
  let mother =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Mother" ()
  in
  let fam1 = Geneweb_test_utils.Mock_db.add_family db ~father ~mother () in
  let other_mother =
    Geneweb_test_utils.Mock_db.add_person db ~first_name:"Mother2" ()
  in
  let fam2 =
    Geneweb_test_utils.Mock_db.add_family db ~father ~mother:other_mother ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  let p_f = Geneweb_test_utils.Mock_db.poi base father in
  let p_m1 = Geneweb_test_utils.Mock_db.poi base mother in
  let p_m2 = Geneweb_test_utils.Mock_db.poi base other_mother in
  let father_unions =
    Gwdb.get_family p_f |> Array.map int_of_ifam |> Array.to_list
  in
  let mother1_unions =
    Gwdb.get_family p_m1 |> Array.map int_of_ifam |> Array.to_list
  in
  let mother2_unions =
    Gwdb.get_family p_m2 |> Array.map int_of_ifam |> Array.to_list
  in
  Geneweb_test_utils.Check.list Alcotest.int ~pos:__POS__
    ~msg:"father has both unions registered in order" [ fam1; fam2 ]
    father_unions;
  Geneweb_test_utils.Check.list Alcotest.int ~pos:__POS__
    ~msg:"first mother has first union registered" [ fam1 ] mother1_unions;
  Geneweb_test_utils.Check.list Alcotest.int ~pos:__POS__
    ~msg:"second mother has second union registered" [ fam2 ] mother2_unions

let test_parent_child_linking () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let fam = Geneweb_test_utils.Mock_db.add_family db () in
  let child1 = Geneweb_test_utils.Mock_db.add_person db ~parents:fam () in
  let child2 = Geneweb_test_utils.Mock_db.add_person db ~parents:fam () in
  let base = Geneweb_test_utils.Mock_db.build db in
  let p_c1 = Geneweb_test_utils.Mock_db.poi base child1 in
  let p_c2 = Geneweb_test_utils.Mock_db.poi base child2 in
  let f_fam = Geneweb_test_utils.Mock_db.foi base fam in
  let c1_parents = Gwdb.get_parents p_c1 |> Option.map int_of_ifam in
  let c2_parents = Gwdb.get_parents p_c2 |> Option.map int_of_ifam in
  let fam_children =
    Gwdb.get_children f_fam |> Array.map Gwdb.int_of_iper |> Array.to_list
  in
  Geneweb_test_utils.Check.option Alcotest.int ~pos:__POS__
    ~msg:"child1 is linked to parent family" (Some fam) c1_parents;
  Geneweb_test_utils.Check.option Alcotest.int ~pos:__POS__
    ~msg:"child2 is linked to parent family" (Some fam) c2_parents;
  Geneweb_test_utils.Check.list Alcotest.int ~pos:__POS__
    ~msg:"family is linked to children in registration order" [ child1; child2 ]
    fam_children

let test_access_and_titles () =
  let db = Geneweb_test_utils.Mock_db.create () in
  let p_id =
    Geneweb_test_utils.Mock_db.add_person db ~access:Def.Private
      ~titles:[ (Def.Tname "Duke", "duke", "of York") ]
      ()
  in
  let pub_id = Geneweb_test_utils.Mock_db.add_person db ~access:Def.Public () in
  let ift_id =
    Geneweb_test_utils.Mock_db.add_person db ~access:Def.IfTitles ()
  in
  let base = Geneweb_test_utils.Mock_db.build db in
  let p = Geneweb_test_utils.Mock_db.poi base p_id in
  let pub_p = Geneweb_test_utils.Mock_db.poi base pub_id in
  let ift_p = Geneweb_test_utils.Mock_db.poi base ift_id in
  Geneweb_test_utils.Check.bool ~pos:__POS__ ~msg:"access level is Private" true
    (Gwdb.get_access p = Def.Private);
  Geneweb_test_utils.Check.bool ~pos:__POS__ ~msg:"access level is Public" true
    (Gwdb.get_access pub_p = Def.Public);
  Geneweb_test_utils.Check.bool ~pos:__POS__ ~msg:"access level is IfTitles"
    true
    (Gwdb.get_access ift_p = Def.IfTitles);
  let titles = Gwdb.get_titles p in
  Geneweb_test_utils.Check.int ~pos:__POS__ ~msg:"person has exactly one title"
    1 (List.length titles);
  match titles with
  | [ title ] ->
      let t_name_str =
        match title.Def.t_name with
        | Def.Tname istr -> Gwdb.sou base istr
        | _ -> ""
      in
      let ident = Gwdb.sou base title.Def.t_ident in
      let place = Gwdb.sou base title.Def.t_place in
      Geneweb_test_utils.Check.string ~pos:__POS__ ~msg:"title name matches"
        "Duke" t_name_str;
      Geneweb_test_utils.Check.string ~pos:__POS__ ~msg:"title ident matches"
        "duke" ident;
      Geneweb_test_utils.Check.string ~pos:__POS__ ~msg:"title place matches"
        "of York" place
  | _ -> Geneweb_test_utils.Check.fail ~pos:__POS__ "expected exactly one title"

let v =
  [
    Geneweb_test_utils.Check.suite "mock-db"
      [
        Geneweb_test_utils.Check.case "children_order" test_children_order;
        Geneweb_test_utils.Check.case "string_deduplication"
          test_string_deduplication;
        Geneweb_test_utils.Check.case "union_registration"
          test_union_registration;
        Geneweb_test_utils.Check.case "parent_child_linking"
          test_parent_child_linking;
        Geneweb_test_utils.Check.case "access_and_titles" test_access_and_titles;
      ];
  ]
