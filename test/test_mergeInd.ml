let empty_string = 0
let quest_string = 1
let ascend parents = { Gwdb.no_ascend with Def.parents }
let descend children = { Def.children }
let union family = { Def.family }
let couple a b = Adef.couple a b

let person i =
  { (Mutil.empty_person empty_string quest_string) with occ = i; key_index = i }

let family i = { (Mutil.empty_family empty_string) with fam_index = i }
let iper (i : int) : Gwdb.iper = i |> Int.to_string |> Gwdb.iper_of_string

let test_is_ancestor =
  let child = person 0 in
  let father = person 1 in
  let mother = person 2 in
  let persons = [| child; father; mother |] in
  let ascends = [| ascend (Some 0); ascend None; ascend None |] in
  let unions = [| union [||]; union [| 0 |]; union [| 0 |] |] in
  let families = [| family 0 |] in
  let couples = [| couple 1 2 |] in
  let descends = [| descend [| 0 |] |] in
  let strings = [| ""; "?" |] in
  let base_notes =
    { Def.nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  let base = Gwdb.make "" [] data in
  let () = Gwdb.load_ascends_array base in
  let child = Gwdb.poi base (iper 0) in
  let father = Gwdb.poi base (iper 1) in
  let mother = Gwdb.poi base (iper 2) in
  let test ~__POS__ ~msg exp p1 p2 _ =
    Alcotest.check' ~pos:__POS__ Alcotest.bool ~msg ~expected:exp
      ~actual:(Geneweb.MergeInd.is_ancestor base p1 p2)
  in
  [
    test ~__POS__ ~msg:"is_ancetor child father" false child father;
    test ~__POS__ ~msg:"is_ancetor father child" true father child;
    test ~__POS__ ~msg:"is_ancetor mother child" true mother child;
  ]

let suite =
  [
    ( "merge-individuals",
      [
        Alcotest.test_case "MergeInd.is_ancestor" `Quick (fun () ->
            List.iter (fun check -> check ()) test_is_ancestor);
      ] );
  ]
