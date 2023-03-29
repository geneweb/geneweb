open Geneweb
open OUnit2
open Def


let create_base () : Gwdb.base =
  let open Gnwb_utils in
  
  let ichild, ifath, imoth = iper 0, iper 1, iper 2 in
  let child = person ichild in
  let father = person ifath in
  let mother = person imoth in
  let persons = [| child; father; mother |] in

  let ifam0 = ifam 0 in
  let families = [| family ifam0 |] in
  let unions = [|
    union ~families:[||];
    union ~families:[| ifam0 |];
    union ~families:[| ifam0 |]
  |] in

  let ascends = [|
    ascend ~parents:(Some (ifam 0));
    ascend ~parents:None;
    ascend ~parents:None
  |] in
  let couples = [| couple ~father:ifath ~mother:imoth |] in
  let descends = [| descend ~children:[| ichild |] |] in
  let strings = default_strings () in
  let base_notes =
    { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  Gwdb.make "" [] data

let test_is_ancestor =
  let open Gnwb_utils.Gwdb_utils in
  let base = create_base () in
  let child = Gwdb.poi base (iper 0) in
  let father = Gwdb.poi base (iper 1) in
  let mother = Gwdb.poi base (iper 2) in
  let test exp p1 p2 _ = assert_equal exp (MergeInd.is_ancestor base p1 p2) in
  [
    "is_ancetor child father" >:: test false child father;
    "is_ancetor father child" >:: test true father child;
    "is_ancetor mother child" >:: test true mother child;
  ]

let suite = [ "MergeInd.is_ancestor" >::: test_is_ancestor ]
