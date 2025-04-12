open Alcotest
open Geneweb
open Def
module Driver = Geneweb_db.Driver

let empty_string = 0
let quest_string = 1
let ascend parents = { Driver.no_ascend with Def.parents }
let descend children = { Def.children }
let union family = { Def.family }
let couple a b = Adef.couple a b

let person i =
  { (Mutil.empty_person empty_string quest_string) with occ = i; key_index = i }

let family i = { (Mutil.empty_family empty_string) with fam_index = i }
let iper (i : int) : Driver.iper = Obj.magic i

let test_is_ancestor () =
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
    { nread = (fun _ _ -> ""); norigin_file = ""; efiles = (fun () -> []) }
  in
  let data =
    ( (persons, ascends, unions),
      (families, couples, descends),
      strings,
      base_notes )
  in
  Driver.make "is_ancestor_base" [] data @@ fun base ->
  let child = Driver.poi base (iper 0) in
  let father = Driver.poi base (iper 1) in
  let mother = Driver.poi base (iper 2) in
  (check bool) "is_ancetor child father" false
    (MergeInd.is_ancestor base child father);
  (check bool) "is_ancetor father child" true
    (MergeInd.is_ancestor base father child);
  (check bool) "is_ancetor mother child" true
    (MergeInd.is_ancestor base mother child);
  ()

let v =
  [
    ( "mergeind-ancestor",
      [ test_case "MergeInd.is_ancestor" `Quick test_is_ancestor ] );
  ]
