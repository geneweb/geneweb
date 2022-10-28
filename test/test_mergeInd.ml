open Geneweb
open OUnit2
open Def

let empty_string = 0
let quest_string = 1

let ascend parents = { Gwdb.no_ascend with Def.parents }
let descend children = { Def.children }
let union family = { Def.family }
let couple a b = Adef.couple a b
let person i = { (Mutil.empty_person empty_string quest_string) with occ = i ; key_index = i }
let family i = { (Mutil.empty_family empty_string) with fam_index = i }

let iper (i : int) = (Obj.magic i : Gwdb.iper)

let test_is_ancestor =
  let child = person 0 in
  let father = person 1 in
  let mother = person 2 in
  let persons = [| child ; father ; mother |] in
  let ascends = [| ascend (Some 0) ; ascend None ; ascend None |] in
  let unions = [| union [||] ; union [|0|] ; union [|0|] |] in
  let families = [| family 0 |] in
  let couples = [| couple 1 2 |] in
  let descends = [| descend [| 0 |] |] in
  let strings = [| "" ; "?" |] in
  let base_notes = { nread = (fun _ _ -> "")
                   ; norigin_file = ""
                   ; efiles = (fun () -> [])
                   }
  in
  let data = ((persons, ascends, unions), (families, couples, descends), strings, base_notes) in
  let base = Gwdb.make "" [] data in
  let child = Gwdb.poi base (iper 0) in
  let father = Gwdb.poi base (iper 1) in
  let mother = Gwdb.poi base (iper 2) in
  let test exp p1 p2 _ = assert_equal exp (MergeInd.is_ancestor base p1 p2) in
  [ "is_ancetor child father" >:: test false child father
  ; "is_ancetor father child" >:: test true father child
  ; "is_ancetor mother child" >:: test true mother child
  ]

let suite =
  [ "MergeInd.is_ancestor" >::: test_is_ancestor ]
