(* $Id: select.ml,v 1.1.1.1 1998-09-01 14:32:11 ddr Exp $ *)

open Def;
open Gutil;

value select_ancestors base per_tab fam_tab flag =
  loop where rec loop iper =
    let i = Adef.int_of_iper iper in
    if per_tab.(i) land flag <> 0 then ()
    else
      do per_tab.(i) := per_tab.(i) lor flag; return
      match (aoi base iper).parents with
      [ Some ifam ->
          let i = Adef.int_of_ifam ifam in
          if fam_tab.(i) land flag <> 0 then ()
          else
            do fam_tab.(i) := fam_tab.(i) lor flag; return
            let cpl = coi base ifam in
            do loop cpl.father;
               loop cpl.mother;
            return ()
      | None -> () ]
;

value select_descendants base per_tab fam_tab flag =
  loop where rec loop iper =
    let i = Adef.int_of_iper iper in
    if per_tab.(i) land flag <> 0 then ()
    else
      do per_tab.(i) := per_tab.(i) lor flag; return
      Array.iter
        (fun ifam ->
          let i = Adef.int_of_ifam ifam in
          if fam_tab.(i) land flag <> 0 then ()
          else
            let fam = foi base ifam in
            let cpl = coi base ifam in
            do fam_tab.(i) := fam_tab.(i) lor flag;
               let i = Adef.int_of_iper cpl.father in
               per_tab.(i) := per_tab.(i) lor flag;
               let i = Adef.int_of_iper cpl.mother in
               per_tab.(i) := per_tab.(i) lor flag;
            return
            Array.iter loop (foi base ifam).children)
        (poi base iper).family
;

value functions base anc desc =
  match (anc, desc) with
  [ (None, None) -> (fun _ -> True, fun _ -> True)
  | _ ->
      let per_tab = Array.create base.persons.len 0 in
      let fam_tab = Array.create base.families.len 0 in
      match (anc, desc) with
      [ (Some iaper, None) ->
          do select_ancestors base per_tab fam_tab 1 iaper; return
          (fun i -> per_tab.(Adef.int_of_iper i) == 1,
           fun i -> fam_tab.(Adef.int_of_ifam i) == 1)
      | (None, Some idper) ->
          do select_descendants base per_tab fam_tab 1 idper; return
          (fun i -> per_tab.(Adef.int_of_iper i) == 1,
           fun i -> fam_tab.(Adef.int_of_ifam i) == 1)
      | (Some iaper, Some idper) ->
          do select_ancestors base per_tab fam_tab 1 iaper;
             select_descendants base per_tab fam_tab 2 idper;
          return
          (fun i -> per_tab.(Adef.int_of_iper i) == 3,
           fun i -> fam_tab.(Adef.int_of_ifam i) == 3)
      | _ -> assert False ] ]

;
