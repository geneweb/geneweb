(* $Id: select.ml,v 3.2 1999-12-13 21:06:48 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

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
            Array.iter loop (doi base ifam).children)
        (uoi base iper).family
;

value select_surname base per_tab fam_tab no_spouses_parents surname =
  let surname = Name.strip_lower surname in
  for i = 0 to base.data.families.len - 1 do
    let fam = base.data.families.get i in
    let cpl = base.data.couples.get i in
    if is_deleted_family fam then ()
    else
      let des = base.data.descends.get i in
      let fath = poi base cpl.father in
      let moth = poi base cpl.mother in
      if Name.strip_lower (sou base fath.surname) = surname
      || Name.strip_lower (sou base moth.surname) = surname
      then
        do fam_tab.(i) := True;
           per_tab.(Adef.int_of_iper cpl.father) := True;
           per_tab.(Adef.int_of_iper cpl.mother) := True;
           Array.iter
             (fun ic ->
                let p = poi base ic in
                if not per_tab.(Adef.int_of_iper ic) &&
                  Name.strip_lower (sou base p.surname) = surname
                then
                  per_tab.(Adef.int_of_iper ic) := True
                else ())
             des.children;
           if no_spouses_parents then ()
           else
             List.iter
               (fun x ->
                  match (aoi base x).parents with
                  [ Some ifam when not fam_tab.(Adef.int_of_ifam ifam) ->
                      let cpl = coi base ifam in
        	      do fam_tab.(Adef.int_of_ifam ifam) := True;
        		 per_tab.(Adef.int_of_iper cpl.father) := True;
        		 per_tab.(Adef.int_of_iper cpl.mother) := True;
                      return ()
                  | _ -> () ])
               [cpl.father; cpl.mother];
        return ()
    else ();
  done
;

value functions base anc desc surnames no_spouses_parents =
  match (anc, desc, surnames) with
  [ (None, None, []) -> (fun _ -> True, fun _ -> True)
  | (None, None, surnames) ->
      let per_tab = Array.create base.data.persons.len False in
      let fam_tab = Array.create base.data.families.len False in
      do List.iter (select_surname base per_tab fam_tab no_spouses_parents)
           surnames;
      return
      (fun i -> per_tab.(Adef.int_of_iper i),
       fun i -> fam_tab.(Adef.int_of_ifam i))
  | _ ->
      let per_tab = Array.create base.data.persons.len 0 in
      let fam_tab = Array.create base.data.families.len 0 in
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
