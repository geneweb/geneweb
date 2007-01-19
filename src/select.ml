(* $Id: select.ml,v 5.16 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gutil;
open Gwdb;

value is_censored_person threshold p =
  match Adef.od_of_codate (get_birth p) with
  [ Some date ->
      match date with
      [ Dgreg dmy _ -> dmy.year >= threshold && get_access p != Public
      | _ -> False ]
  | None -> False ]
;

value is_censored_couple base threshold cpl =
  let fath = poi base (get_father cpl) in
  let moth = poi base (get_mother cpl) in
  is_censored_person threshold fath || is_censored_person threshold moth
;

value censor_person base per_tab fam_tab flag threshold p no_check =
  let ps = poi base (Adef.iper_of_int p) in
  if no_check || is_censored_person threshold ps then
    per_tab.(p) := per_tab.(p) lor flag
  else ()
;

value rec censor_family base per_tab fam_tab flag threshold i no_check =
  let censor_unions p =
    let uni = poi base (Adef.iper_of_int p) in
    Array.iter
      (fun ifam ->
         let f = Adef.int_of_ifam ifam in
         do {
           censor_family base per_tab fam_tab flag threshold f True;
           censor_person base per_tab fam_tab flag threshold p True
         })
      (get_family uni)
  in
  let censor_descendants f =
    let des = foi base (Adef.ifam_of_int f) in
    Array.iter
      (fun iper ->
         let ip = Adef.int_of_iper iper in
         if per_tab.(ip) <> 0 then () else censor_unions ip)
      (get_children des)
  in
  let all_families_censored p =
    let uni = poi base (Adef.iper_of_int p) in
    Array.fold_left
      (fun check ifam -> fam_tab.(Adef.int_of_ifam ifam) = 0 && check) True
      (get_family uni)
  in
  let censor_spouse iper =
    let p = Adef.int_of_iper iper in
    if all_families_censored p then per_tab.(p) := per_tab.(p) lor flag
    else ()
  in
  if fam_tab.(i) <> 0 then ()
  else
    let fam = foi base (Adef.ifam_of_int i) in
    if is_deleted_family fam then ()
    else
      if no_check || is_censored_couple base threshold fam then do {
        fam_tab.(i) := fam_tab.(i) lor flag;
        censor_spouse (get_father fam);
        censor_spouse (get_mother fam);
        censor_descendants i
      }
      else ()
;

value censor_base base per_tab fam_tab flag threshold =
  do {
    for i = 0 to nb_of_families base - 1 do {
      censor_family base per_tab fam_tab flag threshold i False
    };
    for i = 0 to nb_of_persons base - 1 do {
      censor_person base per_tab fam_tab flag threshold i False
    }
  }
;

value restrict_base base per_tab fam_tab flag =
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let fct p = False in
      if base_visible_get base fct i then
        let _ = per_tab.(i) := per_tab.(i) lor flag in ()
      else ()
    };
    for i = 0 to nb_of_families base - 1 do {
      let fam = foi base (Adef.ifam_of_int i) in
      let des_visible =
        Array.fold_left
          (fun check iper -> check || per_tab.(Adef.int_of_iper iper) = 0)
          False (get_children fam)
      in
      let cpl_not_visible =
        per_tab.(Adef.int_of_iper (get_father fam)) <> 0 ||
        per_tab.(Adef.int_of_iper (get_mother fam)) <> 0
      in
      if not des_visible && cpl_not_visible then
        fam_tab.(i) := fam_tab.(i) lor flag
      else ();
    }
  }
;

value flag_family base per_tab fam_tab flag ifam =
  let i = Adef.int_of_ifam ifam in
  let cpl = foi base ifam in
  do {
    fam_tab.(i) := fam_tab.(i) lor flag;
    let i = Adef.int_of_iper (get_father cpl) in
    per_tab.(i) := per_tab.(i) lor flag;
    let i = Adef.int_of_iper (get_mother cpl) in
    per_tab.(i) := per_tab.(i) lor flag
  }
;

value select_ancestors base per_tab fam_tab with_siblings flag iper =
  let rec add_ancestors iper =
    let i = Adef.int_of_iper iper in
    if per_tab.(i) land flag <> 0 then ()
    else do {
      per_tab.(i) := per_tab.(i) lor flag;
      match get_parents (poi base iper) with
      [ Some ifam ->
          let i = Adef.int_of_ifam ifam in
          if fam_tab.(i) land flag <> 0 then ()
          else do {
            fam_tab.(i) := fam_tab.(i) lor flag;
            let cpl = foi base ifam in
            add_ancestors (get_father cpl);
            add_ancestors (get_mother cpl)
          }
      | None -> () ]
    }
  in
  do {
    add_ancestors iper;
    if with_siblings then do {
      let add_sibling_spouse_parents ip =
        match get_parents (poi base ip) with
        [ Some ifam -> flag_family base per_tab fam_tab flag ifam
        | None -> () ]
      in
      let add_siblings_marriages ifam =
        let des = foi base ifam in
        Array.iter
          (fun ip ->
             let i = Adef.int_of_iper ip in
             do {
               per_tab.(i) := per_tab.(i) lor flag;
               Array.iter
                 (fun ifam ->
                    let i = Adef.int_of_ifam ifam in
                    let cpl = foi base ifam in
                    do {
                      fam_tab.(i) := fam_tab.(i) lor flag;
                      List.iter
                        (fun ip ->
                           let i = Adef.int_of_iper ip in
                           do {
                             per_tab.(i) := per_tab.(i) lor flag;
                             add_sibling_spouse_parents ip;
                           })
                        [get_father cpl; get_mother cpl]
                    })
                 (get_family (poi base ip))
             })
          (get_children des)
      in
      let add_siblings iparent =
        Array.iter
          (fun ifam ->
             do {
               flag_family base per_tab fam_tab flag ifam;
               add_siblings_marriages ifam
             })
          (get_family (poi base iparent))
      in
      let anc_flag = 4 in
      let rec ancestors_loop iper =
        let i = Adef.int_of_iper iper in
        if per_tab.(i) land anc_flag <> 0 then ()
        else do {
          per_tab.(i) := per_tab.(i) lor anc_flag;
          match get_parents (poi base iper) with
          [ Some ifam ->
              let i = Adef.int_of_ifam ifam in
              if fam_tab.(i) land anc_flag <> 0 then ()
              else do {
                fam_tab.(i) := fam_tab.(i) lor anc_flag;
                let cpl = foi base ifam in
                add_siblings (get_father cpl);
                add_siblings (get_mother cpl);
                ancestors_loop (get_father cpl);
                ancestors_loop (get_mother cpl)
              }
          | None -> () ]
        }
      in
      let rec remove_anc_flag iper =
        let i = Adef.int_of_iper iper in
        if per_tab.(i) land anc_flag <> 0 then do {
          per_tab.(i) := per_tab.(i) land lnot anc_flag;
          match get_parents (poi base iper) with
          [ Some ifam ->
              let i = Adef.int_of_ifam ifam in
              if fam_tab.(i) land anc_flag <> 0 then do {
                fam_tab.(i) := fam_tab.(i) land lnot anc_flag;
                let cpl = foi base ifam in
                remove_anc_flag (get_father cpl);
                remove_anc_flag (get_mother cpl)
              }
              else ()
          | None -> () ]
        }
        else ()
      in
      ancestors_loop iper;
      remove_anc_flag iper
    }
    else ()
  }
;

value select_descendants
  base per_tab fam_tab no_spouses_parents flag iper maxlev
=
  let mark = Array.create (nb_of_families base) False in
  let select_family ifam cpl =
    let i = Adef.int_of_ifam ifam in
    do {
      fam_tab.(i) := fam_tab.(i) lor flag;
      let i = Adef.int_of_iper (get_father cpl) in
      per_tab.(i) := per_tab.(i) lor flag;
      let i = Adef.int_of_iper (get_mother cpl) in
      per_tab.(i) := per_tab.(i) lor flag
    }
  in
  let rec loop lev iper =
    if maxlev >= 0 && lev > maxlev then ()
    else
      let i = Adef.int_of_iper iper in
      do {
        per_tab.(i) := per_tab.(i) lor flag;
        Array.iter
          (fun ifam ->
             let i = Adef.int_of_ifam ifam in
             if mark.(i) then ()
             else do {
               let fam = foi base ifam in
               mark.(i) := True;
               select_family ifam fam;
               if not no_spouses_parents then
                 let sp = spouse iper fam in
                 match get_parents (poi base sp) with
                 [ Some ifam -> select_family ifam (foi base ifam)
                 | None -> () ]
               else ();
               Array.iter (loop (succ lev)) (get_children fam);
             })
          (get_family (poi base iper))
      }
  in
  loop 0 iper
;

value select_descendants_ancestors base per_tab fam_tab no_spouses_parents ip =
  let new_mark = let r = ref 0 in fun () -> do { incr r; r.val } in
  let tab = Array.create (nb_of_persons base) (new_mark ()) in
  let anc_mark = new_mark () in
  let anclist =
    loop [] ip where rec loop list ip =
      if tab.(Adef.int_of_iper ip) = anc_mark then list
      else do {
        tab.(Adef.int_of_iper ip) := anc_mark;
        match get_parents (poi base ip) with
        [ Some ifam ->
            let cpl = foi base ifam in
            let list = loop list (get_father cpl) in
            loop list (get_mother cpl)
        | None ->
            [ip :: list] ]
      }
  in
  let des_mark = new_mark () in
  List.iter
    (fun ip ->
       loop ip where rec loop ip =
         if tab.(Adef.int_of_iper ip) = des_mark then ()
         else do {
           let u = poi base ip in
           for i = 0 to Array.length (get_family u) - 1 do {
             let ifam = (get_family u).(i) in
             fam_tab.(Adef.int_of_ifam ifam) :=
               fam_tab.(Adef.int_of_ifam ifam) lor 1;
             let des = foi base ifam in
             for i = 0 to Array.length (get_children des) - 1 do {
               loop (get_children des).(i);
             };
           };
           tab.(Adef.int_of_iper ip) := des_mark;
           per_tab.(Adef.int_of_iper ip) :=
             per_tab.(Adef.int_of_iper ip) lor 1;
         })
    anclist
;

value select_surname base per_tab fam_tab no_spouses_parents surname =
  let surname = Name.strip_lower surname in
  for i = 0 to nb_of_families base - 1 do {
    let fam = foi base (Adef.ifam_of_int i) in
    if is_deleted_family fam then ()
    else
      let fath = poi base (get_father fam) in
      let moth = poi base (get_mother fam) in
      if Name.strip_lower (sou base (get_surname fath)) = surname ||
         Name.strip_lower (sou base (get_surname moth)) = surname
      then do {
        fam_tab.(i) := True;
        per_tab.(Adef.int_of_iper (get_father fam)) := True;
        per_tab.(Adef.int_of_iper (get_mother fam)) := True;
        Array.iter
          (fun ic ->
             let p = poi base ic in
             if not per_tab.(Adef.int_of_iper ic) &&
                Name.strip_lower (sou base (get_surname p)) = surname
             then
               per_tab.(Adef.int_of_iper ic) := True
             else ())
          (get_children fam);
        if no_spouses_parents then ()
        else
          List.iter
            (fun x ->
               match get_parents (poi base x) with
               [ Some ifam when not fam_tab.(Adef.int_of_ifam ifam) ->
                   let cpl = foi base ifam in
                   do {
                     fam_tab.(Adef.int_of_ifam ifam) := True;
                     per_tab.(Adef.int_of_iper (get_father cpl)) := True;
                     per_tab.(Adef.int_of_iper (get_mother cpl)) := True
                   }
               | _ -> () ])
            [get_father fam; get_mother fam]
      }
      else ()
  }
;

value select_ancestors_descendants base anc desc ancdesc no_spouses_parents
    censor with_siblings maxlev =
  let tm = Unix.localtime (Unix.time ()) in
  let threshold = 1900 + tm.Unix.tm_year - censor in
  match (anc, desc, ancdesc) with
  [ (None, None, None) ->
      if censor <> 0 || censor = -1 then
        let per_tab = Array.create (nb_of_persons base) 0 in
        let fam_tab = Array.create (nb_of_families base) 0 in
        let _ =
          if censor = -1 then restrict_base base per_tab fam_tab 1      
          else censor_base base per_tab fam_tab 1 threshold
        in
        (fun i -> per_tab.(Adef.int_of_iper i) = 0,
         fun i -> fam_tab.(Adef.int_of_ifam i) = 0)
      else (fun _ -> True, fun _ -> True)
  | (None, None, Some iadper) ->
      let per_tab = Array.create (nb_of_persons base) 0 in
      let fam_tab = Array.create (nb_of_families base) 0 in
      let _ =
        if censor = -1 then restrict_base base per_tab fam_tab 4
        else if censor <> 0 then censor_base base per_tab fam_tab 4 threshold
        else ()
      in
      do {
        select_descendants_ancestors base per_tab fam_tab no_spouses_parents
          iadper;
        (fun i ->
           let fl = per_tab.(Adef.int_of_iper i) in
           fl < 4 && fl > 0,
         fun i ->
           let fl = fam_tab.(Adef.int_of_ifam i) in
           fl < 4 && fl > 0)
      }
  | _ ->
      let per_tab = Array.create (nb_of_persons base) 0 in
      let fam_tab = Array.create (nb_of_families base) 0 in
      let _ =
        if censor = -1 then restrict_base base per_tab fam_tab 4
        else if censor <> 0 then censor_base base per_tab fam_tab 4 threshold
        else ()
      in
      match (anc, desc) with
      [ (Some iaper, None) ->
          do {
            select_ancestors base per_tab fam_tab with_siblings 1 iaper;
            (fun i -> per_tab.(Adef.int_of_iper i) = 1,
             fun i -> fam_tab.(Adef.int_of_ifam i) = 1)
          }
      | (None, Some idper) ->
          do {
            select_descendants base per_tab fam_tab no_spouses_parents 1
              idper maxlev;
            (fun i -> per_tab.(Adef.int_of_iper i) = 1,
             fun i -> fam_tab.(Adef.int_of_ifam i) = 1)
          }
      | (Some iaper, Some idper) ->
          do {
            select_ancestors base per_tab fam_tab False 1 iaper;
            select_descendants base per_tab fam_tab no_spouses_parents 2
              idper maxlev;
            (fun i -> per_tab.(Adef.int_of_iper i) = 3,
             fun i -> fam_tab.(Adef.int_of_ifam i) = 3)
          }
      | _ -> assert False ] ]
;

value select_surnames base surnames no_spouses_parents =
  let per_tab = Array.create (nb_of_persons base) False in
  let fam_tab = Array.create (nb_of_families base) False in
  do {
    List.iter (select_surname base per_tab fam_tab no_spouses_parents)
      surnames;
    (fun i -> per_tab.(Adef.int_of_iper i),
     fun i -> fam_tab.(Adef.int_of_ifam i))
  }
;

value functions
    base anc desc surnames ancdesc no_spouses_parents censor with_siblings
    maxlev =
  let (per_sel1, fam_sel1) =
    select_ancestors_descendants base anc desc ancdesc no_spouses_parents
      censor with_siblings maxlev
  in
  let (per_sel2, fam_sel2) =
    select_surnames base surnames no_spouses_parents
  in
  match (censor, anc, desc, ancdesc, surnames) with
  [ (0, None, None, None, [_ :: _]) -> (per_sel2, fam_sel2)
  | (_, _, _, _, []) -> (per_sel1, fam_sel1)
  | (0, _, _, _, _) ->
      (fun i -> per_sel1 i || per_sel2 i,
       fun i -> fam_sel1 i || fam_sel2 i)
  | _ ->
      (fun i -> per_sel1 i && per_sel2 i,
       fun i -> fam_sel1 i && fam_sel2 i) ]
;
