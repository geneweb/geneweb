(* $Id: checkItem.ml,v 1.11 2007-09-05 13:16:45 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Def;
open Gwdb;

type base_error = error person;
type base_warning = warning person family title;
type base_misc = misc person family title;

value common_prec p1 p2 =
  if p1 = p2 then p1
  else
    match (p1, p2) with
    [ (Sure, _) -> p2
    | (_, Sure) -> p1
    | _ -> Maybe ]
;

value leap_year a =
  if a mod 100 = 0 then a / 100 mod 4 = 0 else a mod 4 = 0
;

value nb_days_in_month =
  let tb = [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |] in
  fun m a ->
    if m = 2 && leap_year a then 29
    else if m >= 1 && m <= 12 then tb.(m - 1)
    else 0
;

value time_elapsed d1 d2 =
  let prec = common_prec d1.prec d2.prec in
  match d1 with
  [ {day = 0; month = 0; year = a1} ->
      {day = 0; month = 0; year = d2.year - a1; prec = prec; delta = 0}
  | {day = 0; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0} ]
  | {day = j1; month = m1; year = a1} ->
      match d2 with
      [ {day = 0; month = 0; year = a2} ->
          {day = 0; month = 0; year = a2 - a1; prec = prec; delta = 0}
      | {day = 0; month = m2; year = a2} ->
          let r = 0 in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = 0; month = month; year = year; prec = prec; delta = 0}
      | {day = j2; month = m2; year = a2} ->
          let (day, r) =
            if j1 <= j2 then (j2 - j1, 0)
            else (j2 - j1 + nb_days_in_month m1 a1, 1)
          in
          let (month, r) =
            if m1 + r <= m2 then (m2 - m1 - r, 0) else (m2 - m1 - r + 12, 1)
          in
          let year = a2 - a1 - r in
          {day = day; month = month; year = year; prec = prec; delta = 0} ] ]
;

value strictly_before_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_elapsed d2 d1 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d1.prec = d2.prec then False
  else if d1.prec = Before && d2.prec = After then True
  else False
;

value strictly_before d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_before_dmy d1 d2
  | _ -> False ]
;

value strictly_after_dmy d1 d2 =
  let {day = d; month = m; year = y; prec = p} = time_elapsed d1 d2 in
  if y < 0 then True
  else if y > 0 then False
  else if m < 0 then True
  else if m > 0 then False
  else if d < 0 then True
  else if d > 0 then False
  else if d2.prec = d1.prec then False
  else if d2.prec = Before && d1.prec = After then True
  else False
;

value strictly_after d1 d2 =
  match (d1, d2) with
  [ (Dgreg d1 _, Dgreg d2 _) -> strictly_after_dmy d1 d2
  | _ -> False ]
;

value birth_before_death base warning p =
  match (Adef.od_of_codate (get_birth p), get_death p) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then warning (BirthAfterDeath p) else ()
  | _ -> () ]
;

value titles_after_birth base warning p t =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  do {
    match (t_date_start, t_date_end) with
    [ (Some d1, Some d2) ->
        if strictly_after d1 d2 then warning (TitleDatesError p t) else ()
    | _ -> () ];
    match Adef.od_of_codate (get_birth p) with
    [ Some d1 ->
        do {
          match t_date_start with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          match t_date_end with
          [ Some d ->
              if strictly_after d1 d then warning (TitleDatesError p t)
              else ()
          | None -> () ];
          ()
        }
    | _ -> () ];
  }
;

value try_to_fix_relation_sex base warning p_ref = do {
  let p_index = Some (get_key_index p_ref) in
  let fixed = ref 0 in
  let not_fixed = ref 0 in
  let changed_related =
    List.fold_right
      (fun ip changed_related ->
         let p = poi base ip in
         let (rparents, changed, not_changed) =
           List.fold_right
             (fun rel (rparents, changed, not_changed) ->
                let (rel, changed, not_changed) =
                  match (p_index = rel.r_fath, p_index = rel.r_moth) with
                  [ (True, False) ->
                      if get_sex p_ref = Female then
                        match rel.r_moth with
                        [ Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Male then
                              let rel =
                                {(rel) with
                                 r_fath = rel.r_moth; r_moth = p_index}
                              in
                              (rel, changed + 1, not_changed)
                            else
                              (rel, changed, not_changed + 1)
                        | None ->
                            let rel =
                              {(rel) with r_fath = None; r_moth = p_index}
                            in
                            (rel, changed + 1, not_changed) ]
                      else (rel, changed, not_changed)
                  | (False, True) ->
                      if get_sex p_ref = Male then
                        match rel.r_fath with
                        [ Some ip ->
                            let oth_p = poi base ip in
                            if get_sex oth_p = Female then
                              let rel =
                                {(rel) with
                                 r_moth = rel.r_fath; r_fath = p_index}
                              in
                              (rel, changed + 1, not_changed)
                            else
                              (rel, changed, not_changed + 1)
                        | None ->
                            let rel =
                              {(rel) with r_moth = None; r_fath = p_index}
                            in
                            (rel, changed + 1, not_changed) ]
                      else (rel, changed, not_changed)
                  | (False, False) -> (rel, changed, not_changed)
                  | (True, True) -> (rel, changed, not_changed + 1) ]
                in
                ([rel :: rparents], changed, not_changed))
             (get_rparents p) ([], 0, 0)
         in
         let _ = do {
           fixed.val := fixed.val + changed;
           not_fixed.val := not_fixed.val + not_changed
         }
         in
         if changed > 0 then [(ip, p, None, Some rparents) :: changed_related]
         else changed_related)
      (get_related p_ref) []
  in
  warning (IncoherentSex p_ref fixed.val not_fixed.val);
  if fixed.val > 0 then Some changed_related else None
};

value related_sex_is_coherent base warning p_ref =
  let p_index = Some (get_key_index p_ref) in
  let merge_sex g1 g2 =
    match (g1, g2) with
    [ (Some Male, Some Male) -> Some Male
    | (Some Female, Some Female) -> Some Female
    | (Some Neuter, Some g) -> Some g
    | (Some g, Some Neuter) -> Some g
    | _ -> None ]
  in
  let check_sex sex rparents =
    List.fold_left
      (fun g rel ->
         match (p_index = rel.r_fath, p_index = rel.r_moth) with
         [ (True, False) -> merge_sex g (Some Male)
         | (False, True) -> merge_sex g (Some Female)
         | (False, False) -> g
         | (True, True) -> None ])
      sex rparents
  in
  let new_sex =
    List.fold_left
      (fun g ip ->
         let p = poi base ip in
         check_sex g (get_rparents p))
      (Some (get_sex p_ref)) (get_related p_ref)
  in
  match new_sex with
  [ Some g ->
      if get_sex p_ref != g then
        Some [(get_key_index p_ref, p_ref, Some g, None)]
      else None
  | None -> try_to_fix_relation_sex base warning p_ref ]
;

value check_normal_marriage_date_for_someone base error warning fam ip =
  let p = poi base ip in
  match Adef.od_of_codate (get_marriage fam) with
  [ Some d2 ->
      do {
        match Adef.od_of_codate (get_birth p) with
        [ Some d1 ->
            if strictly_before d2 d1 then
              warning (MarriageDateBeforeBirth p)
            else ()
        | _ -> () ];
        match get_death p with
        [ Death _ d3 ->
            let d3 = Adef.date_of_cdate d3 in
            if strictly_after d2 d3 then warning (MarriageDateAfterDeath p)
            else ()
        | _ -> () ];
      }
  | None -> () ]
;


(* ************************************************************************* *)
(*  [Fonc] check_normal_marriage_date_for_witness : 
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie les dates des témoins par rapport à la date du
                    mariage.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value check_normal_marriage_date_for_witness base error warning (ifam, fam) =
  let wl = foi base ifam in
  List.iter
    (fun ip -> check_normal_marriage_date_for_someone base error warning fam ip)
    (Array.to_list (get_witnesses wl))
;


(* ************************************************************************* *)
(*  [Fonc] check_normal_marriage_date_for_parent :
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie les dates du conjoint1 et du conjoint2 par
                    rapport à la date du mariage.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value check_normal_marriage_date_for_parent base error warning (ifam, fam) = 
  do {
    let cpl = foi base ifam in
    check_normal_marriage_date_for_someone base error warning fam
      (get_father cpl);
    check_normal_marriage_date_for_someone base error warning fam
      (get_mother cpl)
  }
;


(*
 * Semi sort children by birth dates.
 * If all children have birth dates, no problem.
 * Otherwise, sorting groups of consecutive children who have dates.
 * In not possible cases, try to keep order of children of same sex.
 *   ex: G1, B2 being resp. girl and boy with date(G1) < date(B2)
 *       and G and B begin resp. girls boys without dates
 *     if order is ... B2 B B B G1 ... it becomes ... G1 B2 B B B ...
 *     if order is ... B2 G G G G1 ... it becomes ... G G G G1 B2 ...
 *     if order is ... B2 G B G G1 ... no change (a warning appears).
 *)

value semi_sort base a before comp di =
  loop where rec loop i =
    if i < 0 || i >= Array.length a then ()
    else
      let p1 = poi base a.(i) in
      let d1 =
        match Adef.od_of_codate (get_birth p1) with
        [ Some d1 -> Some d1
        | None -> Adef.od_of_codate (get_baptism p1) ]
      in
      match d1 with
      [ Some d1 ->
          loop_j None (i - di) where rec loop_j sex_interm_sib j =
            if j < 0 || j >= Array.length a then loop (i + di)
            else
              let p2 = poi base a.(j) in
              let d2 =
                match Adef.od_of_codate (get_birth p2) with
                [ Some d2 -> Some d2
                | None -> Adef.od_of_codate (get_baptism p2) ]
              in
              match d2 with
              [ Some d2 ->
                  if comp d1 d2 then do {
                    let j =
                      match sex_interm_sib with
                      [ Some s ->
                          if s = get_sex p1 then None
                          else if s = get_sex p2 then Some j
                          else None
                      | None -> Some j ]
                    in
                    match j with
                    [ Some j ->
                        let k =
                          loop_k (j - di) where rec loop_k k =
                            if k < 0 || k >= Array.length a then k + di
                            else
                              let p3 = poi base a.(k) in
                              let d3 =
                                match Adef.od_of_codate (get_birth p3) with
                                [ Some d3 -> Some d3
                                | None -> Adef.od_of_codate (get_baptism p3) ]
                              in
                              match d3 with
                              [ Some d3 ->
                                  if comp d1 d3 then loop_k (k - di)
                                  else k + di
                              | None -> k + di ]
                        in
                        do  {
                          match before.val with
                          [ Some _ -> ()
                          | None -> before.val := Some (Array.copy a) ];
                          let ip = a.(i) in
                          loop_up i where rec loop_up j =
                            if j = k then ()
                            else do {
                              a.(j) := a.(j - di);
                              loop_up (j - di)
                            };
                          a.(k) := ip;
                          loop (i + di)
                        }
                    | None -> loop (i + di) ]
                  }
                  else loop (i + di)
              | None ->
                  match sex_interm_sib with
                  [ Some s ->
                      if s = get_sex p2 then loop_j sex_interm_sib (j - di)
                      else loop (i + di)
                  | None -> loop_j (Some (get_sex p2)) (j - di) ] ]
      | None -> loop (i + di) ]
;

value sort_children base children = do {
  let before = ref None in
  semi_sort base children before strictly_before 1 1;
  semi_sort base children before strictly_after ~-1 1;
  semi_sort base children before strictly_before 1 1;
  match before.val with
  [ Some b -> Some (b, children)
  | None -> None ]
};

value sort_children2 base warning ifam des =
  let b = get_children des in
  match sort_children base b with
  [ None -> b
  | Some (b, a) -> do {
      warning (ChangedOrderOfChildren ifam des b a);
      a
    } ]
;

value born_after_his_elder_sibling base error warning x np ifam des =
  match (np, Adef.od_of_codate (get_birth x), get_death x) with
  [ (None, _, _) -> ()
  | (Some (elder, d1), Some d2, _) ->
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | (Some (elder, d1), _, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (ChildrenNotInOrder ifam des elder x)
      else ()
  | _ -> () ]
;

value date_of_death =
  fun
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value year_of d = d.year;

value child_born_after_his_parent base error warning x iparent =
  let parent = poi base iparent in
  match
    (Adef.od_of_codate (get_birth parent), Adef.od_of_codate (get_birth x),
     date_of_death (get_death x))
  with
  [ (Some (Dgreg g1 _ as d1), Some (Dgreg g2 _ as d2), _) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_elapsed g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | (Some (Dgreg g1 _ as d1), _, Some (Dgreg g2 _ as d2)) ->
      if strictly_after d1 d2 then warning (ParentBornAfterChild parent x)
      else
        let a = time_elapsed g1 g2 in
        if year_of a < 11 then warning (ParentTooYoung parent a) else ()
  | _ -> () ]
;

value child_born_before_mother_death base warning x imoth =
  let mother = poi base imoth in
  match (Adef.od_of_codate (get_birth x), get_death mother) with
  [ (Some d1, Death _ d2) ->
      let d2 = Adef.date_of_cdate d2 in
      if strictly_after d1 d2 then
        warning (MotherDeadAfterChildBirth mother x)
      else ()
  | _ -> () ]
;

value possible_father base warning x ifath =
  let father = poi base ifath in
  match
    (Adef.od_of_codate (get_birth x), date_of_death (get_death father))
  with
  [ (Some (Dgreg {prec = Before} _), _) |
    (_, Some (Dgreg {prec = After} _)) ->
      ()
  | (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
      let a2 =
        match d2 with
        [ {prec = YearInt a2} -> a2
        | {prec = OrYear a2} -> a2
        | {year = a} -> a ]
      in
      if year_of d1 > a2 + 1 then warning (DeadTooEarlyToBeFather father x)
      else ()
  | _ -> () ]
;

value child_has_sex warning child =
  if get_sex child = Neuter then warning (UndefinedSex child) else ()
;


(* ************************************************************************* *)
(*  [Fonc] check_marriage_sex :
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie le sex du couple et s'il est correct la date de
                    naissance par rapport à la date de décès.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value check_marriage_sex base error warning fam =
  let cpl = fam in
  let fath = poi base (get_father cpl) in
  let moth = poi base (get_mother cpl) in
  do {
    match get_sex fath with
    [ Male -> birth_before_death base warning fath
    | Female | Neuter ->
        if get_relation fam = NoSexesCheckNotMarried
        || get_relation fam = NoSexesCheckMarried then ()
        else error (BadSexOfMarriedPerson fath) ];
    match get_sex moth with
    [ Female -> birth_before_death base warning moth
    | Male | Neuter ->
        if get_relation fam = NoSexesCheckNotMarried ||
           get_relation fam = NoSexesCheckMarried then ()
        else error (BadSexOfMarriedPerson moth) ]
};


(* ************************************************************************* *)
(*  [Fonc] check_children :
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        (ifam * family) -> unit                                              *)
(** [Description] : Vérifie toutes les informations des enfants d'une famille.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - family  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value check_children base error warning (ifam, fam) =
  let cpl = fam in
  let des = fam in
  let after = sort_children2 base warning ifam des in
  let _ =
    List.fold_left
      (fun np child ->
         let child = poi base child in
         do {
           birth_before_death base warning child;
           born_after_his_elder_sibling base error warning child np ifam
             des;
           child_born_after_his_parent base error warning child
             (get_father cpl);
           child_born_after_his_parent base error warning child
             (get_mother cpl);
           child_born_before_mother_death base warning child
             (get_mother cpl);
           possible_father base warning child (get_father cpl);
           child_has_sex warning child;
           match Adef.od_of_codate (get_birth child) with
           [ Some d -> Some (child, d)
           | _ -> np ]
         })
      None (Array.to_list after)
  in
  ()
;

value has_family_sources fam =
  not (is_empty_string (get_fsources fam) 
       && is_empty_string (get_marriage_src fam))
;

value has_person_sources p =
  not (is_empty_string (get_psources p) 
       && is_empty_string (get_baptism_src p)
       && is_empty_string (get_birth_src p) 
       && is_empty_string (get_death_src p) 
       && is_empty_string (get_burial_src p))
;


(* ************************************************************************* *)
(*  [Fonc] check_sources : 
      base -> (Def.misc -> unit) -> ifam -> family -> unit                   *)
(** [Description] : Il y a un avertissment 'miscellaneous' si aucune des 
                    personnes (conjoint1 ET conjoint2) n'a de sources 
                    (indiduelles ou familliales).
    [Args] :
      - base : base
      - misc : fonction qui ajoute un misc à la liste des miscs
      - ifam : ifam
      - fam  : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value check_sources base misc ifam fam =
  if has_family_sources fam then ()
  else
    let cpl = foi base ifam in
    let fath = poi base (get_father cpl) in
    let moth = poi base (get_mother cpl) in
    if has_person_sources fath && has_person_sources moth then ()
    else misc MissingSources
;

(* main *)


(* ************************************************************************* *)
(*  [Fonc] person : base -> (Def.warning -> unit) -> person -> unit          *)
(** [Description] : Vérifie les warnings d'une personne à la validation du
                    formulaire individu.
    [Args] :
      - base    : base
      - warning : fonction qui ajoute un warning à la liste des warnings
      - p       : person
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value person base warning p = do {
  birth_before_death base warning p;
  List.iter (titles_after_birth base warning p) (get_titles p);
  related_sex_is_coherent base warning p;
};


(* ************************************************************************* *)
(*  [Fonc] family :
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        ifam -> family -> unit                                               *)
(** [Description] : En cas de modification d'une famille, on vérifie toutes
                    les personnes accessibles après la validation du 
                    formulaire (famille). 
                    Vérifie s'il y a des erreurs ou des warnings pour le 
                    couple, les parents du couple, les témoins et les enfants
                    du couple.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - fam     : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value family base error warning ifam fam =
  do {
    check_marriage_sex base error warning fam;
    check_normal_marriage_date_for_parent base error warning (ifam, fam);
    check_normal_marriage_date_for_witness base error warning (ifam, fam);
    check_children base error warning (ifam, fam)
  }
;


(* ************************************************************************* *)
(*  [Fonc] reduce_family :
      base -> (Def.error -> unit) -> (Def.warning -> unit) -> 
        ifam -> family -> unit                                               *)
(** [Description] : En cas de modification d'une personne, on ne vérifie que
                    les personnes accessibles après la validation du 
                    formulaire (individu). 
                    Vérifie s'il y a des erreurs ou des warnings pour le 
                    couple, les parents du couple et les enfants du couple.
    [Args] :
      - base    : base
      - error   : fonction qui ajoute une erreur à la liste des erreurs
      - warning : fonction qui ajoute un warning à la liste des warnings
      - ifam    : ifam
      - fam     : family
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                          *)
(* ************************************************************************* *)
value reduce_family base error warning ifam fam =
  do {
    check_marriage_sex base error warning fam;
    check_normal_marriage_date_for_parent base error warning (ifam, fam);
    check_children base error warning (ifam, fam)
  }
;


(* ************************************************************************* *)
(*  [Fonc] check_other_fields : 
      base -> (Def.misc -> unit) -> ifam -> family -> unit                   *)
(** [Description] : Vérifie les autres champs de saisie des formulaires 
                    individu et famille.
    [Args] :
      - base : base
      - misc : fonction qui ajoute un misc à la liste des miscs
      - ifam : ifam
      - fam  : family
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                              *)
(* ************************************************************************* *)
value check_other_fields base misc ifam fam =
 do {
   check_sources base misc ifam fam
 }
;
