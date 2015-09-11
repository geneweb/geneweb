(* camlp5r ./pa_html.cmo *)
(* $Id: updateInd.ml,v 5.15 2008-01-08 11:58:46 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gwdb;
open Hutil;
open TemplAst;
open Util;
open Printf;

value bogus_person_index = Adef.iper_of_int (-1);

value string_person_of base p =
  let fp ip =
    let p = poi base ip in
    (sou base (get_first_name p), sou base (get_surname p), get_occ p,
     Update.Link, "")
  in
  Futil.map_person_ps fp (sou base) (gen_person_of_person p)
;

type event_weights = list (option int);

(* Interpretation of template file 'updind.txt' *)

type env 'a =
  [ Vstring of string
  | Vint of int
  | Vother of 'a
  | Vcnt of ref int
  | Vbool of bool
  | Vevent_weights of event_weights
  | Vnone ]
;

value pevent_weight_str = "pevent_weights";

value get_env v env = try List.assoc v env with [ Not_found -> Vnone ];
value get_env_weights v env =
  try match List.assoc v env with
        [ Vevent_weights xs -> xs
        | _ -> failwith (sprintf "Event weights variable '%s' has bad type" v) ]
  with [ Not_found ->failwith (sprintf "Event weights variable '%s' is not found" v) ]
;
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value obsolete_list = ref [];

value obsolete version var new_var r =
  if List.mem var obsolete_list.val then r
  else IFDEF UNIX THEN do {
    Printf.eprintf "*** <W> updind.txt: \"%s\" obsolete since v%s%s\n"
      var version
      (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
    flush stderr;
    obsolete_list.val := [var :: obsolete_list.val];
    r
  }
  ELSE r END
;

value bool_val x = VVbool x;
value str_val x = VVstring x;

value rec eval_var conf base env p loc sl =
  try eval_special_var conf base p sl with
  [ Not_found -> eval_simple_var conf base env p loc sl ]
and eval_simple_var conf base env p loc =
  fun
  [ ["alias"] -> eval_string_env "alias" env
  | ["acc_if_titles"] -> bool_val (p.access = IfTitles)
  | ["acc_private"] -> bool_val (p.access = Private)
  | ["acc_public"] -> bool_val (p.access = Public)
  | ["bapt_place"] -> str_val (quote_escaped p.baptism_place)
  | ["bapt_note"] -> str_val (quote_escaped p.baptism_note)
  | ["bapt_src"] -> str_val (quote_escaped p.baptism_src)
  | ["birth"; s] -> eval_date_var (Adef.od_of_codate p.birth) s
  | ["birth_place"] -> str_val (quote_escaped p.birth_place)
  | ["birth_note"] -> str_val (quote_escaped p.birth_note)
  | ["birth_src"] -> str_val (quote_escaped p.birth_src)
  | ["bapt"; s] -> eval_date_var (Adef.od_of_codate p.baptism) s
  | ["bt_buried"] ->
        bool_val (match p.burial with [ Buried _ -> True | _ -> False ])
  | ["bt_cremated"] ->
        bool_val (match p.burial with [ Cremated _ -> True | _ -> False ])
  | ["bt_unknown_burial"] -> bool_val (p.burial = UnknownBurial)
  | ["burial"; s] ->
      let od =
        match p.burial with
        [ Buried cod -> Adef.od_of_codate cod
        | Cremated cod -> Adef.od_of_codate cod
        | _ -> None ]
      in
      eval_date_var od s
  | ["burial_place"] -> str_val (quote_escaped p.burial_place)
  | ["burial_note"] -> str_val (quote_escaped p.burial_note)
  | ["burial_src"] -> str_val (quote_escaped p.burial_src)
  | ["cnt"] -> eval_int_env "cnt" env
  | ["dead_dont_know_when"] -> bool_val (p.death = DeadDontKnowWhen)
  | ["death"; s] ->
      let od =
        match p.death with
        [ Death _ cd -> Some (Adef.date_of_cdate cd)
        | _ -> None ]
      in
      eval_date_var od s
  | ["death_place"] -> str_val (quote_escaped p.death_place)
  | ["death_note"] -> str_val (quote_escaped p.death_note)
  | ["death_src"] -> str_val (quote_escaped p.death_src)
  | ["died_young"] -> bool_val (p.death = DeadYoung)
  | ["digest"] -> eval_string_env "digest" env
  | ["dont_know_if_dead"] -> bool_val (p.death = DontKnowIfDead)
  | ["dr_disappeared"] -> eval_is_death_reason Disappeared p.death
  | ["dr_executed"] -> eval_is_death_reason Executed p.death
  | ["dr_killed"] -> eval_is_death_reason Killed p.death
  | ["dr_murdered"] -> eval_is_death_reason Murdered p.death
  | ["dr_unspecified"] -> eval_is_death_reason Unspecified p.death
  | ["event_weight"] ->
     match get_env "weight" env with
       [ Vstring s -> str_val s
       | _         -> failwith "weight variable is not found" ]

  | ["event" :: sl] ->
      let e =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.pevents (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_event_var conf base env e sl
  | ["event_date"; s] ->
      let od =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let e = List.nth p.pevents (i - 1) in
              Adef.od_of_codate e.epers_date
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_date_var od s
  | ["first_name"] -> str_val (quote_escaped p.first_name)
  | ["first_name_alias"] -> eval_string_env "first_name_alias" env
  | ["has_aliases"] -> bool_val (p.aliases <> [])
  | ["has_birth_date"] -> bool_val (Adef.od_of_codate p.birth <> None)
  | ["has_pevent_birth"] ->
        loop p.pevents where rec loop pevents =
          match pevents with
          [ [] -> bool_val False
          | [evt :: l] ->
              if evt.epers_name = Epers_Birth then bool_val True
              else loop l ]
  | ["has_pevent_baptism"] ->
        loop p.pevents where rec loop pevents =
          match pevents with
          [ [] -> bool_val False
          | [evt :: l] ->
              if evt.epers_name = Epers_Baptism then bool_val True
              else loop l ]
  | ["has_pevent_death"] ->
        loop p.pevents where rec loop pevents =
          match pevents with
          [ [] -> bool_val False
          | [evt :: l] ->
              if evt.epers_name = Epers_Death then bool_val True
              else loop l ]
  | ["has_pevent_burial"] ->
        loop p.pevents where rec loop pevents =
          match pevents with
          [ [] -> bool_val False
          | [evt :: l] ->
              if evt.epers_name = Epers_Burial then bool_val True
              else loop l ]
  | ["has_pevent_cremation"] ->
        loop p.pevents where rec loop pevents =
          match pevents with
          [ [] -> bool_val False
          | [evt :: l] ->
              if evt.epers_name = Epers_Cremation then bool_val True
              else loop l ]
  | ["has_pevents"] -> bool_val (p.pevents <> [])
  | ["has_primary_pevents"] ->
      let rec loop pevents =
        match pevents with
        [ [] -> False
        | [evt :: l] ->
            match evt.epers_name with
            [ Epers_Birth | Epers_Baptism | Epers_Death |
              Epers_Burial | Epers_Cremation -> True
            | _ -> loop l ]]
      in
      bool_val (loop p.pevents)
  | ["has_secondary_pevents"] ->
      let rec loop pevents =
        match pevents with
        [ [] -> False
        | [evt :: l] ->
            match evt.epers_name with
            [ Epers_Birth | Epers_Baptism | Epers_Death |
              Epers_Burial | Epers_Cremation -> loop l
            | _ -> True ]]
      in
      bool_val (loop p.pevents)
  | ["has_first_names_aliases"] -> bool_val (p.first_names_aliases <> [])
  | ["has_qualifiers"] -> bool_val (p.qualifiers <> [])
  | ["has_relations"] -> bool_val (p.rparents <> [])
  | ["has_surnames_aliases"] -> bool_val (p.surnames_aliases <> [])
  | ["has_titles"] -> bool_val (p.titles <> [])
  | ["image"] -> str_val (quote_escaped p.image)
  | ["index"] -> str_val (string_of_int (Adef.int_of_iper p.key_index))
  | ["is_female"] -> bool_val (p.sex = Female)
  | ["is_male"] -> bool_val (p.sex = Male)
  | ["is_first"] ->
      match get_env "first" env with
      [ Vbool x -> bool_val x
      | _ -> raise Not_found ]
  | ["is_last"] ->
      match get_env "last" env with
      [ Vbool x -> bool_val x
      | _ -> raise Not_found ]
  | ["nb_pevents"] -> str_val (string_of_int (List.length p.pevents))
  | ["not_dead"] -> bool_val (p.death = NotDead)
  | ["notes"] -> str_val (quote_escaped p.notes)
  | ["next_pevent"] ->
      match get_env "next_pevent" env with
      [ Vcnt c -> str_val (string_of_int c.val)
      | _ -> str_val "" ]
  | ["incr_next_pevent"] ->
      match get_env "next_pevent" env with
      [ Vcnt c -> do { incr c; str_val "" }
      | _ -> str_val "" ]
  | ["occ"] -> str_val (if p.occ <> 0 then string_of_int p.occ else "")
  | ["occupation"] -> str_val (quote_escaped p.occupation)
  | ["of_course_dead"] -> bool_val (p.death = OfCourseDead)
  | ["public_name"] -> str_val (quote_escaped p.public_name)
  | ["qualifier"] -> eval_string_env "qualifier" env
  | ["relation" :: sl] ->
      let r =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.rparents (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_relation_var conf base env r sl
  | ["sources"] -> str_val (quote_escaped p.psources)
  | ["surname"] -> str_val (quote_escaped p.surname)
  | ["surname_alias"] -> eval_string_env "surname_alias" env
  | ["title" :: sl] ->
      let t =
        match get_env "cnt" env with
        [ Vint i ->
            try Some (List.nth p.titles (i - 1)) with [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_title_var conf base env t sl
  | ["title_date_start"; s] ->
      let od =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_start
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_date_var od s
  | ["title_date_end"; s] ->
      let od =
        match get_env "cnt" env with
        [ Vint i ->
            try
              let t = List.nth p.titles (i - 1) in
              Adef.od_of_codate t.t_date_end
            with
            [ Failure _ -> None ]
        | _ -> None ]
      in
      eval_date_var od s
  | ["wcnt"] -> eval_int_env "wcnt" env
  | ["has_witness"] ->
      match get_env "cnt" env with
      [ Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with [ Failure _ -> None ]
          in
          match e with
          [ Some e -> bool_val (e.epers_witnesses <> [| |])
          | None -> raise Not_found ]
      | _ -> raise Not_found ]
  | ["witness" :: sl] ->
      match get_env "cnt" env with
      [ Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with [ Failure _ -> None ]
          in
          match e with
          [ Some e ->
              match get_env "wcnt" env with
              [ Vint i ->
                  let i = i - 1 in
                  let k =
                    if i >= 0 && i < Array.length e.epers_witnesses then
                      fst e.epers_witnesses.(i)
                    else if
                      i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                    then
                      ("", "", 0, Update.Create Neuter None, "")
                    else raise Not_found
                  in
                  eval_person_var conf base env k sl
              | _ -> raise Not_found ]
          | None -> raise Not_found ]
      | _ -> raise Not_found ]
  | ["witness_kind"] ->
      match get_env "cnt" env with
      [ Vint i ->
          let e =
            try Some (List.nth p.pevents (i - 1)) with [ Failure _ -> None ]
          in
          match e with
          [ Some e ->
              match get_env "wcnt" env with
              [ Vint i ->
                  let i = i - 1 in
                  if i >= 0 && i < Array.length e.epers_witnesses then
                    match snd e.epers_witnesses.(i) with
                    [ Witness_GodParent -> str_val "godp"
                    | _ -> str_val "" ]
                  else if
                    i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                  then
                    str_val ""
                  else raise Not_found
              | _ -> raise Not_found ]
          | None -> raise Not_found ]
      | _ -> raise Not_found ]
  | [s] ->
      let v = extract_var "evar_" s in
      if v <> "" then
        match p_getenv (conf.env @ conf.henv) v with
        [ Some vv -> str_val (quote_escaped vv)
        | None -> str_val "" ]
      else
        let v = extract_var "bvar_" s in
        let v =
          if v = "" then extract_var "cvar_" s (* deprecated since 5.00 *)
          else v
        in
        if v <> "" then
          str_val (try List.assoc v conf.base_env with [ Not_found -> "" ])
        else raise Not_found
  | _ -> raise Not_found ]
and eval_date_var od s = str_val (eval_date_var_aux od s)
and eval_date_var_aux od =
  fun
  [ "calendar" ->
      match od with
      [ Some (Dgreg _ Dgregorian) -> "gregorian"
      | Some (Dgreg _ Djulian) -> "julian"
      | Some (Dgreg _ Dfrench) -> "french"
      | Some (Dgreg _ Dhebrew) -> "hebrew"
      | _ -> "" ]
  | "day" ->
      match eval_date_field od with
      [ Some d -> if d.day = 0 then "" else string_of_int d.day
      | None -> "" ]
  | "month" ->
      match eval_date_field od with
      [ Some d ->
          if d.month = 0 then ""
          else
            match od with
            [ Some (Dgreg _ Dfrench) -> short_f_month d.month
            | _ -> string_of_int d.month ]
      | None -> "" ]
  | "orday" ->
      match eval_date_field od with
      [ Some d ->
          match d.prec with
          [ OrYear d2 | YearInt d2 ->
              if d2.day2 = 0 then ""
              else string_of_int d2.day2
          | _ -> "" ]
      | None -> "" ]
  | "ormonth" ->
      match eval_date_field od with
      [ Some d ->
          match d.prec with
          [ OrYear d2 | YearInt d2 ->
              if d2.month2 = 0 then ""
              else
                match od with
                [ Some (Dgreg _ Dfrench) -> short_f_month d2.month2
                | _ -> string_of_int d2.month2 ]
          | _ -> "" ]
      | None -> "" ]
  | "oryear" ->
      match eval_date_field od with
      [ Some d ->
          match d.prec with
          [ OrYear d2 | YearInt d2 -> string_of_int d2.year2
          | _ -> "" ]
      | None -> "" ]
  | "prec" ->
      match od with
      [ Some (Dgreg {prec = Sure} _) -> "sure"
      | Some (Dgreg {prec = About} _) -> "about"
      | Some (Dgreg {prec = Maybe} _) -> "maybe"
      | Some (Dgreg {prec = Before} _) -> "before"
      | Some (Dgreg {prec = After} _) -> "after"
      | Some (Dgreg {prec = OrYear _} _) -> "oryear"
      | Some (Dgreg {prec = YearInt _} _) -> "yearint"
      | _ -> "" ]
  | "text" ->
      match od with
      [ Some (Dtext s) -> s
      | _ -> "" ]
  | "year" ->
      match eval_date_field od with
      [ Some d -> string_of_int d.year
      | None -> "" ]
  | x ->
      let r =
        match x with
        [ "cal_french" -> eval_is_cal Dfrench od
        | "cal_gregorian" -> eval_is_cal Dgregorian od
        | "cal_hebrew" -> eval_is_cal Dhebrew od
        | "cal_julian" -> eval_is_cal Djulian od
        | "prec_no" -> if od = None then "1" else ""
        | "prec_sure" -> eval_is_prec (fun [ Sure -> True | _ -> False ]) od
        | "prec_about" -> eval_is_prec (fun [ About -> True | _ -> False ]) od
        | "prec_maybe" -> eval_is_prec (fun [ Maybe -> True | _ -> False ]) od
        | "prec_before" ->
            eval_is_prec (fun [ Before -> True | _ -> False ]) od
        | "prec_after" ->
            eval_is_prec (fun [ After -> True | _ -> False ]) od
        | "prec_oryear" ->
            eval_is_prec (fun [ OrYear _ -> True | _ -> False ]) od
        | "prec_yearint" ->
            eval_is_prec (fun [ YearInt _ -> True | _ -> False ]) od
        | _ -> raise Not_found ]
      in
      obsolete "5.00" x (if x.[0] = 'c' then "calendar" else "prec") r ]
and eval_date_field =
  fun
  [ Some d ->
      match d with
      [ Dgreg d Dgregorian -> Some d
      | Dgreg d Djulian -> Some (Calendar.julian_of_gregorian d)
      | Dgreg d Dfrench -> Some (Calendar.french_of_gregorian d)
      | Dgreg d Dhebrew -> Some (Calendar.hebrew_of_gregorian d)
      | _ -> None ]
  | None -> None ]
and eval_event_var conf base env e =
  fun
  [ ["e_name"] ->
      match e with
      [ Some {epers_name = name} ->
          match name with
          [ Epers_Birth -> str_val "#birt"
          | Epers_Baptism -> str_val "#bapt"
          | Epers_Death -> str_val "#deat"
          | Epers_Burial -> str_val "#buri"
          | Epers_Cremation -> str_val "#crem"
          | Epers_Accomplishment -> str_val "#acco"
          | Epers_Acquisition -> str_val "#acqu"
          | Epers_Adhesion -> str_val "#adhe"
          | Epers_BaptismLDS -> str_val "#bapl"
          | Epers_BarMitzvah -> str_val "#barm"
          | Epers_BatMitzvah -> str_val "#basm"
          | Epers_Benediction -> str_val "#bles"
          | Epers_ChangeName -> str_val "#chgn"
          | Epers_Circumcision -> str_val "#circ"
          | Epers_ConfirmationLDS -> str_val "#conl"
          | Epers_Confirmation -> str_val "#conf"
          | Epers_Decoration -> str_val "#awar"
          | Epers_DemobilisationMilitaire -> str_val "#demm"
          | Epers_Diploma -> str_val "#degr"
          | Epers_Distinction -> str_val "#dist"
          | Epers_DotationLDS -> str_val "#dotl"
          | Epers_Dotation -> str_val "#endl"
          | Epers_Education -> str_val "#educ"
          | Epers_Election -> str_val "#elec"
          | Epers_Emigration -> str_val "#emig"
          | Epers_Excommunication -> str_val "#exco"
          | Epers_FamilyLinkLDS -> str_val "#flkl"
          | Epers_FirstCommunion -> str_val "#fcom"
          | Epers_Funeral -> str_val "#fune"
          | Epers_Graduate -> str_val "#grad"
          | Epers_Hospitalisation -> str_val "#hosp"
          | Epers_Illness -> str_val "#illn"
          | Epers_Immigration -> str_val "#immi"
          | Epers_ListePassenger -> str_val "#lpas"
          | Epers_MilitaryDistinction -> str_val "#mdis"
          | Epers_MilitaryPromotion -> str_val "#mpro"
          | Epers_MilitaryService -> str_val "#mser"
          | Epers_MobilisationMilitaire -> str_val "#mobm"
          | Epers_Naturalisation -> str_val "#natu"
          | Epers_Occupation -> str_val "#occu"
          | Epers_Ordination -> str_val "#ordn"
          | Epers_Property -> str_val "#prop"
          | Epers_Recensement -> str_val "#cens"
          | Epers_Residence-> str_val "#resi"
          | Epers_Retired -> str_val "#reti"
          | Epers_ScellentChildLDS -> str_val "#slgc"
          | Epers_ScellentParentLDS -> str_val "#slgp"
          | Epers_ScellentSpouseLDS -> str_val "#slgs"
          | Epers_VenteBien -> str_val "#vteb"
          | Epers_Will -> str_val "#will"
          | Epers_Name x -> str_val (quote_escaped x) ]
      | _ -> str_val "" ]
  | ["e_place"] ->
      match e with
      [ Some {epers_place = x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | ["e_note"] ->
      match e with
      [ Some {epers_note = x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | ["e_src"] ->
      match e with
      [ Some {epers_src = x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | _ -> raise Not_found ]
and eval_title_var conf base env t =
  fun
  [ ["t_estate"] ->
      match t with
      [ Some {t_place = x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | ["t_ident"] ->
      match t with
      [ Some {t_ident = x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | ["t_main"] ->
      match t with
      [ Some {t_name = Tmain} -> bool_val True
      | _ -> bool_val False ]
  | ["t_name"] ->
      match t with
      [ Some {t_name = Tname x} -> str_val (quote_escaped x)
      | _ -> str_val "" ]
  | ["t_nth"] ->
      match t with
      [ Some {t_nth = x} -> str_val (if x = 0 then "" else string_of_int x)
      | _ -> str_val "" ]
  | _ -> raise Not_found ]
and eval_relation_var conf base env r =
  fun
  [ ["r_father" :: sl] ->
      let x =
        match r with
        [ Some {r_fath = Some x} -> x
        | _ -> ("", "", 0, Update.Create Neuter None, "") ]
      in
      eval_person_var conf base env x sl
  | ["r_mother" :: sl] ->
      let x =
        match r with
        [ Some {r_moth = Some x} -> x
        | _ -> ("", "", 0, Update.Create Neuter None, "") ]
      in
      eval_person_var conf base env x sl
  | ["rt_adoption"] -> eval_is_relation_type Adoption r
  | ["rt_candidate_parent"] -> eval_is_relation_type CandidateParent r
  | ["rt_empty"] ->
      match r with
      [ Some {r_fath = None; r_moth = None} | None -> bool_val True
      | _ -> bool_val False ]
  | ["rt_foster_parent"] -> eval_is_relation_type FosterParent r
  | ["rt_godparent"] -> eval_is_relation_type GodParent r
  | ["rt_recognition"] -> eval_is_relation_type Recognition r
  | _ -> raise Not_found ]
and eval_person_var conf base env (fn, sn, oc, create, var) =
  fun
  [ ["create"] ->
      match create with
      [ Update.Create _ _ -> bool_val True
      | _ -> bool_val False ]
  | ["create"; "sex"] ->
      match create with
      [ Update.Create Male _ -> str_val "male"
      | Update.Create Female _ -> str_val "female"
      | Update.Create Neuter _ -> str_val "neuter"
      | _ -> str_val "" ]
  | ["first_name"] -> str_val (quote_escaped fn)
  | ["link"] -> bool_val (create = Update.Link)
  | ["occ"] -> str_val (if oc = 0 then "" else string_of_int oc)
  | ["surname"] -> str_val (quote_escaped sn)
  | _ -> raise Not_found ]
and eval_is_cal cal =
  fun
  [ Some (Dgreg _ x) -> if x = cal then "1" else ""
  | _ -> "" ]
and eval_is_prec cond =
  fun
  [ Some (Dgreg {prec = x} _) -> if cond x then "1" else ""
  | _ -> "" ]
and eval_is_death_reason dr =
  fun
  [ Death dr1 _ -> bool_val (dr = dr1)
  | _ -> bool_val False ]
and eval_is_relation_type rt =
  fun
  [ Some {r_fath = None; r_moth = None} -> bool_val False
  | Some {r_type = x} -> bool_val (x = rt)
  | _ -> bool_val False ]
and eval_special_var conf base p =
  fun
  [ ["include_perso_header"] ->
      match p_getint conf.env "i" with
      [ Some i ->
          let has_base_loop =
            try do {
              let _ = Util.create_topological_sort conf base in
              False
            } with [ Consang.TopologicalSortError p -> True ]
          in
          if has_base_loop then VVstring ""
          else do {
            let p = poi base (Adef.iper_of_int i) in
            Perso.interp_templ_with_menu
              (fun _ -> ()) "perso_header" conf base p;
            VVstring ""
          }
      | None -> VVstring "" ]
  | _ -> raise Not_found ]
and eval_int_env var env =
  match get_env var env with
  [ Vint x -> str_val (string_of_int x)
  | _ -> raise Not_found ]
and eval_string_env var env =
  match get_env var env with
  [ Vstring x -> str_val (quote_escaped x)
  | _ -> str_val "" ]
;

(* print *)

value print_foreach print_ast eval_expr =
  let rec print_foreach env p loc s sl _ al =
    match [s :: sl] with
    [ ["alias"] -> print_foreach_string env p al p.aliases s
    | ["first_name_alias"] ->
        print_foreach_string env p al p.first_names_aliases s
    | ["qualifier"] -> print_foreach_string env p al p.qualifiers s
    | ["surname_alias"] -> print_foreach_string env p al p.surnames_aliases s
    | ["relation"] -> print_foreach_relation env p al p.rparents
    | ["title"] -> print_foreach_title env p al p.titles
    | ["pevent"] -> print_foreach_pevent env p al p.pevents
    | ["witness"] -> print_foreach_witness env p al p.pevents
    | _ -> raise Not_found ]
  and print_foreach_string env p al list lab =
    let _ =
      List.fold_left
        (fun cnt nn ->
           let env = [(lab, Vstring nn) :: env] in
           let env = [("cnt", Vint cnt) :: env] in
           do { List.iter (print_ast env p) al; cnt + 1 })
        0 list
    in
    ()
  and print_foreach_relation env p al list =
    let _ =
      List.fold_left
        (fun cnt nn ->
           let env = [("cnt", Vint cnt) :: env] in
           do { List.iter (print_ast env p) al; cnt + 1 })
        1 list
    in
    ()
  and print_foreach_title env p al list =
    let _ =
      List.fold_left
        (fun cnt nn ->
           let env = [("cnt", Vint cnt) :: env] in
           do { List.iter (print_ast env p) al; cnt + 1 })
        1 list
    in
    ()
  and print_foreach_pevent env p al list =
    let weights = get_env_weights pevent_weight_str env in
    let () = assert (List.length weights = List.length list) in
    loop True 1 (weights,list) where rec loop first cnt =
      fun
        [ ([w :: ww],[n :: nn]) ->
          let env =
            [("cnt", Vint cnt); ("first", Vbool first);
             ("weight", Vstring (match w with [Some x -> string_of_int x|None -> ""]));
             ("last", Vbool (nn = [])) :: env]
          in
          do {
            List.iter (print_ast env p) al;
            loop False (cnt + 1) (ww,nn)
          }
      | _ -> () ]
  and print_foreach_witness env p al list =
    match get_env "cnt" env with
    [ Vint i ->
        match
          try Some (List.nth list (i - 1)) with [ Failure _ -> None ]
        with
        [ Some e ->
            let rec loop first wcnt =
              fun
              [ [nn :: l] ->
                  let env =
                    [("wcnt", Vint wcnt); ("first", Vbool first);
                     ("last", Vbool (l = [])) :: env]
                  in
                  do {
                    List.iter (print_ast env p) al;
                    loop False (wcnt + 1) l
                  }
              | [] -> () ]
            in
            loop True 1 (Array.to_list e.epers_witnesses)
        | None -> () ]
    | _ -> () ]
  in
  print_foreach
;

value print_update_ind conf base (weights,p) digest =
  match p_getenv conf.env "m" with
  [ Some ("MRG_IND_OK" | "MRG_MOD_IND_OK") | Some ("MOD_IND" | "MOD_IND_OK") |
    Some ("ADD_IND" | "ADD_IND_OK") ->
      let env =
        [("digest", Vstring digest);
         ("next_pevent", Vcnt (ref (List.length p.pevents + 1)));
         (pevent_weight_str, Vevent_weights weights)
        ]
      in
      Hutil.interp conf base "updind"
        {Templ.eval_var = eval_var conf base;
         Templ.eval_transl _ = Templ.eval_transl conf;
         Templ.eval_predefined_apply _ = raise Not_found;
         Templ.get_vother = get_vother; Templ.set_vother = set_vother;
         Templ.print_foreach = print_foreach}
        env p
  | _ -> incorrect_request conf ]
;

value print_del1 conf base p =
  let title _ =
    let s = transl_nth conf "person/persons" 0 in
    Wserver.wprint "%s" (capitale (transl_decline conf "delete" s))
  in
  do {
    Perso.interp_notempl_with_menu title "perso_header" conf base p;
    tag "h2" begin title False; end;
    tag "form" "method=\"post\" action=\"%s\"" conf.command begin
      tag "p" begin
        Util.hidden_env conf;
        xtag "input" "type=\"hidden\" name=\"m\" value=\"DEL_IND_OK\"";
        xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
          (Adef.int_of_iper (get_key_index p));
        xtag "input" "type=\"submit\" value=\"Ok\"";
      end;
    end;
    trailer conf;
  }
;

value print_add conf base =
  let p =
    {first_name = ""; surname = ""; occ = 0; image = "";
     first_names_aliases = []; surnames_aliases = []; public_name = "";
     qualifiers = []; aliases = []; titles = []; rparents = []; related = [];
     occupation = ""; sex = Neuter; access = IfTitles;
     birth = Adef.codate_None; birth_place = ""; birth_note = "";
     birth_src = ""; baptism = Adef.codate_None; baptism_place = "";
     baptism_note = ""; baptism_src = ""; death = DontKnowIfDead;
     death_place = ""; death_note = ""; death_src = ""; burial = UnknownBurial;
     burial_place = ""; burial_note = ""; burial_src = ""; pevents = [];
     notes = ""; psources = ""; key_index = bogus_person_index}
  in
  let weights = [] in
  let () = assert (List.length weights = List.length p.pevents) in
  print_update_ind conf base (weights,p) ""
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      let sp = string_person_of base p in
      let digest = Update.digest_person sp in
      let weights = list_init (List.length sp.pevents) (fun x -> Some x) in
      print_update_ind conf base (weights,sp) digest
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = poi base (Adef.iper_of_int i) in
      print_del1 conf base p
  | _ -> incorrect_request conf ]
;
