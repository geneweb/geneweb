(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Ast = Geneweb_templ.Ast
module Driver = Geneweb_db.Driver

let string_person_of base p =
  let fp ip =
    let p = Driver.poi base ip in
    ( Driver.sou base (Driver.get_first_name p),
      Driver.sou base (Driver.get_surname p),
      Driver.get_occ p,
      Update.Link,
      "" )
  in
  Futil.map_person_ps fp (Driver.sou base) (Driver.gen_person_of_person p)

(* Interpretation of template file 'updind.txt' *)

type 'a env =
  | Vstring of string
  | Vint of int
  | Vother of 'a
  | Vcnt of int ref
  | Vbool of bool
  | Vnone

let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x
let bool_val = Update_util.bool_val
let str_val = Update_util.str_val
let safe_val = Update_util.safe_val

let rec eval_var conf base env p _loc sl =
  try eval_special_var conf base sl
  with Not_found -> eval_simple_var conf base env p sl

and eval_simple_var conf base env p = function
  | [ "alias" ] -> eval_string_env "alias" env
  | [ "acc_if_titles" ] -> bool_val (p.access = IfTitles)
  | [ "acc_private" ] -> bool_val (p.access = Private)
  | [ "acc_semi_public" ] -> bool_val (p.access = SemiPublic)
  | [ "acc_public" ] -> bool_val (p.access = Public)
  | [ "bapt_place" ] ->
      safe_val (Util.escape_html p.baptism_place :> Adef.safe_string)
  | [ "bapt_note" ] ->
      safe_val (Util.escape_html p.baptism_note :> Adef.safe_string)
  | [ "bapt_src" ] ->
      safe_val (Util.escape_html p.baptism_src :> Adef.safe_string)
  | [ "birth"; s ] -> eval_date_var (Date.od_of_cdate p.birth) s
  | [ "birth_place" ] ->
      safe_val (Util.escape_html p.birth_place :> Adef.safe_string)
  | [ "birth_note" ] ->
      safe_val (Util.escape_html p.birth_note :> Adef.safe_string)
  | [ "birth_src" ] ->
      safe_val (Util.escape_html p.birth_src :> Adef.safe_string)
  | [ "bapt"; s ] -> eval_date_var (Date.od_of_cdate p.baptism) s
  | [ "bt_buried" ] ->
      bool_val (match p.burial with Buried _ -> true | _ -> false)
  | [ "bt_cremated" ] ->
      bool_val (match p.burial with Cremated _ -> true | _ -> false)
  | [ "bt_unknown_burial" ] -> bool_val (p.burial = UnknownBurial)
  | [ "burial"; s ] ->
      let od =
        match p.burial with
        | Buried cod | Cremated cod -> Date.od_of_cdate cod
        | UnknownBurial -> None
      in
      eval_date_var od s
  | [ "burial_place" ] ->
      safe_val (Util.escape_html p.burial_place :> Adef.safe_string)
  | [ "burial_note" ] ->
      safe_val (Util.escape_html p.burial_note :> Adef.safe_string)
  | [ "burial_src" ] ->
      safe_val (Util.escape_html p.burial_src :> Adef.safe_string)
  | [ "cnt" ] -> eval_int_env "cnt" env
  | [ "dead_dont_know_when" ] -> bool_val (p.death = DeadDontKnowWhen)
  | [ "death"; s ] ->
      let od = Date.date_of_death p.death in
      eval_date_var od s
  | [ "death_place" ] ->
      safe_val (Util.escape_html p.death_place :> Adef.safe_string)
  | [ "death_note" ] ->
      safe_val (Util.escape_html p.death_note :> Adef.safe_string)
  | [ "death_src" ] ->
      safe_val (Util.escape_html p.death_src :> Adef.safe_string)
  | [ "died_young" ] -> bool_val (p.death = DeadYoung)
  | [ "digest" ] -> eval_string_env "digest" env
  | [ "dont_know_if_dead" ] -> bool_val (p.death = DontKnowIfDead)
  | [ "dr_disappeared" ] -> eval_is_death_reason Disappeared p.death
  | [ "dr_executed" ] -> eval_is_death_reason Executed p.death
  | [ "dr_killed" ] -> eval_is_death_reason Killed p.death
  | [ "dr_murdered" ] -> eval_is_death_reason Murdered p.death
  | [ "dr_unspecified" ] -> eval_is_death_reason Unspecified p.death
  | "event" :: sl ->
      let e =
        match get_env "cnt" env with
        | Vint i -> (
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_event_var e sl
  | [ "event_date"; s ] ->
      let od =
        match get_env "cnt" env with
        | Vint i -> (
            try
              let e = List.nth p.pevents (i - 1) in
              Date.od_of_cdate e.epers_date
            with Failure _ -> None)
        | _ -> None
      in
      eval_date_var od s
  | [ "event_str" ] -> (
      match get_env "cnt" env with
      | Vint i -> (
          try
            let p = Driver.poi base p.key_index in
            let e = List.nth (Driver.get_pevents p) (i - 1) in
            let name =
              Util.string_of_pevent_name conf base e.epers_name
              |> Adef.safe_fn Utf8.capitalize_fst
            in
            let date =
              match Date.od_of_cdate e.epers_date with
              | Some d -> DateDisplay.string_of_date conf d
              | None -> Adef.safe ""
            in
            let place =
              Util.string_of_place conf (Driver.sou base e.epers_place)
            in
            ([ name; date; (place :> Adef.safe_string) ]
              : Adef.safe_string list
              :> string list)
            |> String.concat ", " |> Adef.safe |> safe_val
          with Failure _ -> str_val "")
      | _ -> str_val "")
  | [ "first_name" ] ->
      safe_val (Util.escape_html p.first_name :> Adef.safe_string)
  | [ "first_name_alias" ] -> eval_string_env "first_name_alias" env
  | [ "has_aliases" ] -> bool_val (p.aliases <> [])
  | [ "has_birth_date" ] -> bool_val (Date.od_of_cdate p.birth <> None)
  | [ "has_pevent_birth" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Birth then bool_val true else loop l
      in
      loop p.pevents
  | [ "has_pevent_baptism" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Baptism then bool_val true else loop l
      in
      loop p.pevents
  | [ "has_pevent_death" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Death then bool_val true else loop l
      in
      loop p.pevents
  | [ "has_pevent_burial" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Burial then bool_val true else loop l
      in
      loop p.pevents
  | [ "has_pevent_cremation" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> bool_val false
        | evt :: l ->
            if evt.epers_name = Epers_Cremation then bool_val true else loop l
      in
      loop p.pevents
  | [ "has_pevents" ] -> bool_val (p.pevents <> [])
  | [ "has_primary_pevents" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> false
        | evt :: l -> (
            match evt.epers_name with
            | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
            | Epers_Cremation ->
                true
            | _ -> loop l)
      in
      bool_val (loop p.pevents)
  | [ "has_secondary_pevents" ] ->
      let rec loop pevents =
        match pevents with
        | [] -> false
        | evt :: l -> (
            match evt.epers_name with
            | Epers_Birth | Epers_Baptism | Epers_Death | Epers_Burial
            | Epers_Cremation ->
                loop l
            | _ -> true)
      in
      bool_val (loop p.pevents)
  | [ "has_first_names_aliases" ] -> bool_val (p.first_names_aliases <> [])
  | [ "has_qualifiers" ] -> bool_val (p.qualifiers <> [])
  | [ "has_relations" ] -> bool_val (p.rparents <> [])
  | [ "has_surnames_aliases" ] -> bool_val (p.surnames_aliases <> [])
  | [ "has_titles" ] -> bool_val (p.titles <> [])
  | [ "image" ] -> safe_val (Util.escape_html p.image :> Adef.safe_string)
  | [ "index" ] -> str_val (Driver.Iper.to_string p.key_index)
  | [ "is_female" ] -> bool_val (p.sex = Female)
  | [ "is_male" ] -> bool_val (p.sex = Male)
  | [ "is_first" ] -> (
      match get_env "first" env with
      | Vbool x -> bool_val x
      | _ -> raise Not_found)
  | [ "is_last" ] -> (
      match get_env "last" env with
      | Vbool x -> bool_val x
      | _ -> raise Not_found)
  | [ "nb_pevents" ] -> str_val (string_of_int (List.length p.pevents))
  | [ "not_dead" ] -> bool_val (p.death = NotDead)
  | [ "notes" ] -> safe_val (Util.escape_html p.notes :> Adef.safe_string)
  | [ "next_pevent" ] -> (
      match get_env "next_pevent" env with
      | Vcnt c -> str_val (string_of_int !c)
      | _ -> str_val "")
  | [ "incr_next_pevent" ] -> (
      match get_env "next_pevent" env with
      | Vcnt c ->
          incr c;
          str_val ""
      | _ -> str_val "")
  | [ "occ" ] -> str_val (string_of_int p.occ)
  | [ "occupation" ] ->
      safe_val (Util.escape_html p.occupation :> Adef.safe_string)
  | [ "of_course_dead" ] -> bool_val (p.death = OfCourseDead)
  | [ "public_name" ] ->
      safe_val (Util.escape_html p.public_name :> Adef.safe_string)
  | [ "qualifier" ] -> eval_string_env "qualifier" env
  | "relation" :: sl ->
      let r =
        match get_env "cnt" env with
        | Vint i -> (
            try Some (List.nth p.rparents (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_relation_var base r sl
  | [ "sources" ] -> safe_val (Util.escape_html p.psources :> Adef.safe_string)
  | [ "surname" ] -> safe_val (Util.escape_html p.surname :> Adef.safe_string)
  | [ "surname_alias" ] -> eval_string_env "surname_alias" env
  | "title" :: sl ->
      let t =
        match get_env "cnt" env with
        | Vint i -> (
            try Some (List.nth p.titles (i - 1)) with Failure _ -> None)
        | _ -> None
      in
      eval_title_var t sl
  | [ "title_date_start"; s ] ->
      let od =
        match get_env "cnt" env with
        | Vint i -> (
            try
              let t = List.nth p.titles (i - 1) in
              Date.od_of_cdate t.t_date_start
            with Failure _ -> None)
        | _ -> None
      in
      eval_date_var od s
  | [ "title_date_end"; s ] ->
      let od =
        match get_env "cnt" env with
        | Vint i -> (
            try
              let t = List.nth p.titles (i - 1) in
              Date.od_of_cdate t.t_date_end
            with Failure _ -> None)
        | _ -> None
      in
      eval_date_var od s
  | [ "wcnt" ] -> eval_int_env "wcnt" env
  | [ "has_witness" ] -> (
      match get_env "cnt" env with
      | Vint i -> (
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          match e with
          | Some e -> bool_val (e.epers_witnesses <> [||])
          | None -> raise Not_found)
      | _ -> raise Not_found)
  | "witness" :: sl -> (
      match get_env "cnt" env with
      | Vint i -> (
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          match e with
          | Some e -> (
              match get_env "wcnt" env with
              | Vint i ->
                  let i = i - 1 in
                  let k =
                    if i >= 0 && i < Array.length e.epers_witnesses then
                      fst e.epers_witnesses.(i)
                    else if
                      i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                    then ("", "", 0, Update.Create (Neuter, None), "")
                    else raise Not_found
                  in
                  eval_person_var base k sl
              | _ -> raise Not_found)
          | None -> raise Not_found)
      | _ -> raise Not_found)
  | [ "witness_kind" ] -> (
      match get_env "cnt" env with
      | Vint i -> (
          let e =
            try Some (List.nth p.pevents (i - 1)) with Failure _ -> None
          in
          match e with
          | Some e -> (
              match get_env "wcnt" env with
              | Vint i ->
                  let i = i - 1 in
                  if i >= 0 && i < Array.length e.epers_witnesses then
                    match snd e.epers_witnesses.(i) with
                    | Witness_GodParent -> str_val "godp"
                    | Witness_CivilOfficer -> str_val "offi"
                    | Witness_ReligiousOfficer -> str_val "reli"
                    | Witness_Informant -> str_val "info"
                    | Witness_Attending -> str_val "atte"
                    | Witness_Mentioned -> str_val "ment"
                    | Witness_Other -> str_val "othe"
                    | Witness -> str_val ""
                  else if i >= 0 && i < 2 && Array.length e.epers_witnesses < 2
                  then str_val ""
                  else raise Not_found
              | _ -> raise Not_found)
          | None -> raise Not_found)
      | _ -> raise Not_found)
  | [ s ] -> Update_util.eval_default_var conf s
  | _ -> raise Not_found

and eval_date_var = Update_util.eval_date_var

and eval_event_var e = function
  | [ "e_name" ] -> (
      match e with
      | Some { epers_name = name; _ } -> (
          match name with
          | Epers_Birth -> str_val "#birt"
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
          | Epers_Residence -> str_val "#resi"
          | Epers_Retired -> str_val "#reti"
          | Epers_ScellentChildLDS -> str_val "#slgc"
          | Epers_ScellentParentLDS -> str_val "#slgp"
          | Epers_ScellentSpouseLDS -> str_val "#slgs"
          | Epers_VenteBien -> str_val "#vteb"
          | Epers_Will -> str_val "#will"
          | Epers_Name x -> safe_val (Util.escape_html x :> Adef.safe_string))
      | _ -> str_val "")
  | [ "e_place" ] -> (
      match e with
      | Some { epers_place = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "e_note" ] -> (
      match e with
      | Some { epers_note = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "e_src" ] -> (
      match e with
      | Some { epers_src = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | _ -> raise Not_found

and eval_title_var t = function
  | [ "t_estate" ] -> (
      match t with
      | Some { t_place = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "t_ident" ] -> (
      match t with
      | Some { t_ident = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "t_main" ] -> (
      match t with
      | Some { t_name = Tmain; _ } -> bool_val true
      | _ -> bool_val false)
  | [ "t_name" ] -> (
      match t with
      | Some { t_name = Tname x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "t_nth" ] -> (
      match t with
      | Some { t_nth = x; _ } -> str_val (if x = 0 then "" else string_of_int x)
      | _ -> str_val "")
  | _ -> raise Not_found

and eval_relation_var base r = function
  | "r_father" :: sl ->
      let x =
        match r with
        | Some { r_fath = Some x; _ } -> x
        | _ -> ("", "", 0, Update.Create (Neuter, None), "")
      in
      eval_person_var base x sl
  | "r_mother" :: sl ->
      let x =
        match r with
        | Some { r_moth = Some x; _ } -> x
        | _ -> ("", "", 0, Update.Create (Neuter, None), "")
      in
      eval_person_var base x sl
  | [ "rt_adoption" ] -> eval_is_relation_type Adoption r
  | [ "rt_candidate_parent" ] -> eval_is_relation_type CandidateParent r
  | [ "rt_empty" ] -> (
      match r with
      | Some { r_fath = None; r_moth = None; _ } | None -> bool_val true
      | _ -> bool_val false)
  | [ "rt_foster_parent" ] -> eval_is_relation_type FosterParent r
  | [ "rt_godparent" ] -> eval_is_relation_type GodParent r
  | [ "rt_recognition" ] -> eval_is_relation_type Recognition r
  | _ -> raise Not_found

and eval_person_var base (fn, sn, oc, create, _) = function
  | [ "create" ] -> (
      match create with
      | Update.Create (_, _) -> bool_val true
      | _ -> bool_val false)
  | [ "create"; s ] -> Update_util.eval_create create s
  | [ "first_name" ] -> safe_val (Util.escape_html fn :> Adef.safe_string)
  | [ "link" ] -> bool_val (create = Update.Link)
  | [ "occ" ] -> str_val (string_of_int oc)
  | [ "surname" ] -> safe_val (Util.escape_html sn :> Adef.safe_string)
  | [ "index" ] -> (
      match Driver.person_of_key base fn sn oc with
      | Some ip -> str_val (Driver.Iper.to_string ip)
      | _ -> str_val (Driver.Iper.to_string Driver.Iper.dummy))
  | [ "sex" ] ->
      let sex =
        match Driver.person_of_key base fn sn oc with
        | Some ip ->
            Driver.get_sex (Driver.poi base ip) |> index_of_sex |> string_of_int
        | _ -> Neuter |> index_of_sex |> string_of_int
      in
      str_val sex
  | _ -> raise Not_found

and eval_is_death_reason dr = function
  | Death (dr1, _) -> bool_val (dr = dr1)
  | _ -> bool_val false

and eval_is_relation_type rt = function
  | Some { r_fath = None; r_moth = None; _ } -> bool_val false
  | Some { r_type = x; _ } -> bool_val (x = rt)
  | _ -> bool_val false

and eval_special_var conf base = function
  | [ "include_menubar" ] -> (
      match p_getenv conf.env "i" with
      | Some i ->
          let p = Driver.poi base (Driver.Iper.of_string i) in
          Perso.interp_templ ~no_headers:true "menubar" conf base p;
          VVstring ""
      | None -> VVstring "")
  | _ -> raise Not_found

and eval_int_env var env =
  match get_env var env with
  | Vint x -> str_val (string_of_int x)
  | _ -> raise Not_found

and eval_string_env var env =
  match get_env var env with
  | Vstring x -> safe_val (Util.escape_html x :> Adef.safe_string)
  | _ -> str_val ""

(* print *)

let print_foreach print_ast _eval_expr =
  let rec print_foreach env p _loc s sl _ al =
    match s :: sl with
    | [ "alias" ] -> print_foreach_string env p al p.aliases s
    | [ "first_name_alias" ] ->
        print_foreach_string env p al p.first_names_aliases s
    | [ "qualifier" ] -> print_foreach_string env p al p.qualifiers s
    | [ "surname_alias" ] -> print_foreach_string env p al p.surnames_aliases s
    | [ "relation" ] -> print_foreach_relation env p al p.rparents
    | [ "title" ] -> print_foreach_title env p al p.titles
    | [ "pevent" ] -> print_foreach_pevent env p al p.pevents
    | [ "witness" ] -> print_foreach_witness env p al p.pevents
    | _ -> raise Not_found
  and print_foreach_string env p al list lab =
    let () =
      ignore
      @@ List.fold_left
           (fun cnt nn ->
             let env =
               Templ.Env.(env |> add lab (Vstring nn) |> add "cnt" (Vint cnt))
             in
             List.iter (print_ast env p) al;
             cnt + 1)
           0 list
    in
    ()
  and print_foreach_relation env p al list =
    let () =
      ignore
      @@ List.fold_left
           (fun cnt _ ->
             let env = Templ.Env.add "cnt" (Vint cnt) env in
             List.iter (print_ast env p) al;
             cnt + 1)
           1 list
    in
    ()
  and print_foreach_title env p al list =
    let () =
      ignore
      @@ List.fold_left
           (fun cnt _ ->
             let env = Templ.Env.add "cnt" (Vint cnt) env in
             List.iter (print_ast env p) al;
             cnt + 1)
           1 list
    in
    ()
  and print_foreach_pevent env p al list =
    let rec loop first cnt = function
      | _ :: l ->
          let env =
            Templ.Env.(
              env |> add "cnt" (Vint cnt) |> add "first" (Vbool first)
              |> add "last" (Vbool (l = [])))
          in
          List.iter (print_ast env p) al;
          loop false (cnt + 1) l
      | [] -> ()
    in
    loop true 1 list
  and print_foreach_witness env p al list =
    match get_env "cnt" env with
    | Vint i -> (
        match List.nth list (i - 1) with
        | exception Failure _ -> ()
        | e ->
            let last = Array.length e.epers_witnesses - 1 in
            Array.iteri
              (fun i _ ->
                let env =
                  Templ.Env.(
                    env
                    |> add "wcnt" (Vint (i + 1))
                    |> add "first" (Vbool (i = 0))
                    |> add "last" (Vbool (i = last)))
                in
                List.iter (print_ast env p) al)
              e.epers_witnesses)
    | _ -> ()
  in
  print_foreach

(* S: check on `m` should be made beforehand; what about plugins?  *)
let print_update_ind conf base p digest =
  match p_getenv conf.env "m" with
  | Some ("MRG_IND_OK" | "MRG_MOD_IND_OK")
  | Some ("MOD_IND" | "MOD_IND_OK")
  | Some ("ADD_IND" | "ADD_IND_OK") ->
      let env =
        Templ.Env.(
          empty
          |> add "digest" (Vstring digest)
          |> add "next_pevent" (Vcnt (ref (List.length p.pevents + 1))))
      in
      let ifun =
        Templ.
          {
            eval_var = eval_var conf base;
            eval_transl = (fun _ -> Templ.eval_transl conf);
            eval_predefined_apply = (fun _ -> raise Not_found);
            get_vother;
            set_vother;
            print_foreach;
          }
      in
      Templ.output conf ifun env p "updind"
  | Some _ | None -> Hutil.incorrect_request conf

let print_del1 conf base p =
  let title _ =
    let s = transl_nth conf "person/persons" 0 in
    Output.print_sstring conf
      (Utf8.capitalize_fst (transl_decline conf "delete" s));
    Output.print_sstring conf " ";
    Output.print_string conf (Util.escape_html (Driver.p_first_name base p));
    Output.print_sstring conf (Format.sprintf ".%d " (Driver.get_occ p));
    Output.print_string conf (Util.escape_html (Driver.p_surname base p))
  in
  Hutil.header conf title;
  Output.printf conf "<form method=\"post\" action=\"%s\">\n" conf.command;
  Output.print_sstring conf "<p>\n";
  Util.hidden_env conf;
  Output.print_sstring conf
    "<input type=\"hidden\" name=\"m\" value=\"DEL_IND_OK\">\n";
  Output.printf conf "<input type=\"hidden\" name=\"i\" value=\"%s\">\n"
    (Driver.Iper.to_string (Driver.get_iper p));
  Output.print_sstring conf
    "<button type=\"submit\" class=\"btn btn-danger btn-lg m-3\">\n";
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 1));
  Output.print_sstring conf "</button>\n";
  Output.print_sstring conf "</p>\n";
  Output.print_sstring conf "</form>\n";
  Hutil.trailer conf

let print_add conf base =
  let p =
    {
      first_name = "";
      surname = "";
      occ = 0;
      image = "";
      first_names_aliases = [];
      surnames_aliases = [];
      public_name = "";
      qualifiers = [];
      aliases = [];
      titles = [];
      rparents = [];
      related = [];
      occupation = "";
      sex = Neuter;
      access = IfTitles;
      birth = Date.cdate_None;
      birth_place = "";
      birth_note = "";
      birth_src = "";
      baptism = Date.cdate_None;
      baptism_place = "";
      baptism_note = "";
      baptism_src = "";
      death = DontKnowIfDead;
      death_place = "";
      death_note = "";
      death_src = "";
      burial = UnknownBurial;
      burial_place = "";
      burial_note = "";
      burial_src = "";
      pevents = [];
      notes = "";
      psources = "";
      key_index = Driver.Iper.dummy;
    }
  in
  print_update_ind conf base p ""

let print_mod conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some i ->
      let p = Driver.poi base (Driver.Iper.of_string i) in
      let sp = string_person_of base p in
      let salt = Option.get conf.secret_salt in
      let digest = Update.digest_person ~salt sp in
      print_update_ind conf base sp digest

let print_del conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some i ->
      let p = Driver.poi base (Driver.Iper.of_string i) in
      print_del1 conf base p

let print_change_event_order conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some i ->
      let p =
        string_person_of base (Driver.poi base (Driver.Iper.of_string i))
      in
      Templ.output conf
        Templ.
          {
            eval_var = eval_var conf base;
            eval_transl = (fun _ -> Templ.eval_transl conf);
            eval_predefined_apply = (fun _ -> raise Not_found);
            get_vother;
            set_vother;
            print_foreach;
          }
        Templ.Env.empty p "updindevt"
