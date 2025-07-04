(* $Id: updateFam.ml,v 5.24 2008-01-09 03:34:36 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let default_source conf =
  match p_getenv conf.env "dsrc" with Some s -> s | None -> ""

let person_key base ip =
  let p = Driver.poi base ip in
  let first_name = Driver.sou base (Driver.get_first_name p) in
  let surname = Driver.sou base (Driver.get_surname p) in
  let occ =
    if first_name = "?" || surname = "?" then
      int_of_string @@ Driver.Iper.to_string ip (* FIXME *)
    else Driver.get_occ p
  in
  (first_name, surname, occ, Update.Link, "")

let string_family_of conf base ifam =
  let fam = Driver.foi base ifam in
  let sfam =
    Futil.map_family_ps (person_key base)
      (fun f -> f)
      (Driver.sou base)
      (Driver.gen_family_of_family fam)
  in
  let scpl =
    Futil.map_couple_p conf.multi_parents (person_key base)
      (Driver.gen_couple_of_family fam)
  in
  let sdes =
    Futil.map_descend_p (person_key base) (Driver.gen_descend_of_family fam)
  in
  (sfam, scpl, sdes)

(* Interpretation of template file 'updfam.txt' *)

type 'a env =
  | Vstring of string
  | Vint of int
  | Vother of 'a
  | Vbool of bool
  | Vnone

let bool_val = Update_util.bool_val
let str_val = Update_util.str_val
let safe_val = Update_util.safe_val
let get_env v env = try Templ.Env.find v env with Not_found -> Vnone
let get_vother = function Vother x -> Some x | _ -> None
let set_vother x = Vother x
let sexes = ref (-1)

module ExtOption = struct
  let bind o f = match o with Some v -> f v | None -> None

  let get : 'a option -> 'a = function
    | Some v -> v
    | None -> raise (Invalid_argument "option is None")
end

let eval_witness_kind = function
  | Witness_GodParent -> str_val "godp"
  | Witness_CivilOfficer -> str_val "offi"
  | Witness_ReligiousOfficer -> str_val "reli"
  | Witness_Informant -> str_val "info"
  | Witness_Attending -> str_val "atte"
  | Witness_Mentioned -> str_val "ment"
  | Witness_Other -> str_val "othe"
  | Witness -> str_val ""

let family_events_opt env fam =
  match get_env "cnt" env with
  | Vint i -> List.nth_opt fam.fevents (i - 1)
  | _ -> None

let witness_person_of_event_opt env e =
  match get_env "wcnt" env with
  | Vint i when i - 1 >= 0 && i - 1 < Array.length e.efam_witnesses ->
      Some (fst e.efam_witnesses.(i - 1))
  | Vint i when i - 1 >= 0 && i - 1 < 2 && Array.length e.efam_witnesses < 2 ->
      Some ("", "", 0, Update.Create (Neuter, None), "")
  | _ -> None

let ( >>= ) x f = ExtOption.bind x f

let rec eval_fwitness conf base env fam sl =
  let fwitness_opt =
    family_events_opt env fam >>= fun e ->
    witness_person_of_event_opt env e >>= fun p -> eval_key_opt conf base p sl
  in
  match fwitness_opt with Some fw -> fw | None -> raise Not_found

(* TODO : function logic around array length is not clear  *)
and eval_child conf base env des sl =
  let k =
    match get_env "cnt" env with
    | Vint i ->
        let i = i - 1 in
        if i >= 0 && i < Array.length des.children then des.children.(i)
        else if i >= 0 && i < 1 && Array.length des.children = 0 then
          ("", "", 0, Update.Create (Neuter, None), "")
        else raise Not_found
    | _ -> raise Not_found
  in
  eval_key conf base k sl

and eval_var conf base env (fam, cpl, des) _loc sl =
  try eval_special_var conf base sl
  with Not_found -> eval_simple_var conf base env (fam, cpl, des) sl

and eval_bvar conf v =
  match List.assoc_opt v conf.base_env with
  | Some v -> Templ.VVstring v
  | None -> VVstring ""

and eval_divorce fam =
  let divorce, separated =
    List.fold_right
      (fun evt (divorce, separated) ->
        let name = evt.efam_name in
        let date = evt.efam_date in
        let place = evt.efam_place in
        let note = evt.efam_note in
        let src = evt.efam_src in
        let wl = evt.efam_witnesses in
        let x = (name, date, place, note, src, wl) in
        if name = Efam_Divorce then (x :: divorce, separated)
        else if name = Efam_Separated then (divorce, x :: separated)
        else (divorce, separated))
      fam.fevents ([], [])
  in
  match (divorce, separated) with
  | (Efam_Divorce, _, _, _, _, _) :: _, _ -> str_val "divorced"
  | _, (Efam_Separated, _, _, _, _, _) :: _ -> str_val "separated"
  | _, _ -> str_val "not divorced nor separated"

(* TODO : rewrite, second case with None passed as an argument looks odd *)
and eval_divorce' fam s =
  let divorce, separated =
    List.fold_right
      (fun evt (divorce, separated) ->
        let name = evt.efam_name in
        let date = evt.efam_date in
        let place = evt.efam_place in
        let note = evt.efam_note in
        let src = evt.efam_src in
        let wl = evt.efam_witnesses in
        let x = (name, date, place, note, src, wl) in
        if name = Efam_Divorce then (x :: divorce, separated)
        else if name = Efam_Separated then (divorce, x :: separated)
        else (divorce, separated))
      fam.fevents ([], [])
  in
  match (divorce, separated) with
  | (Efam_Divorce, d, _, _, _, _) :: _, _ ->
      eval_date_var (Date.od_of_cdate d) s
  | _, (Efam_Separated, d, _, _, _, _) :: _ ->
      eval_date_var (Date.od_of_cdate d) s
  | _, _ -> str_val ""

and eval_is_first env =
  match get_env "first" env with Vbool x -> bool_val x | _ -> raise Not_found

and eval_is_last env =
  match get_env "last" env with Vbool x -> bool_val x | _ -> raise Not_found

(* TODO : feels like it could be simpler *)
and eval_parent conf base env cpl sl =
  match get_env "cnt" env with
  | Vint i ->
      let arr = Gutil.parent_array cpl in
      let i = i - 1 in
      let k =
        if i >= 0 && i < Array.length arr then arr.(i)
        else if i >= 0 && i < 1 && Array.length arr = 0 then
          ("", "", 0, Update.Create (Neuter, None), "")
        else raise Not_found
      in
      eval_parent' conf base env k sl
  | _ -> raise Not_found

(* TODO : more array length logic *)
and eval_witness conf base env fam sl =
  match get_env "cnt" env with
  | Vint i ->
      let i = i - 1 in
      let k =
        if i >= 0 && i < Array.length fam.witnesses then fam.witnesses.(i)
        else if i >= 0 && i < 2 && Array.length fam.witnesses < 2 then
          ("", "", 0, Update.Create (Neuter, None), "")
        else raise Not_found
      in
      eval_key conf base k sl
  | _ -> raise Not_found

(* TODO : rewrite, looks bad *)
and eval_event_str conf base env fam =
  match get_env "cnt" env with
  | Vint i -> (
      try
        let fam = Driver.foi base fam.fam_index in
        let e = List.nth (Driver.get_fevents fam) (i - 1) in
        let name =
          Util.string_of_fevent_name conf base e.efam_name
          |> Adef.safe_fn Utf8.capitalize_fst
        in
        let date =
          match Date.od_of_cdate e.efam_date with
          | Some d -> DateDisplay.string_of_date conf d
          | None -> Adef.safe ""
        in
        let place = Util.string_of_place conf (Driver.sou base e.efam_place) in
        let note = Util.safe_html (Driver.sou base e.efam_note) in
        let src = Util.safe_html (Driver.sou base e.efam_src) in
        let wit =
          Array.fold_right
            (fun (w, _) accu ->
              (transl_nth conf "witness/witnesses" 0
              ^<^ transl conf ":"
              ^<^ Util.gen_person_text conf base (Driver.poi base w))
              :: accu)
            e.efam_witnesses []
        in
        let s =
          String.concat ", "
            ([ name; date; (place :> Adef.safe_string); note; src ]
              :> string list)
        in
        let sw = String.concat ", " (wit :> string list) in
        safe_val (Adef.safe (s ^ ", " ^ sw))
      with Failure _ -> str_val "")
  | _ -> str_val ""

and eval_has_fwitness env fam =
  let has_fwitness_opt =
    family_events_opt env fam >>= fun e ->
    Some (bool_val (e.efam_witnesses <> [||]))
  in
  try ExtOption.get has_fwitness_opt
  with Invalid_argument _ -> raise Not_found

(* TODO : rewrite, looks bad *)
and eval_fwitness_kind env fam =
  match get_env "cnt" env with
  | Vint i -> (
      let e =
        try Some (List.nth fam.fevents (i - 1)) with Failure _ -> None
      in
      match e with
      | Some e -> (
          match get_env "wcnt" env with
          | Vint i ->
              let i = i - 1 in
              if i >= 0 && i < Array.length e.efam_witnesses then
                eval_witness_kind (snd e.efam_witnesses.(i))
              else if i >= 0 && i < 2 && Array.length e.efam_witnesses < 2 then
                str_val ""
              else raise Not_found
          | _ -> raise Not_found)
      | None -> raise Not_found)
  | _ -> raise Not_found

(* TODO : rewrite, looks bad + find a better name *)
and eval_default_var conf s = Update_util.eval_default_var conf s

and eval_event_date env fam s =
  let od =
    family_events_opt env fam >>= fun e -> Date.od_of_cdate e.efam_date
  in
  eval_date_var od s

and eval_simple_var conf base env (fam, cpl, des) = function
  | [ "bvar"; v ] -> eval_bvar conf v
  | "child" :: sl -> eval_child conf base env des sl
  | [ "cnt" ] -> eval_int_env "cnt" env
  | [ "comment" ] -> safe_val (Util.escape_html fam.comment :> Adef.safe_string)
  | [ "digest" ] -> eval_string_env "digest" env
  | [ "divorce" ] -> eval_divorce fam
  | [ "divorce"; s ] -> eval_divorce' fam s
  | "father" :: sl -> eval_key conf base (Gutil.father cpl) sl
  | [ "fsources" ] ->
      safe_val (Util.escape_html fam.fsources :> Adef.safe_string)
  | [ "is_first" ] -> eval_is_first env
  | [ "is_last" ] -> eval_is_last env
  | [ "marriage"; s ] -> eval_date_var (Date.od_of_cdate fam.marriage) s
  | [ "marriage_place" ] ->
      safe_val (Util.escape_html fam.marriage_place :> Adef.safe_string)
  | [ "marriage_note" ] ->
      safe_val (Util.escape_html fam.marriage_note :> Adef.safe_string)
  | [ "marriage_src" ] ->
      safe_val (Util.escape_html fam.marriage_src :> Adef.safe_string)
  | "mother" :: sl -> eval_key conf base (Gutil.mother cpl) sl
  | [ "mrel" ] -> str_val (eval_relation_kind fam.relation)
  | [ "nb_fevents" ] -> str_val (string_of_int (List.length fam.fevents))
  | [ "origin_file" ] ->
      safe_val (Util.escape_html fam.origin_file :> Adef.safe_string)
  | "parent" :: sl -> eval_parent conf base env cpl sl
  | [ "wcnt" ] -> eval_int_env "wcnt" env
  | "witness" :: sl -> eval_witness conf base env fam sl
  | [ "has_fevents" ] -> bool_val (fam.fevents <> [])
  | "event" :: sl ->
      let e = family_events_opt env fam in
      eval_event_var e sl
  | [ "event_date"; s ] -> eval_event_date env fam s
  | [ "event_str" ] -> eval_event_str conf base env fam
  | [ "has_fwitness" ] -> eval_has_fwitness env fam
  | "fwitness" :: sl -> eval_fwitness conf base env fam sl
  | [ "fwitness_kind" ] -> eval_fwitness_kind env fam
  | [ s ] -> eval_default_var conf s
  | _ -> raise Not_found

and eval_date_var = Update_util.eval_date_var

and eval_event_var e = function
  | [ "e_name" ] -> (
      match e with
      | Some { efam_name = name; _ } -> (
          match name with
          | Efam_Marriage -> str_val "#marr"
          | Efam_NoMarriage -> str_val "#nmar"
          | Efam_NoMention -> str_val "#nmen"
          | Efam_Engage -> str_val "#enga"
          | Efam_Divorce -> str_val "#div"
          | Efam_Separated -> str_val "#sep"
          | Efam_Annulation -> str_val "#anul"
          | Efam_MarriageBann -> str_val "#marb"
          | Efam_MarriageContract -> str_val "#marc"
          | Efam_MarriageLicense -> str_val "#marl"
          | Efam_PACS -> str_val "#pacs"
          | Efam_Residence -> str_val "#resi"
          | Efam_Name x -> safe_val (Util.escape_html x :> Adef.safe_string))
      | _ -> str_val "")
  | [ "e_place" ] -> (
      match e with
      | Some { efam_place = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "e_note" ] -> (
      match e with
      | Some { efam_note = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "e_src" ] -> (
      match e with
      | Some { efam_src = x; _ } ->
          safe_val (Util.escape_html x :> Adef.safe_string)
      | _ -> str_val "")
  | [ "e_w_nbr" ] -> (
      match e with
      | Some e -> str_val (string_of_int (Array.length e.efam_witnesses))
      | _ -> str_val "0")
  | _ -> raise Not_found

and eval_parent' conf base env k = function
  | [ "himher" ] ->
      let s =
        match get_env "cnt" env with
        | Vint 1 -> Utf8.capitalize_fst (transl_nth conf "him/her" 0)
        | Vint 2 -> Utf8.capitalize_fst (transl_nth conf "him/her" 1)
        | Vint _ -> transl conf "him/her"
        | _ -> "???"
      in
      str_val s
  | sl -> eval_key conf base k sl

and get_parent_sex conf base fn sn oc =
  match Geneweb_db.Driver.person_of_key base fn sn oc with
  | Some ip -> (
      match pget conf base ip with
      | p -> (
          match Driver.get_sex p with Male -> 0 | Female -> 1 | Neuter -> 2))
  | _ -> -1

and eval_key conf base (fn, sn, oc, create, _) = function
  | [ "create" ] -> str_val (if create <> Update.Link then "create" else "link")
  | [ "create"; s ] -> Update_util.eval_create create s
  | [ "first_name" ] -> safe_val (Util.escape_html fn :> Adef.safe_string)
  | [ "occ" ] -> str_val (string_of_int oc)
  | [ "surname" ] -> safe_val (Util.escape_html sn :> Adef.safe_string)
  | [ "sex" ] ->
      if create = Update.Link then
        let sex = get_parent_sex conf base fn sn oc in
        str_val (string_of_int sex)
      else Update_util.eval_create create "sex"
  | [ "index" ] -> (
      match Driver.person_of_key base fn sn oc with
      | Some ip -> str_val (Driver.Iper.to_string ip)
      | _ -> str_val (Driver.Iper.to_string Driver.Iper.dummy))
  | [ "sexes" ] ->
      (* this is somewhat of a hack to determine same sex situations *)
      (* updateFam.ml does not provide adequate mechanisms to test   *)
      (* for the other parent's sex                                  *)
      (* a possible better mechanism would be to implement a         *)
      (* previous_parent_sex env variable in print_foreach_parent    *)
      if create = Update.Link then (
        let sex = get_parent_sex conf base fn sn oc in
        sexes := if !sexes = -1 then sex else if !sexes = sex then 3 else 4;
        str_val (string_of_int !sexes))
      else Update_util.eval_create create "sex"
  | _ -> raise Not_found

and eval_key_opt conf base p sl = Some (eval_key conf base p sl)

and eval_relation_kind = function
  | Married -> "marr"
  | NotMarried -> "not_marr"
  | Engaged -> "engaged"
  | NoSexesCheckNotMarried -> "nsck"
  | NoSexesCheckMarried -> "nsckm"
  | NoMention -> "no_ment"
  | MarriageBann -> "banns"
  | MarriageContract -> "contract"
  | MarriageLicense -> "license"
  | Pacs -> "pacs"
  | Residence -> "residence"

and eval_special_var conf base = function
  | [ "include_menubar" ] -> (
      match p_getenv conf.env "ip" with
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
  let rec print_foreach env ((fam, cpl, des) as fcd) _ s sl _ al =
    match s :: sl with
    | [ "child" ] -> print_foreach_child env fcd al des.children
    | [ "fevent" ] -> print_foreach_fevent env fcd al fam.fevents
    | [ "fwitness" ] -> print_foreach_fwitness env fcd al fam.fevents
    | [ "witness" ] -> print_foreach_witness env fcd al fam.witnesses
    | [ "parent" ] -> print_foreach_parent env fcd al (Gutil.parent_array cpl)
    | _ -> raise Not_found
  and print_foreach_child env fcd al arr =
    for i = 0 to max 1 (Array.length arr) - 1 do
      let env = Templ.Env.add "cnt" (Vint (i + 1)) env in
      List.iter (print_ast env fcd) al
    done
  and print_foreach_fevent env fcd al list =
    let rec loop first cnt = function
      | _ :: l ->
          let env =
            Templ.Env.(
              env |> add "cnt" (Vint cnt) |> add "first" (Vbool first)
              |> add "last" (Vbool (l = [])))
          in
          List.iter (print_ast env fcd) al;
          loop false (cnt + 1) l
      | [] -> ()
    in
    loop true 1 list
  and print_foreach_fwitness env fcd al list =
    match get_env "cnt" env with
    | Vint i -> (
        match List.nth list (i - 1) with
        | exception Failure _ -> ()
        | e ->
            let rec loop first wcnt = function
              | _ :: l ->
                  let env =
                    Templ.Env.(
                      env |> add "wcnt" (Vint wcnt) |> add "first" (Vbool first)
                      |> add "last" (Vbool (l = [])))
                  in
                  List.iter (print_ast env fcd) al;
                  loop false (wcnt + 1) l
              | [] -> ()
            in
            loop true 1 (Array.to_list e.efam_witnesses))
    | _ -> ()
  and print_foreach_witness env fcd al arr =
    for i = 0 to max 2 (Array.length arr) - 1 do
      let env = Templ.Env.add "cnt" (Vint (i + 1)) env in
      List.iter (print_ast env fcd) al
    done
  and print_foreach_parent env fcd al arr =
    for i = 0 to Array.length arr - 1 do
      let env = Templ.Env.add "cnt" (Vint (i + 1)) env in
      List.iter (print_ast env fcd) al
    done
  in
  print_foreach

let print_update_fam conf base fcd digest =
  match p_getenv conf.env "m" with
  | Some
      ( "ADD_FAM" | "ADD_FAM_OK" | "ADD_PAR" | "ADD_PAR_OK" | "MOD_FAM"
      | "MOD_FAM_OK" | "MRG_DUP_FAM_Y_N" | "MRG_FAM" | "MRG_FAM_OK"
      | "MRG_MOD_FAM_OK" ) ->
      let env = Templ.Env.(add "digest" (Vstring digest) empty) in
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
      Templ.output conf ifun env fcd "updfam"
  | Some _ | None -> Hutil.incorrect_request conf

let print_del1 conf base ifam =
  let cpl = Driver.foi base ifam in
  let ifath = Driver.get_father cpl in
  let imoth = Driver.get_mother cpl in
  let title _ =
    transl_nth conf "family/families" 0
    |> transl_decline conf "delete"
    |> Utf8.capitalize_fst |> Output.print_sstring conf
  in
  Hutil.header conf title;
  Output.print_sstring conf "<h2>\n";
  Output.print_string conf
    (Util.escape_html (Driver.p_first_name base (Driver.poi base ifath)));
  Output.print_sstring conf
    (Format.sprintf ".%d " (Driver.get_occ (Driver.poi base ifath)));
  Output.print_string conf
    (Util.escape_html (Driver.p_surname base (Driver.poi base ifath)));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl conf "and");
  Output.print_sstring conf " ";
  Output.print_string conf
    (Util.escape_html (Driver.p_first_name base (Driver.poi base imoth)));
  Output.print_sstring conf
    (Format.sprintf ".%d " (Driver.get_occ (Driver.poi base imoth)));
  Output.print_string conf
    (Util.escape_html (Driver.p_surname base (Driver.poi base imoth)));
  Output.print_sstring conf {|</h2><form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|"><p>|};
  Util.hidden_env conf;
  Util.hidden_input conf "i" (Adef.encoded @@ Driver.Ifam.to_string ifam);
  (match p_getenv conf.env "ip" with
  | Some ip -> Util.hidden_input conf "ip" (Adef.encoded ip)
  | None -> ());
  Util.hidden_input conf "m" (Adef.encoded "DEL_FAM_OK");
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf "</button></p></form>";
  Hutil.trailer conf

let print_inv1 conf base p ifam1 ifam2 =
  let title _ =
    transl_decline conf "invert" ""
    |> Utf8.capitalize_fst |> Adef.safe |> Output.print_string conf
  in
  let cpl1 = Driver.foi base ifam1 in
  let cpl2 = Driver.foi base ifam2 in
  Hutil.header conf title;
  Output.print_sstring conf
    (Utf8.capitalize_fst
       (transl conf "invert the order of the following families"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf "<ul><li>";
  Update.print_someone conf base (Driver.poi base (Driver.get_father cpl1));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "and" 0);
  Output.print_sstring conf " ";
  Update.print_someone conf base (Driver.poi base (Driver.get_mother cpl1));
  Output.print_sstring conf "</li><li>";
  Update.print_someone conf base (Driver.poi base (Driver.get_father cpl2));
  Output.print_sstring conf " ";
  Output.print_sstring conf (transl_nth conf "and" 0);
  Output.print_sstring conf " ";
  Update.print_someone conf base (Driver.poi base (Driver.get_mother cpl2));
  Output.print_sstring conf "</li></ul>";
  Output.print_sstring conf {|<form method="post" action="|};
  Output.print_sstring conf conf.command;
  Output.print_sstring conf {|"><p>|};
  Util.hidden_env conf;
  Util.hidden_input conf "i"
    (Driver.get_iper p |> Driver.Iper.to_string |> Adef.encoded);
  Util.hidden_input conf "f" (Driver.Ifam.to_string ifam2 |> Adef.encoded);
  Util.hidden_input conf "m" (Adef.encoded "INV_FAM_OK");
  Output.print_sstring conf
    {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
  Output.print_sstring conf "</button></p></form>";
  Hutil.trailer conf

let print_add conf base =
  let fath, moth, digest =
    match p_getenv conf.env "ip" with
    | Some i ->
        let p = Driver.poi base (Driver.Iper.of_string i) in
        let fath =
          if
            Driver.get_sex p = Male
            || (Driver.get_sex p = Neuter && p_getenv conf.env "sex" = Some "M")
          then person_key base (Driver.get_iper p)
          else ("", "", 0, Update.Create (Male, None), "")
        in
        let moth =
          if
            Driver.get_sex p = Female
            || (Driver.get_sex p = Neuter && p_getenv conf.env "sex" = Some "F")
          then person_key base (Driver.get_iper p)
          else ("", "", 0, Update.Create (Female, None), "")
        in
        let digest = string_of_int (Array.length (Driver.get_family p)) in
        (fath, moth, digest)
    | None ->
        ( ("", "", 0, Update.Create (Male, None), ""),
          ("", "", 0, Update.Create (Female, None), ""),
          "" )
  in
  let fam =
    {
      marriage = Date.cdate_None;
      marriage_place = "";
      marriage_note = "";
      marriage_src = "";
      witnesses = [||];
      relation = Married;
      divorce = NotDivorced;
      fevents = [];
      comment = "";
      origin_file = "";
      fsources = default_source conf;
      fam_index = Driver.Ifam.dummy;
    }
  and cpl = Gutil.couple conf.multi_parents fath moth
  and des = { children = [||] } in
  print_update_fam conf base (fam, cpl, des) digest

let print_add_parents conf base =
  match Util.find_person_in_env conf base "p" with
  | None -> Hutil.incorrect_request conf
  | Some p ->
      let fam =
        {
          marriage = Date.cdate_None;
          marriage_place = "";
          marriage_note = "";
          marriage_src = "";
          witnesses = [||];
          relation = Married;
          divorce = NotDivorced;
          fevents = [];
          comment = "";
          origin_file = "";
          fsources = default_source conf;
          fam_index = Driver.Ifam.dummy;
        }
      and cpl =
        Gutil.couple conf.multi_parents
          ( "",
            Driver.sou base (Driver.get_surname p),
            0,
            Update.Create (Neuter, None),
            "" )
          ("", "", 0, Update.Create (Neuter, None), "")
      and des =
        {
          children =
            [|
              ( Driver.sou base (Driver.get_first_name p),
                Driver.sou base (Driver.get_surname p),
                Driver.get_occ p,
                Update.Link,
                "" );
            |];
        }
      in
      print_update_fam conf base (fam, cpl, des) ""

let print_mod conf base =
  match p_getenv conf.env "i" with
  | Some i ->
      let sfam = string_family_of conf base (Driver.Ifam.of_string i) in
      let salt = Option.get conf.secret_salt in
      let digest = Update.digest_family ~salt sfam in
      print_update_fam conf base sfam digest
  | _ -> Hutil.incorrect_request conf

let print_del conf base =
  match p_getenv conf.env "i" with
  | Some i -> print_del1 conf base (Driver.Ifam.of_string i)
  | _ -> Hutil.incorrect_request conf

let rec find_families ifam = function
  | ifam1 :: ifam2 :: ifaml ->
      if ifam2 = ifam then Some (ifam1, ifam2)
      else find_families ifam (ifam2 :: ifaml)
  | _ -> None

let print_inv conf base =
  match (p_getenv conf.env "i", p_getenv conf.env "f") with
  | Some ip, Some ifam -> (
      let u = Driver.poi base (Driver.Iper.of_string ip) in
      match
        find_families
          (Driver.Ifam.of_string ifam)
          (Array.to_list (Driver.get_family u))
      with
      | Some (ifam1, ifam2) ->
          let p = Driver.poi base (Driver.Iper.of_string ip) in
          print_inv1 conf base p ifam1 ifam2
      | _ -> Hutil.incorrect_request conf)
  | _ -> Hutil.incorrect_request conf

let change_order u ifam n =
  let rec loop i = function
    | [] -> if i = n then [ ifam ] else []
    | fam :: faml ->
        if ifam = fam then
          (* S: The following code is strange: if i=n, fam is added to the iterated list;
             at the next iteration, we reach the same block and i = n+1, hence fam is removed *)
          if i = n then ifam :: loop (i + 1) (fam :: faml)
          else loop i faml
            (* S: Same remark than before: fam is added to the iterated list, hence discarded after *)
        else if i = n then ifam :: loop (i + 1) (fam :: faml)
        else fam :: loop (i + 1) faml
  in
  loop 1 (Array.to_list (Driver.get_family u))

let print_change_order conf base =
  match
    (p_getenv conf.env "i", p_getenv conf.env "f", p_getint conf.env "n")
  with
  | Some ip, Some ifam, Some n ->
      let ip = Driver.Iper.of_string ip in
      let ifam = Driver.Ifam.of_string ifam in
      let p = Driver.poi base ip in
      let print_person p sn =
        Output.print_string conf (escape_html @@ Driver.p_first_name base p);
        if Driver.get_occ p <> 0 then (
          Output.print_sstring conf ".";
          Driver.get_occ p |> string_of_int |> Output.print_sstring conf);
        if sn then (
          Output.print_sstring conf " ";
          Output.print_string conf (escape_html @@ Driver.p_surname base p))
      in
      let print_list arr diff_arr =
        Array.iteri
          (fun i ifam ->
            let fam = Driver.foi base ifam in
            let sp = Gutil.spouse (Driver.get_iper p) fam in
            let sp = Driver.poi base sp in
            Output.print_sstring conf "<li";
            if diff_arr.(i) then
              Output.print_sstring conf {| style="background:pink"|};
            Output.print_sstring conf ">";
            print_person p false;
            Output.print_sstring conf " &amp;";
            Output.print_string conf
              (DateDisplay.short_marriage_date_text conf base fam p sp);
            Output.print_sstring conf " ";
            print_person sp true;
            Output.print_sstring conf "</li>")
          arr
      in
      let after = change_order p ifam n in
      let before, after = (Driver.get_family p, Array.of_list after) in
      let title () =
        transl_decline conf "invert" ""
        |> Utf8.capitalize_fst |> Output.print_sstring conf
      in
      (* TODO check if first argument really needs to be [bool -> unit] and not [unit -> unit] *)
      Perso.interp_templ_with_menu
        (fun _b -> title ())
        "perso_header" conf base p;
      Output.print_sstring conf "<h2>";
      title ();
      Output.print_sstring conf "</h2>";
      Output.print_sstring conf
        (Utf8.capitalize_fst
           (transl conf "invert the order of the following families"));
      Update.print_order_changed conf print_list before after;
      Output.print_sstring conf {|<form method="post" action="|};
      Output.print_sstring conf conf.command;
      Output.print_sstring conf {|"><p>|};
      Util.hidden_env conf;
      Util.hidden_input conf "i" (Adef.encoded @@ Driver.Iper.to_string ip);
      Util.hidden_input conf "f" (Adef.encoded @@ Driver.Ifam.to_string ifam);
      Util.hidden_input conf "n" (Adef.encoded @@ string_of_int n);
      Util.hidden_input conf "m" (Adef.encoded "CHG_FAM_ORD_OK");
      Output.print_sstring conf
        {|</p><p><button type="submit" class="btn btn-primary btn-lg">|};
      Output.print_sstring conf
        (Utf8.capitalize_fst (transl_nth conf "validate/delete" 0));
      Output.print_sstring conf "</button></p></form>";
      Hutil.trailer conf
  | _ -> Hutil.incorrect_request conf ~comment:"bad params for CHG_FAM_ORD"

let print_change_event_order conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some i ->
      let i = Driver.Ifam.of_string i in
      let sfam = string_family_of conf base i in
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
      Templ.output conf ifun Templ.Env.empty sfam "updfamevt"
