(* camlp5r ./pa_html.cmo *)
(* $Id: updateFamOk.ml,v 5.53 2008-01-08 02:08:00 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Futil;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
value removed_string = ref [] ;

type create_info =
  Update.create_info ==
    { ci_birth_date : option date;
      ci_birth_place : string;
      ci_death : death;
      ci_death_date : option date;
      ci_death_place : string;
      ci_occupation : string;
      ci_public : bool }
;

value raw_get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value get conf key =
  match p_getenv conf.env key with
  [ Some v -> v
  | None -> failwith (key ^ " unbound") ]
;

value get_nth conf key cnt = p_getenv conf.env (key ^ string_of_int cnt);

value getn conf var key =
  match p_getenv conf.env (var ^ "_" ^ key) with
  [ Some v -> v
  | None -> failwith (var ^ "_" ^ key ^ " unbound") ]
;

value reconstitute_somebody conf var =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname = no_html_tags (only_printable (getn conf var "sn")) in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char) ||
       (List.exists contain_sn Name.forbidden_char) then
      do {
        removed_string.val :=
          [(Name.purge first_name ^ " " ^ Name.purge surname) :: removed_string.val];
        (Name.purge first_name, Name.purge surname)
      }
    else (first_name, surname)
  in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let create =
    match getn conf var "p" with
    [ "create" -> Update.Create sex None
    | _ -> Update.Link ]
  in
  (first_name, surname, occ, create, var)
;

value reconstitute_parent_or_child conf var default_surname =
  let first_name = no_html_tags (only_printable (getn conf var "fn")) in
  let surname =
    let surname = no_html_tags (only_printable (getn conf var "sn")) in
    if surname = "" then default_surname else surname
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let (first_name, surname) =
    let contain_fn = String.contains first_name in
    let contain_sn = String.contains surname in
    if (List.exists contain_fn Name.forbidden_char) ||
       (List.exists contain_sn Name.forbidden_char) then
      do {
        removed_string.val :=
          [(Name.purge first_name ^ " " ^ Name.purge surname) :: removed_string.val];
        (Name.purge first_name, Name.purge surname)
      }
    else (first_name, surname)
  in
  let occ = try int_of_string (getn conf var "occ") with [ Failure _ -> 0 ] in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let death =
      match p_getenv conf.env (var ^ "d_yyyy") with
      [ Some "+" -> DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | _ -> DontKnowIfDead ]
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    let occupation = only_printable (getn conf var "occupation") in
    let public = getn conf (var ^ "b") "yyyy" = "p" in
    {ci_birth_date = b; ci_birth_place = bpl; ci_death = death;
     ci_death_date = d; ci_death_place = dpl; ci_occupation = occupation;
     ci_public = public}
  in
  let sex =
    match p_getenv conf.env (var ^ "_sex") with
    [ Some "M" -> Male
    | Some "F" -> Female
    | _ -> Neuter ]
  in
  let create =
    match getn conf var "p" with
    [ "create" -> Update.Create sex (Some create_info)
    | _ -> Update.Link ]
  in
  (first_name, surname, occ, create, var)
;

value invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match (p_getenv conf.env var, children) with
  [ (Some "on", [c1 :: children]) -> (c1, [c :: children], True)
  | _ -> (c, children, ext) ]
;

value insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  [ (_, Some n) when n > 1 ->
      let children =
        loop children n where rec loop children n =
          if n > 0 then
            let new_child = ("", "", 0, Update.Create Neuter None, "") in
            loop [new_child :: children] (n - 1)
          else children
      in
      (children, True)
  | (Some "on", _) ->
      let new_child = ("", "", 0, Update.Create Neuter None, "") in
      ([new_child :: children], True)
  | _ -> (children, ext) ]
;

value insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  [ (_, Some n) when n > 1 ->
      let parents =
        loop parents n where rec loop parents n =
          if n > 0 then
            let new_parent = ("", "", 0, Update.Create Neuter None, "") in
            loop [new_parent :: parents] (n - 1)
          else parents
      in
      (parents, True)
  | (Some "on", _) ->
      let new_parent = ("", "", 0, Update.Create Neuter None, "") in
      ([new_parent :: parents], True)
  | _ -> (parents, ext) ]
;

value reconstitute_insert_event conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
    [ (_, Some n) when n > 1 -> n
    | (Some "on", _) -> 1
    | _ -> 0 ]
  in
  if n > 0 then
    let el =
      loop el n where rec loop el n =
        if n > 0 then
          let e1 =
            {efam_name = Efam_Name ""; efam_date = Adef.codate_None;
             efam_place = ""; efam_reason = "";efam_note = "";
             efam_src = ""; efam_witnesses = [| |]}
          in
          loop [e1 :: el] (n - 1)
        else el
    in
    (el, True)
  else (el, ext)
;

value rec reconstitute_events conf ext cnt =
  match get_nth conf "e_name" cnt with
  [ Some efam_name ->
      let efam_name =
        match efam_name with
        [ "#marr" -> Efam_Marriage
        | "#nmar" -> Efam_NoMarriage
        | "#nmen" -> Efam_NoMention
        | "#enga" -> Efam_Engage
        | "#div" -> Efam_Divorce
        | "#sep" -> Efam_Separated
        | "#anul" -> Efam_Annulation
        | "#marb" -> Efam_MarriageBann
        | "#marc" -> Efam_MarriageContract
        | "#marl" -> Efam_MarriageLicense
        | "#pacs" -> Efam_PACS
        | "#resi" -> Efam_Residence
        | n -> Efam_Name (no_html_tags (only_printable n)) ]
      in
      let efam_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let efam_place =
        match get_nth conf "e_place" cnt with
        [ Some place -> no_html_tags (only_printable place)
        | _ -> "" ]
      in
      let efam_note =
        match get_nth conf "e_note" cnt with
        [ Some note -> only_printable_or_nl (strip_all_trailing_spaces note)
        | _ -> "" ]
      in
      let efam_src =
        match get_nth conf "e_src" cnt with
        [ Some src -> only_printable src
        | _ -> "" ]
      in
      let (witnesses, ext) =
        loop 1 ext where rec loop i ext =
          match
            try
              Some
                (reconstitute_somebody conf
                   ("e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i))
            with
            [ Failure _ -> None ]
          with
          [ Some c ->
              let (witnesses, ext) = loop (i + 1) ext in
              let c =
                match
                  p_getenv conf.env
                    ("e" ^ string_of_int cnt ^ "_witn" ^
                        string_of_int i ^ "_kind")
                with
                [ Some "godp" -> (c, Witness_GodParent)
                | Some "offi" -> (c, Witness_Officer)
                | _ -> (c, Witness) ]
              in
              match
                p_getenv conf.env
                  ("e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i)
              with
              [ Some "on" ->
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i ^ "_n"
                  in
                  match p_getint conf.env ins_witn_n with
                  [ Some n when n > 1 ->
                      loop_witn n witnesses where rec loop_witn n witnesses =
                        if n = 0 then ([c :: witnesses], True)
                        else
                  let new_witn =
                    (("", "", 0, Update.Create Neuter None, ""), Witness)
                  in
                          let witnesses = [new_witn :: witnesses] in
                          loop_witn (n-1) witnesses
                  | _ ->
                    let new_witn =
                      (("", "", 0, Update.Create Neuter None, ""), Witness)
                    in
                    ([c; new_witn :: witnesses], True) ]
              | _ -> ([c :: witnesses], ext) ]
          | None -> ([], ext) ]
      in
      let (witnesses, ext) =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match p_getenv conf.env evt_ins with
        [ Some "on" ->
            let ins_witn_n =
              "e" ^ string_of_int cnt ^ "_ins_witn0_n"
            in
            match p_getint conf.env ins_witn_n with
            [ Some n when n > 1 ->
                loop_witn n witnesses where rec loop_witn n witnesses =
                  if n = 0 then (witnesses, True)
                  else
            let new_witn =
              (("", "", 0, Update.Create Neuter None, ""), Witness)
            in
                    let witnesses = [new_witn :: witnesses] in
                    loop_witn (n-1) witnesses
            | _ ->
              let new_witn =
                (("", "", 0, Update.Create Neuter None, ""), Witness)
              in
              ([new_witn :: witnesses], True) ]
        | _ -> (witnesses, ext) ]
      in
      let e =
        {efam_name = efam_name; efam_date = Adef.codate_of_od efam_date;
         efam_place = efam_place; efam_reason = "";
         efam_note = efam_note; efam_src = efam_src;
         efam_witnesses = Array.of_list witnesses}
      in
      let (el, ext) = reconstitute_events conf ext (cnt + 1) in
      let (el, ext) = reconstitute_insert_event conf ext cnt el in
      ([e :: el], ext)
  | _ -> ([], ext) ]
;

value rec reconstitute_sorted_fevents conf cnt =
  match (get_nth conf "e_id" cnt, get_nth conf "e_pos" cnt) with
  [ (Some id, Some pos) ->
      let (id, pos) =
        try (int_of_string id, int_of_string pos) with [ Failure _ -> (0, 0) ]
      in
      let el = reconstitute_sorted_fevents conf (cnt + 1) in
      [(id, pos) :: el]
  | _ -> [] ]
;

value reconstitute_from_fevents conf fevents marr div =
  (* On tri les évènements pour être sûr. *)
  let fevents =
    CheckItem.sort_events
      ((fun evt -> CheckItem.Fsort evt.efam_name),
       (fun evt -> evt.efam_date))
      fevents
  in
  let found_marriage = ref False in
  let found_divorce = ref False in
  (* On veut cette fois ci que ce soit le dernier évènement *)
  (* qui soit mis dans les évènements principaux.           *)
  (* Si le dernier évènement est un mariage, on ignore tous les autres. *)
  (* Si on trouve un contrat de mariage après le mariage, on veut tout  *)
  (* quand même que le mariage soit pris par rapport au contrat.        *)
  let rec loop fevents marr div =
    match fevents with
    [ [] -> (marr, div)
    | [evt :: l] ->
        match evt.efam_name with
        [ Efam_Engage ->
            if found_marriage.val then loop l marr div
            else
              let marr =
                (Engaged, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr div
        | Efam_Marriage ->
              let marr =
                (Married, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              (marr, div)
        | Efam_MarriageContract ->
            if found_marriage.val then loop l marr div
            else
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on met une précision "vers".      *)
              let date =
                  match Adef.od_of_codate evt.efam_date with
                  [ Some (Dgreg dmy cal) ->
                      let dmy = {(dmy) with prec = About} in
                      Adef.codate_of_od (Some (Dgreg dmy cal))
                  | _ -> evt.efam_date ]
              in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on ne met pas de lieu.            *)
              let place = "" in
              let marr =
                (Married, date, place, evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr div
        | Efam_NoMention | Efam_MarriageBann | Efam_MarriageLicense |
          Efam_Annulation | Efam_PACS ->
            if found_marriage.val then loop l marr div
            else
              let marr =
                (NoMention, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr div
        | Efam_NoMarriage ->
            if found_marriage.val then loop l marr div
            else
              let marr =
                (NotMarried, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let () = found_marriage.val := True in
              loop l marr div
        | Efam_Divorce ->
            if found_divorce.val then loop l marr div
            else
              let div = Divorced evt.efam_date in
              let () = found_divorce.val := True in
              loop l marr div
        | Efam_Separated ->
            if found_divorce.val then loop l marr div
            else
              let div = Separated in
              let () = found_divorce.val := True in
              loop l marr div
        | _ -> loop l marr div ] ]
  in
  let (marr, div) = loop (List.rev fevents) marr div in
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let marr =
    if not found_marriage.val then (NoMention, Adef.codate_None, "", "", "")
    else marr
  in
  (* Parents de même sexe. *)
  let marr =
    match p_getenv conf.env "nsck" with
    [ Some "on" ->
        let (relation, date, place, note, src) = marr in
        let relation =
          match relation with
          [ Married | NoSexesCheckMarried -> NoSexesCheckMarried
          | _  -> NoSexesCheckNotMarried ]
        in
        (relation, date, place, note, src)
    | _ -> marr ]
  in
  let div =
    if not found_divorce.val then NotDivorced
    else div
  in
  (marr, div)
;

value reconstitute_family conf base =
  let ext = False in
  let relation =
    match (p_getenv conf.env "mrel", p_getenv conf.env "nsck") with
    [ (Some "marr", Some "on") -> NoSexesCheckMarried
    | (Some "marr", Some _ | None) -> Married
    | (Some "not_marr", Some "on") -> NoSexesCheckNotMarried
    | (Some "not_marr", Some _ | None) -> NotMarried
    | (Some "engaged", _) -> Engaged
    | (Some "nsck", _) -> NoSexesCheckNotMarried
    | (Some "nsckm", _) -> NoSexesCheckMarried
    | (Some "no_ment", _) -> NoMention
    | _ -> Married ]
  in
  let marriage = Update.reconstitute_date conf "marr" in
  let marriage_place = no_html_tags (only_printable (get conf "marr_place")) in
  let marriage_note =
    only_printable_or_nl (strip_all_trailing_spaces (get conf "marr_note"))
  in
  let marriage_src = strip_spaces (get conf "marr_src") in
  let (witnesses, ext) =
    loop 1 ext where rec loop i ext =
      match
        try Some (reconstitute_somebody conf ("witn" ^ string_of_int i)) with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (witnesses, ext) = loop (i + 1) ext in
          match p_getenv conf.env ("ins_witn" ^ string_of_int i) with
          [ Some "on" ->
              let new_witn = ("", "", 0, Update.Create Neuter None, "") in
              ([c; new_witn :: witnesses], True)
          | _ -> ([c :: witnesses], ext) ]
      | None -> ([], ext) ]
  in
  let (witnesses, ext) =
    match p_getenv conf.env "ins_witn0" with
    [ Some "on" ->
        let new_witn = ("", "", 0, Update.Create Neuter None, "") in
        ([new_witn :: witnesses], True)
    | _ -> (witnesses, ext) ]
  in
  let divorce =
    match p_getenv conf.env "div" with
    [ Some "not_divorced" -> NotDivorced
    | Some "separated" -> Separated
    | _ ->
        Divorced (Adef.codate_of_od (Update.reconstitute_date conf "div")) ]
  in
  let (events, ext) = reconstitute_events conf ext 1 in
  let (events, ext) = reconstitute_insert_event conf ext 0 events in
  let surname = getn conf "pa1" "sn" in
  let (children, ext) =
    loop 1 ext where rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i)
               surname)
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (children, ext) = loop (i + 1) ext in
          let (c, children, ext) = invert_children conf (c, children, ext) i in
          let (children, ext) = insert_child conf (children, ext) i in
          ([c :: children], ext)
      | None -> ([], ext) ]
  in
  let (children, ext) = insert_child conf (children, ext) 0 in
  let (parents, ext) =
    loop 1 ext where rec loop i ext =
      match
        try
          Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with
        [ Failure _ -> None ]
      with
      [ Some c ->
          let (parents, ext) = loop (i + 1) ext in
          let (parents, ext) = insert_parent conf (parents, ext) i in
          ([c :: parents], ext)
      | None -> ([], ext) ]
  in
  let comment =
    only_printable_or_nl (strip_all_trailing_spaces (get conf "comment"))
  in
  let fsources = only_printable (get conf "src") in
  let origin_file =
    match p_getenv conf.env "origin_file" with
    [ Some x -> x
    | None -> "" ]
  in
  let fam_index =
    match p_getint conf.env "i" with
    [ Some i -> i
    | None -> 0 ]
  in
  (* Mise à jour des évènements principaux. *)
  (* Attention, dans le cas où fevent est vide, i.e. on a valider   *)
  (* avec un texte vide, par exemple lors de l'ajout d'une famille, *)
  (* il faut ajouter un evenement no_mention.                       *)
  let events =
    if events = [] then
      let evt =
        {efam_name = Efam_NoMention ; efam_date = Adef.codate_None;
         efam_place = ""; efam_reason = "";efam_note = "";
         efam_src = ""; efam_witnesses = [| |]}
      in
      [evt]
    else events
  in
  let marriage = Adef.codate_of_od marriage in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let (marr, div) =
    reconstitute_from_fevents conf events
      (relation, marriage, marriage_place, marriage_note, marriage_src)
      divorce
  in
  let (relation, marriage, marriage_place,
       marriage_note, marriage_src) =
    marr
  in
  (* Si parents de même sex ... Pas de mode multi parent. *)
  let relation =
    match parents with
    [ [father; mother] ->
        let father_sex =
          match father with
          [ (_, _, _, Update.Create sex _, _) -> sex
          | (f, s, o, Update.Link, _) ->
              match person_of_key base f s o with
              [ Some ip -> get_sex (poi base ip)
              | _ -> Neuter ] ]
        in
        let mother_sex =
          match mother with
          [ (_, _, _, Update.Create sex _, _) -> sex
          | (f, s, o, Update.Link, _) ->
              match person_of_key base f s o with
              [ Some ip -> get_sex (poi base ip)
              | _ -> Neuter ] ]
        in
        match (father_sex, mother_sex) with
        [ (Male, Male) | (Female, Female) ->
            match relation with
            [ Married -> NoSexesCheckMarried
            | _ -> NoSexesCheckNotMarried ]
        | _ -> relation ]
    | _ -> relation ]
  in
  let divorce = div in
  let fam =
    {marriage = marriage; marriage_place = marriage_place;
     marriage_note = marriage_note; marriage_src = marriage_src;
     witnesses = Array.of_list witnesses; relation = relation;
     divorce = divorce; fevents = events;
     comment = comment; origin_file = origin_file;
     fsources = fsources; fam_index = Adef.ifam_of_int fam_index}
  and cpl = parent conf.multi_parents (Array.of_list parents)
  and des = {children = Array.of_list children} in
  (fam, cpl, des, ext)
;

value strip_events fevents =
  let strip_array_witness pl =
    let pl =
      List.fold_right
        (fun (((f, s, o, c, _), k) as p) pl -> if f = "" then pl else [p :: pl])
        (Array.to_list pl) []
    in
    Array.of_list pl
  in
  List.fold_right
    (fun e accu ->
       let has_name =
         match e.efam_name with
         [ Efam_Name s -> s <> ""
         | _ -> True ]
       in
       if has_name then
         let witnesses = strip_array_witness e.efam_witnesses in
         [ {(e) with efam_witnesses = witnesses} :: accu ]
       else accu)
    fevents []
;

value strip_array_persons pl =
  let pl =
    List.fold_right
      (fun ((f, s, o, c, _) as p) pl -> if f = "" then pl else [p :: pl])
      (Array.to_list pl) []
  in
  Array.of_list pl
;

value error_family conf base err = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err = Printf.sprintf "%s" (capitale (transl conf "error")) in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.printf "%s\n" (capitale err);
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr
};

value check_event_witnesses conf base witnesses =
  let wl = Array.to_list witnesses in
  let rec loop wl =
    match wl with
    [ [] -> None
    | [((fn, sn, _, _, _), _) :: l] ->
        if fn = "" && sn = "" then
          (* Champs non renseigné, il faut passer au suivant *)
          loop l
        else if fn = "" || fn = "?" then
          Some ((transl_nth conf "witness/witnesses" 0) ^ (" : ")
                ^ (transl conf "first name missing"))
        else if sn = "" || sn = "?" then
          Some ((transl_nth conf "witness/witnesses" 0) ^ (" : ")
                ^ (transl conf "surname missing"))
        else loop l ]
  in
  loop wl
;

value check_witnesses conf base fam =
  let wl = Array.to_list fam.witnesses in
  let rec loop wl =
    match wl with
    [ [] -> None
    | [(fn, sn, _, _, _) :: l] ->
        if fn = "" && sn = "" then
          (* Champs non renseigné, il faut passer au suivant *)
          loop l
        else if fn = "" || fn = "?" then
          Some ((transl_nth conf "witness/witnesses" 0) ^ (" : ")
                ^ (transl conf "first name missing"))
        else if sn = "" || sn = "?" then
          Some ((transl_nth conf "witness/witnesses" 0) ^ (" : ")
                ^ (transl conf "surname missing"))
        else loop l ]
  in
  loop wl
;

value check_parents conf base cpl =
  let (fa_fn, fa_sn, _, _, _) = father cpl in
  let (mo_fn, mo_sn, _, _, _) = mother cpl in
  match ((fa_fn = "", fa_sn = ""), (mo_fn = "", mo_sn = "")) with
  [ ((True, True), (True, True)) | ((True, True), (False, False)) |
    ((False, False), (True, True)) | ((False, False), (False, False)) -> None
  | ((False, True), _) ->
      Some ((transl_nth conf "father/mother" 0) ^ (" : ")
            ^ (transl conf "surname missing"))
  | ((True, False), _) ->
      Some ((transl_nth conf "father/mother" 0) ^ (" : ")
            ^ (transl conf "first name missing"))
  | (_, (False, True)) ->
      Some ((transl_nth conf "father/mother" 1) ^ (" : ")
            ^ (transl conf "surname missing"))
  | (_, (True, False)) ->
      Some ((transl_nth conf "father/mother" 1) ^ (" : ")
            ^ (transl conf "first name missing")) ]
;

value check_family conf base fam cpl =
  (* N'est plus nécessaire avec les évènements. Est fait dans fevents. *)
  (*let err_witness = check_witnesses conf base fam in*)
  let err_parents = check_parents conf base cpl in
  let err_fevent_witness =
    (* On regarde si les témoins sont bien renseignés. *)
    loop fam.fevents where rec loop fevents =
      match fevents with
      [ [] -> None
      | [evt :: l] ->
          match check_event_witnesses conf base evt.efam_witnesses with
          [ Some err -> Some err
          | _ -> loop l ] ]
  in
  (*
  let err_witness =
    match (err_witness, err_fevent_witness) with
    [ (Some err, _) | (_, Some err) -> Some err
    | (None, None) -> None ]
  in
  (err_witness, err_parents)
  *)
  (err_fevent_witness, err_parents)
;

value strip_family fam des =
  let fam =
    {(fam) with witnesses = strip_array_persons fam.witnesses;
     fevents = strip_events fam.fevents}
  in
  let des = {children = strip_array_persons des.children} in
  (fam, des)
;

value print_err_parents conf base p = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err =
        Printf.sprintf (fcapitale (ftransl conf "%t already has parents"))
          (fun _ -> Printf.sprintf "\n%s" (referenced_person_text conf base p))
      in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.printf "\n";
  Wserver.printf (fcapitale (ftransl conf "%t already has parents"))
    (fun _ -> Printf.sprintf "\n%s" (referenced_person_text conf base p));
  Wserver.printf "\n";
  html_p conf;
  tag "ul" begin
    html_li conf;
    Wserver.printf "%s%s %d" (capitale (transl conf "first free number"))
      (Util.transl conf ":")
      (Gutil.find_free_occ base (p_first_name base p) (p_surname base p)
         0);
  end;
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr
};

value print_err_father_sex conf base p = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err =
        Printf.sprintf "\n%s\n%s\n" (referenced_person_text conf base p)
          (transl conf "should be male")
      in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n%s\n" (transl conf "should be male");
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr
};

value print_err_mother_sex conf base p = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err =
        Printf.sprintf "\n%s\n%s\n" (referenced_person_text conf base p)
          (transl conf "should be female")
      in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Wserver.printf "\n%s" (referenced_person_text conf base p);
  Wserver.printf "\n%s\n" (transl conf "should be female");
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr
};

value print_err conf base = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err = Printf.sprintf "%s" (capitale (transl conf "error")) in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Update.print_return conf;
  trailer conf;
  raise Update.ModErr
};

value print_error_disconnected conf = do {
  IFDEF API THEN
    if Api_conf.mode_api.val then
      let err =
        Printf.sprintf "%s" (capitale (transl conf "msg error disconnected"))
      in
      raise (Update.ModErrApi err)
    else ()
  ELSE () END;
  let title _ = Wserver.printf "%s" (capitale (transl conf "error")) in
  rheader conf title;
  Hutil.print_link_to_welcome conf True;
  Wserver.printf "%s" (capitale (transl conf "msg error disconnected"));
  trailer conf;
  raise Update.ModErr
};

value family_exclude pfams efam =
  let pfaml =
    List.fold_right
      (fun fam faml -> if fam = efam then faml else [fam :: faml])
      (Array.to_list pfams) []
  in
  Array.of_list pfaml
;

value infer_origin_file_from_other_marriages conf base ifam ip =
  let u = poi base ip in
  let ufams = get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = get_origin_file (foi base ufams.(i)) in
      if sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0
;

value infer_origin_file conf base ifam ncpl ndes =
  let r =
    infer_origin_file_from_other_marriages conf base ifam (Adef.father ncpl)
  in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages conf base ifam (Adef.mother ncpl)
    else r
  in
  let r =
    match r with
    [ Some r -> r
    | None ->
        let afath = poi base (Adef.father ncpl) in
        let amoth = poi base (Adef.mother ncpl) in
        match (get_parents afath, get_parents amoth) with
        [ (Some if1, _)
          when sou base (get_origin_file (foi base if1)) <> "" ->
            (get_origin_file (foi base if1))
        | (_, Some if2)
          when sou base (get_origin_file (foi base if2)) <> "" ->
            (get_origin_file (foi base if2))
        | _ ->
            let rec loop i =
              if i = Array.length ndes.children then
                Gwdb.insert_string base ""
              else
                let cifams = get_family (poi base ndes.children.(i)) in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  sou base (get_origin_file (foi base cifams.(0))) <> ""
                then
                  get_origin_file (foi base cifams.(0))
                else loop (i + 1)
            in
            loop 0 ] ]
  in
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
    [ Not_found -> False ]
  in
  if no_dec && sou base r = "" then print_error_disconnected conf
  else r
;

value fwitnesses_of fevents =
  List.fold_left
    (fun ipl e ->
       List.fold_left
         (fun ipl (ip, _) -> [ip :: ipl])
         ipl (Array.to_list e.efam_witnesses))
    [] fevents
;


(* Lorsqu'on ajout naissance décès par exemple en créant une personne. *)
value patch_person_with_pevents base ip =
  let p = poi base ip in
  let p = gen_person_of_person p in
  let empty_string = Gwdb.insert_string base "" in
  let evt_birth =
    match (Adef.od_of_codate p.birth, p.birth_place) with
    [ (Some d, _) ->
        let evt =
          { epers_name = Epers_Birth; epers_date = p.birth;
            epers_place = p.birth_place; epers_reason = empty_string;
            epers_note = p.birth_note; epers_src = p.birth_src;
            epers_witnesses = [| |] }
        in
        Some evt
    | (_, place) ->
        if sou base place = "" then None
        else
          let evt =
            { epers_name = Epers_Birth; epers_date = Adef.codate_None;
              epers_place = p.birth_place; epers_reason = empty_string;
              epers_note = p.birth_note; epers_src = p.birth_src;
              epers_witnesses = [| |] }
          in
          Some evt ]
  in
  let evt_baptism =
    match (Adef.od_of_codate p.baptism, p.baptism_place) with
    [ (Some d, _) ->
        let evt =
          { epers_name = Epers_Baptism; epers_date = p.baptism;
            epers_place = p.baptism_place; epers_reason = empty_string;
            epers_note = p.baptism_note; epers_src = p.baptism_src;
            epers_witnesses = [| |] }
        in
        Some evt
    | (_, place) ->
        if sou base place = "" then None
        else
          let evt =
            { epers_name = Epers_Baptism; epers_date = Adef.codate_None;
              epers_place = p.baptism_place; epers_reason = empty_string;
              epers_note = p.baptism_note; epers_src = p.baptism_src;
              epers_witnesses = [| |] }
          in
          Some evt ]
  in
  (*
  let evt_death =
    match p.death with
    [ NotDead | DontKnowIfDead -> None
    | Death death_reason cd ->
        let date = Adef.codate_of_od (Some (Adef.date_of_cdate cd)) in
        let evt =
          { epers_name = Epers_Death; epers_date = date;
            epers_place = p.death_place; epers_reason = empty_string;
            epers_note = p.death_note; epers_src = p.death_src;
            epers_witnesses = [| |] }
        in
        Some evt
    | DeadYoung | DeadDontKnowWhen | OfCourseDead -> None ]
  in
  *)
  let evt_death =
    match (p.death, p.death_place) with
    [ (Death death_reason cd, _) ->
        let date = Adef.codate_of_od (Some (Adef.date_of_cdate cd)) in
        let evt =
          { epers_name = Epers_Death; epers_date = date;
            epers_place = p.death_place; epers_reason = empty_string;
            epers_note = p.death_note; epers_src = p.death_src;
            epers_witnesses = [| |] }
        in
        Some evt
    | (_, place) ->
        if sou base place = "" then None
        else
          let date = Adef.codate_of_od None in
          let evt =
            { epers_name = Epers_Death; epers_date = date;
              epers_place = p.death_place; epers_reason = empty_string;
              epers_note = p.death_note; epers_src = p.death_src;
              epers_witnesses = [| |] }
          in
          Some evt ]
  in
  (* Attention, on prend aussi les autres évènements sinon,  *)
  (* on va tout effacer et ne garder que naissance et décès. *)
  let found_birth = ref False in
  let found_death = ref False in
  let pevents =
    loop p.pevents [] where rec loop pevents accu =
      match pevents with
      [ [] ->
          let accu =
            if found_birth.val then accu
            else
              match (evt_birth, evt_baptism) with
              [ (Some evt, None) -> [evt :: accu]
              | (None, Some evt) -> [evt :: accu]
              | _ -> accu ]
          in
          let accu =
            if found_death.val then accu
            else
              match evt_death with
              [ Some evt -> [evt :: accu]
              | None -> accu ]
          in
          List.rev accu
      | [evt :: l] ->
          match evt.epers_name with
          [ Epers_Birth | Epers_Baptism ->
              if found_birth.val then loop l [evt :: accu]
              else
                match (evt_birth, evt_baptism) with
                [ (Some evt2, None) ->
                    let () = found_birth.val := True in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {(evt2) with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l [evt :: accu]
                | (None, Some evt2) ->
                    let () = found_birth.val := True in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {(evt2) with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l [evt :: accu]
                | _ -> loop l [evt :: accu] ]
          | Epers_Death ->
              if found_death.val then loop l [evt :: accu]
              else
                match evt_death with
                [ Some evt2 ->
                    let () = found_death.val := True in
                    (* Si il y avait des témoins, on les remets en place. *)
                    let evt =
                      {(evt2) with epers_witnesses = evt.epers_witnesses}
                    in
                    loop l [evt :: accu]
                | None -> loop l [evt :: accu] ]
          | _ -> loop l [evt :: accu] ] ]
  in
  let p = {(p) with pevents = pevents} in
  patch_person base p.key_index p
;

value patch_parent_with_pevents base cpl =
  Array.iter (patch_person_with_pevents base) (Adef.parent_array cpl)
;

value patch_children_with_pevents base des =
  Array.iter (patch_person_with_pevents base) des.children
;

(* TODO merge avec l'autre ... *)
value reconstitute_from_fevents2 conf base fevents marr div =
  let empty_string = Gwdb.insert_string base "" in
  (* Il faut trier avant de faire un reconstitute ... *)
  let fevents =
    CheckItem.sort_events
      ((fun evt -> CheckItem.Fsort evt.efam_name),
       (fun evt -> evt.efam_date))
      fevents
  in
  let found_marriage = ref False in
  let found_divorce = ref False in
  (* On veut cette fois ci que ce soit le dernier évènement *)
  (* qui soit mis dans les évènements principaux.           *)
  let rec loop fevents marr div wit =
    match fevents with
    [ [] -> (marr, div, wit)
    | [evt :: l] ->
        match evt.efam_name with
        [ Efam_Engage ->
            if found_marriage.val then loop l marr div wit
            else
              let marr =
                (Engaged, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let wit = evt.efam_witnesses in
              let () = found_marriage.val := True in
              loop l marr div wit
        | Efam_Marriage ->
            let marr =
              (Married, evt.efam_date, evt.efam_place,
               evt.efam_note, evt.efam_src)
            in
            let wit = evt.efam_witnesses in
            let () = found_marriage.val := True in
            (marr, div, wit)
        | Efam_MarriageContract ->
            if found_marriage.val then loop l marr div wit
            else
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on met une précision "vers".      *)
              let date =
                  match Adef.od_of_codate evt.efam_date with
                  [ Some (Dgreg dmy cal) ->
                      let dmy = {(dmy) with prec = About} in
                      Adef.codate_of_od (Some (Dgreg dmy cal))
                  | _ -> evt.efam_date ]
              in
              (* Pour différencier le fait qu'on recopie le *)
              (* mariage, on ne met pas de lieu.            *)
              let place = empty_string in
              let marr = (Married, date, place, evt.efam_note, evt.efam_src) in
              let wit = evt.efam_witnesses in
              let () = found_marriage.val := True in
              loop l marr div wit
        | Efam_NoMention | Efam_MarriageBann | Efam_MarriageLicense |
          Efam_Annulation | Efam_PACS ->
            if found_marriage.val then loop l marr div wit
            else
              let marr =
                (NoMention, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let wit = evt.efam_witnesses in
              let () = found_marriage.val := True in
              loop l marr div wit
        | Efam_NoMarriage ->
            if found_marriage.val then loop l marr div wit
            else
              let marr =
                (NotMarried, evt.efam_date, evt.efam_place,
                 evt.efam_note, evt.efam_src)
              in
              let wit = evt.efam_witnesses in
              let () = found_marriage.val := True in
              loop l marr div wit
        | Efam_Divorce ->
            if found_divorce.val then loop l marr div wit
            else
              let div = Divorced evt.efam_date in
              let () = found_divorce.val := True in
              loop l marr div wit
        | Efam_Separated ->
            if found_divorce.val then loop l marr div wit
            else
              let div = Separated in
              let () = found_divorce.val := True in
              loop l marr div wit
        | _ -> loop l marr div wit ] ]
  in
  let (marr, div, wit) = loop (List.rev fevents) marr div [| |] in
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let (marr, wit) =
    if not found_marriage.val then
      ((NoMention, Adef.codate_None, empty_string,
        empty_string, empty_string), [| |])
    else (marr, wit)
  in
  (* Parents de même sexe. *)
  let marr =
    match p_getenv conf.env "nsck" with
    [ Some "on" ->
        let (relation, date, place, note, src) = marr in
        let relation =
          match relation with
          [ Married | NoSexesCheckMarried -> NoSexesCheckMarried
          | _  -> NoSexesCheckNotMarried ]
        in
        (relation, date, place, note, src)
    | _ -> marr ]
  in
  let div =
    if not found_divorce.val then NotDivorced
    else div
  in
  (marr, div, wit)
;

(* On met à jour les témoins maintenant. *)
value update_family_with_fevents conf base fam =
  let (marr, div, witnesses) =
    reconstitute_from_fevents2 conf base fam.fevents
      (fam.relation, fam.marriage, fam.marriage_place,
       fam.marriage_note, fam.marriage_src)
      fam.divorce
  in
  let (relation, marriage, marriage_place,
       marriage_note, marriage_src) =
    marr
  in
  let divorce = div in
  let witnesses = List.map fst (Array.to_list witnesses) in
  let fam =
    {(fam) with marriage = marriage; marriage_place = marriage_place;
     marriage_note = marriage_note; marriage_src = marriage_src;
     relation = relation; divorce = divorce;
     witnesses = Array.of_list witnesses}
  in
  fam
  (*
  let rec loop fevents =
    match fevents with
    [ [] -> []
    | [evt :: l] ->
        match evt.efam_name with
        [ Efam_Marriage | Efam_MarriageBann | Efam_MarriageContract |
          Efam_MarriageLicense ->
            List.map fst (Array.to_list evt.efam_witnesses)
        | _ -> loop l ] ]
  in
  let witnesses = loop (List.rev fam.fevents) in
  let fam = {(fam) with witnesses = Array.of_list witnesses} in
  patch_family base fam.fam_index fam
  *)
;

value effective_mod conf base sfam scpl sdes = do {
  let fi = sfam.fam_index in
  let (oorigin, owitnesses, ofevents) =
    let ofam = foi base fi in
    (get_origin_file ofam, get_witnesses ofam, get_fevents ofam)
  in
  let (oarr, ofather, omother) =
    let ocpl = foi base fi in
    (get_parent_array ocpl, get_father ocpl, get_mother ocpl)
  in
  let ochildren = get_children (foi base fi) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
    [ Some s -> strip_spaces s
    | None -> "" ]
  in
  let ncpl =
    map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath = poi base (Adef.father ncpl) in
  let nmoth = poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
  let nfam =
    IFDEF API THEN
      (* En mode api, on gère directement la relation de même sexe. *)
      if Api_conf.mode_api.val then {(nfam) with relation = sfam.relation}
      else nfam
    ELSE nfam END
  in
  let sfam = {(sfam) with relation = nfam.relation} in
  if sfam.relation <> NoSexesCheckNotMarried &&
     sfam.relation <> NoSexesCheckMarried then do {
    match get_sex nfath with
    [ Female -> print_err_father_sex conf base nfath
    | Male -> ()
    | Neuter ->
        let nfath = {(gen_person_of_person nfath) with sex = Male} in
        patch_person base nfath.key_index nfath ];
    match get_sex nmoth with
    [ Male -> print_err_mother_sex conf base nmoth
    | Female -> ()
    | Neuter ->
        let nmoth = {(gen_person_of_person nmoth) with sex = Female} in
        patch_person base nmoth.key_index nmoth ];
  }
  else ();
  if Adef.father ncpl = Adef.mother ncpl then print_err conf base else ();
  let nfam =
    let origin_file =
      if sfam.origin_file = "" then
        if sou base oorigin <> "" then oorigin
        else infer_origin_file conf base fi ncpl ndes
      else nfam.origin_file
    in
    {(nfam) with origin_file = origin_file; fam_index = fi}
  in
  patch_family base fi nfam;
  patch_couple base fi ncpl;
  patch_descend base fi ndes;
  let narr = Adef.parent_array ncpl in
  for i = 0 to Array.length oarr - 1 do {
    if not (array_mem oarr.(i) narr) then do {
      let ou = poi base oarr.(i) in
      let ou = {family = family_exclude (get_family ou) fi} in
      patch_union base oarr.(i) ou
    }
    else ()
  };
  for i = 0 to Array.length narr - 1 do {
    if not (array_mem narr.(i) oarr) then do {
      let nu = poi base narr.(i) in
      let nu = {family = Array.append (get_family nu) [| fi |]} in
      patch_union base narr.(i) nu;
    }
    else ()
  };
  let cache = Hashtbl.create 101 in
  let find_asc =
    fun ip ->
      try Hashtbl.find cache ip with
      [ Not_found -> do {
          let a = poi base ip in
          let a = {parents = get_parents a; consang = get_consang a} in
          Hashtbl.add cache ip a;
          a
        } ]
  in
  let same_parents =
    Adef.father ncpl = ofather && Adef.mother ncpl = omother
  in
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       let a =
         {parents = None;
          consang =
            if not (array_mem ip ndes.children) then Adef.fix (-1)
            else a.consang}
       in
       Hashtbl.replace cache ip a)
    ochildren;
  Array.iter
    (fun ip ->
       let a = find_asc ip in
       match a.parents with
       [ Some _ -> print_err_parents conf base (poi base ip)
       | None ->
           let a =
             {parents = Some fi;
              consang =
                if not (array_mem ip ochildren) || not same_parents then
                  Adef.fix (-1)
                else a.consang}
           in
           Hashtbl.replace cache ip a ])
    ndes.children;
  Array.iter
    (fun ip ->
       if not (array_mem ip ndes.children) then
         patch_ascend base ip (find_asc ip)
       else ())
    ochildren;
  Array.iter
    (fun ip ->
       if not (array_mem ip ochildren) || not same_parents then
         patch_ascend base ip (find_asc ip)
       else ())
    ndes.children;
  Update.add_misc_names_for_new_persons base created_p.val;
  Update.update_misc_names_of_family base Male {family = get_family nfath};
  let ol_witnesses = Array.to_list owitnesses in
  let nl_witnesses = Array.to_list nfam.witnesses in
  let ol_fevents = fwitnesses_of ofevents in
  let nl_fevents = fwitnesses_of nfam.fevents in
  let ol = List.append ol_witnesses ol_fevents in
  let nl = List.append nl_witnesses nl_fevents in
  let pi = (Adef.father ncpl) in
  Update.update_related_pointers base pi ol nl;
  (fi, nfam, ncpl, ndes)
};

value effective_add conf base sfam scpl sdes =
  let fi = Adef.ifam_of_int (nb_of_families base) in
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with
    [ Some s -> strip_spaces s
    | None -> "" ]
  in
  let ncpl =
    map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p) scpl
  in
  let nfam =
    map_family_ps (Update.insert_person conf base psrc created_p)
      (Gwdb.insert_string base) sfam
  in
  let ndes =
    map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let origin_file = infer_origin_file conf base fi ncpl ndes in
  let nfath_p = poi base (Adef.father ncpl) in
  let nmoth_p = poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
  let nfam =
    IFDEF API THEN
      (* En mode api, on gère directement la relation de même sexe. *)
      if Api_conf.mode_api.val then {(nfam) with relation = sfam.relation}
      else nfam
    ELSE nfam END
  in
  let sfam = {(sfam) with relation = nfam.relation} in
  do {
    if sfam.relation <> NoSexesCheckNotMarried &&
       sfam.relation <> NoSexesCheckMarried then do {
      match get_sex nfath_p with
      [ Female -> print_err_father_sex conf base nfath_p
      | Male -> ()
      | _ ->
          let nfath_p = {(gen_person_of_person nfath_p) with sex = Male} in
          patch_person base nfath_p.key_index nfath_p ];
      match get_sex nmoth_p with
      [ Male -> print_err_mother_sex conf base nmoth_p
      | Female -> ()
      | _ ->
          let nmoth_p = {(gen_person_of_person nmoth_p) with sex = Female} in
          patch_person base nmoth_p.key_index nmoth_p ];
    }
    else if Adef.father ncpl = Adef.mother ncpl then print_err conf base
    else ();
    let nfam = {(nfam) with origin_file = origin_file; fam_index = fi} in
    patch_family base fi nfam;
    patch_couple base fi ncpl;
    patch_descend base fi ndes;
    let nfath_u = {family = Array.append (get_family nfath_p) [| fi |]} in
    let nmoth_u = {family = Array.append (get_family nmoth_p) [| fi |]} in
    patch_union base (Adef.father ncpl) nfath_u;
    patch_union base (Adef.mother ncpl) nmoth_u;
    Array.iter
      (fun ip ->
         let p = poi base ip in
         match get_parents p with
         [ Some _ -> print_err_parents conf base p
         | None ->
             let a = {parents = Some fi; consang = Adef.fix (-1)} in
             patch_ascend base (get_key_index p) a ])
      ndes.children;
    Update.add_misc_names_for_new_persons base created_p.val;
    Update.update_misc_names_of_family base Male nfath_u;
    let nl_witnesses = Array.to_list nfam.witnesses in
    let nl_fevents = fwitnesses_of nfam.fevents in
    let nl = List.append nl_witnesses nl_fevents in
    Update.update_related_pointers base (Adef.father ncpl) [] nl;
    (fi, nfam, ncpl, ndes)
  }
;

value effective_inv conf base ip u ifam =
  let rec loop =
    fun
    [ [ifam1; ifam2 :: ifaml] ->
        if ifam2 = ifam then [ifam2; ifam1 :: ifaml]
        else [ifam1 :: loop [ifam2 :: ifaml]]
    | _ -> do { incorrect_request conf; raise Update.ModErr } ]
  in
  let u = {family = Array.of_list (loop (Array.to_list (get_family u)))} in
  patch_union base ip u
;


(* ************************************************************************ *)
(*  [Fonc] effective_chg_order :
             config -> base -> iper -> person -> ifam -> int -> unit        *)
(** [Description] : Modifie l'ordre de la famille en positionnant la famille
      ifam à la position n. Exemple : [f1 f2 f3 f4] f1 3 => [f2 f3 f1 f4].
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - ip   : iper
      - u    : person
      - ifam : famille à changer de place
      - n    : nouvelle position de la famille
    [Retour] : Néant
    [Rem] : Non exporté en clair hors de ce module.                         *)
(* ************************************************************************ *)
value effective_chg_order conf base ip u ifam n =
  let fam = UpdateFam.change_order conf base ip u ifam n in
  let u = {family = Array.of_list fam} in
  patch_union base ip u
;

value kill_family base ifam1 ip =
  let u = poi base ip in
  let l =
    List.fold_right
      (fun ifam ifaml ->
         if ifam = ifam1 then ifaml else [ifam :: ifaml])
      (Array.to_list (get_family u)) []
  in
  let u = {family = Array.of_list l} in
  patch_union base ip u
;

value kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in
  patch_ascend base ip a
;

value effective_del conf base (ifam, fam) = do {
  kill_family base ifam (get_father fam);
  kill_family base ifam (get_mother fam);
  Array.iter (kill_parents base) (get_children fam);
  delete_family base ifam;
};

value array_forall2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then invalid_arg "array_forall2"
  else
    loop 0 where rec loop i =
      if i = Array.length a1 then True
      else if f a1.(i) a2.(i) then loop (i + 1)
      else False
;

value array_exists f a =
  loop 0 where rec loop i =
    if i = Array.length a then False
    else if f a.(i) then True
    else loop (i + 1)
;

value is_a_link =
  fun
  [ (_, _, _, Update.Link, _) -> True
  | _ -> False ]
;

value is_created_or_already_there ochil_arr nchil schil =
  not (is_a_link schil) || array_mem nchil ochil_arr
;

(* need_check_noloop: optimization
     The no-loop check being a big work on large databases, this
   optimization tests if this is really necessary or not. It is not
   necessary if:
   1/ either all parents are created,
   2/ or all children are created,
   3/ or the new family have the same parents than the old one *and*
      all linked (not created) new children were already children.
*)
(* Replaced && by || to do more checks. *)
(* Improvement : check the name on the parents/children if they linked *)

value need_check_noloop (scpl, sdes, onfs) =
  if array_exists is_a_link (parent_array scpl) ||
     array_exists is_a_link sdes.children
  then
    match onfs with
    [ Some ((opar, ochil), (npar, nchil)) ->
        not
          (array_forall2 (is_created_or_already_there opar) npar
             (parent_array scpl)) ||
        not
          (array_forall2 (is_created_or_already_there ochil) nchil
             sdes.children)
    | None -> True ]
  else False
;

value all_checks_family conf base ifam gen_fam cpl des scdo = do {
  let wl = ref [] in
  let ml = ref [] in
  let error = Update.error conf base in
  let warning w = wl.val := [w :: wl.val] in
  let misc m = ml.val := [m :: ml.val] in
  if need_check_noloop scdo then
    Consang.check_noloop_for_person_list base error
      (Array.to_list (Adef.parent_array cpl))
  else ();
  let fam = family_of_gen_family base (gen_fam, cpl, des) in
  CheckItem.family base error warning ifam fam;
  CheckItem.check_other_fields base misc ifam fam;
  let (wl, ml) = (CheckItem.list_uniq wl.val, CheckItem.list_uniq ml.val) in
  List.iter
    (fun
     [ ChangedOrderOfMarriages p _ after ->
         patch_union base (get_key_index p) {family = after}
     | ChangedOrderOfFamilyEvents ifam  _ after ->
         patch_family base ifam {(gen_fam) with fevents = after}
     | _ -> () ])
    wl;
  (wl, ml)
};

value print_family conf base (wl, ml) cpl des = do {
  let rdsrc =
    match p_getenv conf.env "rdsrc" with
    [ Some "on" -> p_getenv conf.env "src"
    | _ -> p_getenv conf.env "dsrc" ]
  in
  match rdsrc with
  [ Some x ->
      do {
        conf.henv := List.remove_assoc "dsrc" conf.henv;
        if x <> "" then conf.henv := [("dsrc", code_varenv x) :: conf.henv]
        else ()
      }
  | None -> () ];
  tag "ul" begin
    stag "li" begin
      Wserver.printf "%s" (referenced_person_text conf base (poi base (Adef.father cpl)));
    end;
    Wserver.printf "\n";
    stag "li" begin
      Wserver.printf "%s" (referenced_person_text conf base (poi base (Adef.mother cpl)));
    end;
  end;
  if des.children <> [| |] then do {
    tag "ul" begin
      Array.iter
        (fun ip ->
          stag "li" begin
            Wserver.printf "%s" (referenced_person_text conf base (poi base ip));
          end
        )
        des.children;
    end;
  }
  else ();
  Update.print_warnings_and_miscs conf base (wl, ml)
};

value print_mod_ok conf base (wl, ml) cpl des =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family modified"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    (* Si on a supprimé des caractères interdits *)
    if List.length removed_string.val > 0 then
      do {
         Wserver.printf "<h3 class=\"error\">" ;
         Wserver.printf
           (fcapitale (ftransl conf "%s forbidden char"))
           (List.fold_left
              (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ")
              " "
              Name.forbidden_char);
         Wserver.printf "</h3>\n" ;
         List.iter (Wserver.printf "<p>%s</p>") removed_string.val
      }
    else ();
    print_family conf base (wl, ml) cpl des;
    trailer conf
  }
;

value print_change_event_order_ok conf base (wl, ml) cpl des =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family modified"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    print_family conf base (wl, ml) cpl des;
    trailer conf
  }
;

value print_add_ok conf base (wl, ml) cpl des =
  let title _ = Wserver.printf "%s" (capitale (transl conf "family added")) in
  do {
    header conf title;
    print_link_to_welcome conf True;
    (* Si on a supprimé des caractères interdits *)
    if List.length removed_string.val > 0 then
      do {
         Wserver.printf "<h2 class=\"error\">%s</h2>\n" (capitale (transl conf "forbidden char"));
         List.iter (Wserver.printf "<p>%s</p>") removed_string.val
      }
    else ();
    print_family conf base (wl, ml) cpl des;
    trailer conf
  }
;

value print_del_ok conf base wl =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "family deleted"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    match p_getint conf.env "ip" with
    [ Some i ->
        let p = poi base (Adef.iper_of_int i) in
        tag "ul" begin
          Wserver.printf "<li>\n";
          Wserver.printf "%s\n"
            (reference conf base p (person_text conf base p));
        end
    | _ -> () ];
    Update.print_warnings conf base wl;
    trailer conf
  }
;

value print_inv_ok conf base p =
  let title _ =
    Wserver.printf "%s" (capitale (transl conf "inversion done"))
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    Wserver.printf "\n%s" (referenced_person_text conf base p);
    Wserver.printf "\n";
    trailer conf
  }
;

value get_create (_, _, _, create, _) = create;

value forbidden_disconnected conf sfam scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no" with
    [ Not_found -> False ]
  in
  if no_dec then
    if get_create (father scpl) = Update.Link ||
       get_create (mother scpl) = Update.Link then
      False
    else
      List.for_all (fun p -> get_create p <> Update.Link)
        (Array.to_list sdes.children)
  else False
;

value print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string.val := [] in
  let conf = Update.update_conf o_conf in
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf base in
    let redisp =
      match p_getenv conf.env "return" with
      [ Some _ -> True
      | _ -> False ]
    in
    let digest =
      match p_getint conf.env "ip" with
      [ Some ip ->
          string_of_int
            (Array.length (get_family (poi base (Adef.iper_of_int ip))))
      | None -> "" ]
    in
    let sdigest = raw_get conf "digest" in
    if digest <> "" && sdigest <> "" && digest <> sdigest then
      Update.error_digest conf
    else if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
    else if forbidden_disconnected conf sfam scpl sdes then
      print_error_disconnected conf
    else
      match check_family conf base sfam scpl with
      [ (Some err, _) | (_, Some err) -> error_family conf base err
      | (None, None) -> do {
          let (sfam, sdes) = strip_family sfam sdes in
          let (ifam, fam, cpl, des) = effective_add conf base sfam scpl sdes in
          let () = patch_parent_with_pevents base cpl in
          let () = patch_children_with_pevents base des in
          let (wl, ml) =
            all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
          in
          let (changed, act) =
            let fam = Util.string_gen_family base fam in
            let (ip, act) =
              match p_getint conf.env "ip" with
              [ Some i ->
                  if Adef.int_of_iper (Adef.mother cpl) = i then
                    (Adef.mother cpl, "af")
                  else
                    let a = poi base (Adef.iper_of_int i) in
                    match get_parents a with
                    [ Some x when x = ifam -> (Adef.iper_of_int i, "aa")
                    | _ -> (Adef.father cpl, "af") ]
              | None -> (Adef.father cpl, "af") ]
            in
            match act with
            [ "af" ->
                let gen_p =
                  Util.string_gen_person
                    base (gen_person_of_person (poi base ip))
                in
                (U_Add_family gen_p fam, "af")
            | _ ->
                let gen_p =
                  Util.string_gen_person
                    base (gen_person_of_person (poi base ip))
                in
                (U_Add_parent gen_p fam, "aa") ]
          in
          Util.commit_patches conf base;
          History.record conf base changed act;
          Update.delete_topological_sort conf base;
          print_add_ok conf base (wl, ml) cpl des
        } ]
  with
  [ Update.ModErr -> () ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let ifam = Adef.ifam_of_int i in
      let fam = foi base ifam in
      do {
        if not (is_deleted_family fam) then do {
          effective_del conf base (ifam, fam);
          Util.commit_patches conf base;
          let changed =
            let gen_p =
              let p =
                match p_getint conf.env "ip" with
                [ Some i when Adef.int_of_iper (get_mother fam) = i ->
                    poi base (get_mother fam)
                | _ -> poi base (get_father fam) ]
              in
              Util.string_gen_person base (gen_person_of_person p)
            in
            let gen_fam =
              Util.string_gen_family base (gen_family_of_family fam)
            in
            U_Delete_family gen_p gen_fam
          in
          History.record conf base changed "df";
          Update.delete_topological_sort conf base
        }
        else ();
        print_del_ok conf base []
      }
  | _ -> incorrect_request conf ]
;

value print_mod_aux conf base callback =
  try
    let (sfam, scpl, sdes, ext) = reconstitute_family conf base in
    let redisp =
      match p_getenv conf.env "return" with
      [ Some _ -> True
      | _ -> False ]
    in
    let digest =
      let ini_sfam = UpdateFam.string_family_of conf base sfam.fam_index in
      Update.digest_family ini_sfam
    in
    if digest = raw_get conf "digest" then
      if ext || redisp then
        UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
      else
        match check_family conf base sfam scpl with
        [ (Some err, _) | (_, Some err) -> error_family conf base err
        | (None, None) ->
            let (sfam, sdes) = strip_family sfam sdes in
            callback sfam scpl sdes ]
    else Update.error_digest conf
  with
  [ Update.ModErr -> () ]
;

value family_structure conf base ifam =
  let fam = foi base ifam in
  (get_parent_array fam, get_children fam)
;

value print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string.val := [] in
  let o_f =
    let ifam =
      match p_getint o_conf.env "i" with
      [ Some i -> Adef.ifam_of_int i
      | None -> Adef.ifam_of_int (-1) ]
    in
    Util.string_gen_family base (gen_family_of_family (foi base ifam))
  in
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes = do {
    let ofs = family_structure conf base sfam.fam_index in
    let (ifam, fam, cpl, des) = effective_mod conf base sfam scpl sdes in
    let () = patch_parent_with_pevents base cpl in
    let () = patch_children_with_pevents base des in
    let s =
      let sl =
        [fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src]
      in
      let sl =
        loop (fam.fevents) sl where rec loop l accu =
          match l with
          [ [] -> accu
          | [evt :: l] -> loop l [evt.efam_note; evt.efam_src :: accu]]
      in
      String.concat " " (List.map (sou base) sl)
    in
    Notes.update_notes_links_db conf (NotesLinks.PgFam ifam) s;
    let nfs = (Adef.parent_array cpl, des.children) in
    let onfs = Some (ofs, nfs) in
    let (wl, ml) =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match p_getint o_conf.env "ip" with
        [ Some i -> Adef.iper_of_int i
        | None -> Adef.iper_of_int (-1) ]
      in
      let p =
        Util.string_gen_person base (gen_person_of_person (poi base ip))
      in
      let n_f = Util.string_gen_family base fam in
      U_Modify_family p o_f n_f
    in
    History.record conf base changed "mf";
    Update.delete_topological_sort conf base;
    print_mod_ok conf base (wl, ml) cpl des
  }
  in
  print_mod_aux conf base callback
;

value print_inv conf base =
  match (p_getint conf.env "i", p_getint conf.env "f") with
  [ (Some ip, Some ifam) ->
      let p = poi base (Adef.iper_of_int ip) in
      try
        do {
          effective_inv conf base (get_key_index p) p (Adef.ifam_of_int ifam);
          Util.commit_patches conf base;
          let changed =
            let gen_p =
              Util.string_gen_person base (gen_person_of_person p)
            in
            U_Invert_family gen_p (Adef.ifam_of_int ifam)
          in
          History.record conf base changed "if";
          print_inv_ok conf base p
        }
      with
      [ Update.ModErr -> () ]
  | _ -> incorrect_request conf ]
;

value print_change_order_ok conf base =
  match
    (p_getint conf.env "i", p_getint conf.env "f", p_getint conf.env "n")
  with
  [ (Some ip, Some ifam, Some n) ->
      let p = poi base (Adef.iper_of_int ip) in
      try
        do {
          effective_chg_order conf base
            (get_key_index p) p (Adef.ifam_of_int ifam) n;
          Util.commit_patches conf base;
          let changed =
            let gen_p =
              Util.string_gen_person base (gen_person_of_person p)
            in
            U_Invert_family gen_p (Adef.ifam_of_int ifam)
          in
          History.record conf base changed "if";
          print_inv_ok conf base p
        }
      with
      [ Update.ModErr -> () ]
  | _ -> incorrect_request conf ]
;

value print_change_event_order conf base =
  match p_getint conf.env "i" with
  [ Some ifam ->
    try
      do {
        let fam = foi base (Adef.ifam_of_int ifam) in
        let o_f =
          Util.string_gen_family base (gen_family_of_family fam)
        in
        let ht = Hashtbl.create 50 in
        let _ =
          List.fold_left
            (fun id evt ->
               do { Hashtbl.add ht id evt; succ id })
            1 (get_fevents fam)
        in
        let sorted_fevents =
          List.sort
            (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
            (reconstitute_sorted_fevents conf 1)
        in
        let fevents =
          List.fold_right
            (fun (id, _) accu ->
               try [Hashtbl.find ht id :: accu]
               with [ Not_found -> failwith "Sorting event"])
            sorted_fevents []
        in
        let fam = gen_family_of_family fam in
        let fam = {(fam) with fevents = fevents} in
        let fam = update_family_with_fevents conf base fam in
        patch_family base fam.fam_index fam;
        let a = foi base fam.fam_index in
        let cpl = parent conf.multi_parents (get_parent_array a) in
        let des = {children = get_children a} in
        let wl =
          do {
            let wl = ref [] in
            let error = Update.error conf base in
            let warning w = wl.val := [w :: wl.val] in
            let nfam = family_of_gen_family base (fam, cpl, des) in
            CheckItem.family base error warning fam.fam_index nfam;
            List.iter
              (fun
               [ ChangedOrderOfFamilyEvents ifam  _ after ->
                  patch_family base ifam {(fam) with fevents = after}
               | _ -> () ])
             wl.val;
            List.rev wl.val
          }
        in
        Util.commit_patches conf base;
        let changed =
          let ip =
            match p_getint conf.env "ip" with
            [ Some i -> Adef.iper_of_int i
            | None -> Adef.iper_of_int (-1) ]
          in
          let p =
            Util.string_gen_person base (gen_person_of_person (poi base ip))
          in
          let n_f = Util.string_gen_family base fam in
          U_Modify_family p o_f n_f
        in
        History.record conf base changed "mf";
        print_change_event_order_ok conf base (wl, []) cpl des
      }
    with
    [ Update.ModErr -> () ]
  | _ -> incorrect_request conf ]
;
