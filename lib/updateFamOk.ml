(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open Update_util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

(* Liste des string dont on a supprimé un caractère.       *)
(* Utilisé pour le message d'erreur lors de la validation. *)
let removed_string = ref []
let get_purged_fn_sn = Update_util.get_purged_fn_sn removed_string
let reconstitute_somebody = Update_util.reconstitute_somebody removed_string

let reconstitute_parent_or_child conf var default_surname =
  let first_name = only_printable (getn conf var "fn") in
  let surname =
    let surname = only_printable (getn conf var "sn") in
    if surname = "" then default_surname else surname
  in
  (* S'il y a des caractères interdits, on les supprime *)
  let first_name, surname = get_purged_fn_sn first_name surname in
  let occ = try int_of_string (getn conf var "occ") with Failure _ -> 0 in
  let create_info =
    let b = Update.reconstitute_date conf (var ^ "b") in
    let bpl = getn conf (var ^ "b") "pl" in
    let death =
      match p_getenv conf.env (var ^ "d_yyyy") with
      | Some "+" -> DeadDontKnowWhen
      | Some ("-" | "=") -> NotDead
      | Some _ | None -> DontKnowIfDead
    in
    let d = Update.reconstitute_date conf (var ^ "d") in
    let dpl = getn conf (var ^ "d") "pl" in
    let occupation = only_printable (getn conf var "occu") in
    let public = getn conf (var ^ "b") "yyyy" = "p" in
    {
      ci_birth_date = b;
      ci_birth_place = bpl;
      ci_death = death;
      ci_death_date = d;
      ci_death_place = dpl;
      ci_occupation = occupation;
      ci_public = public;
    }
  in
  let sex = getenv_sex conf var in
  let create = getn_p conf var ~create_info sex in
  (first_name, surname, occ, create, var)

let invert_children conf (c, children, ext) i =
  let var = "inv_ch" ^ string_of_int (i + 1) in
  match (p_getenv conf.env var, children) with
  | Some "on", c1 :: children -> (c1, c :: children, true)
  | _ -> (c, children, ext)

let insert_child conf (children, ext) i =
  let var = "ins_ch" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  | _, Some n when n > 1 ->
      let children =
        let rec loop children n =
          if n > 0 then
            let new_child = ("", "", 0, Update.Create (Neuter, None), "") in
            loop (new_child :: children) (n - 1)
          else children
        in
        loop children n
      in
      (children, true)
  | Some "on", _ ->
      let new_child = ("", "", 0, Update.Create (Neuter, None), "") in
      (new_child :: children, true)
  | _ -> (children, ext)

let insert_parent conf (parents, ext) i =
  let var = "ins_pa" ^ string_of_int i in
  match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
  | _, Some n when n > 1 ->
      let parents =
        let rec loop parents n =
          if n > 0 then
            let new_parent = ("", "", 0, Update.Create (Neuter, None), "") in
            loop (new_parent :: parents) (n - 1)
          else parents
        in
        loop parents n
      in
      (parents, true)
  | Some "on", _ ->
      let new_parent = ("", "", 0, Update.Create (Neuter, None), "") in
      (new_parent :: parents, true)
  | _ -> (parents, ext)

let reconstitute_insert_event conf ext cnt el =
  let var = "ins_event" ^ string_of_int cnt in
  let n =
    match (p_getenv conf.env var, p_getint conf.env (var ^ "_n")) with
    | _, Some n when n > 1 -> n
    | Some "on", _ -> 1
    | _ -> 0
  in
  if n > 0 then
    let el =
      let rec loop el n =
        if n > 0 then
          let e1 =
            {
              efam_name = Efam_Name "";
              efam_date = Date.cdate_None;
              efam_place = "";
              efam_reason = "";
              efam_note = "";
              efam_src = "";
              efam_witnesses = [||];
            }
          in
          loop (e1 :: el) (n - 1)
        else el
      in
      loop el n
    in
    (el, true)
  else (el, ext)

let rec reconstitute_events conf ext cnt =
  match get_nth conf "e_name" cnt with
  | None -> ([], ext)
  | Some efam_name ->
      let efam_name =
        match efam_name with
        | "#marr" -> Efam_Marriage
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
        | n -> Efam_Name (only_printable n)
      in
      let efam_date =
        Update.reconstitute_date conf ("e_date" ^ string_of_int cnt)
      in
      let efam_place =
        match get_nth conf "e_place" cnt with
        | Some place -> only_printable place
        | None -> ""
      in
      let efam_note =
        match get_nth conf "e_note" cnt with
        | Some note ->
            only_printable_or_nl (Mutil.strip_all_trailing_spaces note)
        | None -> ""
      in
      let efam_src =
        match get_nth conf "e_src" cnt with
        | Some src -> only_printable src
        | None -> ""
      in
      let witnesses, ext =
        let rec loop i ext =
          let key = "e" ^ string_of_int cnt ^ "_witn" ^ string_of_int i in
          match
            try Some (reconstitute_somebody conf key) with Failure _ -> None
          with
          | None -> ([], ext)
          | Some (fn, sn, occ, create, var) -> (
              let witnesses, ext = loop (i + 1) ext in
              let create = update_ci conf create key in
              let c = (fn, sn, occ, create, var) in
              let c =
                match p_getenv conf.env (key ^ "_kind") with
                | Some "godp" -> (c, Witness_GodParent)
                | Some "offi" -> (c, Witness_CivilOfficer)
                | Some "reli" -> (c, Witness_ReligiousOfficer)
                | Some "info" -> (c, Witness_Informant)
                | Some "atte" -> (c, Witness_Attending)
                | Some "ment" -> (c, Witness_Mentioned)
                | Some "othe" -> (c, Witness_Other)
                | Some _ | None -> (c, Witness)
              in
              match
                p_getenv conf.env
                  ("e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i)
              with
              | Some "on" -> (
                  let ins_witn_n =
                    "e" ^ string_of_int cnt ^ "_ins_witn" ^ string_of_int i
                    ^ "_n"
                  in
                  match p_getint conf.env ins_witn_n with
                  | Some n when n > 1 ->
                      let rec loop_witn n witnesses =
                        if n = 0 then (c :: witnesses, true)
                        else
                          let new_witn =
                            ( ("", "", 0, Update.Create (Neuter, None), ""),
                              Witness )
                          in
                          let witnesses = new_witn :: witnesses in
                          loop_witn (n - 1) witnesses
                      in
                      loop_witn n witnesses
                  | Some _ | None ->
                      let new_witn =
                        (("", "", 0, Update.Create (Neuter, None), ""), Witness)
                      in
                      (c :: new_witn :: witnesses, true))
              | Some _ | None -> (c :: witnesses, ext))
        in
        loop 1 ext
      in
      let witnesses, ext =
        let evt_ins = "e" ^ string_of_int cnt ^ "_ins_witn0" in
        match p_getenv conf.env evt_ins with
        | Some "on" -> (
            let ins_witn_n = "e" ^ string_of_int cnt ^ "_ins_witn0_n" in
            match p_getint conf.env ins_witn_n with
            | Some n when n > 1 ->
                let rec loop_witn n witnesses =
                  if n = 0 then (witnesses, true)
                  else
                    let new_witn =
                      (("", "", 0, Update.Create (Neuter, None), ""), Witness)
                    in
                    let witnesses = new_witn :: witnesses in
                    loop_witn (n - 1) witnesses
                in
                loop_witn n witnesses
            | Some _ | None ->
                let new_witn =
                  (("", "", 0, Update.Create (Neuter, None), ""), Witness)
                in
                (new_witn :: witnesses, true))
        | Some _ | None -> (witnesses, ext)
      in
      let e =
        {
          efam_name;
          efam_date = Date.cdate_of_od efam_date;
          efam_place;
          efam_reason = "";
          efam_note;
          efam_src;
          efam_witnesses = Array.of_list witnesses;
        }
      in
      let el, ext = reconstitute_events conf ext (cnt + 1) in
      let el, ext = reconstitute_insert_event conf ext cnt el in
      (e :: el, ext)

(* S:
 * why is marriage record transformed into a tuple?
 *)
let reconstitute_from_fevents (nsck : bool) (empty_string : 'string)
    (fevents : ('person, 'string) Def.gen_fam_event list) =
  (* On tri les évènements pour être sûr. *)
  let fevents =
    Event.sort_events
      (fun evt -> Event.Fevent evt.efam_name)
      (fun evt -> evt.efam_date)
      fevents
  in
  let found_marriage :
      (Def.relation_kind
      * Def.cdate
      * 'string
      * 'string
      * 'string
      * ('person * Def.witness_kind) array)
      option
      ref =
    ref None
  in

  let found_divorce : Def.divorce option ref = ref None in
  let mk_marr evt kind =
    let e =
      Some
        ( kind,
          evt.efam_date,
          evt.efam_place,
          evt.efam_note,
          evt.efam_src,
          evt.efam_witnesses )
    in
    match !found_marriage with
    | None -> found_marriage := e
    | Some ((NoMention | Residence), _, _, _, _, _)
      when kind <> NoMention && kind <> Residence ->
        found_marriage := e
    | Some (Married, _, _, _, _, _) when kind <> Married -> ()
    | _ -> if kind = Married then found_marriage := e
  in
  let mk_div kind =
    match !found_divorce with
    | None -> found_divorce := Some kind
    | Some _ -> ()
  in
  (* Marriage is more important than any other relation.
     For other cases, latest event is the most important,
     except for NotMention and Residence. *)
  (* FIXME: For now, we ignore Annulation since it gives a wrong date
     (relation on [annulation date] makes no sense) *)
  let rec loop = function
    | [] -> ()
    | evt :: l -> (
        match evt.efam_name with
        | Efam_Engage ->
            mk_marr evt Engaged;
            loop l
        | Efam_Marriage ->
            mk_marr evt Married;
            loop l
        | Efam_MarriageContract ->
            mk_marr evt MarriageContract;
            loop l
        | Efam_NoMention ->
            mk_marr evt NoMention;
            loop l
        | Efam_MarriageBann ->
            mk_marr evt MarriageBann;
            loop l
        | Efam_MarriageLicense ->
            mk_marr evt MarriageLicense;
            loop l
        | Efam_PACS ->
            mk_marr evt Pacs;
            loop l
        | Efam_Residence ->
            mk_marr evt Residence;
            loop l
        | Efam_NoMarriage ->
            mk_marr evt NotMarried;
            loop l
        | Efam_Divorce ->
            mk_div (Divorced evt.efam_date);
            loop l
        | Efam_Separated ->
            mk_div (Separated evt.efam_date);
            loop l
        | Efam_Annulation -> loop l
        | Efam_Name _ -> loop l)
  in
  loop (List.rev fevents);
  (* Il faut gérer le cas où l'on supprime délibérément l'évènement. *)
  let marr, wit =
    match !found_marriage with
    | None ->
        ( (NoMention, Date.cdate_None, empty_string, empty_string, empty_string),
          [||] )
    | Some (kind, date, place, note, src, wit) ->
        ((kind, date, place, note, src), wit)
  in
  (* Parents de même sexe. *)
  let marr =
    if nsck then
      let relation, date, place, note, src = marr in
      let relation =
        match relation with
        | Married -> NoSexesCheckMarried
        | ( NotMarried | Engaged | NoSexesCheckNotMarried | NoMention
          | NoSexesCheckMarried | MarriageBann | MarriageContract
          | MarriageLicense | Pacs | Residence ) as x ->
            x
      in
      (relation, date, place, note, src)
    else marr
  in
  let div = Option.value ~default:NotDivorced !found_divorce in
  (marr, div, wit)

let reconstitute_family conf base nsck =
  let events, ext = reconstitute_events conf false 1 in
  let events, ext = reconstitute_insert_event conf ext 0 events in
  let surname = getn conf "pa1" "sn" in
  let children, ext =
    let rec loop i ext =
      match
        try
          Some
            (reconstitute_parent_or_child conf ("ch" ^ string_of_int i) surname)
        with Failure _ -> None
      with
      | Some c ->
          let children, ext = loop (i + 1) ext in
          let c, children, ext = invert_children conf (c, children, ext) i in
          let children, ext = insert_child conf (children, ext) i in
          (c :: children, ext)
      | None -> ([], ext)
    in
    loop 1 ext
  in
  let children, ext = insert_child conf (children, ext) 0 in
  let parents, ext =
    let rec loop i ext =
      match
        try Some (reconstitute_parent_or_child conf ("pa" ^ string_of_int i) "")
        with Failure _ -> None
      with
      | Some c ->
          let parents, ext = loop (i + 1) ext in
          let parents, ext = insert_parent conf (parents, ext) i in
          (c :: parents, ext)
      | None -> ([], ext)
    in
    loop 1 ext
  in
  let comment =
    only_printable_or_nl (Mutil.strip_all_trailing_spaces (get conf "comment"))
  in
  let fsources = only_printable (get conf "src") in
  let origin_file =
    Option.value ~default:"" (p_getenv conf.env "origin_file")
  in
  let fam_index =
    match p_getenv conf.env "i" with
    | Some i -> Driver.Ifam.of_string i
    | None -> Driver.Ifam.dummy
  in
  (* Mise à jour des évènements principaux. *)
  (* Attention, dans le cas où fevent est vide, i.e. on a valider   *)
  (* avec un texte vide, par exemple lors de l'ajout d'une famille, *)
  (* il faut ajouter un evenement no_mention.                       *)
  let events =
    if events = [] then
      let evt =
        {
          efam_name = Efam_NoMention;
          efam_date = Date.cdate_None;
          efam_place = "";
          efam_reason = "";
          efam_note = "";
          efam_src = "";
          efam_witnesses = [||];
        }
      in
      [ evt ]
    else events
  in
  (* Attention, surtout pas les witnesses, parce que si on en créé un, *)
  (* on le créé aussi dans witness et on ne pourra jamais valider.     *)
  let marr, div, _ =
    (* FIXME: Use witnesses (and Array.map fst witnesses)
       when witnesses will be added inplace *)
    reconstitute_from_fevents nsck "" events
  in
  let relation, marriage, marriage_place, marriage_note, marriage_src = marr in
  (* Si parents de même sex ... Pas de mode multi parent. *)
  let relation =
    match parents with
    | [ father; mother ] -> (
        let father_sex =
          match father with
          | _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ -> (
              match Driver.person_of_key base f s o with
              | Some ip -> Driver.get_sex (Driver.poi base ip)
              | _ -> Neuter)
        in
        let mother_sex =
          match mother with
          | _, _, _, Update.Create (sex, _), _ -> sex
          | f, s, o, Update.Link, _ -> (
              match Driver.person_of_key base f s o with
              | Some ip -> Driver.get_sex (Driver.poi base ip)
              | _ -> Neuter)
        in
        match (father_sex, mother_sex) with
        | Male, Male | Female, Female -> (
            match relation with
            | Married -> NoSexesCheckMarried
            | _ -> NoSexesCheckNotMarried)
        | _ -> relation)
    | _ -> relation
  in
  let divorce = div in
  let fam =
    {
      marriage;
      marriage_place;
      marriage_note;
      marriage_src;
      witnesses = [||];
      relation;
      divorce;
      fevents = events;
      comment;
      origin_file;
      fsources;
      fam_index;
    }
  and cpl = Futil.parent conf.multi_parents (Array.of_list parents)
  and des = { children = Array.of_list children } in
  (fam, cpl, des, ext)

let strip_events fevents =
  let strip_array_witness pl =
    Array.of_list
    @@ Array.fold_right
         (fun (((f, _, _, _, _), _) as p) pl -> if f = "" then pl else p :: pl)
         pl []
  in
  List.fold_right
    (fun e accu ->
      let has_name =
        match e.efam_name with Efam_Name s -> s <> "" | _ -> true
      in
      if has_name then
        let witnesses = strip_array_witness e.efam_witnesses in
        { e with efam_witnesses = witnesses } :: accu
      else accu)
    fevents []

let strip_array_persons pl =
  Array.of_list
  @@ Array.fold_right
       (fun ((f, _, _, _, _) as p) pl -> if f = "" then pl else p :: pl)
       pl []

let error_family conf err =
  Update.prerr conf err @@ fun () ->
  (err |> Update.string_of_error conf : Adef.safe_string :> string)
  |> Utf8.capitalize_fst |> Output.print_sstring conf;
  Output.print_sstring conf "\n";
  Update.print_return conf

let check_parents conf cpl =
  let check get i =
    let fn, sn, _, _, _ = get cpl in
    if fn = "" then
      if sn <> "" then
        Some
          (Update.UERR_missing_first_name
             (transl_nth conf "father/mother" i |> Adef.safe))
      else None
    else if sn = "" then
      Some
        (Update.UERR_missing_surname
           (transl_nth conf "father/mother" i |> Adef.safe))
    else None
  in
  match check Gutil.father 0 with
  | Some _ as err -> err
  | None -> check Gutil.mother 1

let check_family conf fam cpl :
    Update.update_error option * Update.update_error option =
  let err_parents = check_parents conf cpl in
  let err_fevent_witness =
    Update.check_missing_witnesses_names conf
      (fun e -> e.efam_witnesses)
      fam.fevents
  in
  (err_fevent_witness, err_parents)

let strip_family fam des =
  let fam =
    {
      fam with
      witnesses = strip_array_persons fam.witnesses;
      fevents = strip_events fam.fevents;
    }
  in
  let des = { children = strip_array_persons des.children } in
  (fam, des)

let print_err_parents conf base p =
  let err = Update.UERR_already_has_parents (base, p) in
  Update.prerr conf err @@ fun () ->
  Output.print_sstring conf "\n";
  Output.print_string conf (Update.string_of_error conf err);
  Output.print_sstring conf "<p><ul><li>";
  Output.print_sstring conf
    (Utf8.capitalize_fst (transl conf "first free number"));
  Output.print_sstring conf (Util.transl conf ":");
  Output.print_sstring conf @@ string_of_int
  @@ Gutil.find_free_occ base
       (Driver.p_first_name base p)
       (Driver.p_surname base p);
  Output.print_sstring conf "</li></ul>";
  Update.print_return conf

let print_err_sex conf base p =
  let err = Update.UERR_sex_incoherent (base, p) in
  Update.prerr conf err @@ fun () ->
  Output.print_string conf (Update.string_of_error conf err);
  Update.print_return conf

let print_err conf =
  let err =
    Update.UERR (transl conf "error" |> Utf8.capitalize_fst |> Adef.safe)
  in
  Update.prerr conf err @@ fun () -> Update.print_return conf

let print_error_disconnected conf =
  let err =
    Update.UERR
      (transl conf "msg error disconnected" |> Utf8.capitalize_fst |> Adef.safe)
  in
  Update.prerr conf err @@ fun () ->
  Output.print_string conf (Update.string_of_error conf err)

let family_exclude pfams efam =
  let pfaml =
    Array.fold_right
      (fun fam faml -> if fam = efam then faml else fam :: faml)
      pfams []
  in
  Array.of_list pfaml

let infer_origin_file_from_other_marriages base ifam ip =
  let u = Driver.poi base ip in
  let ufams = Driver.get_family u in
  let rec loop i =
    if i = Array.length ufams then None
    else if ufams.(i) = ifam then loop (i + 1)
    else
      let r = Driver.get_origin_file (Driver.foi base ufams.(i)) in
      if Driver.sou base r <> "" then Some r else loop (i + 1)
  in
  loop 0

let infer_origin_file conf base ifam ncpl ndes =
  let r = infer_origin_file_from_other_marriages base ifam (Adef.father ncpl) in
  let r =
    if r = None then
      infer_origin_file_from_other_marriages base ifam (Adef.mother ncpl)
    else r
  in
  let r =
    match r with
    | Some r -> r
    | None -> (
        let afath = Driver.poi base (Adef.father ncpl) in
        let amoth = Driver.poi base (Adef.mother ncpl) in
        match (Driver.get_parents afath, Driver.get_parents amoth) with
        | Some if1, _
          when Driver.sou base (Driver.get_origin_file (Driver.foi base if1))
               <> "" ->
            Driver.get_origin_file (Driver.foi base if1)
        | _, Some if2
          when Driver.sou base (Driver.get_origin_file (Driver.foi base if2))
               <> "" ->
            Driver.get_origin_file (Driver.foi base if2)
        | _ ->
            let rec loop i =
              if i = Array.length ndes.children then
                Driver.insert_string base ""
              else
                let cifams =
                  Driver.get_family (Driver.poi base ndes.children.(i))
                in
                if Array.length cifams = 0 then loop (i + 1)
                else if
                  Driver.sou base
                    (Driver.get_origin_file (Driver.foi base cifams.(0)))
                  <> ""
                then Driver.get_origin_file (Driver.foi base cifams.(0))
                else loop (i + 1)
            in
            loop 0)
  in
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no"
    with Not_found -> false
  in
  if no_dec && Driver.sou base r = "" then print_error_disconnected conf else r

(* TODO EVENT put this in Event *)
let fwitnesses_of fevents =
  List.fold_left
    (fun ipl e ->
      Array.fold_left (fun ipl (ip, _) -> ip :: ipl) ipl e.efam_witnesses)
    [] fevents

(* Lorsqu'on ajout naissance décès par exemple en créant une personne. *)
let patch_person_with_pevents base ip =
  let p = Driver.poi base ip |> Driver.gen_person_of_person in
  let evt ~name ?(date = Date.cdate_None) ~place ~src ~note () =
    {
      epers_name = name;
      epers_date = date;
      epers_place = place;
      epers_reason = Driver.Istr.empty;
      epers_note = note;
      epers_src = src;
      epers_witnesses = [||];
    }
    (* TODO why empty witnesses *)
  in
  let evt_birth =
    let evt ?date () =
      let name = Epers_Birth in
      let place = p.birth_place in
      let note = p.birth_note in
      let src = p.birth_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Option.is_some (Date.od_of_cdate p.birth) then evt ~date:p.birth ()
    else if Driver.sou base p.birth_place = "" then None
    else evt ()
  in
  let evt_baptism =
    let evt ?date () =
      let name = Epers_Baptism in
      let place = p.baptism_place in
      let note = p.baptism_note in
      let src = p.baptism_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    if Option.is_some (Date.od_of_cdate p.baptism) then evt ~date:p.baptism ()
    else if Driver.sou base p.baptism_place = "" then None
    else evt ()
  in
  let evt_death =
    let evt ?date () =
      let name = Epers_Death in
      let place = p.death_place in
      let note = p.death_note in
      let src = p.death_src in
      Some (evt ~name ?date ~place ~note ~src ())
    in
    match Date.date_of_death p.death with
    | Some cd ->
        let date = Date.cdate_of_od (Some cd) in
        evt ~date ()
    | None -> if Driver.sou base p.death_place = "" then None else evt ()
  in
  (* Attention, on prend aussi les autres évènements sinon,  *)
  (* on va tout effacer et ne garder que naissance et décès. *)
  let pevents =
    let found_birth = ref false in
    let found_baptism = ref false in
    let found_death = ref false in
    let replace_witnesses event found new_event =
      (* Si il y avait des témoins, on les remets en place. *)
      if !found then event
      else
        match new_event with
        | None -> event
        | Some new_event ->
            found := true;
            { new_event with epers_witnesses = event.epers_witnesses }
    in
    let l =
      List.map
        (fun evt ->
          match evt.epers_name with
          | Epers_Birth -> replace_witnesses evt found_birth evt_birth
          | Epers_Baptism -> replace_witnesses evt found_baptism evt_baptism
          | Epers_Death -> replace_witnesses evt found_death evt_death
          | _other -> evt)
        p.pevents
    in
    (* add default birth|baptism|death event if it was not found *)
    let complete found event_opt l =
      if found then l
      else match event_opt with None -> l | Some evt -> evt :: l
    in
    complete !found_birth evt_birth l
    |> complete !found_baptism evt_baptism
    |> complete !found_death evt_death
  in
  let p = { p with pevents } in
  Driver.patch_person base p.key_index p

let patch_parent_with_pevents base cpl =
  Array.iter (patch_person_with_pevents base) (Adef.parent_array cpl)

let patch_children_with_pevents base des =
  Array.iter (patch_person_with_pevents base) des.children

(* On met à jour les témoins maintenant. *)
let update_family_with_fevents conf base fam =
  let marr, div, witnesses =
    reconstitute_from_fevents
      (p_getenv conf.env "nsck" = Some "on")
      (Driver.insert_string base "")
      fam.fevents
  in
  let relation, marriage, marriage_place, marriage_note, marriage_src = marr in
  let divorce = div in
  let witnesses = Array.map fst witnesses in
  {
    fam with
    marriage;
    marriage_place;
    marriage_note;
    marriage_src;
    relation;
    divorce;
    witnesses;
  }

let aux_effective_mod conf base nsck sfam scpl sdes fi origin_file =
  let created_p = ref [] in
  let psrc =
    match p_getenv conf.env "psrc" with Some s -> String.trim s | None -> ""
  in
  let ncpl =
    Futil.map_couple_p conf.multi_parents
      (Update.insert_person conf base psrc created_p)
      scpl
  in
  let nfam =
    Futil.map_family_ps
      (Update.insert_person conf base psrc created_p)
      (fun f -> f)
      (Driver.insert_string base)
      sfam
  in
  let ndes =
    Futil.map_descend_p (Update.insert_person conf base psrc created_p) sdes
  in
  let nfath_p = Driver.poi base (Adef.father ncpl) in
  let nmoth_p = Driver.poi base (Adef.mother ncpl) in
  let nfam = update_family_with_fevents conf base nfam in
  let nfam =
    (* En mode api, on gère directement la relation de même sexe. *)
    if conf.api_mode then { nfam with relation = sfam.relation } else nfam
  in
  if not nsck then (
    let exp sex p =
      let s = Driver.get_sex p in
      if s = Neuter then
        let p = { (Driver.gen_person_of_person p) with sex } in
        Driver.patch_person base p.key_index p
      else if s <> sex then print_err_sex conf base p
    in
    exp Male nfath_p;
    exp Female nmoth_p);
  if Adef.father ncpl = Adef.mother ncpl then print_err conf;
  let origin_file = origin_file nfam ncpl ndes in
  let nfam = { nfam with origin_file; fam_index = fi } in
  Driver.patch_family base fi nfam;
  Driver.patch_couple base fi ncpl;
  Driver.patch_descend base fi ndes;
  (nfath_p, nmoth_p, nfam, ncpl, ndes)

let effective_mod conf base nsck sfam scpl sdes =
  let fi = sfam.fam_index in
  let oorigin, owitnesses, ofevents =
    let ofam = Driver.foi base fi in
    ( Driver.get_origin_file ofam,
      Driver.get_witnesses ofam,
      Driver.get_fevents ofam )
  in
  let oarr, ofather, omother =
    let ocpl = Driver.foi base fi in
    ( Driver.get_parent_array ocpl,
      Driver.get_father ocpl,
      Driver.get_mother ocpl )
  in
  let ochildren = Driver.get_children (Driver.foi base fi) in
  let origin_file nfam ncpl ndes =
    if sfam.origin_file = "" then
      if Driver.sou base oorigin <> "" then oorigin
      else infer_origin_file conf base fi ncpl ndes
    else nfam.origin_file
  in
  let _, _, nfam, ncpl, ndes =
    aux_effective_mod conf base nsck sfam scpl sdes fi origin_file
  in
  let narr = Adef.parent_array ncpl in
  for i = 0 to Array.length oarr - 1 do
    if not (Array.mem oarr.(i) narr) then
      let ou = Driver.poi base oarr.(i) in
      let ou = { family = family_exclude (Driver.get_family ou) fi } in
      Driver.patch_union base oarr.(i) ou
  done;
  for i = 0 to Array.length narr - 1 do
    if not (Array.mem narr.(i) oarr) then
      let nu = Driver.poi base narr.(i) in
      let nu = { family = Array.append (Driver.get_family nu) [| fi |] } in
      Driver.patch_union base narr.(i) nu
  done;
  let cache = Hashtbl.create 101 in
  let find_asc ip =
    try Hashtbl.find cache ip
    with Not_found ->
      let a = Driver.poi base ip in
      let a =
        { parents = Driver.get_parents a; consang = Driver.get_consang a }
      in
      Hashtbl.add cache ip a;
      a
  in
  let same_parents = Adef.father ncpl = ofather && Adef.mother ncpl = omother in
  Array.iter
    (fun ip ->
      let a = find_asc ip in
      let a =
        {
          parents = None;
          consang =
            (if not (Array.mem ip ndes.children) then Adef.fix (-1)
             else a.consang);
        }
      in
      Hashtbl.replace cache ip a)
    ochildren;
  Array.iter
    (fun ip ->
      let a = find_asc ip in
      match a.parents with
      | Some _ -> print_err_parents conf base (Driver.poi base ip)
      | None ->
          let a =
            {
              parents = Some fi;
              consang =
                (if (not (Array.mem ip ochildren)) || not same_parents then
                   Adef.fix (-1)
                 else a.consang);
            }
          in
          Hashtbl.replace cache ip a)
    ndes.children;
  Array.iter
    (fun ip ->
      if not (Array.mem ip ndes.children) then
        Driver.patch_ascend base ip (find_asc ip))
    ochildren;
  Array.iter
    (fun ip ->
      if (not (Array.mem ip ochildren)) || not same_parents then
        Driver.patch_ascend base ip (find_asc ip))
    ndes.children;
  let ol =
    Array.fold_right (fun x acc -> x :: acc) owitnesses (fwitnesses_of ofevents)
  in
  let nl =
    Array.fold_right
      (fun x acc -> x :: acc)
      nfam.witnesses
      (fwitnesses_of nfam.fevents)
  in
  let pi = Adef.father ncpl in
  Update.update_related_pointers base pi ol nl;
  (fi, nfam, ncpl, ndes)

let effective_add conf base nsck sfam scpl sdes =
  let fi =
    Driver.insert_family_with_couple_and_descendants base
      (Driver.no_family Driver.Ifam.dummy)
      Driver.no_couple Driver.no_descend
  in
  let origin_file _nfam ncpl ndes = infer_origin_file conf base fi ncpl ndes in
  let nfath_p, nmoth_p, nfam, ncpl, ndes =
    aux_effective_mod conf base nsck sfam scpl sdes fi origin_file
  in
  let nfath_u =
    { family = Array.append (Driver.get_family nfath_p) [| fi |] }
  in
  let nmoth_u =
    { family = Array.append (Driver.get_family nmoth_p) [| fi |] }
  in
  Driver.patch_union base (Adef.father ncpl) nfath_u;
  Driver.patch_union base (Adef.mother ncpl) nmoth_u;
  Array.iter
    (fun ip ->
      let p = Driver.poi base ip in
      match Driver.get_parents p with
      | Some _ -> print_err_parents conf base p
      | None ->
          let a = { parents = Some fi; consang = Adef.fix (-1) } in
          Driver.patch_ascend base (Driver.get_iper p) a)
    ndes.children;
  let nl_witnesses = Array.to_list nfam.witnesses in
  let nl_fevents = fwitnesses_of nfam.fevents in
  let nl = List.append nl_witnesses nl_fevents in
  Update.update_related_pointers base (Adef.father ncpl) [] nl;
  (fi, nfam, ncpl, ndes)

let effective_inv conf base ip u ifam =
  let rec loop = function
    | ifam1 :: ifam2 :: ifaml ->
        if ifam2 = ifam then ifam2 :: ifam1 :: ifaml
        else ifam1 :: loop (ifam2 :: ifaml)
    | _ ->
        Hutil.incorrect_request conf;
        raise
        @@ Update.ModErr
             (Update.UERR (__FILE__ ^ " " ^ string_of_int __LINE__ |> Adef.safe))
  in
  let u =
    { family = Array.of_list (loop (Array.to_list (Driver.get_family u))) }
  in
  Driver.patch_union base ip u

(* ************************************************************************ *)
(*  [Fonc] effective_chg_order : base -> iper -> person -> ifam -> int -> unit        *)

(* ************************************************************************ *)

(** [Description] : Modifie l'ordre de la famille en positionnant la famille
    ifam à la position n. Exemple : [f1 f2 f3 f4] f1 3 => [f2 f3 f1 f4]. [Args]
    :
    - base : base de donnée
    - ip : iper
    - u : person
    - ifam : famille à changer de place
    - n : nouvelle position de la famille [Retour] : Néant [Rem] : Non exporté
      en clair hors de ce module. *)
let effective_chg_order base ip u ifam n =
  let fam = UpdateFam.change_order u ifam n in
  let u = { family = Array.of_list fam } in
  Driver.patch_union base ip u

let effective_del conf base ip fam =
  let ifam = Driver.get_ifam fam in
  Driver.delete_family base ifam;
  let changed =
    let gen_p =
      let p =
        if ip = Driver.get_mother fam then
          Driver.poi base (Driver.get_mother fam)
        else Driver.poi base (Driver.get_father fam)
      in
      Util.string_gen_person base (Driver.gen_person_of_person p)
    in
    let gen_fam =
      Util.string_gen_family base (Driver.gen_family_of_family fam)
    in
    U_Delete_family (gen_p, gen_fam)
  in
  History.record conf base changed "df"

let is_a_link = function _, _, _, Update.Link, _ -> true | _ -> false

let is_created_or_already_there ochil_arr nchil schil =
  (not (is_a_link schil)) || Array.mem nchil ochil_arr

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

let need_check_noloop (scpl, sdes, onfs) =
  if
    Array.exists is_a_link (Gutil.parent_array scpl)
    || Array.exists is_a_link sdes.children
  then
    match onfs with
    | Some ((opar, ochil), (npar, nchil)) ->
        (not
           (Mutil.array_forall2
              (is_created_or_already_there opar)
              npar (Gutil.parent_array scpl)))
        || not
             (Mutil.array_forall2
                (is_created_or_already_there ochil)
                nchil sdes.children)
    | None -> true
  else false

let all_checks_family conf base ifam gen_fam cpl des scdo =
  let wl = ref [] in
  let ml = ref [] in
  let error = Update.def_error conf base in
  let warning w = wl := w :: !wl in
  let misc m = ml := m :: !ml in
  if need_check_noloop scdo then
    Consang.check_noloop_for_person_list base error
      (Array.to_list (Adef.parent_array cpl));
  let fam = Driver.family_of_gen_family base (gen_fam, cpl, des) in
  CheckItem.family base warning ifam fam;
  CheckItem.check_other_fields base misc ifam fam;
  let wl, ml = (List.sort_uniq compare !wl, List.sort_uniq compare !ml) in
  List.iter
    (function
      | ChangedOrderOfMarriages (p, _, after) ->
          Driver.patch_union base (Driver.get_iper p) { family = after }
      | ChangedOrderOfFamilyEvents (ifam, _, after) ->
          Driver.patch_family base ifam { gen_fam with fevents = after }
      | _ -> ())
    wl;
  (wl, ml)

let print_family conf base (wl, ml) cpl des =
  let rdsrc =
    match p_getenv conf.env "rdsrc" with
    | Some "on" -> p_getenv conf.env "src"
    | Some _ | None -> p_getenv conf.env "dsrc"
  in
  (match rdsrc with
  | Some x ->
      conf.henv <- List.remove_assoc "dsrc" conf.henv;
      if x <> "" then conf.henv <- ("dsrc", Mutil.encode x) :: conf.henv
  | None -> ());
  Output.print_sstring conf "<ul>\n";
  Output.print_sstring conf "<li>";
  Output.print_string conf
    (referenced_person_text conf base (Driver.poi base (Adef.father cpl)));
  Output.print_sstring conf "</li>";
  Output.print_sstring conf "\n";
  Output.print_sstring conf "<li>";
  Output.print_string conf
    (referenced_person_text conf base (Driver.poi base (Adef.mother cpl)));
  Output.print_sstring conf "</li>";
  Output.print_sstring conf "</ul>\n";
  if des.children <> [||] then (
    Output.print_sstring conf "<ul>\n";
    Array.iter
      (fun ip ->
        Output.print_sstring conf "<li>";
        Output.print_string conf
          (referenced_person_text conf base (Driver.poi base ip));
        Output.print_sstring conf "</li>")
      des.children;
    Output.print_sstring conf "</ul>\n");
  Update.print_warnings_and_miscs conf base wl ml

let print_title conf fmt _ =
  Output.print_sstring conf (Utf8.capitalize_fst (transl conf fmt))

let print_mod_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family modified";
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.print_sstring conf "<h3 class=\"error\">";
    Output.printf conf
      (fcapitale (ftransl conf "%s forbidden char"))
      (List.fold_left
         (fun acc c -> acc ^ "'" ^ Char.escaped c ^ "' ")
         " " Name.forbidden_char);
    Output.print_sstring conf "</h3>\n";
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_change_event_order_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family modified";
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_add_ok conf base (wl, ml) cpl des =
  Hutil.header conf @@ print_title conf "family added";
  (* Si on a supprimé des caractères interdits *)
  if List.length !removed_string > 0 then (
    Output.printf conf "<h2 class=\"error\">%s</h2>\n"
      (Utf8.capitalize_fst (transl conf "forbidden char"));
    List.iter (Output.printf conf "<p>%s</p>") !removed_string);
  print_family conf base (wl, ml) cpl des;
  Hutil.trailer conf

let print_del_ok conf base wl =
  Hutil.header conf @@ print_title conf "family deleted";
  (match p_getenv conf.env "ip" with
  | Some i ->
      let p = Driver.poi base (Driver.Iper.of_string i) in
      Output.print_sstring conf "<ul><li>";
      Output.print_string conf
        (reference conf base p (gen_person_text conf base p));
      Output.print_sstring conf "\n</ul>"
  | None -> ());
  Update.print_warnings conf base wl;
  Hutil.trailer conf

let print_del conf base =
  match p_getenv conf.env "i" with
  | Some i ->
      let ifam = Driver.Ifam.of_string i in
      let fam = Driver.foi base ifam in
      let ip =
        match p_getenv conf.env "ip" with
        | Some i when Driver.get_mother fam = Driver.Iper.of_string i ->
            Driver.get_mother fam
        | Some _ | None -> Driver.get_father fam
      in
      effective_del conf base ip fam;
      Util.commit_patches conf base;
      print_del_ok conf base []
  | None -> Hutil.incorrect_request conf

let print_inv_ok conf base p =
  Hutil.header conf @@ print_title conf "inversion done";
  Output.print_sstring conf "\n";
  Output.print_string conf (referenced_person_text conf base p);
  Output.print_sstring conf "\n";
  Hutil.trailer conf

let get_create (_, _, _, create, _) = create

let forbidden_disconnected conf scpl sdes =
  let no_dec =
    try List.assoc "propose_add_family" conf.base_env = "no"
    with Not_found -> false
  in
  if no_dec then
    if
      get_create (Gutil.father scpl) = Update.Link
      || get_create (Gutil.mother scpl) = Update.Link
    then false
    else Array.for_all (fun p -> get_create p <> Update.Link) sdes.children
  else false

let print_add o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let conf = Update.update_conf o_conf in
  let nsck = p_getenv conf.env "nsck" = Some "on" in
  let sfam, scpl, sdes, ext = reconstitute_family conf base nsck in
  let redisp = Option.is_some (p_getenv conf.env "return") in
  let digest =
    match p_getenv conf.env "ip" with
    | Some ip ->
        string_of_int
          (Array.length
             (Driver.get_family (Driver.poi base (Driver.Iper.of_string ip))))
    | None -> ""
  in
  let sdigest = get conf "digest" in
  if digest <> "" && sdigest <> "" && digest <> sdigest then
    Update.error_digest conf
  else if ext || redisp then
    UpdateFam.print_update_fam conf base (sfam, scpl, sdes) ""
  else if forbidden_disconnected conf scpl sdes then
    print_error_disconnected conf
  else
    match check_family conf sfam scpl with
    | Some err, _ | _, Some err -> error_family conf err
    | None, None ->
        let sfam, sdes = strip_family sfam sdes in
        let nsck = p_getenv conf.env "nsck" = Some "on" in
        let ifam, fam, cpl, des = effective_add conf base nsck sfam scpl sdes in
        let () = patch_parent_with_pevents base cpl in
        let () = patch_children_with_pevents base des in
        let wl, ml =
          all_checks_family conf base ifam fam cpl des (scpl, sdes, None)
        in
        let changed, act =
          let fam = Util.string_gen_family base fam in
          let ip, act =
            match p_getenv conf.env "ip" with
            | Some i -> (
                let i = Driver.Iper.of_string i in
                if Adef.mother cpl = i then (Adef.mother cpl, "af")
                else
                  let a = Driver.poi base i in
                  match Driver.get_parents a with
                  | Some x when x = ifam -> (i, "aa")
                  | _ -> (Adef.father cpl, "af"))
            | None -> (Adef.father cpl, "af")
          in
          match act with
          | "af" ->
              let gen_p =
                Util.string_gen_person base
                  (Driver.gen_person_of_person (Driver.poi base ip))
              in
              (U_Add_family (gen_p, fam), "af")
          | _ ->
              let gen_p =
                Util.string_gen_person base
                  (Driver.gen_person_of_person (Driver.poi base ip))
              in
              (U_Add_parent (gen_p, fam), "aa")
        in
        Util.commit_patches conf base;
        History.record conf base changed act;
        Update.delete_topological_sort conf base;
        print_add_ok conf base (wl, ml) cpl des

(* If we only have two linked parents,
   with one linked child and not other informations,
   and if a union already exists between the parents,
   edit the existing union in order to add a child.
   Else, create a new union. *)
let print_add_parents o_conf base =
  let conf = Update.update_conf o_conf in
  let nsck = p_getenv conf.env "nsck" = Some "on" in
  let sfam, scpl, sdes, _ = reconstitute_family conf base nsck in
  if
    sfam.marriage = Date.cdate_None
    && sfam.marriage_place = "" && sfam.marriage_note = ""
    && sfam.marriage_src = "" && sfam.witnesses = [||]
    && sfam.relation = Married && sfam.divorce = NotDivorced
    && sfam.fevents
       = [
           {
             efam_name = Efam_Marriage;
             efam_date = Date.cdate_None;
             efam_place = "";
             efam_reason = "";
             efam_note = "";
             efam_src = "";
             efam_witnesses = [||];
           };
         ]
    && sfam.comment = "" && sfam.origin_file = ""
    && sfam.fsources = Option.value ~default:"" (p_getenv conf.env "dsrc")
    && sfam.fam_index = Driver.Ifam.dummy
  then
    match (Adef.father scpl, Adef.mother scpl, sdes.children) with
    | ( (ff, fs, fo, Update.Link, _),
        (mf, ms, mo, Update.Link, _),
        [| (cf, cs, co, Update.Link, _) |] ) -> (
        match
          ( Driver.person_of_key base ff fs fo,
            Driver.person_of_key base mf ms mo,
            Driver.person_of_key base cf cs co )
        with
        | Some fath, Some moth, Some child ->
            let ffam = Driver.get_family @@ Driver.poi base fath in
            let mfam = Driver.get_family @@ Driver.poi base moth in
            let rec loop i =
              if i = -1 then print_add o_conf base
              else
                let ifam = Array.unsafe_get ffam i in
                if Array.exists (( = ) ifam) mfam then (
                  let f = Driver.foi base ifam in
                  let sfam = Driver.gen_family_of_family f in
                  let o_f = Util.string_gen_family base sfam in
                  let scpl = Driver.gen_couple_of_family f in
                  let sdes =
                    {
                      children =
                        Array.append (Driver.gen_descend_of_family f).children
                          [| child |];
                    }
                  in
                  Driver.patch_descend base ifam sdes;
                  Driver.patch_ascend base child
                    { parents = Some ifam; consang = Adef.fix (-1) };
                  Util.commit_patches conf base;
                  let f' =
                    Driver.family_of_gen_family base (sfam, scpl, sdes)
                  in
                  let wl = ref [] in
                  let warning w = wl := w :: !wl in
                  CheckItem.family ~onchange:true base warning ifam f';
                  let n_f = Util.string_gen_family base sfam in
                  let hr =
                    U_Modify_family
                      ( Driver.poi base child |> Driver.gen_person_of_person
                        |> Util.string_gen_person base,
                        o_f,
                        n_f )
                  in
                  History.record conf base hr "mf";
                  Update.delete_topological_sort conf base;
                  print_mod_ok conf base (!wl, []) scpl sdes)
                else loop (i - 1)
            in
            loop (Array.length ffam)
        | _ -> print_add o_conf base)
    | _ -> print_add o_conf base
  else print_add o_conf base

let print_mod_aux conf base callback =
  let nsck = p_getenv conf.env "nsck" = Some "on" in
  let sfam, scpl, sdes, ext = reconstitute_family conf base nsck in
  let redisp = Option.is_some (p_getenv conf.env "return") in
  let digest =
    let ini_sfam = UpdateFam.string_family_of conf base sfam.fam_index in
    let salt = Option.get conf.secret_salt in
    Update.digest_family ~salt ini_sfam
  in
  if digest = get conf "digest" then
    if ext || redisp then
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
    else
      match check_family conf sfam scpl with
      | Some err, _ | _, Some err -> error_family conf err
      | None, None ->
          let sfam, sdes = strip_family sfam sdes in
          callback sfam scpl sdes
  else Update.error_digest conf

let family_structure base ifam =
  let fam = Driver.foi base ifam in
  (Driver.get_parent_array fam, Driver.get_children fam)

let print_mod o_conf base =
  (* Attention ! On pense à remettre les compteurs à *)
  (* zéro pour la détection des caractères interdits *)
  let () = removed_string := [] in
  let o_f =
    let ifam =
      match p_getenv o_conf.env "i" with
      | Some i -> Driver.Ifam.of_string i
      | None -> Driver.Ifam.dummy
    in
    Util.string_gen_family base
      (Driver.gen_family_of_family (Driver.foi base ifam))
  in
  let conf = Update.update_conf o_conf in
  let callback sfam scpl sdes =
    let ofs = family_structure base sfam.fam_index in
    let nsck = p_getenv conf.env "nsck" = Some "on" in
    let ifam, fam, cpl, des = effective_mod conf base nsck sfam scpl sdes in
    let () = patch_parent_with_pevents base cpl in
    let () = patch_children_with_pevents base des in
    Notes.update_notes_links_family base fam;
    (* TODO update_cache_linked_pages *)
    let nfs = (Adef.parent_array cpl, des.children) in
    let onfs = Some (ofs, nfs) in
    let wl, ml =
      all_checks_family conf base ifam fam cpl des (scpl, sdes, onfs)
    in
    Util.commit_patches conf base;
    let changed =
      let ip =
        match p_getenv o_conf.env "ip" with
        | Some i -> Driver.Iper.of_string i
        | None -> Driver.Iper.dummy
      in
      let p =
        Util.string_gen_person base
          (Driver.gen_person_of_person (Driver.poi base ip))
      in
      let n_f = Util.string_gen_family base fam in
      U_Modify_family (p, o_f, n_f)
    in
    History.record conf base changed "mf";
    Update.delete_topological_sort conf base;
    print_mod_ok conf base (wl, ml) cpl des
  in
  print_mod_aux conf base callback

let print_inv conf base =
  match (p_getenv conf.env "i", p_getenv conf.env "f") with
  | Some ip, Some ifam ->
      let ip = Driver.Iper.of_string ip in
      let ifam = Driver.Ifam.of_string ifam in
      let p = Driver.poi base ip in
      effective_inv conf base (Driver.get_iper p) p ifam;
      Util.commit_patches conf base;
      let changed =
        let gen_p =
          Util.string_gen_person base (Driver.gen_person_of_person p)
        in
        U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if";
      print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_order_ok conf base =
  match
    (p_getenv conf.env "i", p_getenv conf.env "f", p_getint conf.env "n")
  with
  | Some ip, Some ifam, Some n ->
      let ip = Driver.Iper.of_string ip in
      let ifam = Driver.Ifam.of_string ifam in
      let p = Driver.poi base ip in
      effective_chg_order base (Driver.get_iper p) p ifam n;
      Util.commit_patches conf base;
      let changed =
        let gen_p =
          Util.string_gen_person base (Driver.gen_person_of_person p)
        in
        U_Invert_family (gen_p, ifam)
      in
      History.record conf base changed "if";
      print_inv_ok conf base p
  | _ -> Hutil.incorrect_request conf

let print_change_event_order conf base =
  match p_getenv conf.env "i" with
  | None -> Hutil.incorrect_request conf
  | Some s ->
      let ifam = Driver.Ifam.of_string s in
      let fam = Driver.foi base ifam in
      let o_f = Util.string_gen_family base (Driver.gen_family_of_family fam) in
      (* TODO_EVENT use Event.sorted_event *)
      let ht = Hashtbl.create 50 in
      let () =
        ignore
        @@ List.fold_left
             (fun id evt ->
               Hashtbl.add ht id evt;
               succ id)
             1 (Driver.get_fevents fam)
      in
      let sorted_fevents =
        List.sort
          (fun (_, pos1) (_, pos2) -> compare pos1 pos2)
          (reconstitute_sorted_events conf 1)
      in
      let fevents =
        List.fold_right
          (fun (id, _) accu ->
            try Hashtbl.find ht id :: accu
            with Not_found -> failwith "Sorting event")
          sorted_fevents []
      in
      let fam = Driver.gen_family_of_family fam in
      let fam = { fam with fevents } in
      let fam = update_family_with_fevents conf base fam in
      Driver.patch_family base fam.fam_index fam;
      let a = Driver.foi base fam.fam_index in
      let cpl = Futil.parent conf.multi_parents (Driver.get_parent_array a) in
      let des = { children = Driver.get_children a } in
      let wl =
        let wl = ref [] in
        let warning w = wl := w :: !wl in
        let nfam = Driver.family_of_gen_family base (fam, cpl, des) in
        CheckItem.family base warning fam.fam_index nfam;
        List.iter
          (function
            | ChangedOrderOfFamilyEvents (ifam, _, after) ->
                Driver.patch_family base ifam { fam with fevents = after }
            | _ -> ())
          !wl;
        List.rev !wl
      in
      Util.commit_patches conf base;
      let changed =
        let ip =
          match p_getenv conf.env "ip" with
          | Some i -> Driver.Iper.of_string i
          | None -> Driver.Iper.dummy
        in
        let p =
          Util.string_gen_person base
            (Driver.gen_person_of_person (Driver.poi base ip))
        in
        let n_f = Util.string_gen_family base fam in
        U_Modify_family (p, o_f, n_f)
      in
      History.record conf base changed "mf";
      print_change_event_order_ok conf base (wl, []) cpl des
