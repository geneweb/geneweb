(* $Id: mergeFamOk.ml,v 5.19 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver

let cat_strings base is1 sep is2 =
  let n1 = Driver.sou base is1 in
  let n2 = Driver.sou base is2 in
  if n1 = "" then n2 else if n2 = "" then n1 else n1 ^ sep ^ n2

let merge_strings base is1 sep is2 =
  if Driver.Istr.equal is1 is2 then Driver.sou base is1
  else cat_strings base is1 sep is2

let sorp base ip =
  let p = Driver.poi base ip in
  ( Driver.sou base (Driver.get_first_name p),
    Driver.sou base (Driver.get_surname p),
    Driver.get_occ p,
    Update.Link,
    "" )

(* TODO: O(n^2) *)
let merge_witnesses base wit1 wit2 =
  Array.of_list
  @@ Array.fold_right
       (fun wit list -> if List.mem wit list then list else wit :: list)
       (Array.map (sorp base) wit1)
       (List.map (sorp base) (Array.to_list wit2))

(* TODO: O(n^2) *)
let merge_event_witnesses wit1 wit2 =
  Array.of_list
  @@ Array.fold_right
       (fun wit list -> if List.mem wit list then list else wit :: list)
       wit1 (Array.to_list wit2)

(* ********************************************************************** *)
(*  [Fonc] merge_events : config -> list -> list -> list                  *)

(* ********************************************************************** *)

(** [Description] : Essaye de merger le plus possible d'evenement famille a
    partir des deux listes d'evenements famille. [Args] :
    - l1 : fevents de fam1
    - l2 : fevents de fam2 [Retour] : la fusion des fevents [Rem] : Non exporté
      en clair hors de ce module. *)
let merge_events conf l1 l2 =
  let merge_strings s1 sep s2 =
    if s1 = s2 then s1
    else if s1 = "" then s2
    else if s2 = "" then s1
    else s1 ^ sep ^ s2
  in
  let field x1 x2 null = if null x1 then x2 else x1 in
  let need_selection x1 x2 = x1 <> "" && x2 <> "" && x1 <> x2 in
  let string_event_date e =
    match Date.od_of_cdate e.efam_date with
    | None -> Adef.safe ""
    | Some d -> DateDisplay.string_of_ondate conf d
  in
  let can_merge_event e1 e2 =
    not
      (need_selection
         (string_event_date e1 :> string)
         (string_event_date e2 :> string)
      || need_selection e1.efam_place e2.efam_place
      || need_selection e1.efam_note e2.efam_note
      || need_selection e1.efam_src e2.efam_src)
  in
  let list_mem e l =
    let found_marriage = ref false in
    let found_divorce = ref false in
    match e.efam_name with
    | Efam_Marriage | Efam_NoMarriage | Efam_NoMention | Efam_Engage
    | Efam_Divorce | Efam_Separated ->
        List.fold_right
          (fun e1 (mem, l1) ->
            if e1.efam_name = e.efam_name then
              match e1.efam_name with
              | Efam_Marriage | Efam_NoMarriage | Efam_NoMention ->
                  if !found_marriage then (mem, e1 :: l1)
                  else if e.efam_name = e1.efam_name && can_merge_event e e1
                  then
                    let date =
                      field e.efam_date e1.efam_date (( = ) Date.cdate_None)
                    in
                    let place = field e.efam_place e1.efam_place (( = ) "") in
                    let note =
                      merge_strings e.efam_note "<br>\n" e1.efam_note
                    in
                    let src = merge_strings e.efam_src ", " e1.efam_src in
                    let witnesses =
                      merge_event_witnesses e1.efam_witnesses e.efam_witnesses
                    in
                    let e1 =
                      {
                        e1 with
                        efam_date = date;
                        efam_place = place;
                        efam_note = note;
                        efam_src = src;
                        efam_witnesses = witnesses;
                      }
                    in
                    let _ = found_marriage := true in
                    (true, e1 :: l1)
                  else (mem, e1 :: l1)
              | Efam_Annulation | Efam_Divorce | Efam_Separated ->
                  if !found_divorce then (mem, e1 :: l1)
                  else if e.efam_name = e1.efam_name && can_merge_event e e1
                  then
                    let date =
                      field e.efam_date e1.efam_date (( = ) Date.cdate_None)
                    in
                    let place = field e.efam_place e1.efam_place (( = ) "") in
                    let note =
                      merge_strings e.efam_note "<br>\n" e1.efam_note
                    in
                    let src = merge_strings e.efam_src ", " e1.efam_src in
                    let witnesses =
                      merge_event_witnesses e1.efam_witnesses e.efam_witnesses
                    in
                    let e1 =
                      {
                        e1 with
                        efam_date = date;
                        efam_place = place;
                        efam_note = note;
                        efam_src = src;
                        efam_witnesses = witnesses;
                      }
                    in
                    let _ = found_marriage := true in
                    (true, e1 :: l1)
                  else (mem, e1 :: l1)
              | _ -> (mem, e1 :: l1)
            else (mem, e1 :: l1))
          l (false, [])
    | _ -> (false, l)
  in
  let rec merge_events_aux l1 l2 =
    match l2 with
    | [] -> l1
    | e2 :: l2 ->
        let mem, l1 = list_mem e2 l1 in
        if mem then merge_events_aux l1 l2
        else merge_events_aux (l1 @ [ e2 ]) l2
  in
  merge_events_aux l1 l2

let reconstitute conf base ifam1 fam1 fam2 =
  let field name proj null =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    match p_getenv conf.env name with
    | Some "1" -> x1
    | Some "2" -> x2
    | _ -> if null x1 then x2 else x1
  in
  let merge_possible_event conv proj =
    let l1 = List.map conv (proj fam1) in
    let l2 = List.map conv (proj fam2) in
    merge_events conf l1 l2
  in
  let fam =
    {
      marriage = field "marriage" Driver.get_marriage (( = ) Date.cdate_None);
      marriage_place =
        field "marriage_place"
          (fun f -> Driver.sou base (Driver.get_marriage_place f))
          (( = ) "");
      marriage_note =
        merge_strings base
          (Driver.get_marriage_note fam1)
          "<br>\n"
          (Driver.get_marriage_note fam2);
      marriage_src =
        merge_strings base
          (Driver.get_marriage_src fam1)
          ", "
          (Driver.get_marriage_src fam2);
      witnesses =
        merge_witnesses base
          (Driver.get_witnesses fam1)
          (Driver.get_witnesses fam2);
      relation = field "relation" Driver.get_relation (( = ) Married);
      divorce = field "divorce" Driver.get_divorce (( = ) NotDivorced);
      fevents =
        merge_possible_event
          (Futil.map_fam_event (sorp base) (Driver.sou base))
          Driver.get_fevents;
      comment =
        merge_strings base (Driver.get_comment fam1) ", "
          (Driver.get_comment fam2);
      origin_file = Driver.sou base (Driver.get_origin_file fam1);
      fsources =
        merge_strings base (Driver.get_fsources fam1) ", "
          (Driver.get_fsources fam2);
      fam_index = ifam1;
    }
  in
  let des =
    {
      children =
        Array.map
          (UpdateFam.person_key base)
          (Array.append (Driver.get_children fam1) (Driver.get_children fam2));
    }
  in
  (fam, des)

let print_merge conf base =
  match (p_getenv conf.env "i", p_getenv conf.env "i2") with
  | Some f1, Some f2 ->
      let ifam1 = Driver.Ifam.of_string f1 in
      let fam1 = Driver.foi base ifam1 in
      let fam2 = Driver.foi base (Driver.Ifam.of_string f2) in
      let sfam, sdes = reconstitute conf base ifam1 fam1 fam2 in
      let digest =
        let ini_sfam = UpdateFam.string_family_of conf base ifam1 in
        Update.digest_family ini_sfam
      in
      let scpl =
        Futil.map_couple_p conf.multi_parents
          (UpdateFam.person_key base)
          (Driver.gen_couple_of_family (Driver.foi base sfam.fam_index))
      in
      UpdateFam.print_update_fam conf base (sfam, scpl, sdes) digest
  | _ -> Hutil.incorrect_request conf

let print_mod_merge_ok conf base wl cpl des =
  let title _ =
    Output.print_sstring conf (Utf8.capitalize_fst (transl conf "merge done"))
  in
  Hutil.header conf title;
  UpdateFamOk.print_family conf base wl cpl des;
  MergeDisplay.print_possible_continue_merging conf base;
  Hutil.trailer conf

let effective_mod_merge conf base o_f1 o_f2 sfam scpl sdes =
  match p_getenv conf.env "i2" with
  | None -> Hutil.incorrect_request conf
  | Some i2 ->
      let ifam2 = Driver.Ifam.of_string i2 in
      UpdateFamOk.effective_del conf base Driver.Iper.dummy
        (Driver.foi base ifam2);
      let ifam, fam, cpl, des =
        UpdateFamOk.effective_mod conf base true sfam scpl sdes
      in
      let wl =
        UpdateFamOk.all_checks_family conf base ifam fam cpl des
          (scpl, sdes, None)
      in
      Util.commit_patches conf base;
      let s =
        let sl =
          [ fam.comment; fam.fsources; fam.marriage_note; fam.marriage_src ]
        in
        let sl =
          let rec loop l accu =
            match l with
            | [] -> accu
            | evt :: l -> loop l (evt.efam_note :: evt.efam_src :: accu)
          in
          loop fam.fevents sl
        in
        String.concat " " (List.map (Driver.sou base) sl)
      in
      Notes.update_notes_links_db base (Def.NLDB.PgFam ifam) s;
      (* TODO update_cache_linked_pages *)
      let changed =
        let gen_p =
          let p =
            match p_getenv conf.env "ip" with
            | Some i ->
                let ip = Driver.Iper.of_string i in
                if Adef.mother cpl = ip then Driver.poi base (Adef.mother cpl)
                else Driver.poi base (Adef.father cpl)
            | None -> Driver.poi base (Adef.father cpl)
          in
          Util.string_gen_person base (Driver.gen_person_of_person p)
        in
        let n_f = Util.string_gen_family base fam in
        U_Merge_family (gen_p, o_f1, o_f2, n_f)
      in
      History.record conf base changed "ff";
      print_mod_merge_ok conf base wl cpl des

let print_mod_merge o_conf base =
  let get_gen_family i =
    match p_getenv o_conf.env i with
    | Some i ->
        let fam = Driver.foi base (Driver.Ifam.of_string i) in
        Util.string_gen_family base (Driver.gen_family_of_family fam)
    | None ->
        let fam = Driver.foi base Driver.Ifam.dummy in
        Util.string_gen_family base (Driver.gen_family_of_family fam)
  in
  let o_f1 = get_gen_family "i" in
  let o_f2 = get_gen_family "i2" in
  let conf = Update.update_conf o_conf in
  UpdateFamOk.print_mod_aux conf base (effective_mod_merge conf base o_f1 o_f2)
