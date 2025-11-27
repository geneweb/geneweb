(* Copyright (c) 1998-2007 INRIA *)

let print_merge conf base =
  match
    (Util.p_getenv conf.Config.env "i1", Util.p_getenv conf.Config.env "i2")
  with
  | Some i1, Some i2 ->
      let p1 = Gwdb.poi base (Gwdb.iper_of_string i1) in
      let p2 = Gwdb.poi base (Gwdb.iper_of_string i2) in
      let p = MergeIndOk.reconstitute conf base p1 p2 in
      let sp = UpdateInd.string_person_of base p1 in
      let digest = Update.digest_person sp in
      UpdateInd.print_update_ind conf base p digest
  | _ -> Hutil.incorrect_request conf

let print_mod_merge_ok conf base wl p pgl1 ofn1 osn1 oocc1 pgl2 ofn2 osn2 oocc2
    =
  Hutil.header conf (fun _ ->
      Util.transl conf "merge done"
      |> Utf8.capitalize_fst |> Output.print_sstring conf);
  Hutil.print_link_to_welcome conf true;
  Output.print_sstring conf " ";
  Output.print_string conf
    (NameDisplay.referenced_person_text conf base
       (Gwdb.poi base p.Def.key_index));
  Output.print_sstring conf " ";
  Update.print_warnings conf base wl;
  let pi = p.Def.key_index in
  let np = Gwdb.poi base pi in
  let nfn = Gwdb.p_first_name base np in
  let nsn = Gwdb.p_surname base np in
  let nocc = Gwdb.get_occ np in
  if
    ((ofn1 <> nfn || osn1 <> nsn || oocc1 <> nocc) && pgl1 <> [])
    || ((ofn2 <> nfn || osn2 <> nsn || oocc2 <> nocc) && pgl2 <> [])
  then (
    Output.print_sstring conf
      {|<div class="alert alert-danger mx-auto mt-1" role="alert">|};
    Output.printf conf (Util.ftransl conf "name changed. update linked pages");
    Output.print_sstring conf "</div>";
    let aux n txt ofn osn oocc =
      Output.print_sstring conf {|<span class="unselectable float-left">|};
      Util.transl conf txt |> Utf8.capitalize_fst |> Output.print_sstring conf;
      if n = "" then (
        Output.print_sstring conf " ";
        Output.print_sstring conf n);
      Output.print_sstring conf (Util.transl conf ":");
      Output.print_sstring conf {|</span> <span class="float-left ml-1">|};
      Output.print_string conf (Util.escape_html ofn);
      Output.print_sstring conf {|/|};
      Output.print_string conf (Util.escape_html osn);
      if oocc <> 0 then (
        Output.print_sstring conf "/";
        Output.print_sstring conf (string_of_int oocc));
      Output.print_sstring conf {|</span>|};
      Output.print_sstring conf "<span>";
      Output.print_sstring conf
        (Utf8.capitalize_fst (Util.transl conf "linked pages"));
      Output.print_sstring conf (Util.transl conf ":");
      Output.print_sstring conf "</span>"
    in
    aux "" "new name" nfn nsn nocc;
    Output.print_sstring conf {|<br>|};
    aux "1" "old name" ofn1 osn1 oocc1;
    NotesDisplay.print_linked_list conf base pgl1;
    aux "2" "old name" ofn2 osn2 oocc2;
    NotesDisplay.print_linked_list conf base pgl2);
  MergeDisplay.print_possible_continue_merging conf base;
  Hutil.trailer conf

(* we check that the new access post merge is one of the two merged persons. *)
let check_person_access_before_merge previous_p1 previous_p2 new_p =
  let illegal1 =
    Update.is_illegal_access_update ~previous_access:previous_p1.Def.access
      ~new_access:new_p.Def.access
  in
  let illegal2 =
    Update.is_illegal_access_update ~previous_access:previous_p2.Def.access
      ~new_access:new_p.Def.access
  in
  if not (illegal1 && illegal2) then None
  else
    Some
      (Update.UERR_illegal_access_update
         (previous_p1.Def.access, new_p.Def.access))

let check_person_before_merge conf base previous_p1 previous_p2 new_p =
  let bind_none x f = match x with Some _ -> x | None -> f () in
  let ( >>= ) = bind_none in
  Update.check_missing_name base new_p >>= fun () ->
  Update.check_person_occurrence_number new_p >>= fun () ->
  Update.check_missing_witnesses_names conf
    (fun e -> e.Def.epers_witnesses)
    new_p.Def.pevents
  >>= fun () -> check_person_access_before_merge previous_p1 previous_p2 new_p

let print_mod_merge o_conf base =
  let get_gen_person i =
    match Util.p_getenv o_conf.Config.env i with
    | Some i ->
        Gwdb.gen_person_of_person (Gwdb.poi base (Gwdb.iper_of_string i))
    | None -> assert false
  in
  let o_p1 = get_gen_person "i" in
  let o_p2 = get_gen_person "i2" in
  let conf = Update.update_conf o_conf in
  let check_person_f conf base =
    check_person_before_merge conf base o_p1 o_p2
  in
  UpdateIndOk.print_mod_aux ~check_person_f conf base (fun p ->
      MergeIndOk.effective_mod_merge conf base o_p1 o_p2 p print_mod_merge_ok)
