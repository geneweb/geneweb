(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
open MergeIndOk
module Driver = Geneweb_db.Driver

let print_merge conf base =
  match (p_getenv conf.env "i1", p_getenv conf.env "i2") with
  | Some i1, Some i2 ->
      let p1 = Driver.poi base (Driver.Iper.of_string i1) in
      let p2 = Driver.poi base (Driver.Iper.of_string i2) in
      let p = reconstitute conf base p1 p2 in
      let sp = UpdateInd.string_person_of base p1 in
      let salt = Option.get conf.secret_salt in
      let digest = Update.digest_person ~salt sp in
      UpdateInd.print_update_ind conf base p digest;
      MergeIndOk.merge_carrousel conf base p1 p2 p
  | _ -> Hutil.incorrect_request conf

let print_mod_merge_ok conf base wl p pgl1 ofn1 osn1 oocc1 pgl2 ofn2 osn2 oocc2
    =
  Hutil.header conf (fun _ ->
      transl conf "merge done" |> Utf8.capitalize_fst
      |> Output.print_sstring conf);
  Output.print_sstring conf " ";
  Output.print_string conf
    (referenced_person_text conf base (Driver.poi base p.key_index));
  Output.print_sstring conf " ";
  Update.print_warnings conf base wl;
  let pi = p.key_index in
  let np = Driver.poi base pi in
  let nfn = Driver.p_first_name base np in
  let nsn = Driver.p_surname base np in
  let nocc = Driver.get_occ np in
  if
    ((ofn1 <> nfn || osn1 <> nsn || oocc1 <> nocc) && pgl1 <> [])
    || ((ofn2 <> nfn || osn2 <> nsn || oocc2 <> nocc) && pgl2 <> [])
  then (
    Output.print_sstring conf
      {|<div class="alert alert-danger mx-auto mt-1" role="alert">|};
    Output.printf conf (ftransl conf "name changed. updated linked pages");
    Output.print_sstring conf "</div>";
    let aux n txt ofn osn oocc =
      Output.print_sstring conf {|<span class="unselectable float-left">|};
      transl conf txt |> Utf8.capitalize_fst |> Output.print_sstring conf;
      if n = "" then (
        Output.print_sstring conf " ";
        Output.print_sstring conf n);
      Output.print_sstring conf (transl conf ":");
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
        (Utf8.capitalize_fst (transl conf "linked pages"));
      Output.print_sstring conf (transl conf ":");
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

let print_mod_merge o_conf base =
  let get_gen_person i =
    match p_getenv o_conf.env i with
    | Some i ->
        Util.string_gen_person base
          (Driver.gen_person_of_person
             (Driver.poi base (Driver.Iper.of_string i)))
    | None -> assert false
  in
  let o_p1 = get_gen_person "i" in
  let o_p2 = get_gen_person "i2" in
  let conf = Update.update_conf o_conf in
  UpdateIndOk.print_mod_aux conf base (fun p ->
      effective_mod_merge conf base o_p1 o_p2 p print_mod_merge_ok)
