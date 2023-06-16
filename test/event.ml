(* test event order with evt2.gw *)
(* event input order is conserved at gw import/export;
   but nothing in gedcom specify this order! *)

open Geneweb
open Event

(* todo test_util module *)
let gw_import filename =
  let base_filename = Filename.concat Filename.current_dir_name "royal_gw" in
  let state = Gwc_lib.State.default in
  (* set file to import *)
  let files = [ (filename, false, "merge", 0) ] in
  let state = { state with files; force = true; out_file = base_filename } in
  Gwc_lib.make_base ~save_mem:false state;
  Gwdb.open_base base_filename

let get_person base fn sn =
  match Gwdb.person_of_key base fn sn 0 with
  | None -> failwith (Format.sprintf {|person "%s %s" not found|} fn sn)
  | Some iper -> Gwdb.poi base iper

let print_event_names base events =
  let pp fmt s = Format.fprintf fmt "%s" s in
  List.iter
    (fun e ->
      let name =
        match e with
        | Event.Pevent e ->
            let e = Futil.map_epers (Gwdb.sou base) e in
            Def_show.show_gen_pers_event_name pp e
        | Fevent e ->
            let e = Futil.map_efam (Gwdb.sou base) e in
            Def_show.show_gen_fam_event_name pp e
      in
      Format.eprintf "%s@." name)
    events

let good_order =
  [
    Pevent Epers_Birth;
    Pevent Epers_Residence;
    Pevent Epers_Baptism;
    Pevent Epers_Graduate;
    Pevent Epers_Hospitalisation;
    Pevent Epers_MobilisationMilitaire;
    Pevent Epers_Illness;
    Pevent Epers_Election;
    Pevent Epers_Emigration;
    Pevent Epers_Dotation;
    Fevent Efam_MarriageContract;
    Fevent Efam_Marriage;
    Pevent Epers_Excommunication;
    Pevent Epers_Death;
  ]

let base = gw_import (Filename.concat "assets" "evt2.gw")

let events =
  let conf = { Config.empty with wizard = true } in
  let p = get_person base "a" "A" in
  Event.sorted_events conf base p |> List.map get_name

let () =
  assert (List.length events = 14);
  if events <> good_order then (
    Format.eprintf "bad order: @.";
    print_event_names base events;
    assert false)
