(* todo cram test for this *)
(* test event order with evt2.gw *)
(* event input order is conserved at gw import/export;
   but nothing in gedcom specify this order! *)

open Alcotest
open Geneweb
open Event

(* todo test_util module *)
let gw_import filename base_filename =
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

let good_order =
  [
    Pevent (Epers_Name "custom 1");
    Pevent Epers_Residence;
    Pevent Epers_MobilisationMilitaire;
    Pevent (Epers_Name "custom 2");
    Pevent Epers_Birth;
    Pevent Epers_Baptism;
    Pevent Epers_Graduate;
    Pevent Epers_Hospitalisation;
    Pevent Epers_Illness;
    Pevent Epers_Election;
    Pevent Epers_Emigration;
    Pevent (Epers_Name "custom 3");
    Pevent Epers_Dotation;
    Fevent Efam_MarriageContract;
    Fevent Efam_Marriage;
    Pevent Epers_Excommunication;
    Pevent (Epers_Name "custom 4");
    Pevent Epers_Death;
    Pevent (Epers_Name "custom 5");
    Pevent (Epers_Name "custom 6");
  ]

let base_filename = Filename.concat Filename.current_dir_name "evt2_gw"

let events =
  let base = gw_import (Filename.concat "assets" "evt2.gw") base_filename in
  let conf = { Config.empty with wizard = true } in
  let p = get_person base "a" "A" in
  let events =
    Event.sorted_events conf base p
    |> List.map get_name
    |> List.map (fun e ->
           match e with
           | Pevent e -> Pevent (Futil.map_epers (Gwdb.sou base) e)
           | Fevent e -> Fevent (Futil.map_efam (Gwdb.sou base) e))
  in
  Gwdb.close_base base;
  events

let fmt_l fmt l =
  let pp fmt s = Format.fprintf fmt "%s" s in
  Format.fprintf fmt "%a"
    (Format.pp_print_list (fun fmt e ->
         match e with
         | Event.Pevent e -> Def_show.pp_gen_pers_event_name pp fmt e
         | Fevent e -> Def_show.pp_gen_fam_event_name pp fmt e))
    l

let testable_events = Alcotest.testable fmt_l ( = )

let test_order () =
  (check int) "lengths" (List.length good_order) (List.length events);
  (check testable_events) "events = good_order" events good_order

let a_warnings, b_warnings =
  let base = Gwdb.open_base base_filename in
  let warnings p =
    let ws = ref [] in
    Geneweb.CheckItem.person base (fun w -> ws := w :: !ws) p;
    !ws
  in
  let a_ws = warnings (get_person base "a" "A") in
  let b_ws = warnings (get_person base "b" "B") in
  Gwdb.close_base base;
  (a_ws, b_ws)

let test_warnings () =
  (check int) "person a warnings length" 1 (List.length a_warnings);
  (check int) "person b warnings length" 0 (List.length b_warnings)

let v =
  [
    ("event-order", [ test_case "Event order" `Quick test_order ]);
    ("event-warning", [ test_case "Event warnings" `Quick test_warnings ]);
  ]
