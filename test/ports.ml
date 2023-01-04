(* tests for import/export of .ged and .gw *)

let original_ged = Filename.concat "assets" "royal92.ged"
let exported_ged = Filename.concat "assets" "royal92_exported.ged"
let exported_gw = Filename.concat "assets" "royal92_exported.gw"

let ged_import filename =
  let opts, state = Ged2gwb_lib.make_opts_and_state () in
  let opts = { opts with in_file = filename } in
  Ged2gwb_lib.make_base opts state

let ged_export base outfile =
  let opts = Gwexport.default_opts in
  let oc =
    let oc = open_out outfile in
    (outfile, output_string oc, fun () -> close_out oc)
  in
  let opts = { opts with base = Some (outfile, base); oc } in
  let select = Gwexport.select opts [] in
  Gwb2ged_lib.gwb2ged false opts select

let gw_export base outfile =
  let opts = Gwexport.default_opts in
  let oc = open_out outfile in
  let opts =
    {
      opts with
      base = Some (Gwdb.bname base, base);
      oc = (outfile, output_string oc, fun () -> close_out oc);
    }
  in
  Gwu_lib.gwu_simple ~export_isolated:true opts

let gw_import filename =
  let base_filename = Filename.concat Filename.current_dir_name "royal_gw" in
  let state = Gwc_lib.State.default in
  (* set file to import *)
  let files = [ (filename, false, "merge", 0) ] in
  let state = { state with files; force = true; out_file = base_filename } in
  Gwc_lib.make_base state;
  Gwdb.open_base base_filename

(* import ged *)
let base_1 = ged_import original_ged
let nb_real_persons_1 = Gwdb.nb_of_real_persons base_1
let nb_families_1 = Gwdb.nb_of_families base_1

(* export ged *)
let () = ged_export base_1 exported_ged
let () = Gwdb.close_base base_1

(* re-import and check *)
let base_2 = ged_import exported_ged
let nb_real_persons_2 = Gwdb.nb_of_real_persons base_2
let nb_families_2 = Gwdb.nb_of_families base_2
let () = assert (nb_real_persons_1 = 3010)
let () = assert (nb_families_1 = 1425)
let () = assert (nb_real_persons_1 = nb_real_persons_2)
let () = assert (nb_families_1 = nb_families_2)

(* export to .gw *)
let () = gw_export base_2 exported_gw
let () = Gwdb.close_base base_2

(* import .gw and check *)
let base_3 = gw_import exported_gw
let nb_real_persons_3 = Gwdb.nb_of_real_persons base_3
let nb_families_3 = Gwdb.nb_of_families base_3

(* TODO base_3 has 13 more persons
   let () = assert (nb_real_persons_1 = nb_real_persons_3)
*)
let () = assert (nb_families_1 = nb_families_3)
let () = Gwdb.close_base base_3

(* -- test abc.ged -- *)

(* test on a base made from a ged import -> gw export -> gw import *)
let base =
  let base = ged_import (Filename.concat "assets" "abc.ged") in
  gw_export base "abc.gw";
  gw_import "abc.gw"

let () = assert (Gwdb.nb_of_persons base = 3)

let a =
  match Gwdb.person_of_key base "a" "A" 0 with
  | None -> failwith {|person "a A" not found|}
  | Some iper -> Gwdb.poi base iper

(* check birth event *)
let () =
  (match Date.cdate_to_dmy_opt (Gwdb.get_birth a) with
  | None -> failwith {|no birth date for person "a A"|}
  | Some dmy ->
      assert (
        Date.compare_dmy dmy
          { day = 1; month = 1; year = 1990; prec = Def.Sure; delta = 0 }
        = 0));
  assert (
    Gwdb.sou base (Gwdb.get_birth_note a) = "This is a note on a birth event");
  assert (
    Gwdb.sou base (Gwdb.get_birth_src a) = "This is a source on a birth event");
  assert (Gwdb.sou base (Gwdb.get_birth_place a) = "Paris")

(* check family marriage event *)
let () =
  let fam = Gwdb.foi base (Gwdb.get_family a).(0) in
  assert (Gwdb.sou base (Gwdb.get_comment fam) = "This is a comment on a family");
  let fevents = Gwdb.get_fevents fam in
  let marriage =
    match
      List.filter (fun event -> event.Def.efam_name = Def.Efam_Marriage) fevents
    with
    | [] -> failwith "no Efam_Marriage"
    | e :: [] -> e
    | _l -> failwith "duplicate Efam_Marriage"
  in
  (match Date.cdate_to_dmy_opt marriage.efam_date with
  | None -> failwith "no marriage date"
  | Some dmy ->
      assert (
        Date.compare_dmy dmy
          { day = 18; month = 05; year = 2013; prec = Def.Sure; delta = 0 }
        = 0));

  assert (
    Gwdb.sou base marriage.efam_note = "This is a note on a marriage event");
  assert (
    Gwdb.sou base marriage.efam_src = "This is a source on a marriage event");
  assert (Gwdb.sou base marriage.efam_place = "Lyon");
  (* TODO check witness note here *)
  let witness, witness_kind =
    assert (Array.length marriage.efam_witnesses = 1);
    let w, wk = marriage.efam_witnesses.(0) in
    (Gwdb.poi base w, wk)
  in
  assert (Gwdb.sou base (Gwdb.get_first_name witness) = "c");
  assert (Gwdb.sou base (Gwdb.get_surname witness) = "C");
  assert (witness_kind = Def.Witness_Attending)

(* check Degree event *)
let () =
  let pevents = Gwdb.get_pevents a in
  let diploma =
    match
      List.filter
        (fun event -> event.Def.epers_name = Def.Epers_Diploma)
        pevents
    with
    | [] -> failwith "no Epers_Diploma"
    | e :: [] -> e
    | _l -> failwith "duplicate Epers_Diploma"
  in
  assert (Gwdb.sou base diploma.epers_note = "This is a note on a diploma event");
  assert (
    Gwdb.sou base diploma.epers_src = "This is a source on a diploma event");
  (* TODO WNOTE *)
  let _witness, witness_kind =
    let witnesses = diploma.epers_witnesses in
    assert (Array.length witnesses = 1);
    witnesses.(0)
  in
  assert (witness_kind = Def.Witness)
