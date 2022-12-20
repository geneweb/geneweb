(* test for import/export of .ged and .gw *)

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
let nb_persons_1 = Gwdb.nb_of_persons base_1
let nb_families_1 = Gwdb.nb_of_families base_1

(* export ged *)
let () = ged_export base_1 exported_ged
let () = Gwdb.close_base base_1

(* re-import and check *)
let base_2 = ged_import exported_ged
let nb_persons_2 = Gwdb.nb_of_persons base_2
let nb_families_2 = Gwdb.nb_of_families base_2
let () = assert (nb_persons_1 = 3300)
let () = assert (nb_families_1 = 1425)
let () = assert (nb_persons_1 = nb_persons_2)
let () = assert (nb_families_1 = nb_families_2)

(* export to .gw *)
let () = gw_export base_2 exported_gw
let () = Gwdb.close_base base_2

(* import .gw and check *)
let base_3 = gw_import exported_gw
let nb_persons_3 = Gwdb.nb_of_persons base_3
let nb_families_3 = Gwdb.nb_of_families base_3
let () = Gwdb.close_base base_3

(* TODO base_3 has 13 more persons
   let () = assert (nb_persons_1 = nb_persons_3)
*)
let () = assert (nb_families_1 = nb_families_3)
