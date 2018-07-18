(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Def
open Gwdb
open Printf


let designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)

let kill_parents base ip =
  let a = {parents = None; consang = Adef.fix (-1)} in patch_ascend base ip a

let check_cpl base nb_ind nb_fam =
  if nb_fam > 0 then
    let ifam = Adef.ifam_of_int 0 in
    let fam = foi base ifam in
    if not (is_deleted_family fam) then
      let neg_cpl =
        List.exists (fun ip -> ip = Adef.iper_of_int (-1))
          (Array.to_list (get_parent_array fam))
      in
      if neg_cpl then
        begin
          Array.iter (kill_parents base) (get_children fam);
          delete_family base ifam;
          Gwdb.commit_patches base
        end

let check bname =
  let base = Gwdb.open_base bname in
  (*
  let nb_fam = nb_of_families base in
  if nb_fam > 0 then
    let ifam = Adef.ifam_of_int 0 in
    let fam = foi base ifam in
    if not (is_deleted_family fam) then
      let neg_cpl =
        List.exists
          (fun ip -> ip = Adef.iper_of_int (-1))
          (Array.to_list (get_parent_array fam))
      in
      if neg_cpl then do {
        eprintf "%s\n" bname;
        flush stderr;
      }
      else ()
    else ()
  else ()
  *)
  let nb_fam = nb_of_families base in
  let nb_ind = nb_of_persons base in check_cpl base nb_ind nb_fam


(**/**)

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false (fun () -> check !bname)
    ~onerror:(fun () ->
        eprintf "Cannot lock database. Try again.\n";
        flush stderr)

let _ = main ()
