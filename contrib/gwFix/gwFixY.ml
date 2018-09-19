(* $Id: gwFixY.ml,v 0.01 2015-04-29 09:48:20 flh Exp $ *)

open Def
open Gwdb

(**/**)

let trace = ref false

let fix_occu_y base =
  let changed = ref false in
  let nb_ind_modified = ref 0 in
  let nb_fam_modified = ref 0 in
  let regexp_one = Str.regexp_string "Y," in
  let regexp_two = Str.regexp_string "Y<br>" in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let updt = ref false in
    let occu = sou base (get_occupation p) in
    let () = if Str.string_match regexp_one occu 0 then updt := true in
    let new_occu = Str.global_replace regexp_one "" occu in
    let new_occu = Util.only_printable new_occu in
    let new_occu = Gwdb.insert_string base new_occu in
    let new_pevents =
      List.map
        (fun evt ->
           let note = sou base evt.epers_note in
           let () =
             if Str.string_match regexp_one note 0 ||
                Str.string_match regexp_two note 0
             then
               updt := true
           in
           let new_note = Str.global_replace regexp_two "" note in
           let new_note = Str.global_replace regexp_one "" new_note in
           let new_note = Util.only_printable new_note in
           let new_note = Gwdb.insert_string base new_note in
           {evt with epers_note = new_note})
        (get_pevents p)
    in
    if !updt then
      begin
        if !trace then
          begin
            Printf.eprintf "Modifiy person : %s\n" (Gutil.designation base p);
            flush stderr
          end;
        let gp =
          {(gen_person_of_person p) with occupation = new_occu;
           pevents = new_pevents}
        in
        patch_person base gp.key_index gp;
        changed := true;
        incr nb_ind_modified
      end
  done;
  for i = 0 to nb_of_families base - 1 do
    let fam = foi base (Adef.ifam_of_int i) in
    let updt = ref false in
    let new_fevents =
      List.map
        (fun evt ->
           let note = sou base evt.efam_note in
           let () =
             if Str.string_match regexp_one note 0 ||
                Str.string_match regexp_two note 0
             then
               updt := true
           in
           let new_note = Str.global_replace regexp_two "" note in
           let new_note = Str.global_replace regexp_one "" new_note in
           let new_note = Util.only_printable new_note in
           let new_note = Gwdb.insert_string base new_note in
           {evt with efam_note = new_note})
        (get_fevents fam)
    in
    if !updt then
      begin
        if !trace then
          begin let fath = poi base (get_father fam) in
            let moth = poi base (get_mother fam) in
            Printf.eprintf "Modifiy family : %s %s\n" (Gutil.designation base fath)
              (Gutil.designation base moth);
            flush stderr
          end;
        let gf = {(gen_family_of_family fam) with fevents = new_fevents} in
        patch_family base gf.fam_index gf;
        changed := true;
        incr nb_fam_modified
      end
  done;
  if !changed then
    begin
      commit_patches base;
      Printf.eprintf "Number of modified persons: %d\n" !nb_ind_modified;
      Printf.eprintf "Number of modified families: %d\n" !nb_fam_modified;
      flush stderr
    end


(**/**)

let bname = ref ""

let speclist = ["-t", Arg.Set trace, "trace modified person"]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false
    (fun () -> let base = Gwdb.open_base !bname in fix_occu_y base)
    ~onerror:(fun () ->
        Printf.eprintf "Cannot lock database. Try again.\n";
        flush stderr)

let _ = main ()
