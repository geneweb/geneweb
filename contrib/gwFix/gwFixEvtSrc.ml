(* $Id: gw_fix_burial.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Def
open Gwdb

let trace = ref false

let update_database_with_burial base =
  let empty_string = Gwdb.insert_string base "" in
  let base_changed = ref false in
  let nb_modified = ref 0 in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    let evt_birth =
      match Adef.od_of_codate (get_birth p) with
        Some d -> None
      | None ->
          if sou base (get_birth_place p) <> "" then None
          else if sou base (get_birth_src p) = "" then None
          else
            let evt =
              {epers_name = Epers_Birth; epers_date = Adef.codate_None;
               epers_place = empty_string; epers_reason = empty_string;
               epers_note = empty_string; epers_src = get_birth_src p;
               epers_witnesses = [| |]}
            in
            Some evt
    in
    let evt_bapt =
      match Adef.od_of_codate (get_baptism p) with
        Some d -> None
      | None ->
          if sou base (get_baptism_place p) <> "" then None
          else if sou base (get_baptism_src p) = "" then None
          else
            let evt =
              {epers_name = Epers_Baptism; epers_date = Adef.codate_None;
               epers_place = empty_string; epers_reason = empty_string;
               epers_note = empty_string; epers_src = get_baptism_src p;
               epers_witnesses = [| |]}
            in
            Some evt
    in
    let evt_death =
      match get_death p with
        NotDead | DontKnowIfDead ->
          if sou base (get_death_place p) = "" &&
             sou base (get_death_src p) = ""
          then
            None
          else
            let evt =
              {epers_name = Epers_Death; epers_date = Adef.codate_None;
               epers_place = get_death_place p; epers_reason = empty_string;
               epers_note = empty_string; epers_src = get_death_src p;
               epers_witnesses = [| |]}
            in
            Some evt
      | _ -> None
    in
    let evt_burial =
      match get_burial p with
        UnknownBurial ->
          if sou base (get_burial_place p) = "" &&
             sou base (get_burial_src p) = ""
          then
            None
          else
            let evt =
              {epers_name = Epers_Burial; epers_date = Adef.codate_None;
               epers_place = get_burial_place p; epers_reason = empty_string;
               epers_note = empty_string; epers_src = get_burial_src p;
               epers_witnesses = [| |]}
            in
            Some evt
      | _ -> None
    in
    let pevents = [evt_birth; evt_bapt; evt_death; evt_burial] in
    let (changed, pevents) =
      List.fold_right
        (fun evt (changed, pevents) ->
           match evt with
             Some evt -> true, evt :: pevents
           | None -> changed, pevents)
        pevents (false, [])
    in
    if changed then
      begin
        if !trace then
          begin
            Printf.eprintf "Modifiy person : %s\n" (Gutil.designation base p);
            flush stderr
          end;
        let pevents = get_pevents p @ pevents in
        let gp = {(gen_person_of_person p) with pevents = pevents} in
        patch_person base gp.key_index gp;
        base_changed := true;
        incr nb_modified
      end
  done;
  if !base_changed then
    begin
      commit_patches base;
      Printf.eprintf "Number of modified persons: %d\n" !nb_modified;
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
      ~onerror:(fun () ->
          Printf.eprintf "Cannot lock database. Try again.\n";
          flush stderr)
      (fun () ->
         let base = Gwdb.open_base !bname in update_database_with_burial base)

let _ = main ()
