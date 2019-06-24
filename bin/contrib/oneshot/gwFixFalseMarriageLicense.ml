open Geneweb
open Def
open Gwdb

let () =
  let bname = ref "" in
  let speclist = [] in
  let anonfun i = bname := i in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " base" in
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false ~onerror:Lock.print_try_again @@
  fun () ->
  let base = Gwdb.open_base !bname in
  let nb_fam = nb_of_families base in
  let nbfix = ref 0 in
  for i = 0 to nb_fam - 1 do
    let ifam = Adef.ifam_of_int i in
    let fam = foi base ifam in
    if not @@ is_deleted_family fam then begin
      if get_relation fam = MarriageLicense
      then
        let fevents = get_fevents fam in
        let (modif, fevents) =
          List.fold_right
            begin fun e (modif, fevents) ->
              if e.efam_name = Efam_MarriageLicense
              && e.efam_date = Adef.cdate_None
              && is_empty_string e.efam_place
              && e.efam_witnesses = [||]
              then (true, fevents)
              else (modif, e :: fevents)
            end
            fevents (false, [])
        in
        if modif then
          let relation0 = get_relation fam in
          let marriage0 = get_marriage fam in
          let marriage_place0 = get_marriage_place fam in
          let marriage_note0 = get_marriage_note fam in
          let marriage_src0 = get_marriage_src fam in
          let divorce0 = get_divorce fam in
          let marr_data0 = (relation0, marriage0, marriage_place0, marriage_note0, marriage_src0) in
          let (relation, marriage, marriage_place, marriage_note, marriage_src) as marr_data, divorce, witnesses =
            UpdateFamOk.reconstitute_from_fevents false (insert_string base "") fevents marr_data0 divorce0
          in
          if marr_data0 <> marr_data then begin
            let witnesses = Array.map fst witnesses in
            let fam' =
              { (gen_family_of_family fam)
                with relation ; marriage ; marriage_place ; marriage_note ; marriage_src ; divorce ; witnesses ; fevents }
            in
            incr nbfix ;
            patch_family base ifam fam' ;
            let aux get = let p = poi base @@ get fam in p_first_name base p ^ " " ^ p_surname base p in
            print_endline @@ Printf.sprintf "%s & %s" (aux get_father) (aux get_mother)
          end
    end
  done ;
  if !nbfix <> 0 then commit_patches base
