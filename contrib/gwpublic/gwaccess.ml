(* camlp4r *)

open Gwdb

let input_person file =
  let pl = ref [] in
  begin match (try Some (open_in file) with Sys_error _ -> None) with
    Some ic ->
      begin try
        while true do let line = input_line ic in pl := line :: !pl done
      with End_of_file -> ()
      end;
      close_in ic
  | None -> Printf.eprintf "Error while opening file %s\n" file; flush stderr
  end;
  List.rev !pl

let access_everybody access bname =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if get_access p <> access then
      let p = {(gen_person_of_person p) with Def.access = access} in
      patch_person base p.Def.key_index p
  done;
  commit_patches base

let access_some access bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      if get_access p <> access then
        begin let p = {(gen_person_of_person p) with Def.access = access} in
          patch_person base p.Def.key_index p
        end;
      commit_patches base
  | _ ->
      match Gutil.person_of_string_dot_key base key with
        Some ip ->
          let p = poi base ip in
          if get_access p <> access then
            begin let p =
              {(gen_person_of_person p) with Def.access = access}
            in
              patch_person base p.Def.key_index p
            end;
          commit_patches base
      | None -> Printf.eprintf "Bad key %s\n" key; flush stderr

let access_some_list access bname file =
  if Sys.file_exists file then
    let pl = input_person file in List.iter (access_some access bname) pl
  else
    begin
      Printf.eprintf "File does not exist : %s\n" file;
      flush stderr;
      exit 2
    end
