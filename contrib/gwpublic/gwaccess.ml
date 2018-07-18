(* camlp4r *)

open Gwdb;

value input_person file = do {
  let pl = ref [] in
  match try Some (open_in file) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        try
          while True do {
            let line = input_line ic in
            pl.val := [ line :: pl.val ]
          }
        with [ End_of_file -> () ];
        close_in ic
      }
  | None ->
      do {
        Printf.eprintf "Error while opening file %s\n" file;
        flush stderr;
      }] ;
  List.rev pl.val
};

value access_everybody access bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> access then
        let p = {(gen_person_of_person p) with Def.access = access} in
        patch_person base p.Def.key_index p
      else ();
    };
    commit_patches base;
  }
;

value access_some access bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      do {
         if get_access p <> access then
         let p = {(gen_person_of_person p) with Def.access = access} in
         patch_person base p.Def.key_index p
         else ();
         commit_patches base;
      }
  | _ ->
      match Gutil.person_of_string_dot_key base key with
      [ Some ip ->
          let p = poi base ip in
          do {
             if get_access p <> access then
             let p = {(gen_person_of_person p) with Def.access = access} in
             patch_person base p.Def.key_index p
             else ();
             commit_patches base;
          }
      | None ->
          do {
            Printf.eprintf "Bad key %s\n" key;
            flush stderr;
            (*
               Si on appel access_some sur une liste et qu'il
               y'a une mauvaise clé, alors on quitte tout le
               script, c'est un peu radical.
            *)
            (*exit 2*)
          } ] ]
;

value access_some_list access bname file =
  if Sys.file_exists file then
    let pl = input_person file in
    List.iter (access_some access bname) pl
  else
    do {
      Printf.eprintf "File does not exist : %s\n" file;
      flush stderr;
      exit 2
    }
;
