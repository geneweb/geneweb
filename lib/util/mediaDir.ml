type file = { name : string; size : int }
type t = { files : file list; dirs : string list }

let saved = "saved"

let read dir =
  let visible name = name <> "" && name.[0] <> '.' && name.[0] <> '~' in
  let add (files, dirs) name =
    if not (visible name) then (files, dirs)
    else
      match Unix.stat (Filename.concat dir name) with
      | { st_kind = Unix.S_REG; st_size; _ } ->
          ({ name; size = st_size } :: files, dirs)
      | { st_kind = Unix.S_DIR; _ } when name <> saved -> (files, name :: dirs)
      | _ -> (files, dirs)
      | exception Unix.Unix_error _ -> (files, dirs)
  in
  match Sys.readdir dir with
  | names ->
      let files, dirs = Array.fold_left add ([], []) names in
      {
        files = List.sort (fun a b -> String.compare a.name b.name) files;
        dirs = List.sort String.compare dirs;
      }
  | exception Sys_error _ -> { files = []; dirs = [] }
