module Compat = Geneweb_compat
module Fpath = Fpath

exception File_error of string

let file_exists path = Sys.file_exists (Fpath.to_string path)
let readdir path = Sys.readdir (Fpath.to_string path)

let rec remove_recursive path_str =
  if Sys.is_directory path_str then begin
    Array.iter
      (fun name -> remove_recursive (Filename.concat path_str name))
      (Sys.readdir path_str);
    Unix.rmdir path_str
  end
  else Sys.remove path_str

let remove ?(force = false) ?(recursive = false) path =
  let path_str = Fpath.to_string path in
  let exists = Sys.file_exists path_str in
  if not exists then (if not force then Sys.remove path_str)
  else if recursive then remove_recursive path_str
  else Sys.remove path_str

let rename src dst = Sys.rename (Fpath.to_string src) (Fpath.to_string dst)
let stat path = Unix.stat (Fpath.to_string path)
let openfile path flags perm = Unix.openfile (Fpath.to_string path) flags perm

let check_suffix path suffix =
  Filename.check_suffix (Fpath.to_string path) suffix

let chop_suffix path suffix = Filename.chop_suffix (Fpath.to_string path) suffix
let chmod path perm = Unix.chmod (Fpath.to_string path) perm
let opendir path = Unix.opendir (Fpath.to_string path)
let is_directory path = Sys.is_directory (Fpath.to_string path)
let raise_error ppf = Format.ksprintf (fun s -> raise (File_error s)) ppf

let check_perm perm path =
  if Sys.win32 then true
  else
    let Unix.{ st_perm; _ } = Unix.stat path in
    st_perm = perm

let check_kind ~kind path =
  let Unix.{ st_kind; _ } = Unix.stat path in
  match kind with `File -> st_kind = Unix.S_REG | `Dir -> st_kind = Unix.S_DIR

let create_file ?required_perm path =
  let path_str = Fpath.to_string path in
  let perm, check_perm =
    match required_perm with
    | Some perm -> (perm, check_perm perm)
    | None -> (0o644, fun (_ : string) -> true)
  in
  let () =
    if Sys.file_exists path_str then (
      if not @@ check_kind ~kind:`File path_str then
        raise_error "%s exists but it is not a regular file" path_str)
    else Unix.openfile path_str [ Unix.O_CREAT ] perm |> Unix.close
  in
  if not @@ check_perm path_str then
    raise_error "%s has not the required permissions %o" path_str perm

let mkdir ~perm dir =
  if Sys.file_exists dir then (
    if not @@ check_kind ~kind:`Dir dir then
      raise_error "%s exists but it is not a directory" dir)
  else Unix.mkdir dir perm

let mkdir_p ?(perm = 0o755) d =
  let rec loop d =
    let d1 = Filename.dirname d in
    if d1 <> d && String.length d1 < String.length d then loop d1;
    if not (Sys.file_exists d) then
      try Unix.mkdir d perm
      with Unix.Unix_error (_, _, _) -> Printf.eprintf "Failed mkdir: %s\n" d
  in
  loop @@ Fpath.to_string d

let iter_path_entries f path =
  let rec loop path =
    match (Filename.dirname path, Filename.basename path) with
    | ("." | "/"), _ -> f path
    | path, base ->
        loop path;
        f (Filename.concat path base)
  in
  loop path

let create_dir ?(parent = false) ?required_perm path =
  let path_str = Fpath.to_string path in
  if String.equal path_str "" then
    (* The basename of an empty path is implemented-defined in POSIX.
       We do not support this case to simplify the function. *)
    invalid_arg "create_dir";
  let perm, check_perm =
    match required_perm with
    | Some perm -> (perm, check_perm perm)
    | None -> (0o755, fun (_ : string) -> true)
  in
  let () =
    if parent then iter_path_entries (mkdir ~perm) path_str
    else mkdir ~perm path_str
  in
  if not @@ check_perm path_str then
    raise_error "%s has not the required permissions %o" path_str perm

type entry =
  | File of Fpath.t
  | Dir of Fpath.t
  | Exn of { path : Fpath.t; exn : exn; bt : Printexc.raw_backtrace }

let walk_folder ?(recursive = false) f path acc =
  let path_str = Fpath.to_string path in
  let rec walk_siblings dirs path_str handle acc =
    match Unix.readdir handle with
    | exception End_of_file -> (dirs, acc)
    | "." | ".." -> walk_siblings dirs path_str handle acc
    | s -> (
        let fl = Filename.concat path_str s in
        match (Unix.stat fl).st_kind with
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            let acc =
              f (Exn { path = Fpath.of_string path_str; exn; bt }) acc
            in
            walk_siblings dirs path_str handle acc
        | Unix.S_REG ->
            walk_siblings dirs path_str handle
              (f (File (Fpath.of_string fl)) acc)
        | Unix.S_DIR ->
            let dirs = if recursive then fl :: dirs else dirs in
            walk_siblings dirs path_str handle
              (f (Dir (Fpath.of_string fl)) acc)
        | _ -> walk_siblings dirs path_str handle acc)
  in
  let rec traverse stack acc =
    match stack with
    | [] -> acc
    | path_str :: stack ->
        let stack, acc =
          match Unix.opendir path_str with
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              (stack, f (Exn { path = Fpath.of_string path_str; exn; bt }) acc)
          | handle ->
              Fun.protect ~finally:(fun () -> Unix.closedir handle) @@ fun () ->
              walk_siblings stack path_str handle acc
        in
        traverse stack acc
  in
  traverse [ path_str ] acc

let copy_file ?(perm = 0o640) ?(overwrite = true) src dst =
  let src_str = Fpath.to_string src in
  let dst_str = Fpath.to_string dst in
  let sz = 8192 in
  let buf = Bytes.create sz in
  let flags =
    if overwrite then [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
    else [ Open_wronly; Open_creat; Open_excl; Open_binary ]
  in
  Compat.In_channel.with_open_bin src_str @@ fun ic ->
  Compat.Out_channel.with_open_gen flags perm dst_str @@ fun oc ->
  let rec loop () =
    match Compat.In_channel.input ic buf 0 sz with
    | 0 -> ()
    | r ->
        Compat.Out_channel.output oc buf 0 r;
        loop ()
  in
  loop ()

let open_in_text s = open_in_bin @@ Fpath.to_string s
let open_out_text s = open_out_bin @@ Fpath.to_string s
let open_in_bin s = open_in_bin @@ Fpath.to_string s
let open_out_bin s = open_out_bin @@ Fpath.to_string s

let with_open_out_text s =
  Compat.Out_channel.with_open_text @@ Fpath.to_string s

let with_open_out_bin s = Compat.Out_channel.with_open_bin @@ Fpath.to_string s
let with_open_in_text s = Compat.In_channel.with_open_text @@ Fpath.to_string s
let with_open_in_bin s = Compat.In_channel.with_open_bin @@ Fpath.to_string s
