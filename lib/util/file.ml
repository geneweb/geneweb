exception File_error of string

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
  let perm, check_perm =
    match required_perm with
    | Some perm -> (perm, check_perm perm)
    | None -> (0o644, fun (_ : string) -> true)
  in
  let () =
    if Sys.file_exists path then (
      if not @@ check_kind ~kind:`File path then
        raise_error "%s exists but it is not a regular file" path)
    else Unix.openfile path [ Unix.O_CREAT ] perm |> Unix.close
  in
  if not @@ check_perm path then
    raise_error "%s has not the required permissions %o" path perm

let mkdir ~perm dir =
  if Sys.file_exists dir then (
    if not @@ check_kind ~kind:`Dir dir then
      raise_error "%s exists but it is not a directory" dir)
  else Unix.mkdir dir perm

let ( // ) = Filename.concat

let iter_path_entries f path =
  let rec loop path =
    match (Filename.dirname path, Filename.basename path) with
    | ("." | "/"), _ -> f path
    | path, base ->
        loop path;
        f (path // base)
  in
  loop path

let create_dir ?(parent = false) ?required_perm path =
  if String.equal path "" then
    (* The basename of an empty path is implemented-defined in POSIX.
       We do not support this case to simplify the function. *)
    invalid_arg "create_dir";
  let perm, check_perm =
    match required_perm with
    | Some perm -> (perm, check_perm perm)
    | None -> (0o755, fun (_ : string) -> true)
  in
  let () =
    if parent then iter_path_entries (mkdir ~perm) path else mkdir ~perm path
  in
  if not @@ check_perm path then
    raise_error "%s has not the required permissions %o" path perm

let walk_folder ?(recursive = false) f path acc =
  let rec walk_siblings dirs path handle acc =
    match Unix.readdir handle with
    | exception End_of_file -> (dirs, acc)
    | "." | ".." -> walk_siblings dirs path handle acc
    | s -> (
        let fl = Filename.concat path s in
        let stat = Unix.stat fl in
        match stat.st_kind with
        | Unix.S_REG -> walk_siblings dirs path handle (f (`File fl) acc)
        | Unix.S_DIR ->
            let dirs = if recursive then fl :: dirs else dirs in
            walk_siblings dirs path handle (f (`Dir fl) acc)
        | _ -> walk_siblings dirs path handle acc)
  in
  let rec traverse stack acc =
    match stack with
    | [] -> acc
    | path :: stack ->
        let stack, acc =
          let handle = Unix.opendir path in
          Fun.protect ~finally:(fun () -> Unix.closedir handle) @@ fun () ->
          walk_siblings stack path handle acc
        in
        traverse stack acc
  in
  traverse [ path ] acc
