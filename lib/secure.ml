(* $Id: secure.ml,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* secure open; forbids to access anywhere in the machine;
   this is an extra security: the program should check for
   correct open instead of hoping Secure do it for it *)

let ok_path = ref []
let lang_path_r = ref []
let base_dir_r = ref Filename.current_dir_name

let decompose =
  let rec loop r s =
    let b = Filename.basename s in
    if b = "" || b = Filename.current_dir_name || b = Filename.dir_sep then
      let d = Filename.dirname s in
      if d = "" || d = Filename.current_dir_name then r
      else if d = s then d :: r
      else loop r d
    else if b = s then b :: r
    else loop (b :: r) (Filename.dirname s)
  in
  loop []

let add_path path s = path := s :: !path; ok_path := decompose s :: !ok_path

let add_lang_path = add_path lang_path_r
let set_base_dir s = base_dir_r := s; ok_path := decompose s :: !ok_path
let lang_path () = !lang_path_r
let base_dir () = !base_dir_r

let suffix d df =
  let rec loop =
    function
      x :: xl, y :: yl -> if x = y then loop (xl, yl) else None
    | [], df -> Some df
    | d, [] -> None
  in
  loop (d, df)

let check_open fname =
  try
    if String.contains fname '\000' then raise Exit;
    let df = decompose fname in
    let rec loop =
      function
        d :: dl ->
          begin match suffix d df with
            Some bf -> if List.mem Filename.parent_dir_name bf then raise Exit
          | None -> loop dl
          end
      | [] ->
          if Filename.is_relative fname then
            (if List.mem Filename.parent_dir_name df then raise Exit)
          else raise Exit
    in
    loop !ok_path
  with Exit ->
    if Sys.unix then
      begin
        Printf.eprintf "*** secure rejects open %s\n" (String.escaped fname);
        flush stderr
      end;
    raise (Sys_error "invalid access")

let open_in fname = check_open fname; Pervasives.open_in fname
let open_in_bin fname = check_open fname; Pervasives.open_in_bin fname
let open_out fname = check_open fname; Pervasives.open_out fname
let open_out_bin fname = check_open fname; Pervasives.open_out_bin fname
let open_out_gen mode perm fname =
  check_open fname; Pervasives.open_out_gen mode perm fname
