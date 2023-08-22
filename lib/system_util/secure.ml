(* $Id: secure.ml,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* secure open; forbids to access anywhere in the machine;
   this is an extra security: the program should check for
   correct open instead of hoping Secure do it for it *)

let ok_r = ref []
let assets_r = ref []
let bd_r = ref Filename.current_dir_name

(* [decompose: string -> string list] decompose a path into a list of
   directory and a basename. "a/b/c" -> [ "a" ; "b"; "c" ] *)
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

(* add asset to the list of allowed to acces assets *)
let add_assets d =
  assets_r := d :: !assets_r;
  ok_r := decompose d :: !ok_r

(* set base dir to which acces could be allowed *)
let set_base_dir d =
  let ok = decompose d in
  bd_r := d;
  ok_r := ok :: (List.filter (( <> ) ok)) !ok_r

(* get all assets *)
let assets () = !assets_r
let base_dir () = !bd_r

(* [list_check_prefix d df] returns either [None] if [d] is not a prefix of
   [df], or [Some suffix], where [df = d @ suffix] *)
let list_check_prefix d df =
  let rec loop = function
    | x :: xl, y :: yl -> if x = y then loop (xl, yl) else None
    | [], df -> Some df
    | _, [] -> None
  in
  loop (d, df)

(** Check if a filename is safe to read:
    * it must not contain the '\000' character
    * it must either be relative to the local directory OR
      included in one of the allowed directories (base_dir or assets)
    * the relative part does not contain the '..' directory
*)
let check fname =
  if String.contains fname '\000' then false
  else
    let df = decompose fname in
    let rec loop = function
      | d :: dl -> (
          match list_check_prefix d df with
          | Some bf when not (List.mem Filename.parent_dir_name bf) -> true
          | _ -> loop dl)
      | [] ->
          if Filename.is_relative fname then
            not (List.mem Filename.parent_dir_name df)
          else false
    in
    loop !ok_r

let check_open fname =
  if not (check fname) then (
    if Sys.unix then (
      Printf.eprintf "*** secure rejects open %s\n" (String.escaped fname);
      flush stderr);
    raise (Sys_error "invalid access"))

(* The following functions perform a [check] before opening the file,
   preventing potential attacks on the system.
*)
let open_in fname =
  check_open fname;
  Stdlib.open_in fname

let open_in_bin fname =
  check_open fname;
  Stdlib.open_in_bin fname

let open_out fname =
  check_open fname;
  Stdlib.open_out fname

let open_out_bin fname =
  check_open fname;
  Stdlib.open_out_bin fname

let open_out_gen mode perm fname =
  check_open fname;
  Stdlib.open_out_gen mode perm fname
