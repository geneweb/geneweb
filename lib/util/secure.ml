(* $Id: secure.ml,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* secure open; forbids to access anywhere in the machine;
   this is an extra security: the program should check for
   correct open instead of hoping Secure do it for it *)

let ok_r = ref []
let assets_r = ref []
let bd_r = ref Filename.current_dir_name

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

let add_assets d =
  assets_r := d :: !assets_r ;
  ok_r := decompose d :: !ok_r

let set_base_dir d =
  let ok = decompose d in
  bd_r := d ;
  ok_r := ok :: (List.filter ((<>) ok)) !ok_r

let assets () = !assets_r
let bd () = !bd_r

let suffix d df =
  let rec loop =
    function
      x :: xl, y :: yl -> if x = y then loop (xl, yl) else None
    | [], df -> Some df
    | _, [] -> None
  in
  loop (d, df)

let check fname =
  if String.contains fname '\000' then false
  else
    let df = decompose fname in
    let rec loop = function
      | d :: dl ->
        begin match suffix d df with
          | Some bf -> not (List.mem Filename.parent_dir_name bf)
          | None -> loop dl
        end
      | [] ->
        if Filename.is_relative fname
        then not (List.mem Filename.parent_dir_name df)
        else false
    in
    loop !ok_r

let check_open fname =
  if not (check fname) then begin
    if Sys.unix then
      begin
        Printf.eprintf "*** secure rejects open %s\n" (String.escaped fname);
        flush stderr
      end;
    raise (Sys_error "invalid access")
  end

let open_in fname = check_open fname; Stdlib.open_in fname
let open_in_bin fname = check_open fname; Stdlib.open_in_bin fname
let open_out fname = check_open fname; Stdlib.open_out fname
let open_out_bin fname = check_open fname; Stdlib.open_out_bin fname
let open_out_gen mode perm fname =
  check_open fname; Stdlib.open_out_gen mode perm fname
