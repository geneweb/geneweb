(* $Id: secure.ml,v 5.2 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

(* secure open; forbids to access anywhere in the machine;
   this is an extra security: the program should check for
   correct open instead of hoping Secure do it for it *)

value ok_path = ref [];
value lang_path_r = ref [];
value doc_path_r = ref [];
value base_dir_r = ref Filename.current_dir_name;

value decompose =
  loop [] where rec loop r s =
    let b = Filename.basename s in
    if b = "" || b = Filename.current_dir_name then
      let d = Filename.dirname s in
      if d = "" || d = Filename.current_dir_name then r
      else if d = s then [d :: r]
      else loop r d
    else if b = s then [b :: r]
    else loop [b :: r] (Filename.dirname s)
;

value add_path path s =
  do {
    path.val := [s :: path.val];
    ok_path.val := [decompose s :: ok_path.val]
  }
;

value add_lang_path = add_path lang_path_r;
value add_doc_path = add_path doc_path_r;
value set_base_dir s =
  do {
    base_dir_r.val := s;
    ok_path.val := [decompose s :: ok_path.val]
  }
;
value lang_path () = lang_path_r.val;
value doc_path () = doc_path_r.val;
value base_dir () = base_dir_r.val;

value suffix d df =
  loop (d, df) where rec loop =
    fun
    [ ([x :: xl], [y :: yl]) ->
        if x = y then loop (xl, yl) else None
    | ([], df) -> Some df
    | (d, []) -> None ]
;

value check_open fname =
  try
    do {
      if String.contains fname '\000' then raise Exit else ();
      let df = decompose fname in
      loop ok_path.val where rec loop =
        fun
        [ [d :: dl] ->
            match suffix d df with
            [ Some bf ->
                if List.mem Filename.parent_dir_name bf then raise Exit
                else ()
            | None -> loop dl ]
        | [] ->
            if Filename.is_relative fname then
              if List.mem Filename.parent_dir_name df then raise Exit
              else ()
            else raise Exit ]
    }
  with
  [ Exit ->
      do {
        IFDEF UNIX THEN
          do {
            Printf.eprintf "*** secure rejects open %s\n"
              (String.escaped fname);
            flush stderr;
          }
        ELSE () END;
        raise (Sys_error "invalid access")
      } ]
;

value open_in fname =
  do { check_open fname; Pervasives.open_in fname }
;
value open_in_bin fname =
  do { check_open fname; Pervasives.open_in_bin fname }
;
value open_out fname =
  do { check_open fname; Pervasives.open_out fname }
;
value open_out_bin fname =
  do { check_open fname; Pervasives.open_out_bin fname }
;
value open_out_gen mode perm fname =
  do { check_open fname; Pervasives.open_out_gen mode perm fname }
;
