(* $Id: secure.ml,v 4.1 2002-12-31 08:38:07 ddr Exp $ *)
(* Copyright (c) 2002 INRIA *)

(* secure open; forbids to access anywhere in the machine;
   this is an extra security: the program should check for
   correct open instead of hoping Secure do it for it *)

value lang_path = ref [];
value doc_path = ref [];

value start_with s p =
  String.length p <= String.length s &&
  String.sub s 0 (String.length p) = p
;

value string_contains s ss =
  let sslen = String.length ss in
  let mlen = String.length s - sslen in
  loop 0 where rec loop i =
    if i >= mlen then False
    else if String.sub s i sslen = ss then True
    else loop (i + 1)
;

value check_open fname =
  let ok_path = lang_path.val @ doc_path.val in
  try
    do {
      if String.contains fname '\000' then raise Exit else ();
      let (bname, is_absolute) =
        loop ok_path where rec loop =
          fun
          [ [d :: dl] ->
              if start_with fname d then
                let len = String.length d in
                (String.sub fname (String.length fname - len) len, False)
              else loop dl
          | [] -> (fname, True) ]
      in
      if string_contains bname ".." || string_contains bname "::" then
        raise Exit
      else ();
      if is_absolute && not (Filename.is_relative fname) then raise Exit
      else ();
    }
  with
  [ Exit ->
      do {
        ifdef UNIX then
          do {
            Printf.eprintf "*** secure rejects open %s\n"
              (String.escaped fname);
            flush stderr;
          }
        else ();
        raise (Sys_error "")
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
