(* $Id: hist.ml,v 4.7 2006-10-30 09:37:58 deraugla Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Gwdb;
open Printf;

value line_tpl = "0000-00-00 00:00:00 xx .";

value person_of_line_exists base line =
  let i = try String.index line ']' + 2 with [ Not_found -> 20 ] in
  let key = String.sub line (i + 3) (String.length line - i - 3) in
  match person_ht_find_all base key with
  [ [ip] -> True
  | _ -> False ]
;

value histselect bname base =
  let () = load_strings_array base in
  let ic = open_in (Filename.concat (bname ^ ".gwb") "history") in
  try
    while True do {
      let line = input_line ic in
      if person_of_line_exists base line then printf "%s\n" line else ();
      flush stdout
    }
  with
  [ End_of_file -> close_in ic ]
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    let base = Gwdb.open_base bname.val in
    histselect bname.val base
  }
;

Printexc.catch main ();
