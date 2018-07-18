(* $Id: hist.ml,v 4.7 2006-10-30 09:37:58 deraugla Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Gutil
open Gwdb
open Printf

let line_tpl = "0000-00-00 00:00:00 xx ."

let person_of_line_exists base line =
  let i = try String.index line ']' + 2 with Not_found -> 20 in
  let key = String.sub line (i + 3) (String.length line - i - 3) in
  match person_ht_find_all base key with
    [ip] -> true
  | _ -> false

let histselect bname base =
  let () = load_strings_array base in
  let ic = open_in (Filename.concat (bname ^ ".gwb") "history") in
  try
    while true do
      let line = input_line ic in
      if person_of_line_exists base line then printf "%s\n" line; flush stdout
    done
  with End_of_file -> close_in ic

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Argl.parse speclist (fun s -> bname := s) usage;
  let base = Gwdb.open_base !bname in histselect !bname base

let _ = Printexc.print main ()
