(* $Id: chkimg.ml,v 4.9 2007-09-04 03:06:50 deraugla Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Gutil;
open Def;
open Printf;

value get_images_names bname =
  let dh = Unix.opendir (Filename.concat "images" bname) in
  let list = ref [] in
  do {
    try while True do { list.val := [Unix.readdir dh :: list.val] } with
    [ End_of_file -> () ];
    Unix.closedir dh;
    list.val
  }
;

value check_key_aux base ifname fname =
  try
    let d2 = String.rindex fname '.' in
    let d1 = String.rindex_from fname (d2 - 1) '.' in
    do {
      if d2 = d1 then raise Not_found else ();
      let fn = String.sub fname 0 d1 in
      let sn = String.sub fname (d2 + 1) (String.length fname - d2 - 1) in
      let oc = int_of_string (String.sub fname (d1 + 1) (d2 - d1 - 1)) in
      let ip = Gwdb.person_of_key base fn sn oc in
      if ip = None then raise Not_found else ()
    }
  with
  [ Not_found -> do { printf "... nobody: %s\n" ifname; flush stdout }
  | x -> do { printf "error at %s\n" fname; flush stdout; raise x } ]
;

value check_key base fname =
  if Filename.check_suffix fname ".jpg" then
    check_key_aux base fname (Filename.chop_suffix fname ".jpg")
  else if Filename.check_suffix fname ".gif" then
    check_key_aux base fname (Filename.chop_suffix fname ".gif")
  else printf "... alone: %s\n" fname
;

value chkimg bname base =
  let list = get_images_names bname in
  do { List.iter (check_key base) list; flush stdout }
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    let base = Gwdb.open_base bname.val in
    chkimg bname.val base
  }
;

Printexc.catch main ();
