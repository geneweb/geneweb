(* $Id: chkimg.ml,v 4.9 2007-09-04 03:06:50 deraugla Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Printf

let get_images_names bname =
  let dh = Unix.opendir (Filename.concat "images" bname) in
  let list = ref [] in
  begin try while true do list := Unix.readdir dh :: !list done with
    End_of_file -> ()
  end;
  Unix.closedir dh;
  !list

let check_key_aux base ifname fname =
  try
    let d2 = String.rindex fname '.' in
    let d1 = String.rindex_from fname (d2 - 1) '.' in
    if d2 = d1 then raise Not_found;
    let fn = String.sub fname 0 d1 in
    let sn = String.sub fname (d2 + 1) (String.length fname - d2 - 1) in
    let oc = int_of_string (String.sub fname (d1 + 1) (d2 - d1 - 1)) in
    let ip = Gwdb.person_of_key base fn sn oc in
    if ip = None then raise Not_found
  with
    Not_found -> printf "... nobody: %s\n" ifname; flush stdout
  | x -> printf "error at %s\n" fname; flush stdout; raise x

let check_key base fname =
  if Filename.check_suffix fname ".jpg" then
    check_key_aux base fname (Filename.chop_suffix fname ".jpg")
  else if Filename.check_suffix fname ".gif" then
    check_key_aux base fname (Filename.chop_suffix fname ".gif")
  else printf "... alone: %s\n" fname

let chkimg bname base =
  let list = get_images_names bname in
  List.iter (check_key base) list; flush stdout

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Argl.parse speclist (fun s -> bname := s) usage;
  let base = Gwdb.open_base !bname in chkimg !bname base

let _ = Printexc.print main ()
