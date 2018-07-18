(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Gwdb
open Printf


let designation base ip p =
  let first_name = p_first_name base p in
  let surname = p_surname base p in
  if first_name = "?" || surname = "?" then
    "i=" ^ string_of_int (Adef.int_of_iper ip)
  else
    Mutil.iso_8859_1_of_utf_8
      (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ surname)

let check_name base nb_ind fix =
  printf "Check colon\n";
  flush stdout;
  for i = 0 to nb_ind - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    if String.contains fn ':' || String.contains sn ':' then
      begin
        printf "*** bad name : %s %s (%d) => %s\n" fn sn i
          (designation base ip (poi base ip));
        flush stdout
      end
  done


let check bname =
  let base = Gwdb.open_base bname in
  let fix = ref false in
  let nb_ind = nb_of_persons base in
  check_name base nb_ind fix;
  if !fix then Gwdb.commit_patches base
  else begin printf "No change\n"; flush stdout end


(**/**)

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  match
    Lock.control (Mutil.lock_file !bname) false (fun () -> check !bname)
  with
    Some x -> x
  | None -> eprintf "Cannot lock database. Try again.\n"; flush stderr

let _ = main ()
