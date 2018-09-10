(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

open Gwdb

let check_name base nb_ind fix =
  Printf.printf "Check colon\n";
  flush stdout;
  for i = 0 to nb_ind - 1 do
    let ip = Adef.iper_of_int i in
    let p = poi base ip in
    let fn = sou base (get_first_name p) in
    let sn = sou base (get_surname p) in
    if String.contains fn ':' || String.contains sn ':' then
      begin
        Printf.printf "*** bad name : %s %s (%d) => %s\n" fn sn i
          (Gutil.designation base (poi base ip));
        flush stdout
      end
  done


let check bname =
  let base = Gwdb.open_base bname in
  let fix = ref false in
  let nb_ind = nb_of_persons base in
  check_name base nb_ind fix;
  if !fix then Gwdb.commit_patches base
  else begin Printf.printf "No change\n"; flush stdout end


(**/**)

let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false (fun () -> check !bname)
    ~onerror:(fun () ->
        Printf.eprintf "Cannot lock database. Try again.\n";
        flush stderr)

let _ = main ()
