(* camlp4r *)

open Gwdb


let bname = ref ""

let speclist = []
let anonfun i = bname := i
let usage = "Usage: gwBaseCompatiblePlus base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let base = Gwdb.open_base !bname in
  if nb_of_persons base > 0 then
    try
      for i = 0 to nb_of_persons base - 1 do
        let p = poi base (Adef.iper_of_int i) in
        let _ = sou base (get_birth_note p) in
        let _ = sou base (get_baptism_note p) in
        let _ = sou base (get_death_note p) in
        let _ = sou base (get_burial_note p) in let _ = get_pevents p in ()
      done
    with _ ->
      Printf.eprintf "GeneWeb base not compatible\n"; flush stderr; exit 2

let _ = main ()






