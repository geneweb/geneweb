(* $Id: recover.ml,v 5.3 2007-09-12 09:58:44 ddr Exp $ *)


let recover db = let base = Iolight.input db in Outbase.output "a" base

let database = ref ""
let usage_msg = "Usage: recover base.gwb"
let speclist = []
let anonfun db = database := db

let main () = Arg.parse speclist anonfun usage_msg; recover !database

let _ =
  try main () with
    exc ->
      Printf.eprintf "Exception raised: %s\n" (Printexc.to_string exc); flush stderr
