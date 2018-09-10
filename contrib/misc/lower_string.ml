(* $Id: convert.ml,v 1.00 2013/09/05 10:09:42 flh Exp $ *)


(**/**) (* main *)

let speclist = [] 
let anonfun _ = ()
let usage = "Usage: " ^ Sys.argv.(0) ^ " <string>"


let main () =
  Arg.parse speclist anonfun usage;
  if Array.length Sys.argv <> 2 then
    begin Arg.usage speclist usage; exit 2 end;
  let s = Sys.argv.(1) in Printf.fprintf stdout "%s" (Name.lower s)

let _ = main ()

















