open Geneweb

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

















