(* camlp5r *)
(* $Id: convert.ml,v 1.00 2013/09/05 10:09:42 flh Exp $ *)


(**/**) (* main *)

value speclist = []; 
value anonfun _ = ();
value usage = "Usage: " ^ Sys.argv.(0) ^ " <string>";


value main () = do {
  Arg.parse speclist anonfun usage;
  if Array.length Sys.argv <> 2 then do { Arg.usage speclist usage; exit 2; } else ();
  let s = Sys.argv.(1) in
  Printf.fprintf stdout "%s" (Name.lower s);
};

main ();

















