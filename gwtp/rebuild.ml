(* $Id: rebuild.ml,v 1.1 2000-07-27 02:26:21 ddr Exp $ *)
(* Rebuild all files of a GeneWeb data base from a data base directory
   containing only the file "base" *)

value rebuild bname =
  let base = Iolight.input bname in
  Iobase.output bname base
;

value bname = ref "";

value speclist = [];
value anonfun s = bname.val := s;
value usage = "Usage: rebuild db";

value main () =
  do Argl.parse [] anonfun usage;
     if bname.val = "" then
       do Printf.printf "Missing data base\n";
          Argl.usage speclist usage;
       return exit 2
     else ();
     rebuild bname.val;
  return ()
;

Printexc.catch main ();
