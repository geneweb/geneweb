(* camlp5r *)
(* $Id: recover.ml,v 5.3 2007-09-12 09:58:44 ddr Exp $ *)

open Printf;

value recover db =
  let base = Iolight.input db in
  Outbase.output "a" base
;

value database = ref "";
value usage_msg = "Usage: recover base.gwb";
value speclist = [];
value anonfun db = database.val := db;

value main () =
  do {
    Arg.parse speclist anonfun usage_msg;
    recover database.val;
  }
;

try main () with exc ->
  do {
    eprintf "Exception raised: %s\n" (Printexc.to_string exc);
    flush stderr;
  };
