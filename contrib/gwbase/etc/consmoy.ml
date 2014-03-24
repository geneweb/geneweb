(* $Id: consmoy.ml,v 4.7 2006-10-04 10:51:20 deraugla Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gwdb;
open Printf;

value consmoy bname base =
  let cons = ref 0.0 in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let a = aoi base (Adef.iper_of_int i) in
      cons.val := cons.val +. Adef.float_of_fix (get_consang a)
    };
    printf "average consanguinity: %f\n"
      (cons.val /. float (nb_of_persons base));
    flush stdout;
    ()
  }
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    let base = Gwdb.make_base (Iobase.input bname.val) in
    consmoy bname.val base
  }
;

Printexc.catch main ();
