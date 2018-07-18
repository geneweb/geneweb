(* $Id: consmoy.ml,v 4.7 2006-10-04 10:51:20 deraugla Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Gwdb
open Printf

let consmoy bname base =
  let cons = ref 0.0 in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    cons := !cons +. Adef.float_of_fix (get_consang p)
  done;
  printf "average consanguinity: %f\n" (!cons /. float (nb_of_persons base));
  flush stdout;
  ()

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Argl.parse speclist (fun s -> bname := s) usage;
  let base = Gwdb.open_base !bname in consmoy !bname base

let _ = Printexc.print main ()
