(* $Id: check_apostr.ml,v 4.4 2005-01-17 12:53:08 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Geneweb

let cnt = ref 0

(* Scan a base to identify potential conflicts arising when:
  - replacing ’ by ' in the lower function
  - properly treating supplementary Latin accented characters (for vietnameese)
  resolution typically consists in changing the occ number (+1)
*)

let scan base =
  Printf.printf "\nChecking duplicates with apostrophe and viet accents";
  let ht = Hashtbl.create (Gwdb.nb_of_persons base) in
  Gwdb.Collection.iteri begin fun i p ->
    let fn = Gwdb.sou base (Gwdb.get_first_name p) in
    let sn = Gwdb.sou base (Gwdb.get_surname p) in
    let oc = string_of_int (Gwdb.get_occ p) in
    let fn1 = Name.lower ~apostr:true fn in
    let sn1 = Name.lower ~apostr:true sn in
    let k = fn1 ^ "." ^ oc ^ " " ^ sn1 in
    let v = fn  ^ "." ^ oc ^ " " ^ sn in
    if fn <> "?" && sn <> "?" then
      begin
        if not (Hashtbl.mem ht k) then
          Hashtbl.add ht k v
        else
          begin
          Printf.printf "\nconflit %s avec %s..." v (Hashtbl.find ht k) ;
          incr cnt
          end
      end
  end (Gwdb.persons base)

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Arg.parse speclist (fun s -> bname := s) usage;
  let base = Gwdb.open_base !bname in
  scan base

let _ = main ()
