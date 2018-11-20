(* $Id: fpla.ml,v 5.8 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)
(* First Parentless Ancestor *)

open Geneweb
open Gwdb

let make_table base =
  let _ = load_ascends_array base in
  let _ = load_couples_array base in
  let fpla = Array.make (nb_of_persons base) None in
  let cnt = ref (nb_of_persons base) in
  while !cnt > 0 do
    Array.iteri
      (fun i v ->
         if v = None then
           let ip = Adef.iper_of_int i in
           match get_parents (poi base ip) with
             Some ifam ->
               let cpl = coi base ifam in
               begin match
                 fpla.(Adef.int_of_iper (get_father cpl)),
                 fpla.(Adef.int_of_iper (get_mother cpl))
               with
                 Some (m, k), Some (n, l) ->
                   decr cnt;
                   let v =
                     if m + k < n + 2 * l then m + k, 2 * k
                     else n + 2 * l, 2 * l
                   in
                   fpla.(i) <- Some v
               | _ -> ()
               end
           | None -> decr cnt; fpla.(i) <- Some (1, 1))
      fpla
  done;
  Array.mapi
    (fun i v ->
       match v with
         Some (m, k) -> i, m
       | None -> failwith "internal error")
    fpla

let first_parentless_ancestor base =
  let tab = make_table base in
  Array.sort (fun (_, s1) (_, s2) -> compare s2 s1) tab;
  Printf.printf "First parentless ancestor\n\n";
  Array.iter
    (fun (i, s) ->
       let p = poi base (Adef.iper_of_int i) in
       Printf.printf "Sosa %d  \t%s.%d %s\n" s (p_first_name base p) (get_occ p)
         (p_surname base p);
       flush stdout)
    tab

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  Argl.parse speclist (fun s -> bname := s) usage;
  Secure.set_base_dir (Filename.dirname !bname);
  let base = Gwdb.open_base !bname in first_parentless_ancestor base

let _ = Printexc.print main ()
