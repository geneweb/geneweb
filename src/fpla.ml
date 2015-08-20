(* $Id: fpla.ml,v 5.8 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)
(* First Parentless Ancestor *)

open Gwdb;
open Printf;

value make_table base =
  let _ = load_ascends_array base in
  let _ = load_couples_array base in
  let fpla = Array.make (nb_of_persons base) None in
  let cnt = ref (nb_of_persons base) in
  do {
    while cnt.val > 0 do {
      Array.iteri
        (fun i v ->
           if v = None then
             let ip = Adef.iper_of_int i in
             match get_parents (poi base ip) with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 match
                   (fpla.(Adef.int_of_iper (get_father cpl)),
                    fpla.(Adef.int_of_iper (get_mother cpl)))
                 with
                 [ (Some (m, k), Some (n, l)) ->
                     do {
                       decr cnt;
                       let v =
                         if m + k < n + 2 * l then (m + k, 2 * k)
                         else (n + 2 * l, 2 * l)
                       in
                       fpla.(i) := Some v;
                     }
                 | _ -> () ]
             | None -> do { decr cnt; fpla.(i) := Some (1, 1) } ]
           else ())
        fpla
    };
    Array.mapi
      (fun i v ->
         match v with
         [ Some (m, k) -> (i, m)
         | None -> failwith "internal error" ])
      fpla
  }
;

value first_parentless_ancestor base =
  let tab = make_table base in
  do {
    Array.sort (fun (_, s1) (_, s2) -> compare s2 s1) tab;
    printf "First parentless ancestor\n\n";
    Array.iter
      (fun (i, s) ->
         let p = poi base (Adef.iper_of_int i) in
         do {
           printf "Sosa %d  \t%s.%d %s\n" s (p_first_name base p) (get_occ p)
             (p_surname base p);
           flush stdout;
         })
      tab;
  }
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    Secure.set_base_dir (Filename.dirname bname.val);
    let base = Gwdb.open_base bname.val in
    first_parentless_ancestor base
  }
;

Printexc.catch main ();
