(* $Id: fpla.ml,v 4.5 2004-12-14 09:30:12 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)
(* First Parentless Ancestor *)

open Gutil;
open Def;
open Printf;

value make_table base =
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
  let fpla = Array.create base.data.ascends.len None in
  let cnt = ref base.data.ascends.len in
  do {
    while cnt.val > 0 do {
      Array.iteri
        (fun i v ->
           if v == None then
             let ip = Adef.iper_of_int i in
             match (aoi base ip).parents with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 match
                   (fpla.(Adef.int_of_iper (father cpl)),
                    fpla.(Adef.int_of_iper (mother cpl)))
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
           printf "Sosa %d  \t%s.%d %s\n" s (p_first_name base p) p.occ
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
  let cnt = ref 0 in
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    Secure.set_base_dir (Filename.dirname bname.val);
    let base = Iobase.input bname.val in
    first_parentless_ancestor base
  }
;

Printexc.catch main ();
