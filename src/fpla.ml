(* $Id: fpla.ml,v 4.0 2001-03-16 19:34:39 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)
(* First Parentless Ancestor *)

open Gutil;
open Def;

value make_table base =
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
  let fpla = Array.create base.data.ascends.len None in
  let cnt = ref base.data.ascends.len in
  do while cnt.val > 0 do
       Array.iteri
         (fun i v ->
            if v == None then
              let ip = Adef.iper_of_int i in
              match (aoi base ip).parents with
              [ Some ifam ->
                  let cpl = coi base ifam in
                  match
                    (fpla.(Adef.int_of_iper cpl.father),
                     fpla.(Adef.int_of_iper cpl.mother))
                  with
                  [ (Some (m, k), Some (n, l)) ->
                      do decr cnt;
(*
                         Printf.eprintf "\r%d     " cnt.val;
                         flush stderr;
*)
                         let v =
                           if m + k < n + 2 * l then (m + k, 2 * k)
                           else (n + 2 * l, 2 * l)
                         in
                         fpla.(i) := Some v;
                      return ()
                  | _ -> () ]
              | None ->
                  do decr cnt;
(*
                     Printf.eprintf "\r%d     " cnt.val;
                     flush stderr;
*)
                     fpla.(i) := Some (1, 1);
                  return () ]
            else ())
         fpla;
     done;
(*
     Printf.eprintf "\n";
     flush stderr;
*)
  return
  Array.mapi
    (fun i v ->
       match v with
       [ Some (m, k) -> (i, m)
       | None -> failwith "internal error" ])
    fpla
;

value first_parentless_ancestor base =
  let tab = make_table base in
  do Array.sort (fun (_, s1) (_, s2) -> compare s2 s1) tab;
     Printf.printf "First parentless ancestor\n\n";
     Array.iter
       (fun (i, s) ->
          let p = poi base (Adef.iper_of_int i) in
          do Printf.printf "Sosa %d  \t%s.%d %s\n" s
               (p_first_name base p) p.occ (p_surname base p);
             flush stdout;
          return ())
       tab;
  return ()
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  let cnt = ref 0 in
  do Argl.parse speclist (fun s -> bname.val := s) usage; return
  let base = Iobase.input bname.val in
  first_parentless_ancestor base
;

Printexc.catch main ();
