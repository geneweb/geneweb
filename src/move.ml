(* $Id: move.ml,v 2.1 1999-07-09 21:23:04 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Gutil;
open Def;

value array_forall f a =
  loop 0 where rec loop i =
    if i == Array.length a then True
    else if f a.(i) then loop (i + 1)
    else False
;

value move base =
  let _ = base.data.persons.array () in
  let _ = base.data.ascends.array () in
  let _ = base.data.families.array () in
  let _ = base.data.couples.array () in
  for i = 0 to base.data.persons.len - 1 do
    let p = base.data.persons.get i in
    let a = base.data.ascends.get i in
    match (a.parents, p.family) with
    [ (Some ifamc, [| ifams |]) ->
        let famc = foi base ifamc in
        let cpl = coi base ifamc in
        let fams = foi base ifams in
        let fath = poi base cpl.father in
        let moth = poi base cpl.mother in
        if (aoi base cpl.father).parents = None
        && (aoi base cpl.mother).parents = None
        && Array.length fath.family = 1
        && Array.length moth.family = 1
        && array_forall
             (fun ip ->
                let p = poi base ip in
                Array.length p.family == 0 ||
                (foi base p.family.(0)).origin_file == fams.origin_file)
             famc.children
        && famc.origin_file <> fams.origin_file then
          do Printf.printf "%s\n" (denomination base p);
             Printf.printf "  in %s\n" (sou base fams.origin_file);
             Printf.printf "  parents in %s\n" (sou base famc.origin_file);
             flush stdout;
          return ()
        else ()
    | _ -> () ];
  done
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  let cnt = ref 0 in
  do Argl.parse speclist (fun s -> bname.val := s) usage; return
  let base = Iobase.input bname.val in
  move base
;

Printexc.catch main ();
