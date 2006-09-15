(* $Id: consangAll.ml,v 5.3 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Gutil;
open Gwdb;

value no_consang = Adef.fix (-1);

value rec clear_descend_consang base mark ifam =
  let des = doi base ifam in
  Array.iter
    (fun ip ->
       if not mark.(Adef.int_of_iper ip) then do {
         let a = aoi base ip in
         set_consang a no_consang;
         mark.(Adef.int_of_iper ip) := True;
         let u = uoi base ip in
         Array.iter (clear_descend_consang base mark) u.family
       }
       else ())
    des.children
;

value relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab False ip1 ip2)
;

value trace quiet cnt max_cnt =
  do {
    if quiet then ProgrBar.run (max_cnt - cnt) max_cnt
    else do {
      Printf.eprintf "%7d\008\008\008\008\008\008\008" cnt;
      flush stderr;
    }
  }
;

value compute base from_scratch quiet =
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
  let tab =
    Consang.make_relationship_info base (Consang.topological_sort base aoi)
  in
  let consang_tab = Array.create base.data.families.len no_consang in
  let cnt = ref 0 in
  do {
    if not from_scratch then
      let mark = Array.create base.data.ascends.len False in
      match base.func.patched_ascends () with
      [ [] -> ()
      | list ->
          List.iter
            (fun ip ->
               let u = uoi base ip in
               Array.iter (clear_descend_consang base mark) u.family)
            list ]
    else ();
    for i = 0 to base.data.ascends.len - 1 do {
      let a = base.data.ascends.get i in
      if from_scratch then set_consang a no_consang
      else
        match parents a with
        [ Some ifam -> consang_tab.(Adef.int_of_ifam ifam) := consang a
        | None -> () ];
      if consang a == no_consang then incr cnt else ()
    };
    let max_cnt = cnt.val in
    let most = ref None in
    Printf.eprintf "To do: %d persons\n" max_cnt;
    if max_cnt = 0 then ()
    else if quiet then ProgrBar.start ()
    else Printf.eprintf "Computing consanguinity...";
    flush stderr;
    let running = ref True in
    while running.val do {
      running.val := False;
      for i = 0 to base.data.ascends.len - 1 do {
        let a = base.data.ascends.get i in
        if consang a == no_consang then
          match parents a with
          [ Some ifam ->
              let pconsang = consang_tab.(Adef.int_of_ifam ifam) in
              if pconsang == no_consang then
                let cpl = coi base ifam in
                let fath = aoi base (father cpl) in
                let moth = aoi base (mother cpl) in
                if consang fath != no_consang &&
                   consang moth != no_consang then
                   do {
                  let consang =
                    relationship base tab (father cpl) (mother cpl)
                  in
                  trace quiet cnt.val max_cnt;
                  decr cnt;
                  set_consang a (Adef.fix_of_float consang);
                  consang_tab.(Adef.int_of_ifam ifam) := Gutil.consang a;
                  if not quiet then
                    let better =
                      match most.val with
                      [ Some m -> Gutil.consang a > Gutil.consang m
                      | None -> True ]
                    in
                    if better then do {
                      Printf.eprintf "\nMax consanguinity %g for %s... "
                        consang (designation base (base.data.persons.get i));
                      flush stderr;
                      most.val := Some a
                    }
                    else ()
                  else ()
                }
                else running.val := True
              else do {
                trace quiet cnt.val max_cnt; decr cnt; set_consang a pconsang
              }
          | None ->
              do {
                trace quiet cnt.val max_cnt;
                decr cnt;
                set_consang a (Adef.fix_of_float 0.0)
              } ]
        else ()
      }
    };
    if max_cnt = 0 then ()
    else if quiet then ProgrBar.finish ()
    else Printf.eprintf " done   \n";
    flush stderr;
  }
;
