(* $Id: consangAll.ml,v 3.4 2000-10-28 18:07:12 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;

value no_consang = Adef.fix (-1);

value rec clear_descend_consang base mark ifam =
  let des = doi base ifam in
  Array.iter
    (fun ip ->
       if not mark.(Adef.int_of_iper ip) then
         let a = aoi base ip in
         do a.consang := no_consang; mark.(Adef.int_of_iper ip) := True; return
         let u = uoi base ip in
         Array.iter (clear_descend_consang base mark) u.family
       else ())
    des.children
;

value relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab False ip1 ip2)
;

value progr_bar_size = 60;
value progr_bar_draw_rep = 5;
value progr_bar_draw = "|/-\\";
value progr_bar_empty = '.';
value progr_bar_full = '#';

value progr_bar_draw_len = String.length progr_bar_draw;
value progr_bar_cnt = progr_bar_size * progr_bar_draw_rep * progr_bar_draw_len;

value trace quiet cnt max_cnt =
  do if quiet then
       let x = max_cnt - cnt in
       let already_disp = x * progr_bar_size / max_cnt in
       let to_disp = (x + 1) * progr_bar_size / max_cnt in
       do for i = already_disp + 1 to to_disp do
            Printf.eprintf "%c" progr_bar_full;
          done;
          let already_disp = x * progr_bar_cnt / max_cnt in
          let to_disp = (x + 1) * progr_bar_cnt / max_cnt in
          if cnt = 1 then Printf.eprintf " \008"
          else if to_disp > already_disp then
            let k = to_disp mod progr_bar_draw_len in
            Printf.eprintf "%c\008" progr_bar_draw.[k]
          else ();
       return ()
     else Printf.eprintf "%6d\008\008\008\008\008\008" cnt;
     flush stderr;
  return ()
;

value compute base from_scratch quiet =
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
  let tab =
    Consang.make_relationship_table base (Consang.topological_sort base)
  in
  let consang_tab = Array.create base.data.families.len no_consang in
  let cnt = ref 0 in
  do if not from_scratch then
       let mark = Array.create base.data.ascends.len False in
       match base.func.patched_ascends () with
       [ [] -> ()
       | list ->
           let _ = base.data.families.array () in
           do List.iter
                (fun ip ->
                   let u = uoi base ip in
                   Array.iter (clear_descend_consang base mark) u.family)
                list;
              base.data.families.clear_array ();
           return () ]
     else ();
     for i = 0 to base.data.ascends.len - 1 do
       let a = base.data.ascends.get i in
       do if from_scratch then a.consang := no_consang
          else
            match a.parents with
            [ Some ifam -> consang_tab.(Adef.int_of_ifam ifam) := a.consang
            | None -> () ];
       return
       if a.consang == no_consang then incr cnt else ();
     done;
  return
  let max_cnt = cnt.val in
  let most = ref None in
  do Printf.eprintf "To do: %d persons\n" max_cnt;
     if max_cnt = 0 then ()
     else if quiet then
       do for i = 1 to progr_bar_size do
            Printf.eprintf "%c" progr_bar_empty;
          done;
          Printf.eprintf "\r";
       return ()
     else Printf.eprintf "Computing consanguinity...";
     flush stderr;
     let running = ref True in
     while running.val do
       running.val := False;
       for i = 0 to base.data.ascends.len - 1 do
         let a = base.data.ascends.get i in
         if a.consang == no_consang then
           match a.parents with
           [ Some ifam ->
               let pconsang = consang_tab.(Adef.int_of_ifam ifam) in
               if pconsang == no_consang then
                 let cpl = coi base ifam in
                 let fath = aoi base cpl.father in
                 let moth = aoi base cpl.mother in
                 if fath.consang != no_consang && moth.consang != no_consang
                 then
                   let consang = relationship base tab cpl.father cpl.mother in
                   do trace quiet cnt.val max_cnt;
                      decr cnt;
                      a.consang := Adef.fix_of_float consang;
                      consang_tab.(Adef.int_of_ifam ifam) := a.consang;
                      if not quiet then
                        let better =
                          match most.val with
                          [ Some m -> a.consang > m.consang
                          | None -> True ]
                        in
                        if better then
                          do Printf.eprintf
                               "\nMax consanguinity %g for %s... "
                               consang
                               (denomination base (base.data.persons.get i));
                             flush stderr;
                          return most.val := Some a
                        else ()
                      else ();
                   return ()
                 else running.val := True
               else
                 do trace quiet cnt.val max_cnt;
                    decr cnt;
                    a.consang := pconsang;
                 return ()
           | None ->
               do trace quiet cnt.val max_cnt;
                  decr cnt;
                  a.consang := Adef.fix_of_float 0.0;
               return () ]
         else ();
       done;
     done;
     if max_cnt = 0 then ()
     else if quiet then Printf.eprintf "\n"
     else Printf.eprintf " done   \n";
     flush stderr;
  return ()
;
