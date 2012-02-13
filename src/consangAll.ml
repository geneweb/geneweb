(* $Id: consangAll.ml,v 5.35 2007/02/21 18:14:01 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Def;
open Gwdb;

value designation base p =
  let first_name = p_first_name base p in
  let nom = p_surname base p in
  Mutil.iso_8859_1_of_utf_8
    (first_name ^ "." ^ string_of_int (get_occ p) ^ " " ^ nom)
;

value rec clear_descend_consang base cset mark ifam =
  let des = foi base ifam in
  Array.iter
    (fun ip ->
       if not mark.(Adef.int_of_iper ip) then do {
         cset (Adef.int_of_iper ip) Adef.no_consang;
         mark.(Adef.int_of_iper ip) := True;
         let u = poi base ip in
         Array.iter (clear_descend_consang base cset mark) (get_family u)
       }
       else ())
    (get_children des)
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

value compute base tlim from_scratch quiet = do {
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let (fget, cget, cset, carray) = ascends_array base in
  try do {
    let tab =
      let ts = Consang.topological_sort base poi in
      Consang.make_relationship_info base ts
    in
    let consang_tab = Array.create (nb_of_families base) Adef.no_consang in
    let cnt = ref 0 in
    if not from_scratch then
      let mark = Array.create (nb_of_persons base) False in
      match patched_ascends base with
      [ [] -> ()
      | list ->
          List.iter
            (fun ip ->
               let u = poi base ip in
               Array.iter (clear_descend_consang base cset mark)
                 (get_family u))
            list ]
    else ();
    for i = 0 to nb_of_persons base - 1 do {
      if from_scratch then do {
        cset i Adef.no_consang;
        incr cnt;
      }
      else do {
        let cg = cget i in
        match fget i with
        [ Some ifam -> consang_tab.(Adef.int_of_ifam ifam) := cg
        | None -> () ];
        if cg = Adef.no_consang then incr cnt else ()
      };
    };
    let max_cnt = cnt.val in
    let most = ref None in
    Printf.eprintf "To do: %d persons\n" max_cnt;
    if max_cnt = 0 then ()
    else if quiet then ProgrBar.start ()
    else Printf.eprintf "Computing consanguinity...";
    flush stderr;
    let running = ref True in
    let end_time = Unix.time () +. float tlim in
    while running.val && (tlim < 0 || Unix.time () < end_time) do {
      running.val := False;
      for i = 0 to nb_of_persons base - 1 do {
        if cget i = Adef.no_consang then
          match fget i with
          [ Some ifam ->
              let pconsang = consang_tab.(Adef.int_of_ifam ifam) in
              if pconsang = Adef.no_consang then
                let cpl = foi base ifam in
                let ifath = get_father cpl in
                let imoth = get_mother cpl in
                if cget (Adef.int_of_iper ifath) != Adef.no_consang &&
                   cget (Adef.int_of_iper imoth) != Adef.no_consang
                then do {
                  let consang = relationship base tab ifath imoth in
                  trace quiet cnt.val max_cnt;
                  decr cnt;
                  let cg = Adef.fix_of_float consang in
                  cset i cg;
                  consang_tab.(Adef.int_of_ifam ifam) := cg;
                  if not quiet then
                    let better =
                      match most.val with
                      [ Some m -> cg > cget m
                      | None -> True ]
                    in
                    if better then do {
                      Printf.eprintf "\nMax consanguinity %g for %s... "
                        consang
                        (designation base (poi base (Adef.iper_of_int i)));
                      flush stderr;
                      most.val := Some i
                    }
                    else ()
                  else ()
                }
                else running.val := True
              else do {
                trace quiet cnt.val max_cnt;
                decr cnt;
                cset i pconsang;
              }
          | None ->
              do {
                trace quiet cnt.val max_cnt;
                decr cnt;
                cset i (Adef.fix_of_float 0.0);
              } ]
        else ()
      }
    };
    if max_cnt = 0 then ()
    else if quiet then ProgrBar.finish ()
    else Printf.eprintf " done   \n";
    flush stderr;
  }
  with
  [ Sys.Break -> do { Printf.eprintf "\n"; flush stderr; () } ];
  carray
};
