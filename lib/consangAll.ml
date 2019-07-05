(* Copyright (c) 1998-2007 INRIA *)

open Gwdb

let rec clear_descend_consang base cset mark ifam =
  let des = foi base ifam in
  Array.iter
    (fun ip ->
       if not mark.(Adef.int_of_iper ip) then
         begin
           cset (Adef.int_of_iper ip) Adef.no_consang;
           mark.(Adef.int_of_iper ip) <- true;
           let u = poi base ip in
           Array.iter (clear_descend_consang base cset mark) (get_family u)
         end)
    (get_children des)

let relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab false ip1 ip2)

let trace verbosity cnt max_cnt =
  if verbosity >= 2 then
    begin
      Printf.eprintf "%7d\008\008\008\008\008\008\008" cnt;
      flush stderr
    end
  else if verbosity >= 1 then
    ProgrBar.run (max_cnt - cnt) max_cnt

let compute ?(verbosity = 2) base tlim from_scratch =
  let () = load_ascends_array base in
  let () = load_couples_array base in
  let (fget, cget, cset, carray) = ascends_array base in
  begin try
    let tab =
      let ts = Consang.topological_sort base poi in
      Consang.make_relationship_info base ts
    in
    let consang_tab = Array.make (nb_of_families base) Adef.no_consang in
    let cnt = ref 0 in
    if not from_scratch then
      begin let mark = Array.make (nb_of_persons base) false in
        match patched_ascends base with
          [] -> ()
        | list ->
            List.iter
              (fun ip ->
                 let u = poi base ip in
                 Array.iter (clear_descend_consang base cset mark)
                   (get_family u))
              list
      end;
    for i = 0 to nb_of_persons base - 1 do
      if from_scratch then begin cset i Adef.no_consang; incr cnt end
      else
        let cg = cget i in
        begin match fget i with
          Some ifam -> consang_tab.(Adef.int_of_ifam ifam) <- cg
        | None -> ()
        end;
        if cg = Adef.no_consang then incr cnt
    done;
    let max_cnt = !cnt in
    let most = ref None in
    if verbosity >= 1 then Printf.eprintf "To do: %d persons\n" max_cnt;
    if max_cnt <> 0 then
      if verbosity >= 2 then begin
        Printf.eprintf "Computing consanguinity...";
        flush stderr
      end
      else if verbosity >= 1 then ProgrBar.start () ;
    let running = ref true in
    let end_time = Unix.time () +. float tlim in
    while !running && (tlim < 0 || Unix.time () < end_time) do
      running := false;
      for i = 0 to nb_of_persons base - 1 do
        if cget i = Adef.no_consang then
          match fget i with
            Some ifam ->
              let pconsang = consang_tab.(Adef.int_of_ifam ifam) in
              if pconsang = Adef.no_consang then
                let cpl = foi base ifam in
                let ifath = get_father cpl in
                let imoth = get_mother cpl in
                if cget (Adef.int_of_iper ifath) != Adef.no_consang &&
                   cget (Adef.int_of_iper imoth) != Adef.no_consang
                then
                  let consang = relationship base tab ifath imoth in
                  trace verbosity !cnt max_cnt;
                  decr cnt;
                  let cg = Adef.fix_of_float consang in
                  cset i cg;
                  consang_tab.(Adef.int_of_ifam ifam) <- cg;
                  (if verbosity >= 2 then
                     let better =
                       match !most with
                         Some m -> cg > cget m
                       | None -> true
                     in
                     if better then
                       begin
                         Printf.eprintf "\nMax consanguinity %g for %s... "
                           consang
                           (Gutil.designation base (poi base (Adef.iper_of_int i)));
                         flush stderr;
                         most := Some i
                       end)
                else running := true
              else
                begin trace verbosity !cnt max_cnt; decr cnt; cset i pconsang end
          | None ->
              trace verbosity !cnt max_cnt;
              decr cnt;
              cset i (Adef.fix_of_float 0.0)
      done
    done;
    if max_cnt <> 0 then
      if verbosity >= 2 then begin
        Printf.eprintf " done   \n";
        flush stderr
      end
    else if verbosity >= 1 then ProgrBar.finish ()
  with Sys.Break when verbosity > 0 -> Printf.eprintf "\n"; flush stderr; ()
  end;
  carray
