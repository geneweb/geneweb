(* Copyright (c) 1998-2007 INRIA *)

module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection
module Gutil = Geneweb_db.Gutil

(* let rec clear_descend_consang base consang mark ifam =
 *   let des = foi base ifam in
 *   Array.iter
 *     (fun ip ->
 *        if not (Collection.Marker.get mark ip) then
 *          begin
 *            consang ip Adef.no_consang;
 *            Collection.Marker.set mark ip true ;
 *            let u = poi base ip in
 *            Array.iter (clear_descend_consang base consang mark) (get_family u)
 *          end)
 *     (get_children des) *)

let relationship base tab ip1 ip2 =
  fst (Consang.relationship_and_links base tab false ip1 ip2)

let trace verbosity cnt max_cnt =
  if verbosity >= 2 then (
    Printf.eprintf "%7d\008\008\008\008\008\008\008" cnt;
    flush stderr)
  else if verbosity >= 1 then ProgrBar.run (max_cnt - cnt) max_cnt

let consang_array base =
  let patched = ref false in
  let fget i = Driver.get_parents @@ Driver.poi base i in
  let cget i = Driver.get_consang @@ Driver.poi base i in
  let cset i v =
    patched := true;
    Driver.patch_ascend base i
      Def.
        { (Driver.gen_ascend_of_person @@ Driver.poi base i) with consang = v }
  in
  (fget, cget, cset, patched)

let compute ?(verbosity = 2) base from_scratch =
  Driver.load_ascends_array base;
  Driver.load_couples_array base;
  let fget, cget, cset, patched = consang_array base in
  (try
     let tab =
       let ts = Consang.topological_sort base Driver.poi in
       Consang.make_relationship_info base ts
     in
     let persons = Driver.ipers base in
     let families = Driver.ifams base in
     let consang_tab = Driver.ifam_marker families Adef.no_consang in
     let cnt = ref 0 in
     (* FIXME *)
     (* if not from_scratch then
      *   begin
      *     let mark = Collection.Marker.make (Geneweb_db.Collection.length persons) false in
      *     List.iter
      *       (fun ip ->
      *          let u = poi base ip in
      *          Array.iter (clear_descend_consang base cset mark) (get_family u))
      *       (patched_ascends base)
      *   end; *)
     Collection.iter
       (fun i ->
         if from_scratch then (
           cset i Adef.no_consang;
           incr cnt)
         else
           let cg = cget i in
           Option.iter
             (fun ifam -> Collection.Marker.set consang_tab ifam cg)
             (fget i);
           if cg = Adef.no_consang then incr cnt)
       persons;
     (* number of persons which need consanguinity to be computed *)
     let max_cnt = !cnt in
     let most = ref None in
     if verbosity >= 1 then Printf.eprintf "To do: %d persons\n" max_cnt;
     if max_cnt <> 0 then
       if verbosity >= 2 then (
         Printf.eprintf "Computing consanguinity...";
         flush stderr)
       else if verbosity >= 1 then ProgrBar.start ();
     let running = ref true in
     while !running do
       running := false;
       Collection.iter
         (fun i ->
           (* if person's consanguinity wasn't calculated *)
           if cget i = Adef.no_consang then
             match fget i with
             (* if person has parents *)
             | Some ifam ->
                 let pconsang = Collection.Marker.get consang_tab ifam in
                 (* if parent's family's consanguinity wasn't calculated *)
                 if pconsang = Adef.no_consang then
                   let cpl = Driver.foi base ifam in
                   let ifath = Driver.get_father cpl in
                   let imoth = Driver.get_mother cpl in
                   (* if parent's consanguinity was calculated *)
                   if
                     cget ifath != Adef.no_consang
                     && cget imoth != Adef.no_consang
                   then (
                     let consang = relationship base tab ifath imoth in
                     trace verbosity !cnt max_cnt;
                     decr cnt;
                     let cg = Adef.fix_of_float consang in
                     cset i cg;
                     Collection.Marker.set consang_tab ifam cg;
                     if verbosity >= 2 then
                       if
                         match !most with Some m -> cg > cget m | None -> true
                       then (
                         Printf.eprintf "\nMax consanguinity %g for %s... "
                           consang
                           (Gutil.designation base (Driver.poi base i));
                         flush stderr;
                         most := Some i)
                     (* if it wasn't makes further another run over persons *))
                   else running := true
                   (* if it was then set to person his family's consanguinity *)
                 else (
                   trace verbosity !cnt max_cnt;
                   decr cnt;
                   cset i pconsang)
             (* if he doesn't then set his consanguinity to 0 *)
             | None ->
                 trace verbosity !cnt max_cnt;
                 decr cnt;
                 cset i (Adef.fix_of_float 0.0))
         persons
     done;
     if max_cnt <> 0 then
       if verbosity >= 2 then (
         Printf.eprintf " done   \n";
         flush stderr)
       else if verbosity >= 1 then ProgrBar.finish ()
   with Sys.Break when verbosity > 0 ->
     Printf.eprintf "\n";
     flush stderr;
     ());
  if !patched then Driver.commit_patches base;
  !patched
