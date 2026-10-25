(* anoma: write the anomalies report of an existing base, without rebuilding
   it, and merge the pending verifications.

   gwc and ged2gwb write the same report while building a base; anoma
   refreshes it for a base edited through gwd since (a cron job on a large
   base, or the gwsetup page). gwd shows it with m=ANOMA. *)

open Geneweb
module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs

let usage =
  "Usage: anoma [options] <base>\n\n\
   Check the base, write <base>.gwb/config/anomalies.jsonl, then merge the \
   pending verifications.\n\n\
   Options:"

let () =
  let bname = ref "" in
  let history_days = ref Anomaly.default_history_days in
  let fold_only = ref false in
  let speclist =
    Arg.align
      [
        ( "-bd",
          Arg.String Secure.set_base_dir,
          Printf.sprintf "<DIR> bases directory (default %S)"
            (Dirs.path Secure.default_base_dir) );
        ( "-hist",
          Arg.Set_int history_days,
          Printf.sprintf
            "<DAYS> report who last modified each person within this many days \
             (default %d; 0: none)"
            Anomaly.default_history_days );
        ("-fold", Arg.Set fold_only, " only merge the pending verifications");
      ]
  in
  Arg.parse speclist
    (fun s ->
      if !bname = "" then bname := s
      else raise (Arg.Bad ("unexpected argument " ^ s)))
    usage;
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  let bname = Filename.remove_extension (Filename.basename !bname) in
  let bdir = Filename.concat (Secure.base_dir ()) (bname ^ ".gwb") in
  if not (Sys.file_exists bdir) then (
    Printf.eprintf "anoma: no base %s\n" bdir;
    exit 2);
  (if not !fold_only then
     let anomalies =
       Driver.with_database
         (Filename.concat (Secure.base_dir ()) bname)
         Anomaly.collect
     in
     Anomaly.write ~history_days:!history_days ~bdir ~producer:"anoma" anomalies);
  let r = AnomalyWhitelist.fold (GWPARAM.base_config_dir bname) in
  Printf.eprintf "%d verification files merged, %d verified anomalies\n%!"
    r.fragments r.entries
