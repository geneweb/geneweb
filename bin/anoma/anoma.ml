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
  let fold_only = ref false in
  let no_check = ref false in
  let speclist =
    Arg.align
      [
        ( "-bd",
          Arg.String Secure.set_bases_dir,
          Printf.sprintf "<DIR> bases directory (default %S)"
            (Dirs.path Secure.default_bases_dir) );
        ("-fold", Arg.Set fold_only, " only merge the pending verifications");
        ( "-no-check",
          Arg.Set no_check,
          " do not scan the sources for [[w:ident]] signatures (much faster; \
           responsibility then comes from the history only)" );
      ]
  in
  Arg.parse speclist
    (fun s ->
      if !bname = "" then bname := GWPARAM.bname_of_arg ~prog:"anoma" s
      else raise (Arg.Bad ("unexpected argument " ^ s)))
    usage;
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  let bname = !bname in
  let bpath = GWPARAM.bpath bname in
  if not (Sys.file_exists bpath) then (
    Printf.eprintf "anoma: no base %s\n" bpath;
    exit 2);
  (if not !fold_only then
     let verbose = Unix.isatty Unix.stderr in
     let anomalies =
       Driver.with_database
         (Filename.concat (Secure.bases_dir ()) bname)
         (fun base ->
           let anomalies = Anomaly.collect ~verbose base in
           Anomaly.write ~verbose ~signatures:(not !no_check) ~base ~bpath
             ~producer:"anoma" anomalies;
           anomalies)
     in
     Printf.eprintf "%d anomalies written to %s\n%!" (List.length anomalies)
       (Anomaly.file (GWPARAM.base_config_dir bname)));
  let r = AnomalyWhitelist.fold (GWPARAM.base_config_dir bname) in
  Printf.eprintf "%d verification files merged, %d verified anomalies\n%!"
    r.fragments r.entries
