module Database = Geneweb_db.Database
module Gwdb_gc = Geneweb_db.Db_gc

let dry_run = ref false
let bname = ref ""

let speclist =
  [ ("--dry-run", Arg.Set dry_run, " do not commit changes (only print)") ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let () =
  Arg.parse speclist anonfun usage;
  let bname =
    match !bname with
    | "" ->
        Arg.usage speclist usage;
        exit 1
    | s -> s
  in
  let dry_run = !dry_run in
  Secure.set_base_dir (Filename.dirname bname);
  let lock_file = Mutil.lock_file bname in
  let on_exn exn bt =
    Format.eprintf "%a@." Lock.pp_exception (exn, bt);
    exit 2
  in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  Database.with_database bname @@ fun base ->
  let p, f, s = Gwdb_gc.gc ~dry_run base in
  Printf.printf
    "%s:\n\tnb of persons: %d\n\tnb of families: %d\n\tnb of strings: %d\n"
    bname (List.length p) (List.length f) (List.length s)
