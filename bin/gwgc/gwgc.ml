module Database = Geneweb_db.Database
module Gwdb_gc = Geneweb_db.Db_gc
module Fpath = Geneweb_fs.Fpath

let dry_run = ref false
let bdir = ref ""

let speclist =
  [ ("--dry-run", Arg.Set dry_run, " do not commit changes (only print)") ]

let anonfun i = bdir := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let () =
  Arg.parse speclist anonfun usage;
  let bdir =
    match !bdir with
    | "" ->
        Arg.usage speclist usage;
        exit 1
    | s -> Fpath.of_string s
  in
  let dry_run = !dry_run in
  Secure.set_base_dir (Fpath.dirname bdir);
  let lock_file = Mutil.lock_file bdir in
  let on_exn exn bt =
    Format.eprintf "%a@." Lock.pp_exception (exn, bt);
    exit 2
  in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  Database.with_database bdir @@ fun base ->
  let p, f, s = Gwdb_gc.gc ~dry_run base in
  Fmt.pr "%a:\n\tnb of persons: %d\n\tnb of families: %d\n\tnb of strings: %d@."
    Fpath.pp bdir (List.length p) (List.length f) (List.length s)
