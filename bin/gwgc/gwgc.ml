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
  Lock.control (Mutil.lock_file bname) true ~onerror:Lock.print_try_again
  @@ fun () ->
  let base = Database.opendb bname in
  let p, f, s = Gwdb_gc.gc ~dry_run base in
  Printf.printf
    "%s:\n\tnb of persons: %d\n\tnb of families: %d\n\tnb of strings: %d\n"
    bname (List.length p) (List.length f) (List.length s)
