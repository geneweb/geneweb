(* Copyright (c) 1998-2007 INRIA *)

module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs
module Gutil = Geneweb_db.Gutil

let bname = ref None
let scratch = ref false
let verbosity = ref 2
let fast = ref false
let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"
let bases_dir = ref (Dirs.path Secure.default_base_dir)

let speclist =
  [
    ("-bd", Arg.String (fun s -> bases_dir := s), "Bases folder");
    ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
    ("-fast", Arg.Set fast, " faster, but use more memory");
    ("-scratch", Arg.Set scratch, " from scratch");
    ( "-mem",
      Arg.Set Geneweb_db.Outbase.save_mem,
      " Save memory, but slower when rewritting database" );
    ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
  ]
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  |> Arg.align

let anonfun s =
  match !bname with
  | None -> bname := Some s
  | _ -> raise (Arg.Bad "Cannot treat several databases")

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Arg.parse speclist anonfun errmsg;
  match !bname with
  | None ->
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage@.";
      exit 2
  | _ -> ();
  if !verbosity = 0 then Mutil.verbose := false;
  let bpath = Filename.concat !bases_dir (Option.value ~default:"" !bname) in

  Secure.set_base_dir (Filename.dirname bpath);
  let lock_file = Mutil.lock_file bpath in
  let on_exn exn bt =
    Logs.err (fun k -> k "%a" Lock.pp_exception (exn, bt));
    exit 2
  in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  Driver.with_database bpath (fun base ->
      if !fast then (
        Driver.load_persons_array base;
        Driver.load_families_array base;
        Driver.load_ascends_array base;
        Driver.load_unions_array base;
        Driver.load_couples_array base;
        Driver.load_descends_array base;
        Driver.load_strings_array base);
      try
        Sys.catch_break true;
        if ConsangAll.compute ~verbosity:!verbosity base !scratch then
          Driver.sync base
      with Consang.TopologicalSortError p ->
        Logs.err (fun k ->
            k "Loop in database, %s is his/her own ancestor."
              (Gutil.designation base p));
        exit 2)
