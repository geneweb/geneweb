(* Copyright (c) 1998-2007 INRIA *)

module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Fpath = Geneweb_fs.Fpath

let bpath = ref None
let scratch = ref false
let verbosity = ref 2
let fast = ref false
let errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>"

let speclist =
  [
    ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
    ("-fast", Arg.Set fast, " faster, but use more memory");
    ("-scratch", Arg.Set scratch, " from scratch");
    ( "-mem",
      Arg.Set Geneweb_db.Outbase.save_mem,
      " Save memory, but slower when rewritting database" );
    ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
  ]
  |> List.sort compare |> Arg.align

let raise_bad = Fmt.kstr (fun s -> raise (Arg.Bad s))

let anonfun s =
  match !bpath with
  | None -> bpath := Some s
  | Some _ -> raise_bad "Cannot treat several databases"

let parse_cmd () =
  Arg.parse speclist anonfun errmsg;
  if Option.is_none !bpath then raise_bad "Missing file name."

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  parse_cmd ();
  if !verbosity = 0 then Mutil.verbose := false;
  let bpath = Fpath.of_string @@ Option.get !bpath in
  Secure.set_base_dir (Fpath.dirname bpath);
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
