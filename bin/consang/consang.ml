(* Copyright (c) 1998-2007 INRIA *)

module Outbase = Geneweb_db.Outbase
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let fname = ref ""
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

let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

let () =
  Arg.parse speclist anonfun errmsg;
  if !fname = "" then (
    Printf.eprintf "Missing file name\n";
    Printf.eprintf "Use option -help for usage@.";
    exit 2);
  if !verbosity = 0 then Mutil.verbose := false;
  Secure.set_base_dir (Filename.dirname !fname);
  let lock_file = Mutil.lock_file !fname in
  let on_exn exn bt =
    Format.eprintf "%a@." Lock.pp_exception (exn, bt);
    exit 2
  in
  Lock.control ~on_exn ~wait:true ~lock_file @@ fun () ->
  Driver.with_database !fname (fun base ->
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
        Printf.printf "\nError: loop in database, %s is his/her own ancestor.\n"
          (Gutil.designation base p);
        flush stdout;
        exit 2)
