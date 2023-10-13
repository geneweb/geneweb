(* Copyright (c) 1998-2007 INRIA *)

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
    ("-scratch", Arg.Set scratch, ": from scratch");
    ( "-mem",
      Arg.Set Outbase.save_mem,
      ": Save memory, but slower when rewritting database" );
    ("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database.");
  ]

let anonfun s =
  if !fname = "" then fname := s
  else raise (Arg.Bad "Cannot treat several databases")

let main () =
  Arg.parse speclist anonfun errmsg;
  if !fname = "" then (
    Printf.eprintf "Missing file name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2);
  if !verbosity = 0 then Mutil.verbose := false;
  Secure.set_base_dir (Filename.dirname !fname);
  Lock.control_retry (Mutil.lock_file !fname) ~onerror:Lock.print_error_and_exit
    (fun () ->
      let base = Gwdb.open_base !fname in
      if !fast then (
        Gwdb.load_persons_array base;
        Gwdb.load_families_array base;
        Gwdb.load_ascends_array base;
        Gwdb.load_unions_array base;
        Gwdb.load_couples_array base;
        Gwdb.load_descends_array base;
        Gwdb.load_strings_array base);
      try
        Sys.catch_break true;
        if ConsangAll.compute ~verbosity:!verbosity base !scratch then
          Gwdb.sync base
      with Consang.TopologicalSortError p ->
        Printf.printf "\nError: loop in database, %s is his/her own ancestor.\n"
          (Gutil.designation base p);
        flush stdout;
        exit 2)

let _ = Printexc.print main ()
