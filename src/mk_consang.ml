(* camlp4r ./pa_lock.cmo *)
(* $Id: mk_consang.ml,v 4.2 2001-04-23 03:02:38 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

value fname = ref "";
value indexes = ref False;
value scratch = ref False;
value quiet = ref False;

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist =
  [("-q", Arg.Set quiet, ": quiet mode");
   ("-i", Arg.Set indexes, ": build the indexes again");
   ("-scratch", Arg.Set scratch, ": from scratch");
   ("-mem", Arg.Set Iobase.save_mem,
    ": Save memory, but slower when rewritting data base");
   ("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock data base.")]
;
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several data bases")
;

value simple_output bname base =
  let no_patches =
    let bname =
      if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
    in
    not (Sys.file_exists (Filename.concat bname "patches"))
  in
  Iobase.gen_output (no_patches && not indexes.val) bname base
;

value main () =
  do {
    Argl.parse speclist anonfun errmsg;
    if fname.val = "" then do {
      Printf.eprintf "Missing file name\n";
      Printf.eprintf "Use option -help for usage\n";
      flush stderr;
    }
    else ();
    let f () =
      let base = Iobase.input fname.val in
      try
        do {
          Sys.catch_break True;
          try ConsangAll.compute base scratch.val quiet.val with
          [ Sys.Break -> do { Printf.eprintf "\n"; flush stderr; () } ];
          simple_output fname.val base;
        }
      with
      [ Consang.TopologicalSortError p ->
          do {
            Printf.printf
              "\nError: loop in database, %s is his/her own ancestor.\n"
              (Gutil.denomination base p);
            flush stdout;
            exit 2
          } ]
    in
    lock (Iobase.lock_file fname.val) with
    [ Accept -> f ()
    | Refuse ->
        do {
          Printf.eprintf "Base is locked. Waiting... ";
          flush stderr;
          lock_wait (Iobase.lock_file fname.val) with
          [ Accept -> do { Printf.eprintf "Ok\n"; flush stderr; f () }
          | Refuse ->
              do {
                Printf.printf "\nSorry. Impossible to lock base.\n";
                flush stdout;
                exit 2
              } ]
        } ]
  }
;

Printexc.catch main ();
