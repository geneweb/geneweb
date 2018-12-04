(* Copyright (c) 1998-2007 INRIA *)

open Geneweb
open Gwcomp


let f fi =
  { Gwclib.before = begin fun () -> () end
  ; onread = begin fun v x separate shift ->
      fi.Db1link.f_curr_src_file <- v ;
      fi.Db1link.f_curr_gwo_file <- x ;
      fi.Db1link.f_separate <- separate ;
      fi.Db1link.f_shift <- shift ;
      Hashtbl.clear fi.Db1link.f_local_names
    end
  ; onclose = begin fun _ch -> () end
  }

let out_file = ref (Filename.concat Filename.current_dir_name "a")

let force = ref false

let speclist =
  [ ("-cg", Arg.Set Db1link.do_consang, "Compute consanguinity")
  ; ("-ds", Arg.String (fun s -> Db1link.default_source := s), "<str> Set the source field for persons and families without source data")
  ; ("-f", Arg.Set force, " Remove database if already existing")
  ; ("-mem", Arg.Set Outbase.save_mem, " Save memory, but slower")
  ; ("-nc", Arg.Clear Db1link.do_check, "No consistency check")
  ; ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.")
  ; ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.")
  ; ("-nopicture", Arg.Set Gwcomp.no_picture, " do not create associative pictures")
  ; ("-o", Arg.String (fun s -> out_file := s), "<file> Output database (default: a.gwb)")
  ; ("-part", Arg.String (fun s -> Db1link.particules_file := s), "<file> Particles file (default = predefined particles)" )
  ; ("-q", Arg.Clear Mutil.verbose, " no verbose")
  ; ("-stats", Arg.Set Db1link.pr_stats, "Print statistics")
  ; ("-v", Arg.Set Mutil.verbose, " verbose")
  ]

let main () =
  let main = Gwclib.main speclist in
  let bdir =
    if Filename.check_suffix !out_file ".gwb" then !out_file
    else !out_file ^ ".gwb"
  in
  if not !force && Sys.file_exists bdir then
    begin
      Printf.printf "The database \"%s\" already exists. \
                     Use option -f to overwrite it."
        !out_file;
      flush stdout;
      exit 2
    end;
  Secure.set_base_dir (Filename.dirname !out_file);
  Lock.control (Mutil.lock_file !out_file) false ~onerror:Lock.print_error_and_exit @@ fun () ->
  if not (main f (fun f -> Db1link.link f bdir))
  then begin
    Printf.eprintf "*** database not created\n";
    flush stderr;
    exit 2
  end

let () =
  try main ()
  with
  | Failure txt -> Printf.printf "Failed: %s\n" txt; flush stdout; exit 2
  | exc -> Printexc.print raise exc
