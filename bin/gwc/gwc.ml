(* Copyright (c) 1998-2007 INRIA *)

module Outbase = Geneweb_db.Outbase

(** Checks a .gwo header and prints fails if header is absent or not compatible.
*)

let check_magic fname ic =
  let b = really_input_string ic (String.length Gwcomp.magic_gwo) in
  if b <> Gwcomp.magic_gwo then
    if String.sub Gwcomp.magic_gwo 0 4 = String.sub b 0 4 then
      failwith ("\"" ^ fname ^ "\" is a GeneWeb object file, but not compatible")
    else
      failwith
        ("\"" ^ fname
       ^ "\" is not a GeneWeb object file, or it is a very old version")

(** [next_family_fun_templ gwo_list fi] creates a function that read sucessivly
    a [Gwcomp.gw_syntax] for all .gwo files. In details it does :

    - Switch to the next element in the [gwo_list] if reached the end of the
      current file. Each element is [(gwo,separate, bnotes, shift)] where [gwo]
      is .gwo filename and [separate], [bnotes], [shift] are captured options
      from command line related to the giving file.
    - Modify [fi] with mentioned previusly information if needed.
    - Start/continue to read current .gwo file content and return
      [Gwcomp.gw_syntax]. [None] is returned when reading of the last .gwo file
      reaches end of file *)
let next_family_fun_templ gwo_list fi =
  let ngwo = List.length gwo_list in
  let run =
    if ngwo < 10 || not !Gwcomp.verbose then fun () -> ()
    else if ngwo < 60 then (fun () ->
      Printf.eprintf ".";
      flush stderr)
    else
      let bar_cnt = ref 0 in
      let run () =
        ProgrBar.run !bar_cnt ngwo;
        incr bar_cnt
      in
      ProgrBar.empty := 'o';
      ProgrBar.full := '*';
      ProgrBar.start ();
      run
  in
  let ic_opt = ref None in
  let gwo_list = ref gwo_list in
  fun () ->
    let rec loop () =
      let r =
        match !ic_opt with
        | Some ic -> (
            match
              try Some (input_value ic : Gwcomp.gw_syntax)
              with End_of_file -> None
            with
            | Some fam -> Some fam
            | None ->
                close_in ic;
                ic_opt := None;
                None)
        | None -> None
      in
      let bnotes_of_string = function
        | "merge" -> `merge
        | "erase" -> `erase
        | "first" -> `first
        | "drop" -> `drop
        | _ -> assert false
      in
      match r with
      | Some fam -> Some fam
      | None -> (
          (* switch to the next .gwo file *)
          match !gwo_list with
          | (x, separate, bnotes, shift) :: rest ->
              run ();
              gwo_list := rest;
              let ic = open_in_bin x in
              check_magic x ic;
              fi.Db1link.f_curr_src_file <- input_value ic;
              fi.Db1link.f_curr_gwo_file <- x;
              fi.Db1link.f_separate <- separate;
              fi.Db1link.f_bnotes <- bnotes_of_string bnotes;
              fi.Db1link.f_shift <- shift;
              Hashtbl.clear fi.Db1link.f_local_names;
              ic_opt := Some ic;
              loop ()
          | [] ->
              if ngwo < 10 || not !Gwcomp.verbose then ()
              else if ngwo < 60 then (
                Printf.eprintf "\n";
                flush stderr)
              else ProgrBar.finish ();
              None)
    in
    loop ()

let just_comp = ref false
let in_file = ref ""
let separate = ref false
let bnotes = ref "merge"
let shift = ref 0
let files = ref []
let kill_gwo = ref false

let speclist =
  [
    ( "-bd",
      Arg.String Secure.set_base_dir,
      "<DIR> Specify where the “bases” directory with databases is installed \
       (default if empty is “.”)." );
    ( "-bnotes",
      Arg.Set_string bnotes,
      " [drop|erase|first|merge] Behavior for base notes of the next file. \
       [drop]: dropped. [erase]: erase the current content. [first]: dropped \
       if current content is not empty. [merge]: concatenated to the current \
       content. Default: " ^ !bnotes ^ "" );
    ("-c", Arg.Set just_comp, " Only compiling");
    ("-cg", Arg.Set Db1link.do_consang, " Compute consanguinity");
    ( "-ds",
      Arg.Set_string Db1link.default_source,
      "<str> Set the source field for persons and families without source data"
    );
    ("-f", Arg.Set Geneweb.GWPARAM.force, " Remove database if already existing");
    ("-gwo", Arg.Set kill_gwo, " Suppress .gwo files after base creation");
    ("-mem", Arg.Set Outbase.save_mem, " Save memory, but slower");
    ("-nc", Arg.Clear Db1link.do_check, " No consistency check");
    ("-nofail", Arg.Set Gwcomp.no_fail, " No failure in case of error");
    ("-nolock", Arg.Set Lock.no_lock_flag, " Do not lock database");
    ( "-nopicture",
      Arg.Set Gwcomp.no_picture,
      " Do not create associative pictures" );
    ( "-o",
      Arg.Set_string Gwcomp.out_file,
      "<file> Output database (default: <input file name>.gwb, a.gwb if not \
       available). Alphanumerics and -" );
    ( "-particles",
      Arg.Set_string Db1link.particules_file,
      "<file> Particles file (default = predefined particles)" );
    ("-q", Arg.Clear Gwcomp.verbose, " Quiet");
    ("-reorg", Arg.Set Geneweb.GWPARAM.reorg, " Mode reorg");
    ("-rgpd", Arg.String (fun s -> Gwcomp.rgpd_dir := s), "<dir> Rgpd directory");
    ("-sep", Arg.Set separate, " Separate all persons in next file");
    ("-sh", Arg.Set_int shift, "<int> Shift all persons numbers in next files");
    ("-stats", Arg.Set Db1link.pr_stats, " Print statistics");
    ("-v", Arg.Set Gwcomp.verbose, " Verbose");
  ]
  |> List.sort compare |> Arg.align

let anonfun x =
  let bn = !bnotes in
  let sep = !separate in
  if Filename.check_suffix x ".gw" then in_file := x
  else if Filename.check_suffix x ".gwo" then in_file := x
  else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
  separate := false;
  bnotes := "merge";
  files := (x, sep, bn, !shift) :: !files

let errmsg =
  "Usage: gwc [options] [files]\n\
   where [files] are a list of files:\n\
  \  source files end with .gw\n\
  \  object files end with .gwo\n\
   and [options] are:"

let main () =
  (try
     if Sys.is_directory !Gwcomp.rgpd_dir then Gwcomp.rgpd := true
     else Gwcomp.rgpd := false
   with Sys_error _ ->
     Printf.eprintf "Warning: failed testing for %s\n" !Gwcomp.rgpd_dir;
     Gwcomp.rgpd := false);
  Arg.parse speclist anonfun errmsg;
  if not (Array.mem "-bd" Sys.argv) then Secure.set_base_dir ".";
  in_file :=
    if !in_file <> "" then
      Filename.remove_extension (Filename.basename !in_file)
    else !in_file;
  if List.length !files > 1 && not (Array.mem "-o" Sys.argv) then (
    Printf.eprintf "The database name must be specified with -o\n";
    flush stdout;
    exit 2);
  if !in_file <> "" && not (Array.mem "-o" Sys.argv) then
    Gwcomp.out_file := !in_file;
  if not (Mutil.good_name (Filename.basename !Gwcomp.out_file)) then (
    (* Util.transl conf not available !*)
    Printf.eprintf "The database name \"%s\" contains a forbidden character.\n"
      !Gwcomp.out_file;
    Printf.eprintf "Allowed characters: a..z, A..Z, 0..9, -\n";
    flush stderr;
    exit 2);
  let bname = Filename.remove_extension (Filename.basename !Gwcomp.out_file) in
  Geneweb.GWPARAM.init bname;
  let dist_etc_d = Filename.concat (Filename.dirname Sys.argv.(0)) "etc" in
  if !Db1link.particules_file = "" then
    Db1link.particules_file := Filename.concat dist_etc_d "particles.txt";

  if !Gwcomp.rgpd then
    Printf.eprintf "Rgpd status: True, files in: %s\n" !Gwcomp.rgpd_dir
  else Printf.eprintf "Rgpd status: False\n";
  let gwo = ref [] in
  List.iter
    (fun (x, separate, bnotes, shift) ->
      if Filename.check_suffix x ".gw" then (
        (try Gwcomp.comp_families x
         with e ->
           Printf.eprintf "File \"%s\", line %d:\n" x !Gwcomp.line_cnt;
           raise e);
        gwo := (x ^ "o", separate, bnotes, shift) :: !gwo)
      else if Filename.check_suffix x ".gwo" then
        gwo := (x, separate, bnotes, shift) :: !gwo
      else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\"")))
    (List.rev !files);
  if not !just_comp then (
    let bdir = !Geneweb.GWPARAM.bpath bname in
    (* test_base will fail if base exists and force has not been set (-f) *)
    Geneweb.GWPARAM.test_base bname;
    Geneweb.GWPARAM.init_etc bname;
    let lock_file = Mutil.lock_file bdir in
    let on_exn exn bt =
      Format.eprintf "%a@." Lock.pp_exception (exn, bt);
      exit 2
    in
    Lock.control ~on_exn ~wait:false ~lock_file (fun () ->
        let next_family_fun = next_family_fun_templ (List.rev !gwo) in
        if Db1link.link next_family_fun bdir then (
          Geneweb.Util.print_default_gwf_file bname;
          if !kill_gwo then
            List.iter
              (fun (x, _separate, _bnotes, _shift) ->
                if Sys.file_exists (x ^ "o") then Mutil.rm (x ^ "o"))
              (List.rev !files))
        else (
          Printf.eprintf "*** database not created\n";
          flush stderr;
          exit 2)))

let _ = main ()
