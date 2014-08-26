(* $Id: gwc1.ml,v 1.00 2013-09-10 11:16:35 flh Exp $ *)
(* wrapper *)


value is_base_converted bname =
  let fname = Filename.concat bname "converted" in
  Sys.file_exists fname
(*
  let fname = "mon/nom/de/fichier" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop () where rec loop () =
        match try Some (input_line ic) with [ End_of_file -> None ] with
        [ Some line -> 
            if line = bname then do { close_in ic; True } else loop ()
        | None -> do { close_in ic; False } ]
  | None -> False ]
*)
;

value gwc1_moins = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_moins/gw/gwc1";
value gwc1_plus = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_plus/gw/gwc1";


(**/**) 


value just_comp = ref False;
value out_file = ref (Filename.concat Filename.current_dir_name "a");
value force = ref False;

value separate = ref False;
value shift = ref 0;
value files = ref [];

value speclist =
  [("-c", Arg.Set just_comp, "Only compiling");
   ("-o", Arg.String (fun s -> out_file.val := s),
    "<file> Output database (default: a.gwb)");
   ("-f", Arg.Set force, " Remove database if already existing");
   ("-stats", Arg.Set Db1link.pr_stats, "Print statistics");
   ("-nc", Arg.Clear Db1link.do_check, "No consistency check");
   ("-cg", Arg.Set Db1link.do_consang, "Compute consanguinity");
   ("-sep", Arg.Set separate, " Separate all persons in next file");
   ("-sh", Arg.Int (fun x -> shift.val := x),
    "<int> Shift all persons numbers in next files");
   ("-ds", Arg.String (fun s -> Db1link.default_source.val := s), "\
     <str> Set the source field for persons and families without source data");
   ("-part", Arg.String (fun s -> Db1link.particules_file.val := s), "\
     <file> Particles file (default = predefined particles)");
   ("-mem", Arg.Set Outbase.save_mem, " Save memory, but slower");
   ("-nolock", Arg.Set Lock.no_lock_flag, " do not lock database.");
   ("-nofail", Arg.Set Gwcomp.no_fail, " no failure in case of error.");
   ("-nopicture", Arg.Set Gwcomp.no_picture, " do not create associative pictures"); 
   ("-q", Arg.Clear Mutil.verbose, " no verbose");
   ("-v", Arg.Set Mutil.verbose, " verbose")]
;

value anonfun x =
  let sep = separate.val in
  do {
    if Filename.check_suffix x ".gw" then ()
    else if Filename.check_suffix x ".gwo" then ()
    else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
    separate.val := False;
    files.val := [(x, sep, shift.val) :: files.val]
  }
;

value errmsg =
  "\
Usage: gwc [options] [files]
where [files] are a list of files:
  source files end with .gw
  object files end with .gwo
and [options] are:"
;

value main () = do {
  Argl.parse speclist anonfun errmsg;
  let bname = out_file.val in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  if is_base_converted bname then 
    let () = Sys.argv.(0) := gwc1_plus in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
  else 
    let () = Sys.argv.(0) := gwc1_moins in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
};

main ();
