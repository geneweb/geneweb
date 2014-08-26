(* $Id: consang.ml,v 1.00 2013-09-10 11:03:06 flh Exp $ *)
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

value consang_moins = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_moins/gw/consang";
value consang_plus = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_plus/gw/consang";



(**/**) 


value fname = ref "";
value indexes = ref False;
value scratch = ref False;
value quiet = ref False;
value tlim = ref (-1);

value errmsg = "usage: " ^ Sys.argv.(0) ^ " [options] <file_name>";
value speclist =
  [("-q", Arg.Set quiet, ": quiet mode");
   ("-i", Arg.Set indexes, ": build the indexes again");
   ("-t", Arg.Int (fun i -> tlim.val := i), " <int>: time limit in seconds");
   ("-scratch", Arg.Set scratch, ": from scratch");
   ("-mem", Arg.Set Outbase.save_mem,
    ": Save memory, but slower when rewritting database");
   ("-nolock", Arg.Set Lock.no_lock_flag, ": do not lock database.")]
;
value anonfun s =
  if fname.val = "" then fname.val := s
  else raise (Arg.Bad "Cannot treat several databases")
;

value main () = do {
  Argl.parse speclist anonfun errmsg;
  if fname.val = "" then do {
    Printf.eprintf "Missing file name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2;
  }
  else ();
  let bname = fname.val in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  if is_base_converted bname then 
    let () = Sys.argv.(0) := consang_plus in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
  else 
    let () = Sys.argv.(0) := consang_moins in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
};

main ();
