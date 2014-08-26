(* $Id: gwu.ml,v 1.00 2013-09-10 11:23:55 flh Exp $ *)
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

value gwu_moins = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_moins/gw/gwu";
value gwu_plus = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_plus/gw/gwu";


(**/**)


value raw_output = ref False;
value no_picture = ref False;
value isolated = ref False;
value old_gw = ref False;

value surnames = ref [];
value no_spouses_parents = ref False;
value no_notes = ref False;
value censor = ref 0;
value with_siblings = ref False;
value maxlev = ref (-1);

value sep_limit = ref 21;
value only_file = ref "";
value separate_list = ref [];

value in_file = ref "";
value out_file = ref "";
value out_dir = ref "";
value anc_1st = ref "";
value anc_occ = ref 0;
value anc_2nd = ref "";
value desc_1st = ref "";
value desc_occ = ref 0;
value desc_2nd = ref "";
value ancdesc_1st = ref "";
value ancdesc_occ = ref 0;
value ancdesc_2nd = ref "";

type arg_state =
  [ ASnone
  | ASwaitAncOcc
  | ASwaitAncSurn
  | ASwaitDescOcc
  | ASwaitDescSurn
  | ASwaitAncdescOcc
  | ASwaitAncdescSurn ]
;
value arg_state = ref ASnone;
value mem = ref False;

value speclist =
  [("-o", Arg.String (fun s -> out_file.val := s),
    "<file>    output file name (else stdout)");
   ("-odir", Arg.String (fun s -> out_dir.val := s),
    "<dir>  create files from original name in directory (else on -o file)");
   ("-mem", Arg.Set mem, "        save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do { anc_1st.val := s; arg_state.val := ASwaitAncOcc }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors of...");
   ("-d",
    Arg.String
      (fun s -> do { desc_1st.val := s; arg_state.val := ASwaitDescOcc }),
    "\"<1st_name>\" [num] \"<surname>\" : select descendants of...");
   ("-ad",
    Arg.String
      (fun s ->
         do { ancdesc_1st.val := s; arg_state.val := ASwaitAncdescOcc }),
    "\
\"<1st_name>\" [num] \"<surname>\" : select ancestors of...
    and all their descendants (has no effect if -a and/or -d used,
    option -nsp is forced).");
   ("-aws",
    Arg.String
      (fun s ->
         do {
           anc_1st.val := s;
           arg_state.val := ASwaitAncOcc;
           with_siblings.val := True;
           ()
         }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-maxlev", Arg.Int (fun i -> maxlev.val := i),
    "\"<level>\" : maximum level of generations of descendants");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for options -s and -d)");
   ("-nn", Arg.Set no_notes, ": no (database) notes");
   ("-nopicture", Arg.Set no_picture, ": Don't extract individual picture.");
   ("-isolated", Arg.Set isolated, ": Export isolated persons (work only if export all database).");
   ("-c", Arg.Int (fun i -> censor.val := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.");
   ("-raw", Arg.Set raw_output,
    "raw output (without possible utf-8 conversion)");
   ("-old_gw", Arg.Set old_gw, "Do not export additional fields \
(for backward compatibility)");
   ("-v", Arg.Set Mutil.verbose, "verbose");
   ("-sep",
    Arg.String (fun s -> separate_list.val := [s :: separate_list.val]), "\
\"1st_name.num surname\" :
     To use together with the option \"-odir\": separate this person and
     all his ancestors and descendants sharing the same surname. All the
     concerned families are displayed on standard output instead of their
     associated files. This option can be used several times.");
   ("-sep_only_file", Arg.String (fun s -> only_file.val := s), "\
<file> :
     With option \"-sep\", tells to separate only groups of that file.");
   ("-sep_limit", Arg.Int (fun i -> sep_limit.val := i), "\
<num> :
     When using the option \"-sep\", groups of families can become isolated
     in the files. Gwu reconnects them to the separated families (i.e.
     displays them to standard output) if the size of these groups is less
     than " ^ string_of_int sep_limit.val ^ "\
. The present option changes this limit.")]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if in_file.val = "" then in_file.val := s
      else raise (Arg.Bad "Cannot treat several databases")
  | ASwaitAncOcc ->
      try
        do { anc_occ.val := int_of_string s; arg_state.val := ASwaitAncSurn }
      with
      [ Failure _ ->
          do { anc_occ.val := 0; anc_2nd.val := s; arg_state.val := ASnone } ]
  | ASwaitAncSurn -> do { anc_2nd.val := s; arg_state.val := ASnone }
  | ASwaitDescOcc ->
      try
        do {
          desc_occ.val := int_of_string s; arg_state.val := ASwaitDescSurn
        }
      with
      [ Failure _ ->
          do {
            desc_occ.val := 0; desc_2nd.val := s; arg_state.val := ASnone
          } ]
  | ASwaitDescSurn -> do { desc_2nd.val := s; arg_state.val := ASnone }
  | ASwaitAncdescOcc ->
      try
        do {
          ancdesc_occ.val := int_of_string s;
          arg_state.val := ASwaitAncdescSurn
        }
      with
      [ Failure _ ->
          do {
            ancdesc_occ.val := 0;
            ancdesc_2nd.val := s;
            arg_state.val := ASnone
          } ]
  | ASwaitAncdescSurn ->
      do { ancdesc_2nd.val := s; arg_state.val := ASnone } ]
;

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
[options] <base_file>
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"
;


value main () = do {
  Argl.parse speclist anonfun errmsg;
  let bname = in_file.val in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  if is_base_converted bname then 
    let () = Sys.argv.(0) := gwu_plus in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
  else 
    let () = Sys.argv.(0) := gwu_moins in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
};

main ();
