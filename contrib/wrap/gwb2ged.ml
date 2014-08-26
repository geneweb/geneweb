(* $Id: gwb2ged.ml,v 1.00 2013-09-10 11:13:33 flh Exp $ *)
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

value gwb2ged_moins = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_moins/gw/gwb2ged";
value gwb2ged_plus = Filename.concat (Filename.dirname Sys.argv.(0)) "../../gw_plus/gw/gwb2ged";


(**/**) 


type charset =
  [ Ansel
  | Ascii
  | Utf8 ]
;

value charset = ref Utf8;
value no_notes = ref False;
value no_picture = ref False;

value surnames = ref [];
value no_spouses_parents = ref False;
value censor = ref 0;
value with_siblings = ref False;

value ifile = ref "";
value ofile = ref "a.ged";
value mem = ref False;
value anc_1st = ref "";
value anc_occ = ref 0;
value anc_2nd = ref "";
value desc_1st = ref "";
value desc_occ = ref 0;
value desc_2nd = ref "";

type arg_state =
  [ ASnone | ASwaitAncOcc | ASwaitAncSurn | ASwaitDescOcc | ASwaitDescSurn ]
;
value arg_state = ref ASnone;

value errmsg =
  "Usage: " ^ Sys.argv.(0) ^ " \
<base> [options]
If both options -a and -d are used, intersection is assumed.
If several options -s are used, union is assumed.
Options are:"
;

value speclist =
  [("-charset",
    Arg.String
      (fun x ->
         do {
           arg_state.val := ASnone;
           match x with
           [ "ASCII" -> charset.val := Ascii
           | "ANSEL" -> charset.val := Ansel
           | "UTF-8" -> charset.val := Utf8
           | _ -> raise (Arg.Bad "bad -charset value") ]
         }),
    "[ASCII|ANSEL|UTF-8]: set charset; default is UTF-8.");
   ("-o",
    Arg.String (fun x -> do { ofile.val := x; arg_state.val := ASnone }),
    "<ged>: output file name (default: a.ged)");
   ("-mem",
    Arg.Unit (fun () -> do { mem.val := True; arg_state.val := ASnone }),
    ": save memory space, but slower");
   ("-a",
    Arg.String
      (fun s -> do { anc_1st.val := s; arg_state.val := ASwaitAncOcc }),
    "\"<1st_name>\" [num] \"<surname>\": select ancestors of");
   ("-d",
    Arg.String
      (fun s -> do { desc_1st.val := s; arg_state.val := ASwaitDescOcc }),
    "\"<1st_name>\" [num] \"<surname>\": select descendants of");
   ("-aws",
    Arg.String
      (fun s ->
         do {
           anc_1st.val := s;
           arg_state.val := ASwaitAncOcc;
           with_siblings.val := True;
         }),
    "\"<1st_name>\" [num] \"<surname>\" : select ancestors with siblings");
   ("-s", Arg.String (fun x -> surnames.val := [x :: surnames.val]),
    "\"<surname>\" : select this surname (option usable several times)");
   ("-nsp", Arg.Set no_spouses_parents,
    ": no spouses' parents (for options -s and -d)");
   ("-nn", Arg.Set no_notes, ": no (database) notes");
   ("-nopicture", Arg.Set no_picture, ": Don't extract individual picture.");
   ("-c", Arg.Int (fun i -> censor.val := i), "\
<num> :
     When a person is born less than <num> years ago, it is not exported unless
     it is Public. All the spouses and descendants are also censored.")]
;

value anonfun s =
  match arg_state.val with
  [ ASnone ->
      if ifile.val = "" then ifile.val := s
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
  | ASwaitDescSurn -> do { desc_2nd.val := s; arg_state.val := ASnone } ]
;

value main () = do {
  Argl.parse speclist anonfun errmsg;
  let bname = ifile.val in
  let bname =
    if Filename.check_suffix bname ".gwb" then bname
    else bname ^ ".gwb"
  in
  if is_base_converted bname then 
    let () = Sys.argv.(0) := gwb2ged_plus in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
  else 
    let () = Sys.argv.(0) := gwb2ged_moins in 
    let pid = Unix.create_process Sys.argv.(0) Sys.argv Unix.stdin Unix.stdout Unix.stderr in
    let (_, _) = Unix.waitpid [] pid in
    ()
};

main ();
