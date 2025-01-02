let add b f l = if b then f :: l else l

let check ~dry_run ~verbosity ~fast ~f_parents ~f_children ~p_parents
    ~p_families ~p_NBDS ~pevents_witnesses ~fevents_witnesses ~marriage_divorce
    ~invalid_utf8 ~key bname =
  let v1 = !verbosity >= 1 in
  let v2 = !verbosity >= 2 in
  if not v1 then Geneweb_util.Mutil.verbose := false;
  let fast = !fast in
  let base = Gwdb.open_base bname in
  let fix = ref 0 in
  let nb_fam = Gwdb.nb_of_families base in
  let nb_ind = Gwdb.nb_of_persons base in
  if fast then (
    Gwdb.load_strings_array base;
    Gwdb.load_persons_array base);
  let person_fixes = [] in
  let family_fixes = [] in
  let family_fixes =
    add !f_parents Geneweb.Fixbase.fix_family_parents family_fixes
  in
  let family_fixes =
    add !f_children Geneweb.Fixbase.fix_family_children family_fixes
  in
  let person_fixes =
    add !p_parents Geneweb.Fixbase.fix_person_parents person_fixes
  in
  let person_fixes = add !p_NBDS Geneweb.Fixbase.fix_nbds person_fixes in
  let person_fixes =
    add !p_families Geneweb.Fixbase.fix_person_unions person_fixes
  in
  let person_fixes =
    add !pevents_witnesses Geneweb.Fixbase.fix_person_events_witnesses
      person_fixes
  in
  let family_fixes =
    add !fevents_witnesses Geneweb.Fixbase.fix_family_events_witnesses
      family_fixes
  in
  let family_fixes =
    add !marriage_divorce Geneweb.Fixbase.fix_family_divorce family_fixes
  in
  let person_fixes =
    add !invalid_utf8 Geneweb.Fixbase.fix_person_utf8_sequence person_fixes
  in
  let family_fixes =
    add !invalid_utf8 Geneweb.Fixbase.fix_family_utf8_sequence family_fixes
  in
  let person_fixes =
    add !key (Geneweb.Fixbase.fix_person_key base) person_fixes
  in
  let person_fixes = List.rev person_fixes in
  let family_fixes = List.rev family_fixes in

  let i' = ref 0 in
  let cnt = ref 0 in
  let progress =
    if v2 then (fun i n ->
      Geneweb_util.ProgrBar.run i n;
      i' := i)
    else if v1 then Geneweb_util.ProgrBar.run
    else fun _ _ -> ()
  in
  let report =
    if v2 then
      Some
        (fun s ->
          incr cnt;
          Geneweb_util.ProgrBar.suspend ();
          print_endline @@ "\t" ^ Geneweb.Fixbase.string_of_patch base s;
          flush stdout;
          Geneweb_util.ProgrBar.restart !i' (nb_ind + nb_fam))
    else Some (fun _ -> incr cnt)
  in

  if v1 then Geneweb_util.ProgrBar.start ();
  fix :=
    Geneweb.Fixbase.perform_fixes ~report ~progress ~base ~person_fixes
      ~family_fixes;
  if v1 then Geneweb_util.ProgrBar.finish ();
  if fast then (
    Gwdb.clear_strings_array base;
    Gwdb.clear_persons_array base);
  if not !dry_run then (
    if !fix <> 0 then (
      Gwdb.commit_patches base;
      if v1 then (
        Printf.printf "%n changes commited\n" !fix;
        flush stdout))
    else if v1 then (
      Printf.printf "No change\n";
      flush stdout);
    if v1 then (
      Printf.printf "Rebuilding the indexes..\n";
      flush stdout);
    Gwdb.sync ~save_mem:false
      ~tasks:[ (fun () -> Caches.write_caches base) ]
      base;
    if v1 then (
      Printf.printf "Done";
      flush stdout))

(**/**)

let bname = ref ""
let verbosity = ref 2
let fast = ref false
let f_parents = ref false
let f_children = ref false
let p_parents = ref false
let p_families = ref false
let p_NBDS = ref false
let pevents_witnesses = ref false
let fevents_witnesses = ref false
let marriage_divorce = ref false
let invalid_utf8 = ref false
let key = ref false
let index = ref false
let dry_run = ref false

let speclist =
  [
    ("-dry-run", Arg.Set dry_run, " do not commit changes (only print)");
    ("-q", Arg.Unit (fun () -> verbosity := 1), " quiet mode");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " very quiet mode");
    ("-fast", Arg.Set fast, " fast mode. Needs more memory.");
    ("-families-parents", Arg.Set f_parents, " missing doc");
    ("-families-children", Arg.Set f_children, " missing doc");
    ("-persons-NBDS", Arg.Set p_NBDS, " missing doc");
    ("-persons-parents", Arg.Set p_parents, " missing doc");
    ("-persons-families", Arg.Set p_families, " missing doc");
    ("-pevents-witnesses", Arg.Set pevents_witnesses, " missing doc");
    ("-fevents-witnesses", Arg.Set fevents_witnesses, " missing doc");
    ("-marriage-divorce", Arg.Set marriage_divorce, " missing doc");
    ("-person-key", Arg.Set key, " missing doc");
    ( "-index",
      Arg.Set index,
      " rebuild index. It is automatically enable by any other option." );
    ("-invalid-utf8", Arg.Set invalid_utf8, " missing doc");
  ]

let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION] base"

let main () =
  Arg.parse speclist anonfun usage;
  Secure.set_base_dir (Filename.dirname !bname);
  if !bname = "" then (
    Arg.usage speclist usage;
    exit 2);
  Geneweb_util.Lock.control (Files.lock_file !bname) false ~onerror:Geneweb_util.Lock.print_try_again
  @@ fun () ->
  if
    !f_parents || !f_children || !p_parents || !p_families || !pevents_witnesses
    || !fevents_witnesses || !marriage_divorce || !p_NBDS || !invalid_utf8
    || !key || !index
  then ()
  else (
    f_parents := true;
    f_children := true;
    p_parents := true;
    p_families := true;
    pevents_witnesses := true;
    fevents_witnesses := true;
    marriage_divorce := true;
    p_NBDS := true;
    invalid_utf8 := true;
    key := true);
  check ~dry_run ~fast ~verbosity ~f_parents ~f_children ~p_NBDS ~p_parents
    ~p_families ~pevents_witnesses ~fevents_witnesses ~marriage_divorce
    ~invalid_utf8 ~key !bname

let () = main ()
