open GwuLib

let isolated = ref false

let speclist opts =
  ( "-odir",
    Arg.String (fun s -> GwuLib.out_dir := s),
    "<dir>  create files from original name in directory (else on -o file)" )
  :: ( "-isolated",
       Arg.Set isolated,
       " export isolated persons (work only if export all database)." )
  :: ( "-old_gw",
       Arg.Set GwuLib.old_gw,
       " do not export additional fields (for backward compatibility: < 7.00)"
     )
  :: ( "-raw",
       Arg.Set GwuLib.raw_output,
       " raw output (without possible utf-8 conversion)" )
  :: ( "-sep",
       Arg.String (fun s -> GwuLib.separate_list := s :: !GwuLib.separate_list),
       "<1st_name.num surname> To use together with the option \"-odir\": \
        separate this person and all his ancestors and descendants sharing the \
        same surname. All the concerned families are displayed on standard \
        output instead of their associated files. This option can be used \
        several times." )
  :: ( "-sep_only_file",
       Arg.String (fun s -> GwuLib.only_file := s),
       "<file> with option \"-sep\", tells to separate only groups of that \
        file." )
  :: ( "-sep_limit",
       Arg.Int (fun i -> GwuLib.sep_limit := i),
       "<num> When using the option \"-sep\", groups of families can become \
        isolated in the files. Gwu reconnects them to the separated families \
        (i.e. displays them to standard output) if the size of these groups is \
        less than "
       ^ string_of_int !GwuLib.sep_limit
       ^ ". The present option changes this limit." )
  :: Gwexport.speclist opts
  |> Arg.align

let main () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) (Gwexport.anonfun opts) Gwexport.errmsg;
  let opts = !opts in
  match opts.Gwexport.base with
  | None -> assert false
  | Some (ifile, base) ->
      let select = Gwexport.select opts [] in
      let in_dir =
        if Filename.check_suffix ifile ".gwb" then ifile else ifile ^ ".gwb"
      in
      let src_oc_ht = Hashtbl.create 1009 in
      Gwdb.load_ascends_array base;
      Gwdb.load_strings_array base;
      if not opts.Gwexport.mem then (
        Gwdb.load_couples_array base;
        Gwdb.load_unions_array base;
        Gwdb.load_descends_array base;
        ());
      let _ofile, oc, close = opts.Gwexport.oc in
      if not !GwuLib.raw_output then oc "encoding: utf-8\n";
      if !GwuLib.old_gw then oc "\n" else oc "gwplus\n\n";
      GwuLib.prepare_free_occ base;
      GwuLib.gwu opts !isolated base in_dir !out_dir src_oc_ht select;
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht;
      close ()

let _ = main ()
