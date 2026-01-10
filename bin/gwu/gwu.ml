open GwuLib
module Driver = Geneweb_db.Driver

let isolated = ref false

let speclist opts =
  ( "-odir",
    Arg.String (fun s -> GwuLib.out_dir := s),
    "<dir> create files from original name in directory (else on -o file)" )
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
       " <1st_name.num surname> To use together with the option \"-odir\": \
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
  :: ( "-all_files",
       Arg.Set GwuLib.all_files,
       " save all content of notes_d in the .gw file, including files without \
        Wiki links. Default is no." )
  :: Gwexport.speclist opts
  |> List.sort compare |> Arg.align

let bname = ref None

let anonfun s =
  if !bname = None then (
    Secure.set_base_dir (Filename.dirname s);
    bname := Some s)
  else raise (Arg.Bad "Cannot treat several databases")

let () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) anonfun Gwexport.errmsg;
  let opts = !opts in
  match !bname with
  | None -> raise @@ Arg.Bad "Expect a database"
  | Some bname ->
      Driver.with_database bname @@ fun base ->
      let select = Gwexport.select base opts [] in
      let in_dir =
        if Filename.check_suffix bname ".gwb" then bname else bname ^ ".gwb"
      in
      let src_oc_ht = Hashtbl.create 1009 in
      Driver.load_ascends_array base;
      Driver.load_strings_array base;
      if not opts.Gwexport.mem then (
        Driver.load_couples_array base;
        Driver.load_unions_array base;
        Driver.load_descends_array base;
        ());
      let _ofile, oc, close = opts.Gwexport.oc in
      if not !GwuLib.raw_output then oc "encoding: utf-8\n";
      if !GwuLib.old_gw then oc "\n" else oc "gwplus\n\n";
      GwuLib.prepare_free_occ base;
      GwuLib.gwu opts !isolated base in_dir !out_dir src_oc_ht select;
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht;
      close ()
