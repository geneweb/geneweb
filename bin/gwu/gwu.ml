open GwuLib
module Driver = Geneweb_db.Driver

let isolated = ref false
let bases_dir = ref "."

let speclist opts =
  ( "-odir",
    Arg.String (fun s -> GwuLib.out_dir := s),
    "<dir> create files from original name in directory (else on -o file)" )
  :: ( "-bd",
       Arg.String
         (fun s ->
           if !bases_dir = "." then bases_dir := s
           else if !bases_dir <> Filename.dirname s then
             raise (Arg.Bad "-bd conflicts with base path (1)")),
       " defines bases folder" )
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
  :: Gwexport.speclist opts
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  |> Arg.align

let bname = ref None

let anonfun s =
  if !bname = None then (
    bname := Some (Filename.basename s);
    if s <> Option.value ~default:"" !bname then
      if !bases_dir = "." then bases_dir := Filename.dirname s
      else if !bases_dir <> Filename.dirname s then
        raise (Arg.Bad "-bd conflicts with base path (2)"))
  else raise (Arg.Bad "Cannot treat several databases")

let () =
  flush stderr;
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) anonfun Gwexport.errmsg;
  Printf.eprintf "Start gwu, out_file: %s, bname: %s\n" !Gwexport.out_file
    (Option.value ~default:"" !bname);
  let oc, name, close =
    if !Gwexport.out_file = "" then
      let bname = Option.value ~default:"" !bname in
      let path = Filename.concat !bases_dir (bname ^ ".gw") in
      let oc = open_out path in
      (oc, bname, fun () -> close_out oc)
    else
      let path = !Gwexport.out_file in
      let oc = open_out path in
      (oc, !Gwexport.out_file, fun () -> close_out oc)
  in
  opts := { !opts with oc = (name, output_string oc, close) };
  let opts = !opts in
  Secure.set_base_dir !bases_dir;
  match !bname with
  | None -> raise @@ Arg.Bad "Expect a database"
  | Some bname ->
      Printf.eprintf "S_dir: %s, B_dir: %s, bname: %s\n" (Secure.base_dir ())
        !bases_dir bname;
      let in_dir =
        let full = Filename.concat (Secure.base_dir ()) bname in
        if Filename.check_suffix full ".gwb" then full else full ^ ".gwb"
      in
      Driver.with_database in_dir @@ fun base ->
      let select = Gwexport.select base opts [] in
      let src_oc_ht = Hashtbl.create 1009 in
      Driver.load_ascends_array base;
      Driver.load_strings_array base;
      if not opts.Gwexport.mem then (
        Driver.load_couples_array base;
        Driver.load_unions_array base;
        Driver.load_descends_array base;
        ());
      (if opts.Gwexport.test then
         let iper_filter, ifam_filter = select in
         let nb_persons =
           Geneweb_db.Collection.fold
             (fun count i ->
               if iper_filter i then (
                 Printf.eprintf "P: %s\n"
                   (Geneweb_db.Gutil.designation base (Driver.poi base i));
                 count + 1)
               else count)
             0 (Driver.ipers base)
         in
         let nb_families =
           Geneweb_db.Collection.fold
             (fun count i ->
               if ifam_filter i then (
                 let father = Driver.get_father (Driver.foi base i) in
                 let mother = Driver.get_mother (Driver.foi base i) in
                 Printf.eprintf "F: %s & %s\n"
                   (Geneweb_db.Gutil.designation base (Driver.poi base father))
                   (Geneweb_db.Gutil.designation base (Driver.poi base mother));
                 count + 1)
               else count)
             0 (Driver.ifams base)
         in
         Printf.eprintf "Exporting %d persons and %d families\n" nb_persons
           nb_families);
      let _ofile, oc, close = opts.Gwexport.oc in
      if not !GwuLib.raw_output then oc "encoding: utf-8\n";
      if !GwuLib.old_gw then oc "\n" else oc "gwplus\n\n";
      GwuLib.prepare_free_occ base;
      GwuLib.gwu opts !isolated base in_dir !out_dir src_oc_ht select;
      Hashtbl.iter (fun _ (_, _, close) -> close ()) src_oc_ht;
      close ()
