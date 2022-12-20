let isolated = ref false

let speclist opts =
  ( "-odir",
    Arg.String (fun s -> Gwu_lib.out_dir := s),
    "<dir>  create files from original name in directory (else on -o file)" )
  :: ( "-isolated",
       Arg.Set isolated,
       " export isolated persons (work only if export all database)." )
  :: ( "-old_gw",
       Arg.Set Gwu_lib.old_gw,
       " do not export additional fields (for backward compatibility: < 7.00)"
     )
  :: ( "-raw",
       Arg.Set Gwu_lib.raw_output,
       " raw output (without possible utf-8 conversion)" )
  :: ( "-sep",
       Arg.String
         (fun s -> Gwu_lib.separate_list := s :: !Gwu_lib.separate_list),
       "<1st_name.num surname> To use together with the option \"-odir\": \
        separate this person and all his ancestors and descendants sharing the \
        same surname. All the concerned families are displayed on standard \
        output instead of their associated files. This option can be used \
        several times." )
  :: ( "-sep_only_file",
       Arg.String (fun s -> Gwu_lib.only_file := s),
       "<file> with option \"-sep\", tells to separate only groups of that \
        file." )
  :: ( "-sep_limit",
       Arg.Int (fun i -> Gwu_lib.sep_limit := i),
       "<num> When using the option \"-sep\", groups of families can become \
        isolated in the files. Gwu reconnects them to the separated families \
        (i.e. displays them to standard output) if the size of these groups is \
        less than "
       ^ string_of_int !Gwu_lib.sep_limit
       ^ ". The present option changes this limit." )
  :: Gwexport.speclist opts
  |> Arg.align

let main () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) (Gwexport.anonfun opts) Gwexport.errmsg;
  let opts = !opts in
  Gwu_lib.gwu_simple ~export_isolated:!isolated opts

let _ = Printexc.catch main ()
