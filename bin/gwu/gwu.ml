open Geneweb
open GwuLib

let speclist =
  ( "-odir", Arg.String (fun s -> GwuLib.out_dir := s)
  , "<dir>  create files from original name in directory (else on -o file)" )
  :: ( "-isolated", Arg.Set GwuLib.isolated
     , " export isolated persons (work only if export all database)." )
  :: ("-old_gw", Arg.Set GwuLib.old_gw
     , " do not export additional fields (for backward compatibility: < 7.00)" )
  :: ( "-raw", Arg.Set GwuLib.raw_output
     , " raw output (without possible utf-8 conversion)" )
  :: ( "-sep", Arg.String (fun s -> GwuLib.separate_list := s :: !GwuLib.separate_list)
     , "<1st_name.num surname> To use together with the option \
        \"-odir\": separate this person and all his ancestors and \
        descendants sharing the same surname. All the \
        concerned families are displayed on standard output instead of their \
        associated files. This option can be used several times." )
  :: ( "-sep_only_file", Arg.String (fun s -> GwuLib.only_file := s)
     , "<file> with option \"-sep\", tells to separate only groups of that file." )
  :: ("-sep_limit", Arg.Int (fun i -> GwuLib.sep_limit := i)
     , "<num> When using the option \"-sep\", groups of families can become isolated \
        in the files. Gwu reconnects them to the separated families (i.e. \
        displays them to standard output) if the size of these groups is less \
        than " ^ string_of_int !GwuLib.sep_limit ^ ". The present option changes this limit.")
  :: Gwexport.speclist
  |> Arg.align

let main () =
  Arg.parse speclist Gwexport.anonfun Gwexport.errmsg ;
  let opts = !Gwexport.opts in
  match opts.base with
  | None -> assert false
  | Some (ifile, base) ->
    let select = Gwexport.select opts [] in
    let in_dir = if Filename.check_suffix ifile ".gwb" then ifile else ifile ^ ".gwb" in
    let src_oc_ht = Hashtbl.create 1009 in
    let () = Gwdb.load_ascends_array base in
    let () = Gwdb.load_strings_array base in
    if not opts.mem then begin
      let () = Gwdb.load_couples_array base in
      let () = Gwdb.load_unions_array base in
      let () = Gwdb.load_descends_array base in ()
    end ;
    let ofile, oc = opts.oc in
    if not !GwuLib.raw_output then Printf.fprintf oc "encoding: utf-8\n";
    if !GwuLib.old_gw then Printf.fprintf oc "\n" else Printf.fprintf oc "gwplus\n\n";
    GwuLib.prepare_free_occ base ;
    GwuLib.gwu opts base in_dir !out_dir src_oc_ht select ;
    Hashtbl.iter (fun _ (oc, _) -> flush oc; close_out oc) src_oc_ht ;
    flush oc ;
    if ofile <> "" then close_out oc

let _ = Printexc.catch main ()
