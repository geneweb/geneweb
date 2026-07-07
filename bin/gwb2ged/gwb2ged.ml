module Driver = Geneweb_db.Driver

let with_indexes = ref false

let speclist opts =
  ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
  :: Gwexport.speclist opts
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  |> Arg.align

let bname = ref None

let anonfun s =
  match !bname with
  | None -> bname := Some s
  | Some _ -> raise (Arg.Bad "Cannot treat several databases")

let usage = "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] base"

let ansel_warning =
  "Warning: ANSEL charset was administratively withdrawn in 2013. UTF-8 is \
   recommended for new GEDCOM files."

let () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) anonfun usage;
  let bases_dir = !Gwexport.bases_dir in
  Secure.set_base_dir bases_dir;
  if !opts.Gwexport.charset = Gwexport.Ansel then
    Printf.eprintf "%s\n%!" ansel_warning;
  match !bname with
  | None ->
      Arg.usage (speclist opts) usage;
      exit 2
  | Some bname ->
      let oc, name, close =
        if !Gwexport.out_file = "" then (stdout, "", fun () -> flush stdout)
        else
          let path = Gwexport.resolve_out_file () in
          let oc = open_out path in
          (oc, path, fun () -> close_out oc)
      in
      opts := { !opts with Gwexport.oc = (name, output_string oc, close) };
      Driver.with_database (Filename.concat bases_dir bname) @@ fun base ->
      let select = Gwexport.select base !opts [] in
      Gwb2gedLib.gwb2ged base !with_indexes !opts select
