module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs

let raise_bad fmt = Format.kasprintf (fun s -> raise (Arg.Bad s)) fmt

let parse_cmd () =
  let with_indexes = ref false in
  let bname = ref None in
  let opts = ref Gwexport.default_opts in
  let speclist opts =
    ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
    :: Gwexport.speclist opts
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
    |> Arg.align
  in
  let ansel_warning =
    "Warning: ANSEL charset was administratively withdrawn in 2013. UTF-8 is \
     recommended for new GEDCOM files."
  in
  let anonfun s =
    match !bname with
    | None -> bname := Some s
    | Some _ -> raise_bad "Cannot treat several databases"
  in
  let usage = "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] base" in
  Arg.parse (speclist opts) anonfun usage;
  if !opts.Gwexport.charset = Gwexport.Ansel then
    Printf.eprintf "%s\n%!" ansel_warning;
  let bname =
    match !bname with
    | None -> raise_bad "a database name is mandatory"
    | Some s ->
        if not @@ Mutil.good_name s then
          raise_bad "%s is not a valid database name" s;
        s
  in
  (bname, !opts, !with_indexes)

let ( // ) = Filename.concat

let () =
  let bname, opts, with_indexes = parse_cmd () in
  Secure.set_bases_dir opts.bases_dir;
  let oc, name, close =
    if !Gwexport.out_file = "" then (stdout, "", fun () -> flush stdout)
    else
      let path = Gwexport.resolve_out_file opts in
      let oc = open_out path in
      (oc, path, fun () -> close_out oc)
  in
  let opts = { opts with Gwexport.oc = (name, output_string oc, close) } in
  Driver.with_database (opts.bases_dir // bname) @@ fun base ->
  let select = Gwexport.select base opts [] in
  Gwb2gedLib.gwb2ged base with_indexes opts select
