module Driver = Geneweb_db.Driver

let with_indexes = ref false
let bases_dir = ref "."

let speclist opts =
  ("-bd", Arg.String (fun s -> bases_dir := s), "Bases folder")
  :: ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
  :: Gwexport.speclist opts
  |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  |> Arg.align

let bname = ref ""

let anonfun s =
  if !bname = "" then (
    Secure.set_base_dir (Filename.dirname s);
    bname := s)
  else raise (Arg.Bad "Cannot treat several databases")

let usage = "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] base"

let ansel_warning =
  "Warning: ANSEL charset was administratively withdrawn in 2013. UTF-8 is \
   recommended for new GEDCOM files."

let () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) anonfun usage;
  if !opts.Gwexport.charset = Gwexport.Ansel then
    Printf.eprintf "%s\n%!" ansel_warning;
  if !bname = "" then (
    Arg.usage (speclist opts) usage;
    exit 2);
  let bpath = Filename.concat !bases_dir !bname in
  Driver.with_database bpath @@ fun base ->
  let select = Gwexport.select base !opts [] in
  Gwb2gedLib.gwb2ged base !with_indexes !opts select
