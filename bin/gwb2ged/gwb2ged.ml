module Driver = Geneweb_db.Driver
module Fpath = Geneweb_fs.Fpath

let with_indexes = ref false

let speclist opts =
  ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
  :: Gwexport.speclist opts
  |> List.sort compare |> Arg.align

let bpath = ref None

let anonfun s =
  if !bpath = None then bpath := Some s
  else raise (Arg.Bad "Cannot treat several databases")

let ansel_warning =
  "Warning: ANSEL charset was administratively withdrawn in 2013. UTF-8 is \
   recommended for new GEDCOM files."

let () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) anonfun Gwexport.errmsg;
  if !opts.Gwexport.charset = Gwexport.Ansel then
    Printf.eprintf "%s\n%!" ansel_warning;
  let bpath = Fpath.of_string @@ Option.get !bpath in
  Secure.set_base_dir (Fpath.dirname bpath);
  Driver.with_database bpath @@ fun base ->
  let select = Gwexport.select base !opts [] in
  Gwb2gedLib.gwb2ged base !with_indexes !opts select
