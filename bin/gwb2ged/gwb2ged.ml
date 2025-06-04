module Driver = Geneweb_db.Driver

let with_indexes = ref false

let speclist opts =
  ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
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
  match !bname with
  | None -> raise @@ Arg.Bad "Expect a database"
  | Some bname ->
      Driver.with_database bname @@ fun base ->
      let select = Gwexport.select base !opts [] in
      Gwb2gedLib.gwb2ged base !with_indexes !opts select
