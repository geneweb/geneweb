let with_indexes = ref false

let speclist opts =
  ("-indexes", Arg.Set with_indexes, " export indexes in gedcom")
  :: Gwexport.speclist opts
  |> Arg.align

let main () =
  let opts = ref Gwexport.default_opts in
  Arg.parse (speclist opts) (Gwexport.anonfun opts) Gwexport.errmsg;
  let opts = !opts in
  let select = Gwexport.select opts [] in
  Gwb2gedLib.gwb2ged !with_indexes opts select

let _ = main ()
