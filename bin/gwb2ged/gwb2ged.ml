open Geneweb
let with_indexes = ref false

let speclist =
  ( "-indexes", Arg.Set with_indexes, " export indexes in gedcom" )
  :: Gwexport.speclist
  |> Arg.align

let main () =
  Arg.parse speclist Gwexport.anonfun Gwexport.errmsg ;
  let select = Gwexport.select !Gwexport.opts [] in
  Gwb2gedLib.gwb2ged !with_indexes !Gwexport.opts select

let _ = Printexc.catch main ()
