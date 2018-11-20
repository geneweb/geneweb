open Geneweb

module SelroyGwu = GwuLib.Make(SelroySelect)

let _ = SelroyGwu.run_main ()
