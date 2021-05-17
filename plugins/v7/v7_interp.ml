open Geneweb.Config
open Geneweb.Gwdb

let templ
  : (?no_headers:bool -> string -> config -> base -> person -> unit) ref
  = ref (fun ?no_headers _ _ _ _ -> assert false)

let templ_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)

let notempl_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)
