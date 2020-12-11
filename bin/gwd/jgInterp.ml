open Geneweb
open Jingoo

let render ~conf ~file ~models =
  let dir = Util.search_in_lang_path "etc" in
  let env =
    { Jg_types.autoescape = false
    ; template_dirs = [ dir ]
    ; filters = []
    ; extensions = []
    ; strict_mode = true
    }
  in
  let output x = Output.print_string conf @@ Jg_runtime.string_of_tvalue x in
  let ctx =
    let models = fun x -> List.assoc x models in
    Jg_interp.init_context ~env ~models ~output ()
  in
  Jg_interp.from_file ~env ~ctx ~models ~output file
