open Geneweb.Config
open Gwdb

module Templ = V7_templ
module Util = Geneweb.Util
module Hutil = Geneweb.Hutil
module Output = Geneweb.Output

let error_cannot_access conf fname =
  let title _ = Output.print_string conf "Error" in
  Hutil.header conf title;
  Output.print_string conf "<ul>\n";
  Output.print_string conf "<li>\n";
  Output.printf conf "Cannot access file \"%s.txt\".\n" fname;
  Output.print_string conf "</li>\n";
  Output.print_string conf "</ul>\n";
  Hutil.trailer conf

let gen_interp header conf fname ifun env ep =
  Geneweb.Templ_parser.wrap fname begin fun () ->
    match Templ.input_templ conf fname with
    | Some astl ->
      if header then Util.html conf;
      Templ.interp_ast conf ifun env ep astl
    | None -> error_cannot_access conf fname
  end

let templ
  : (?no_headers:bool -> string -> config -> base -> person -> unit) ref
  = ref (fun ?no_headers:_ _ _ _ _ -> assert false)

let templ_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)

let notempl_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)
