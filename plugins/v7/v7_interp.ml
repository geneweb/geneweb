open Geneweb.Config
open Geneweb.Gwdb

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
  let v = !(Templ.template_file) in
  Templ.template_file := fname;
  begin try
      match Templ.input_templ conf fname with
      | Some astl ->
        if header then Util.html conf;
        let full_name = Util.etc_file_name conf fname in
        Templ.interp_ast conf ifun env ep
          (Templ.begin_end_include conf full_name astl)
      | None -> error_cannot_access conf fname
    with e -> Templ.template_file := v; raise e
  end;
  Templ.template_file := v

let templ
  : (?no_headers:bool -> string -> config -> base -> person -> unit) ref
  = ref (fun ?no_headers:_ _ _ _ _ -> assert false)

let templ_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)

let notempl_with_menu
  : ( (bool -> unit) -> string -> config -> base -> person -> unit) ref
  = ref (fun _ _ _ _ _ -> assert false)
