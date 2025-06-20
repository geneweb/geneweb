module Html = Tyxml.Html
module Ast = Geneweb_tlsw.Ast
module Parser = Geneweb_tlsw.Parser
module Loc = Geneweb_tlsw.Loc
module Compat = Geneweb_compat

let on_err ~loc err =
  Fmt.pr "Syntax error: %s@ %a@." err Loc.pp_with_input loc;
  exit 1

let tlsw_to_html name s =
  let l = Parser.parse ~on_err s |> List.map Ast.to_html in
  Html.(
    html
      (head (title (txt name)) [])
      (body l))
    [@ocamlformat "disable"]

let () =
  let name = Sys.argv.(1) in
  let s = Compat.In_channel.with_open_text name Compat.In_channel.input_all in
  let html = tlsw_to_html name s in
  Format.printf "%a@." Html.(pp ~indent:true ()) html
