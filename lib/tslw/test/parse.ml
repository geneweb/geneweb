module Html = Tyxml.Html
module Ast = Geneweb_tslw.Ast
module Parser = Geneweb_tslw.Parser

let tslw_to_html name s =
  let l = Parser.parse s |> List.map Ast.to_html in
  Html.(
    html
      (head (title (txt name)) [])
      (body l))
    [@ocamlformat "disable"]

let () =
  let name = Sys.argv.(1) in
  let s = In_channel.with_open_text name In_channel.input_all in
  let html = tslw_to_html name s in
  Format.printf "%a@." Html.(pp ~indent:true ()) html
