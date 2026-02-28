module Parser = Geneweb_templ.Parser
module Ast = Geneweb_templ.Ast

let parse_file fl =
  let resolve_include _loc s = s in
  Parser.parse ~cached:false ~on_exn:Printexc.raise_with_backtrace
    ~resolve_include (`File fl)

let () =
  let ast = parse_file Sys.argv.(1) in
  Fmt.pr "@[%a@]@." Fmt.(list ~sep:semi Ast.pp) ast
