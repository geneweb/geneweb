module Parser = Geneweb_templ.Parser
module Ast = Geneweb_templ.Ast
module Fpath = Geneweb_fs.Fpath

let parse_file fl =
  let resolve_include _loc fp = fp in
  Parser.parse ~cached:false ~on_exn:Printexc.raise_with_backtrace
    ~resolve_include (`File fl)

let () =
  let ast = parse_file @@ Fpath.of_string Sys.argv.(1) in
  Fmt.pr "@[%a@]@." Fmt.(list ~sep:semi Ast.pp) ast
