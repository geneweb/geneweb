module Parser = Geneweb_templ.Parser
module Ast = Geneweb_templ.Ast
module Compat = Geneweb_compat

let parse_file fl =
  let resolve_include _loc s = s in
  Parser.parse ~cached:false ~on_exn:Printexc.raise_with_backtrace
    ~resolve_include (`File fl)

(* HOTFIX: dune 3.24 changed the way it handles path. The latest versions
   introduces systematically a leading dot and the previous version trim it.
   This fix can be removed after a while. *)
let normalize_path s =
  if Compat.String.starts_with ~prefix:"./" s then s else "./" ^ s

let () =
  let path = normalize_path Sys.argv.(1) in
  let ast = parse_file path in
  Fmt.pr "@[%a@]@." Fmt.(list ~sep:semi Ast.pp) ast
