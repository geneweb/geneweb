let add_extension path ext = Format.sprintf "%s%s" path ext

let pp_rule ppf path =
  let target_gzip = add_extension path ".gz" in
  let target_brotli = add_extension path ".br" in
  Format.fprintf ppf
    {|
(rule
  (alias install)
  (package geneweb)
  (targets %s %s)
  (deps (:input %s))
  (action
    (concurrent
      (run gzip -9 -k -f %%{input})
      (run brotli -q 11 %%{input}))))@?|}
    target_gzip target_brotli path

let pp_newline ppf () = Format.fprintf ppf "\n"

let () =
  let files =
    Array.to_list @@ Sys.readdir Sys.argv.(1)
    |> List.filter (fun f ->
        match Filename.extension f with ".css" | ".js" -> true | _ -> false)
    |> List.sort String.compare
  in
  Format.printf "%a@." (Format.pp_print_list ~pp_sep:pp_newline pp_rule) files
