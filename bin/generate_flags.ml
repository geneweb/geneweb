let pp_space ppf () = Format.fprintf ppf " "
let pp_string ppf s = Format.fprintf ppf "%s" s

let () =
  let cclib =
    match (Sys.getenv_opt "LINKING_MODE", Sys.argv.(1)) with
    | None, _ -> []
    | Some "static", "Unix" -> [ "-cclib"; "-static"; "-cclib"; "-no-pie" ]
    | _, _ -> failwith "Static linkage not supported for this operating system"
  in
  Format.printf "(%a)@." (Format.pp_print_list ~pp_sep:pp_space pp_string) cclib
