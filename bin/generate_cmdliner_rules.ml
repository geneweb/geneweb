module Fs = Filesystem

let ( // ) = Filename.concat
let pp_rule ppf s = Format.fprintf ppf "(cmdliner-support/share/%s as %s)" s s

let () =
  Sys.chdir @@ (Sys.argv.(1) // "share");
  let files =
    Filesystem.walk_folder ~recursive:true
      (fun e acc -> match e with File s -> s :: acc | Dir _ | Exn _ -> acc)
      "." []
  in
  let pp_sep ppf () = Format.fprintf ppf "@," in
  Format.printf "@[(%a)@]" Format.(pp_print_list ~pp_sep pp_rule) files
