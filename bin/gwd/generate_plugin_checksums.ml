module Plugin = Geneweb_plugin
module Fpath = Geneweb_fs.Fpath
module File = Geneweb_fs.File

let () =
  let path = Sys.argv.(1) in
  let plugins = Sys.readdir path in
  let checksums =
    Array.fold_left
      (fun acc dir ->
        ( Filename.basename dir,
          Plugin.checksum (Fpath.of_string (Filename.concat path dir)) )
        :: acc)
      [] plugins
  in
  let pp_checksum ppf (dir, sum) = Format.fprintf ppf "(%S, %S)" dir sum in
  let pp_sep ppf () = Format.fprintf ppf ";@, " in
  Format.printf
    {|@[
  let checksums = [
    %a
    ]@]|}
    (Format.pp_print_list ~pp_sep pp_checksum)
    checksums
