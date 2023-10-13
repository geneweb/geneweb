let root = Filename.concat (Filename.get_temp_dir_name ()) ("gwrepl." ^ Data.md5)
let path = Filename.concat root

let mkdir_p ~verbose x =
  if verbose then print_string ("mkdir: " ^ x ^ "...");
  let rec loop x =
    let y = Filename.dirname x in
    if y <> x && String.length y < String.length x then loop y;
    try Unix.mkdir x 0o755 with Unix.Unix_error (_, _, _) -> ()
  in
  loop x;
  if verbose then print_endline "OK!"

let output_file ~verbose (file, contents) =
  if verbose then print_string ("unpacking: " ^ file ^ "...");
  let oc = open_out_bin (path file) in
  output_string oc contents;
  close_out oc;
  if verbose then print_endline "OK!"

let unpack ~force_unpack ~verbose =
  if force_unpack || not (Sys.file_exists root) then (
    Array.iter (fun dir -> mkdir_p ~verbose (path dir)) Data.directories;
    Array.iter (output_file ~verbose) Data.cmas;
    Array.iter (output_file ~verbose) Data.cmis;
    Array.iter (output_file ~verbose) Data.shared)

let run ~ppf ~verbose ~noprompt =
  Clflags.noversion := true;
  Clflags.noinit := true;
  if Array.length Sys.argv <> 1 || noprompt then Clflags.noprompt := true;
  Array.iter
    (fun dir ->
      if verbose then print_endline ("directory: " ^ dir);
      path dir |> Topdirs.dir_directory)
    Data.directories;
  Array.iter
    (fun (file, _) ->
      if verbose then print_endline ("load: " ^ file);
      path file |> Topdirs.dir_load ppf)
    Data.cmas;
  Toploop.loop ppf

(** For script execution, run:
      cat <script.ml> | [ GWREPL_PPF=/dev/null ] [ GWREPL_VERBOSE=1 ] [ GWREPL_FORCE_UNPACK=1 ] [ GWREPL_NOPROMPT=1 ] gwrepl.exe [scrip_arg1] ...
    For interactive toplevel, run:
      gwrepl.exe *)
let () =
  let ppf =
    match Sys.getenv_opt "GWREPL_PPF" with
    | None | Some ("STD" | "std") -> Format.std_formatter
    | Some ("ERR" | "err") -> Format.err_formatter
    | Some path ->
        let oc = open_out path in
        Format.make_formatter (Stdlib.output_substring oc) (fun () ->
            Stdlib.flush oc)
  in
  let verbose = Sys.getenv_opt "GWREPL_VERBOSE" <> None in
  let force_unpack = Sys.getenv_opt "GWREPL_FORCE_UNPACK" <> None in
  let noprompt = Sys.getenv_opt "GWREPL_NOPROMPT" <> None in
  unpack ~force_unpack ~verbose;
  run ~ppf ~verbose ~noprompt
