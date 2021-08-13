let root =
  Filename.concat (Filename.get_temp_dir_name ()) ("gwrepl." ^ Data.md5)

let path = Filename.concat root

let mkdir_p ~verbose x =
  if verbose then print_string ("mkdir: " ^ x ^ "...") ;
  let rec loop x =
    let y = Filename.dirname x in
    if y <> x && String.length y < String.length x then loop y;
    try Unix.mkdir x 0o755 with Unix.Unix_error (_, _, _) -> ()
  in
  loop x ;
  if verbose then print_endline "OK!"

let output_file ~verbose (file, contents) =
  if verbose then print_string ("unpacking: " ^ file ^ "...") ;
  let oc = open_out_bin (path file) in
  output_string oc contents ;
  close_out oc ;
  if verbose then print_endline "OK!"

let unpack ~force_unpack ~verbose =
  if force_unpack || not (Sys.file_exists root) then begin
    Array.iter begin fun dir -> mkdir_p ~verbose (path dir) end Data.directories ;
    Array.iter (output_file ~verbose) Data.cmas ;
    Array.iter (output_file ~verbose) Data.cmis ;
    Array.iter (output_file ~verbose) Data.shared ;
  end

let run ~verbose ~noprompt =
  Clflags.noversion := true ;
  Clflags.noinit := true ;
  if Array.length Sys.argv <> 1 || noprompt then Clflags.noprompt := true ;
  Array.iter begin fun dir ->
    if verbose then print_endline ("directory: " ^ dir) ;
    path dir |> Topdirs.dir_directory
  end Data.directories ;
  Array.iter begin fun (file, _) ->
    if verbose then print_endline ("load: " ^ file) ;
    path file |> Topdirs.dir_load Format.std_formatter
  end Data.cmas ;
  Toploop.loop Format.std_formatter

(** For script execution, run:
    cat <script.ml> | [ GWREPL_VERBOSE=1 ] [ GWREPL_FORCE_UNPACK=1 ] [ GWREPL_NOPROMPT=1 ] gwrepl.exe [scrip_arg1] ...  *)
(** For interactive toplevel, run:
    gwrepl.exe *)
let () =
  let verbose = Sys.getenv_opt "GWREPL_VERBOSE" <> None in
  let force_unpack = Sys.getenv_opt "GWREPL_FORCE_UNPACK" <> None in
  let noprompt = Sys.getenv_opt "GWREPL_NOPROMPT" <> None in
  unpack ~force_unpack ~verbose ;
  run ~verbose ~noprompt
