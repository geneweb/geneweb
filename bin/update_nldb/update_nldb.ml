open Geneweb
module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs

let debug = ref false
let quiet = ref false
let bases_dir = ref None
let set_bases_dir s = bases_dir := Some s

let get_bases_dir () =
  match !bases_dir with
  | Some s -> s
  | None -> Dirs.path Secure.default_base_dir

let parse_cmd () =
  let fname = ref "" in
  let errmsg = Format.sprintf "usage: %s [options] <file_name>" Sys.argv.(0) in

  let speclist =
    [
      ( "-bd",
        Arg.String set_bases_dir,
        Fmt.str
          "<DIR> Specify where the bases directory with databases is installed \
           (default if empty is %S)."
          (Dirs.name Secure.default_base_dir) );
      ("-debug", Arg.Set debug, " Debug mode.");
      ("-q", Arg.Set quiet, " Quiet: no section headers nor page names.");
    ]
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
    |> Arg.align
  in
  let anonfun s =
    if !fname = "" then fname := s
    else raise (Arg.Bad "Cannot treat several databases")
  in
  Arg.parse speclist anonfun errmsg;
  (!fname, get_bases_dir ())

let ( // ) = Filename.concat

let main () =
  let fname, bases_dir = parse_cmd () in
  Secure.set_base_dir bases_dir;
  let bname = bases_dir // fname in
  if fname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2);
  Driver.with_database bname @@ fun base ->
  Sys.catch_break true;
  Driver.load_strings_array base;
  Driver.load_unions_array base;
  try UpdateNldb.compute ~verbose:(not !quiet) ~debug:!debug base bname
  with Sys.Break ->
    Printf.eprintf "\n";
    flush stderr

let _ = Printexc.print main ()
