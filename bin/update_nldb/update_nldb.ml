open Geneweb
module Driver = Geneweb_db.Driver
module Dirs = Geneweb_dirs

let bases_dir = ref None
let set_bases_dir s = bases_dir := Some s

let get_bases_dir () =
  match !bases_dir with
  | Some s -> s
  | None -> Dirs.path Secure.default_bases_dir

let parse_cmd () =
  let bname = ref "" in
  let errmsg = Format.sprintf "usage: %s [options] <file_name>" Sys.argv.(0) in

  let speclist =
    [
      ( "-bd",
        Arg.String set_bases_dir,
        Fmt.str
          "<DIR> Specify where the bases directory with databases is installed \
           (default if empty is %S)."
          (Dirs.name Secure.default_bases_dir) );
    ]
    |> Arg.align
  in
  let anonfun s =
    if !bname = "" then bname := GWPARAM.bname_of_arg ~prog:"update_nldb" s
    else raise (Arg.Bad "Cannot treat several databases")
  in
  Arg.parse speclist anonfun errmsg;
  (!bname, get_bases_dir ())

let main () =
  let bname, bases_dir = parse_cmd () in
  Secure.set_bases_dir bases_dir;
  if bname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "Use option -help for usage\n";
    flush stderr;
    exit 2);
  let bpath = GWPARAM.bpath bname in
  Driver.with_database bpath @@ fun base ->
  Sys.catch_break true;
  try Notes.rebuild_links_db ~verbose:true base bpath
  with Sys.Break ->
    Printf.eprintf "\n";
    flush stderr

let _ = Printexc.print main ()
