
type options = {
  base : (string * Gwdb.base) option;
  oc : string * (string -> unit) * (unit -> unit);
}

let speclist (c : options ref) =
  [
    ("-o",
     Arg.String
       (fun s ->
          let oc = open_out s in
          c := { !c with oc = (s, output_string oc, fun () -> close_out oc) }),
     "<GW> output file name (default: stdout)." );
  ]

let default_opts = {
  base = None;
  oc = "", (fun _ -> ()), (fun () -> ());
}

let anonfun c s =
  if !c.base = None then (
    Secure.set_base_dir (Filename.dirname s);
    c := { !c with base = Some (s, Gwdb.open_base s) })
  else raise (Arg.Bad "Cannot treat several databases")

let errmsg = "Usage: " ^ Sys.argv.(0) ^ " <BASE> [OPT]"

let handle_options opts =
  match opts.base with
  | None -> assert false
  | Some (ifile, base) ->
    let _in_dir =
      if Filename.check_suffix ifile ".gwb" then ifile else ifile ^ ".gwb"
    in
    ifile, base

let main () =
  let opts = ref default_opts in
  Arg.parse (speclist opts) (anonfun opts) errmsg;
  let _ifile, base = handle_options !opts in
  Gwdb.set_fpoi_cache base false;
  let diffs = Diff_computation.updates_from_patch base in
  let cmds = List.map (Commands.command_of_person_diff base) diffs in
  let cmds = List.sort Commands.cmp_command cmds in
  List.iter (fun c -> Commands.string_of_command c |> print_endline) cmds;
  ()

let _ = Printexc.catch main ()
