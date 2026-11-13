(* Open Gwdb *)
open Gwdb

let bname = ref ""
let fast = ref false
let cutoff_year = ref (None : int option)
let whitelist_file = ref ""
let blacklist_file = ref ""
let no_consang = ref false
let verbosity = ref 2

let speclist =
  [
    ( "-y",
      Arg.Int (fun y -> cutoff_year := Some y),
      "<year> Cutoff year for public/private date estimation." );
    ( "--whitelist",
      Arg.String (fun s -> whitelist_file := s),
      "<file> File containing whitelist person keys." );
    ( "--blacklist",
      Arg.String (fun s -> blacklist_file := s),
      "<file> File containing blacklist person keys." );
    ("--no-consang", Arg.Set no_consang, " Skip consanguinity calculation.");
    ("-fast", Arg.Set fast, " Fast mode (load arrays in memory).");
    ("-q", Arg.Unit (fun () -> verbosity := 1), " Quiet mode.");
    ("-qq", Arg.Unit (fun () -> verbosity := 0), " Very quiet mode.");
    ("-nolock", Arg.Set Lock.no_lock_flag, " Do not lock database.");
  ]

let anonfun s =
  if !bname = "" then bname := s
  else raise (Arg.Bad "Cannot process multiple databases.")

let usage = "Usage: gwresync [options] <base>"

let main () =
  Arg.parse (Arg.align speclist) anonfun usage;
  if !bname = "" then (
    Printf.eprintf "Missing database name\n";
    Printf.eprintf "%s\n" (Arg.usage_string speclist usage);
    flush stderr;
    exit 2);
  Secure.set_base_dir (Filename.dirname !bname);
  Lock.control_retry (Files.lock_file !bname) ~onerror:Lock.print_error_and_exit
    (fun () ->
      let base = Gwdb.open_base !bname in
      if !verbosity >= 1 then (
        Printf.printf "Base opened: %s\n" (Gwdb.bname base);
        flush stdout))

let () = main ()
