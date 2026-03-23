(* Copyright (c) 1998-2007 INRIA *)
module Compat = Geneweb_compat
module GWPARAM = Geneweb.GWPARAM
module Dirs = Geneweb_dirs

type kind = Gw | Gwo

type input = {
  fname : string;
  kind : kind;
  separate : bool;
  bnotes : Db1link.bnotes;
  shift : int;
}

let failwith fmt = Format.ksprintf failwith fmt

(* Checks a .gwo header and prints fails if header is absent or not
   compatible. *)
let check_magic ~fname ic =
  let len = String.length Gwcomp.magic_gwo in
  let magic = really_input_string ic len in
  if String.compare magic Gwcomp.magic_gwo <> 0 then
    if String.sub Gwcomp.magic_gwo 0 4 = String.sub magic 0 4 then
      failwith
        "%S is a GeneWeb object file but not compatible with the current \
         GeneWeb version."
        fname
    else
      failwith "%S is not a GeneWeb object file or it is a very old version."
        fname

let open_gwo_file file_info (fname, separate, bnotes, shift, _was_gw) =
  let ic = open_in_bin fname in
  check_magic ~fname ic;
  file_info.Db1link.f_curr_src_file <- input_value ic;
  file_info.Db1link.f_curr_gwo_file <- fname;
  file_info.Db1link.f_separate <- separate;
  file_info.Db1link.f_bnotes <- bnotes;
  file_info.Db1link.f_shift <- shift;
  Hashtbl.clear file_info.Db1link.f_local_names;
  ic

let iterator_gwo_file file_info input =
  let ic = open_gwo_file file_info input in
  let next () =
    match (input_value ic : Gwcomp.gw_syntax) with
    | exception End_of_file -> None
    | v -> Some v
  in
  (ic, next)

(* FIXME: File descriptor could leak if an exception is raised by the code
   using this iterator. This is not a big issue as these exceptions are
   not catch in the current code and file descriptors are released by the
   operating system at the end of the current process. *)
let iterator_gwo_files bar inputs file_info =
  let queue = Queue.of_seq @@ List.to_seq inputs in
  let curr = ref (iterator_gwo_file file_info @@ Queue.pop queue) in
  let len = Queue.length queue in
  let cnt = ref 0 in
  let rec next () =
    let ic, it = !curr in
    match it () with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        close_in_noerr ic;
        Printexc.raise_with_backtrace exn bt
    | Some _ as v -> v
    | None -> (
        close_in ic;
        match Queue.pop queue with
        | exception Queue.Empty -> None
        | gwo ->
            incr cnt;
            ProgrBar.progress bar !cnt len;
            curr := iterator_gwo_file file_info gwo;
            next ())
  in
  next

let generate_database ~no_warn ~bdir bar gwo_files =
  let iterator = iterator_gwo_files bar gwo_files in
  match Db1link.link ~no_warn iterator bdir with
  | exception Db1link.Critical_import_error msg ->
      Fmt.epr "CRITICAL ERROR: %s@." msg;
      exit 2
  | true -> ()
  | false ->
      Fmt.epr "Database not created.@.";
      exit 2

let compile_gw_files ~bname bar inputs =
  let len = List.length inputs in
  List.mapi
    (fun i { fname; kind; separate; bnotes; shift } ->
      let fname, was_gw =
        match kind with
        | Gw ->
            let output = Format.sprintf "%so" (Filename.basename fname) in
            let () =
              try Gwcomp.compile ~bname ~output fname
              with e ->
                let bt = Printexc.get_raw_backtrace () in
                Printf.eprintf "File %S, line %d:\n" fname !Gwcomp.line_cnt;
                Printexc.raise_with_backtrace e bt
            in
            (output, true)
        | Gwo -> (fname, false)
      in
      ProgrBar.progress bar i len;
      (fname, separate, bnotes, shift, was_gw))
    inputs

let base_dir = ref None
let set_base_dir s = base_dir := Some s
let just_comp = ref false
let kill_gwo = ref false
let no_warn = ref false
let ngrams_arg = ref None
let prog = ref false
let default_separate = false
let separate = ref default_separate
let default_bnotes = Db1link.Merge
let bnotes = ref default_bnotes
let raise_bad fmt = Format.ksprintf (fun s -> raise (Arg.Bad s)) fmt

let bnotes_to_string b =
  match b with
  | Db1link.Merge -> "merge"
  | Erase -> "erase"
  | First -> "first"
  | Drop -> "drop"

let parse_bnotes s =
  match s with
  | "merge" -> Db1link.Merge
  | "erase" -> Erase
  | "first" -> First
  | "drop" -> Drop
  | _ -> raise_bad "Unexpected bnote value %S" s

let set_bnotes s = bnotes := parse_bnotes s
let shift = ref 0
let rev_inputs : input list ref = ref []
let add_input i = rev_inputs := i :: !rev_inputs
let output : string option ref = ref None
let set_output s = output := Some s

(* Parse -ngrams argument: <bi>[,<tri>[,<quad>]] *)
let parse_ngrams s =
  match String.split_on_char ',' s with
  | [ bi_str ] ->
      let bi = int_of_string bi_str in
      let tri = max 10 (bi / 25) in
      let quad = max 5 (bi / 50) in
      (bi, tri, quad)
  | [ bi_str; tri_str ] ->
      let bi = int_of_string bi_str in
      let tri = int_of_string tri_str in
      let quad = max 5 (tri / 2) in
      (bi, tri, quad)
  | [ bi_str; tri_str; quad_str ] ->
      (int_of_string bi_str, int_of_string tri_str, int_of_string quad_str)
  | _ -> raise_bad "Invalid -ngrams format. Use: <bi>[,<tri>[,<quad>]]"

let set_ngrams_arg s = ngrams_arg := Some (parse_ngrams s)

let speclist =
  [
    ( "-bd",
      Arg.String set_base_dir,
      Fmt.str
        "<DIR> Specify where the “bases” directory with databases is installed \
         (default if empty is %S)."
        Dirs.(name Secure.default_base_dir)
      |> String.split_on_char '\\' |> String.concat "/" );
    ( "-bnotes",
      Arg.String set_bnotes,
      " [drop|erase|first|merge] Behavior for base notes of the next file. \
       [drop]: dropped. [erase]: erase the current content. [first]: dropped \
       if current content is not empty. [merge]: concatenated to the current \
       content. Default: "
      ^ bnotes_to_string default_bnotes );
    ("-c", Arg.Set just_comp, " Only compiling");
    ("-cg", Arg.Set Db1link.do_consang, " Compute consanguinity");
    ( "-ds",
      Arg.Set_string Db1link.default_source,
      "<str> Set the source field for persons and families without source data"
    );
    ("-f", Arg.Set GWPARAM.force, " Remove database if already existing");
    ("-gwo", Arg.Set kill_gwo, " Suppress .gwo files after base creation");
    ("-mem", Arg.Set Geneweb_db.Outbase.save_mem, " Save memory, but slower");
    ("-nc", Arg.Clear Db1link.do_check, " No consistency check");
    ("-nofail", Arg.Set Gwcomp.no_fail, " No failure in case of error");
    ( "-ngrams",
      Arg.String set_ngrams_arg,
      "<bi>[,<tri>[,<quad>]] N-gram indexing thresholds (e.g., '500,20,10' or \
       '500')" );
    ("-nolock", Arg.Set Lock.no_lock_flag, " Do not lock database");
    ( "-nopicture",
      Arg.Set Gwcomp.no_picture,
      " Do not create associative pictures" );
    ("-nowarn", Arg.Set no_warn, " Do not show warnings during import");
    ( "-o",
      Arg.String set_output,
      "<file> Output database (default: <input file name>.gwb, a.gwb if not \
       available). Alphanumerics and -" );
    ( "-particles",
      Arg.Set_string Db1link.particules_file,
      "<file> Particles file (default = predefined particles)" );
    ("-q", Arg.Clear Gwcomp.verbose, " Quiet");
    ("-reorg", Arg.Set GWPARAM.reorg, " Mode reorg");
    ("-rgpd", Arg.String (fun s -> Gwcomp.rgpd_dir := s), "<dir> Rgpd directory");
    ("-sep", Arg.Set separate, " Separate all persons in next file");
    ("-sh", Arg.Set_int shift, "<int> Shift all persons numbers in next files");
    ("-stats", Arg.Set Db1link.pr_stats, " Print statistics");
    ("-prog", Arg.Set prog, " show progress bar");
    ("-v", Arg.Set Gwcomp.verbose, " Verbose");
    ( "-roglo_special",
      Arg.Set Gwcomp.roglo_special,
      " Special treatment for Roglo (ignore multiple relations definitions)" );
  ]
  |> List.sort compare |> Arg.align

let errmsg =
  "Usage: gwc [options] [files]\n\
   where [files] are a list of files:\n\
  \  source files end with .gw\n\
  \  object files end with .gwo\n\
   and [options] are:"

let anonfun fname =
  let bn = !bnotes in
  let sep = !separate in
  let kind =
    if Filename.check_suffix fname ".gw" then Gw
    else if Filename.check_suffix fname ".gwo" then Gwo
    else (
      Fmt.epr "gwc: Don’t know what to do with %S.@." fname;
      Fmt.epr "%s" (Arg.usage_string speclist errmsg);
      exit 2)
  in
  separate := default_separate;
  bnotes := default_bnotes;
  add_input { fname; kind; separate = sep; bnotes = bn; shift = !shift }

let parse_output inputs output =
  match (inputs, output) with
  | [], _ -> raise_bad "You must specify at least one input file."
  | _ :: _ :: _, None ->
      raise_bad
        "In presence of several input files, we cannot infer the output \
         database name. Please specify it with -o option."
  | [ { fname; _ } ], None ->
      (* We infer the name of the database in this case. *)
      Filename.(basename (remove_extension fname))
  | _, Some bname -> bname

let parse_cmd () =
  Arg.parse speclist anonfun errmsg;
  let inputs = List.rev !rev_inputs in
  let bname = parse_output inputs !output in
  let base_dir = Option.value ~default:Filename.current_dir_name !base_dir in
  (inputs, bname, base_dir)

let with_timer f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  (result, stop -. start)

let pp_duration ppf d =
  Fmt.pf ppf "%d min %d sec" (int_of_float (d /. 60.0)) (int_of_float d mod 60)

let ( // ) = Filename.concat

let check_database_exists base_dir bname =
  let path = base_dir // Fmt.str "%s.gwb" bname in
  Sys.file_exists path

let cleanup gwo_files =
  List.iter
    (fun (fname, _, _, _, was_gw) -> if was_gw then Mutil.rm fname)
    gwo_files

let () =
  let inputs, bname, base_dir = parse_cmd () in
  Secure.set_base_dir base_dir;
  GWPARAM.init ();
  let dist_etc_d = Filename.concat (Filename.dirname Sys.argv.(0)) "etc" in
  if Compat.String.is_empty !Db1link.particules_file then
    Db1link.particules_file := Filename.concat dist_etc_d "particles.txt";
  if !Gwcomp.verbose then
    if !Gwcomp.rgpd then
      Format.eprintf "Rgpd status: True, files in: %s@." !Gwcomp.rgpd_dir
    else Format.eprintf "Rgpd status: False@.";
  let gwo_files, duration =
    with_timer @@ fun () ->
    ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter @@ fun bar ->
    compile_gw_files bar ~bname inputs
  in
  Format.eprintf "Compilation: %a@." pp_duration duration;
  if not !just_comp then (
    if (not !GWPARAM.force) && check_database_exists base_dir bname then (
      if !kill_gwo then cleanup gwo_files;
      Fmt.epr "Database %S already exists. Use -f to overwrite.@." bname;
      exit 2);
    let bdir = GWPARAM.create_base_and_config bname in
    let lock_file = Mutil.lock_file bdir in
    let on_exn exn bt =
      Fmt.epr "%a@." Lock.pp_exception (exn, bt);
      exit 2
    in
    Option.iter
      (fun (bi, tri, quad) ->
        Geneweb_db.Outbase.set_ngram_thresholds bi tri quad;
        Format.eprintf "N-grams enabled: bi≥%d, tri≥%d, quad≥%d@." bi tri quad)
      !ngrams_arg;
    Lock.control ~on_exn ~wait:false ~lock_file @@ fun () ->
    let (), duration =
      with_timer @@ fun () ->
      ProgrBar.with_bar ~disabled:(not !prog) Format.std_formatter @@ fun bar ->
      generate_database ~no_warn:!no_warn ~bdir bar gwo_files
    in
    Format.eprintf "Database generation: %a@." pp_duration duration;
    if !kill_gwo then cleanup gwo_files)
