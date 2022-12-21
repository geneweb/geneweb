let just_comp = ref false
let out_file = ref (Filename.concat Filename.current_dir_name "a")
let force = ref false
let separate = ref false
let bnotes = ref "merge"
let shift = ref 0
let files = ref []
let no_fail = ref false
let no_picture = ref false
let create_all_keys = ref false
let line_cnt = ref 0

(** Default source field for persons and families without source data *)
let default_source = ref ""

(** Base consistency check *)
let do_check = ref true

(** Compute consanguinity *)
let do_consang = ref false

(** Print base's statistics *)
let pr_stats = ref false

(** File containing the particles to use *)
let particules_file = ref ""

let make_state () =
  Gwc_lib.State.
    {
      just_comp = !just_comp;
      out_file = !out_file;
      force = !force;
      separate = !separate;
      bnotes = !bnotes;
      shift = !shift;
      files = !files;
      no_fail = !no_fail;
      no_picture = !no_picture;
      create_all_keys = !create_all_keys;
      line_cnt = !line_cnt;
      default_source = !default_source;
      do_check = !do_check;
      do_consang = !do_consang;
      pr_stats = !pr_stats;
      particules_file = !particules_file;
    }

let speclist =
  [
    ( "-bnotes",
      Arg.Set_string bnotes,
      "[drop|erase|first|merge] Behavior for base notes of the next file. \
       [drop]: dropped. [erase]: erase the current content. [first]: dropped \
       if current content is not empty. [merge]: concatenated to the current \
       content. Default: " ^ !bnotes ^ "" );
    ("-c", Arg.Set just_comp, " Only compiling");
    ("-cg", Arg.Set do_consang, " Compute consanguinity");
    ( "-ds",
      Arg.Set_string default_source,
      "<str> Set the source field for persons and families without source data"
    );
    ("-f", Arg.Set force, " Remove database if already existing");
    ("-mem", Arg.Set Outbase.save_mem, " Save memory, but slower");
    ("-nc", Arg.Clear do_check, " No consistency check");
    ("-nofail", Arg.Set no_fail, " No failure in case of error");
    ("-nolock", Arg.Set Lock.no_lock_flag, " Do not lock database");
    ("-nopicture", Arg.Set no_picture, " Do not create associative pictures");
    ("-o", Arg.Set_string out_file, "<file> Output database (default: a.gwb)");
    ( "-particles",
      Arg.Set_string particules_file,
      "<file> Particles file (default = predefined particles)" );
    ("-q", Arg.Clear Mutil.verbose, " Quiet");
    ("-sep", Arg.Set separate, " Separate all persons in next file");
    ("-sh", Arg.Set_int shift, "<int> Shift all persons numbers in next files");
    ("-stats", Arg.Set pr_stats, " Print statistics");
    ("-v", Arg.Set Mutil.verbose, " Verbose");
  ]
  |> List.sort compare |> Arg.align

let anonfun x =
  let bn = !bnotes in
  let sep = !separate in
  if Filename.check_suffix x ".gw" then ()
  else if Filename.check_suffix x ".gwo" then ()
  else raise (Arg.Bad ("Don't know what to do with \"" ^ x ^ "\""));
  separate := false;
  bnotes := "merge";
  files := (x, sep, bn, !shift) :: !files

let errmsg =
  "Usage: gwc [options] [files]\n\
   where [files] are a list of files:\n\
  \  source files end with .gw\n\
  \  object files end with .gwo\n\
   and [options] are:"

let main () =
  Mutil.verbose := false;
  Arg.parse speclist anonfun errmsg;
  Gwc_lib.make_base (make_state ())

let () = main ()
