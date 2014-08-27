(*
   Copyright 2009, 2010, 2011 Anton Lavrik

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


open Piqi_common 


(* command-line parameters *)
let odir = ref ""
let ifile = ref ""
let ofile = ref ""


let flag_leave_tmp_files = ref false


let ich = ref stdin
let och = ref stdout


let open_input = function
  | "-" | "" -> stdin
  | f ->
      try
        let c = open_in_bin f in
        ifile := f;
        ich := c; c
      with Sys_error s ->
        piqi_error ("failed to open input file: " ^ s)


let open_output = function
  | "-" | "" -> stdout
  | f ->
      try 
        let c = open_out_bin f in
        ofile := f;
        och := c; c
      with Sys_error s ->
        piqi_error ("failed to open output file: " ^ s)


let close_output () =
  close_out !och


let close () =
  if !och != stdout
  then close_out !och;

  if !ich != stdin
  then close_in !ich;
  ()


let tmp_files = ref []

let add_tmp_file (f:string) =
  tmp_files := f::!tmp_files


let delete_file fname =
  try Sys.remove fname
  with _ -> ()


let cleanup () =
  close ();
  (* remove temporary files *)
  if not !flag_leave_tmp_files
  then List.iter delete_file !tmp_files;
  ()


let cleanup_on_error () =
  (try cleanup () with _ -> ());
  (* delete output file *)
  if !och != stdout
  then delete_file !ofile;
  ()


let chdir_output dir =
  try
    begin
      match dir with 
        | "" -> ()
        | _ -> Sys.chdir dir
    end
  with Sys_error s ->
    piqi_error ("failed to chdir to output directory: " ^ dir)


let arg_count = ref 0

let default_anon_fun s =
  match !arg_count with
    | 1 ->
        (* the first positional argument is input file *)
        ifile := s
    | 2 ->
        (* the second positional argument is output file unless overriden by -o
         * option *)
        if !ofile = ""
        then ofile := s
    | _ -> ()

let anon_fun f s =
  incr arg_count;
  f s


let arg_I =
  "-I", Arg.String Config.add_path,
    "<dir> add directory to the list of imported .piqi search paths"

let arg_o =
  "-o", Arg.Set_string ofile,
    "<output file> specify output file; use '-' for stdout"

let arg_C =
  "-C", Arg.Set_string odir,
    "<output directory> specify output directory"

let arg__ =
   "--", Arg.Rest (anon_fun default_anon_fun),
     "supply other arguments possibly including '-' for stdin input/output"

let arg__leave_tmp_files =
   "--leave-tmp-files", Arg.Set flag_leave_tmp_files,
     "don't delete temporary files created during command execution"

let arg__no_warnings =
   "--no-warnings", Arg.Set Config.flag_no_warnings,
     "don't print warnings"

let arg__trace =
   "--trace", Arg.Set Config.flag_trace,
     "turn on tracing"

let arg__debug =
   "--debug", Arg.Set_int Config.debug_level,
     "<level> debug level; any number greater than 0 turns on debug messages"

let arg__noboot =
   "--noboot", Arg.Set Config.noboot,
     "don't boot, i.e. don't use boot definitions while processing .piqi"


let common_speclist =
  [
   arg_I;
   arg__no_warnings;
   arg__trace;
   arg__debug;
   arg__noboot;
  ]


let common_usage = "Usage: piqi <command> [options] <.piqi file>\nOptions:"


let parse_args
    ?(speclist=common_speclist) ?(usage=common_usage)
    ?(min_arg_count=1) ?(max_arg_count=1) ?(custom_anon_fun=default_anon_fun) () =
  (* XXX
  (* overwrite argv[1] to contain piqi command *)
  let cmd = "piqi " ^ Sys.argv.(1) in
  Sys.argv.(1) <- cmd;
  *)
  Arg.parse speclist (anon_fun custom_anon_fun) usage;
  if !arg_count < min_arg_count || !arg_count > max_arg_count
  then
    begin
      Arg.usage speclist usage;
      exit 3;
    end


type command = 
  {
    name : string;
    descr : string;
    run : (unit -> unit);
  }

(* top-level command *)
let command :command option ref = ref None

(* sub-command *)
let commands :command list ref = ref []


let register f descr =
  let cmd = { name = ""; descr = descr; run = f } in
  command := Some cmd


let register_command f name descr =
  let cmd = { name = name; descr = descr; run = f } in
  commands := cmd :: !commands


let usage_string = ref "Usage: piqi <command> [--help] [options] ..."


let usage () =
  prerr_endline !usage_string;
  prerr_endline "Commands:";
  let print_cmd = Printf.eprintf "  %-15s%s\n" in
  List.iter
    (fun cmd -> print_cmd cmd.name cmd.descr)
    (List.rev !commands);
  print_cmd "version" "print version";
  prerr_endline 
    "\nMore information is available at http://piqi.org/doc/\n"


let exit_usage () =
  usage ();
  exit 2


let find_command name =
  try
    List.find (function x when x.name = name -> true | _ -> false) !commands
  with Not_found ->
    exit_usage ()


let die s =
  prerr_endline s;
  exit 1


let run_command cmd =
  try
    cmd.run ();
    cleanup ();
  with
    | Piqi_error s ->
        cleanup_on_error ();
        die s
    | Piqi_common.Error (loc, s) ->
        cleanup_on_error ();
        die (Piqi_common.strerr loc s)
    | Sys_error s ->
        cleanup_on_error ();
        die ("uncaught system error: " ^ s)


let print_version () = 
  print_endline Piqi_version.version


let run_subcommand cmd_name =
  match cmd_name with
    | "version" | "--version" ->
        print_version ()
    | "help" | "--help" | "-h" ->
        usage ()
    | _ ->
        (* find command by command name passed as the first argument *)
        let cmd = find_command cmd_name in
        Arg.current := 1; (* subcommand -- skip argv[0] *)
        run_command cmd


let run () =
  (* set .piqi search path to contain CWD and $PIQI_DIR *)
  Config.init_paths ();

  if !Sys.interactive
  then () (* don't do anything in interactive (toplevel) mode *)
  else
    match !command with
      | Some cmd -> (* top-level command *)
          run_command cmd
      | None -> (* subcommand *)
          if Array.length Sys.argv < 2
          then exit_usage ()
          else run_subcommand Sys.argv.(1)

