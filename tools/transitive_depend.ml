(* nocamlp5 *)

module StrMap = Map.Make(String)

let depend = ".depend"

let split_string s =
  let s = String.trim s in
  if s = "" then
    []
  else
    let rec loop cur wend l =
      if cur < 0 then
        String.sub s 0 (wend + 1) :: l
      else if s.[cur] = ' ' then
        if wend > cur then
          loop (cur - 1) (cur - 1) (String.sub s (cur + 1) (wend - cur) :: l)
        else
          loop (cur - 1) (cur - 1) l
      else
        loop (cur - 1) wend l in
    let last = String.length s - 1 in
    loop last last []

let add_deps ~rhs deps lhs =
  let dep = try StrMap.find lhs deps with Not_found -> [] in
  StrMap.add lhs (dep @ rhs) deps

let treat_line ~filter_map ~deps line =
  match String.index line ':' with
  | exception Not_found -> deps
  | i_colon ->
    let lhs = split_string (String.sub line 0 i_colon)
              |> filter_map
    and rhs = split_string (String.sub line (i_colon + 1) (String.length line - i_colon - 1))
              |> filter_map in
    List.fold_left (add_deps ~rhs) deps lhs

let rec read_non_comment_line ich =
  let line = input_line ich in
  if String.length line > 0 && line.[0] == '#' then
    read_non_comment_line ich
  else
    line

let rec read_depend_line ich =
  let line = read_non_comment_line ich in
  if String.length line > 0 && line.[String.length line - 1] = '\\' then
    line ^ (read_depend_line ich)
  else
    line

let parent_prefix = Filename.parent_dir_name ^ Filename.dir_sep
let parent_prefix_len = String.length parent_prefix

(* concatpath "A/B" "../C/D" gives "A/C/D" *)
let rec concat_path pre post =
  if pre = "" || pre = Filename.current_dir_name then
    post
  else
    match String.sub post 0 parent_prefix_len with
    | exception Invalid_argument _ -> post
    | x when x = parent_prefix ->
      let pre = Filename.dirname pre
      and post = String.sub post parent_prefix_len (String.length post - parent_prefix_len) in
      concat_path pre post
    | _ -> Filename.concat pre post

let read_depend ~filter_map deps inc_dir =
  let ich = open_in (Filename.concat inc_dir depend) in
  let filter_map l = filter_map l
                     |> List.map (concat_path inc_dir) in
  let rec loop deps =
    match read_depend_line ich with
    | exception End_of_file -> close_in ich; deps
    | line -> treat_line ~filter_map ~deps line |> loop in
  loop deps

(* Collect all dependencies as an associative array *)
let compute_deps ~filter_map ~inc_dirs =
  List.fold_left (read_depend ~filter_map) StrMap.empty inc_dirs

let print_graph ~deps ~targets =
  let ingraph = Hashtbl.create 10 in
  let rec add x =
    if not (Hashtbl.mem ingraph x) then (
      Hashtbl.add ingraph x ();
      match StrMap.find x deps with
      | exception Not_found -> Printf.printf "%S\n" x
      | dep ->
        Printf.printf "%S -> {" x;
        List.iter (Printf.printf "%S ") dep;
        Printf.printf "}\n";
        List.iter add dep
    ) in
  print_endline "digraph {";
  List.iter add targets;
  print_endline "}"

(* Compute the transitive closure of dependencies from targets
   + avoid infinite loops in case of cyclic dependencies
   + do not add duplicates *)
let print_deps ~deps ~prefix ~targets = 
  let inres = Hashtbl.create 10
  and instack = Hashtbl.create 10 in
  let rec add x =
    if not (Hashtbl.mem instack x) then (
      Hashtbl.add instack x ();
      let dep = try StrMap.find x deps with Not_found -> [] in
      List.iter add dep;
      if not (Hashtbl.mem inres x) then (
        Hashtbl.add inres x ();
        Printf.printf "%s " x
      )
    ) in
  let print_one target =
    Hashtbl.clear inres;
    Hashtbl.clear instack;
    Hashtbl.add inres target ();
    print_string (prefix target);
    add target;
    print_newline () in
  List.iter print_one targets

(* filter is a bash pattern, we only support (yet)
   ""     : no filter
   "*xyz" : ends with xyz
   done without regexp to avoid dependency on Str.cm(x)a *)
let make_filter_map ~noext ~filter =
  let map =
    if noext then List.map Filename.chop_extension
    else fun l -> l
  and filter =
    if filter = "" then
      fun l -> l
    else
      let suff = String.sub filter 1 (String.length filter - 1) in
      if filter.[0] <> '*' ||
        List.exists (String.contains suff) ['*';'?';'+';'!';'[';'(';'@'] then
        failwith (Printf.sprintf "Unsupported filter: '%s'" filter);
      List.filter (fun x -> Filename.check_suffix x suff) in
  fun l -> filter l |> map

(* varname is a printf format string, we only support
   constant strings
   strings with one %s *)
let make_var_prefix ~varname =
  if varname = "" then
    fun _ -> ""
  else match String.index varname '%' with
  | exception Not_found -> (fun _ -> varname ^ " := ")
  | i_percent ->
    if i_percent + 1 >= String.length varname
      || varname.[i_percent + 1] <> 's'
      || String.contains_from varname (i_percent + 1) '%' then
      failwith (Printf.sprintf "Unsupported varname: '%s'" varname);
    let pre = String.sub varname 0 i_percent
    and post = String.sub varname (i_percent + 2) (String.length varname - i_percent - 2) in
    fun target -> Printf.sprintf "%s%s%s := " pre target post

let main () =
  let inc_dirs = ref []
  and filter = ref ""
  and noext = ref false
  and targets = ref []
  and graph = ref false
  and varname = ref ""
  and add_to_list list_ref x =
    list_ref := x :: !list_ref in
  let arg_spec = [
    "-h", Arg.Rest (fun _ -> raise (Arg.Help "Help requested")), "prints this help";
    "-I", Arg.String (add_to_list inc_dirs), "consider <incdir>/.depend file";
    "-filter", Arg.Set_string filter, "filter files according to <pattern>";
    "-noext", Arg.Set noext, "remove extensions of files";
    "-varname", Arg.Set_string varname, "if specified write '<varname> :=' before each line (%s will be replaced by the name of the target)";
  ]
  and usage_msg =
    let prog_name = "transitive_depend" in
    Printf.sprintf "Usage  : %s <options> [<targets>]\n
Usage 2: %s -graph <options> [<targets>]\n" prog_name prog_name in
  let usage prefix =
    Arg.usage arg_spec (Printf.sprintf "%s\n%s" prefix usage_msg) in
  match Arg.parse arg_spec (add_to_list targets) usage_msg with
  | exception Arg.Help msg -> usage msg
  | exception Arg.Bad msg -> usage msg
  | () ->
    let inc_dirs =
      if !graph then !inc_dirs
      else "" :: !inc_dirs
    and filter_map = make_filter_map ~noext:!noext ~filter:!filter in
    let deps = compute_deps ~filter_map ~inc_dirs in
    let targets =
      if !targets = [] then StrMap.bindings deps |> List.map fst
      else !targets in
    if !graph then
      print_graph ~deps ~targets
    else
      let prefix = make_var_prefix ~varname:!varname in
      print_deps ~deps ~prefix ~targets

let _ = main ()
