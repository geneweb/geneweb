open Config
module Code = Geneweb_http.Code

let ( // ) = Filename.concat

(* Same layout as anoma: <bases_dir>/<base>.gwb/config/ *)
let config_dir conf = !GWPARAM.bpath conf.bname // "config"
let report_path conf = config_dir conf // "anomalies.html"
let whitelist_path conf = config_dir conf // "whitelist_warnings.txt"
let fragments_dir conf = config_dir conf // "whitelist_d"

(* anoma is installed beside gwd; keep gwd's extension (".exe" on Windows). *)
let anoma_bin () =
  let gwd = Sys.executable_name in
  Filename.dirname gwd // ("anoma" ^ Filename.extension gwd)

let valid_codes = [ "M"; "B"; "D"; "E"; "CO"; "EO" ]

(* A fragment line is built here as "key: code" on one line, never copied from
   the request, so a key with a control character is rejected rather than
   allowed to inject extra lines into the shared whitelist. *)
let clean_key s =
  let s = String.trim s in
  if s = "" || String.length s > 300 then None
  else if String.exists (fun c -> Char.code c < 32) s then None
  else Some s

(* The fragment file name comes from the wizard login: keep it to a safe
   character set so it cannot escape whitelist_d/. *)
let fragment_name user =
  let user = if user = "" then "anonymous" else user in
  String.map
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> c
      | _ -> '_')
    user
  ^ ".ok"

let read_file path =
  match open_in_bin path with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))

let rec mkdir_p d =
  if not (Sys.file_exists d) then (
    let parent = Filename.dirname d in
    if parent <> d then mkdir_p parent;
    try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let is_directory d = try Sys.is_directory d with Sys_error _ -> false

(* Live "*.ok" fragments in [dir], as (wizard, path), sorted by wizard. *)
let fragment_files dir =
  if not (is_directory dir) then []
  else
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun n ->
        n <> ""
        && n.[0] <> '.'
        && Filename.check_suffix n ".ok"
        && not (is_directory (dir // n)))
    |> List.sort compare
    |> List.map (fun n -> (Filename.chop_suffix n ".ok", dir // n))

(* O_APPEND: a single line-sized write is not interleaved with another
   append to the same file. *)
let append_line path line =
  let fd = Unix.openfile path [ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      let s = line ^ "\n" in
      ignore (Unix.write_substring fd s 0 (String.length s)))

(* Escape for a JS double-quoted string inside an inline <script>: "<" is
   escaped too, so a login cannot close the script element. *)
let js_string s =
  let b = Buffer.create (String.length s + 8) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | '<' -> Buffer.add_string b "\\u003c"
      | '\n' | '\r' -> ()
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* Replace the first occurrence of [needle] in [hay] by [rep]. *)
let replace_first hay needle rep =
  let hl = String.length hay and nl = String.length needle in
  let rec find i =
    if i + nl > hl then None
    else if String.sub hay i nl = needle then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> hay
  | Some i -> String.sub hay 0 i ^ rep ^ String.sub hay (i + nl) (hl - i - nl)

(* Run [prog args] without a shell, stdout and stderr merged. *)
let run_capture prog args =
  let r, w = Unix.pipe ~cloexec:true () in
  match
    Unix.create_process prog (Array.of_list (prog :: args)) Unix.stdin w w
  with
  | exception Unix.Unix_error (err, _, _) ->
      Unix.close r;
      Unix.close w;
      Error (Unix.error_message err)
  | pid ->
      Unix.close w;
      let ic = Unix.in_channel_of_descr r in
      let out =
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () -> In_channel.input_all ic)
      in
      let _, status = Unix.waitpid [] pid in
      Ok (out, status)

let capitalized conf key = Utf8.capitalize_fst (Util.transl conf key)

let page ?(error = false) conf ~title body =
  let title_str = capitalized conf title in
  Hutil.header ~error conf (fun _ ->
      Output.print_string conf (Util.escape_html title_str));
  body ();
  Hutil.trailer conf

let print_pre conf txt =
  Output.print_sstring conf "<pre>";
  Output.print_string conf (Util.escape_html txt);
  Output.print_sstring conf "</pre>\n"

let plain conf status msg =
  Output.status conf status;
  Util.html ~content_type:"text/plain" conf;
  Output.print_sstring conf msg;
  Output.print_sstring conf "\n"

(* ---- m=ANOMA ------------------------------------------------------------ *)

let print_report conf _base =
  match read_file (report_path conf) with
  | Some html ->
      let html =
        replace_first html {|var GWWARN_ME=""|}
          (Printf.sprintf {|var GWWARN_ME="%s"|} (js_string conf.user))
      in
      Output.status conf Code.OK;
      Util.html conf;
      Output.print_sstring conf html
  | None ->
      Output.status conf Code.Not_Found;
      page ~error:true conf ~title:"anomalies report not found" (fun () ->
          print_pre conf (report_path conf))

(* ---- m=OK_ADD ----------------------------------------------------------- *)

let print_ok_add conf _base =
  match (Util.p_getenv conf.env "key", Util.p_getenv conf.env "code") with
  | Some key, Some code -> (
      let code = String.uppercase_ascii (String.trim code) in
      match clean_key key with
      | Some key when List.mem code valid_codes -> (
          let dir = fragments_dir conf in
          match
            mkdir_p dir;
            append_line
              (dir // fragment_name conf.user)
              (Printf.sprintf "%s: %s" key code)
          with
          | () -> plain conf Code.OK "OK"
          | exception (Unix.Unix_error _ as e) ->
              plain conf Code.Service_Unavailable
                ("error: " ^ Printexc.to_string e))
      | Some _ | None -> plain conf Code.Bad_Request "bad key or code")
  | Some _, None | None, Some _ | None, None ->
      plain conf Code.Bad_Request "missing key or code"

(* ---- m=OK_FILE ---------------------------------------------------------- *)

let print_ok_file conf _base =
  page conf ~title:"ignored anomalies" (fun () ->
      let path = whitelist_path conf in
      Output.printf conf "<h2>%s</h2>\n"
        (Util.escape_html (Filename.basename path) :> string);
      (match read_file path with
      | Some txt -> print_pre conf txt
      | None ->
          Output.printf conf "<p><i>%s</i></p>\n"
            (capitalized conf "no consolidated whitelist yet"));
      match fragment_files (fragments_dir conf) with
      | [] -> ()
      | frags ->
          Output.printf conf "<h2>%s</h2>\n"
            (capitalized conf "pending wizard contributions");
          Output.printf conf
            {|<p><a class="btn btn-outline-primary" href="%sm=OK_FOLD">%s</a></p>|}
            (Util.commd conf :> string)
            (capitalized conf "consolidate now");
          List.iter
            (fun (wizard, p) ->
              Output.printf conf "<h3>%s</h3>\n"
                (Util.escape_html wizard :> string);
              match read_file p with
              | Some txt -> print_pre conf txt
              | None -> ())
            frags)

(* ---- m=OK_FOLD ---------------------------------------------------------- *)

let print_ok_fold conf _base =
  page conf ~title:"anomalies whitelist consolidation" (fun () ->
      let prog = anoma_bin () in
      (if not (Sys.file_exists prog) then
         Output.printf conf "<p>%s</p>\n"
           (Util.escape_html
              (Printf.sprintf (Util.ftransl conf "anoma not found at %s") prog)
             :> string)
       else
         (* only server-side values: nothing from the request reaches argv *)
         match run_capture prog [ conf.bname; "-bd"; Secure.base_dir () ] with
         | Error msg -> print_pre conf (prog ^ ": " ^ msg)
         | Ok (out, status) -> (
             print_pre conf out;
             match status with
             | Unix.WEXITED 0 -> ()
             | Unix.WEXITED n | Unix.WSIGNALED n | Unix.WSTOPPED n ->
                 Output.printf conf "<p>%s</p>\n"
                   (Printf.sprintf (Util.ftransl conf "anoma exit status %d") n)
             ));
      Output.printf conf {|<p><a href="%sm=OK_FILE">%s</a></p>|}
        (Util.commd conf :> string)
        (capitalized conf "ignored anomalies"))
