(* gwwarn_impl.ml — gwd plugin for anoma/gwwarn.

   Request modes:

     m=OK_FILE  show the ignored file (<base_dir>/<base>.ok) AND the pending
                per-wizard fragments in <base>.ok.d/ not yet consolidated,
                read-only. Never editable — hand-editing races with anoma's
                fold and with the wizard fragments.

     m=OK_FOLD  (wizard) consolidate on demand: run `anoma <base> -bd <bd>`
                (no -in => fold only), folding the pending fragments into
                <base>.ok and archiving them, then show the command output.

     m=ANOMA    serve the anomalies report through gwd, so it is same-origin
                with the base and the wizard session cookie reaches Submit.
                Point the welcome page's "anomalies" link at it:
                    <a href="%prefix;m=ANOMA">anomalies</a>

     m=OK_ADD   the report's "Submit to base" button issues this per entry;
                a logged-in wizard appends ONE verified anomaly to their own
                fragment  <base_dir>/<base>.ok.d/<wizard>.ok , which anoma
                folds into <base>.ok and archives. Each wizard writes a
                different file, so many wizards can submit at once with no
                locking and no lost updates:
                    <base>_w?m=OK_ADD&key=<person-or-couple>&code=<M|B|D|E|CO|EO>
                e.g. key = "Jean.0 Dupont"                  code = M
                     key = "Jean.0 Dupont & Marie.0 Martin" code = D

   Registration follows this tree's convention (cf. plugins/forum,
   plugins/no_index): Geneweb_register.Registration.register ~name mode-list,
   where each handler has type  config -> string option -> bool  (the second
   argument is ignored here). Build it as a dune-site plugin like the others
   (see plugins/gwwarn/dune).
*)

open Geneweb
open Config
module Registration = Geneweb_register.Registration

(* anoma is installed in the same directory as gwd, so derive its path from the
   running gwd executable — nothing to configure. m=OK_FOLD runs
   `anoma <base> -bd <base_dir>` (no -in => consolidate & exit). (If your gwd is a symlink resolved
   elsewhere, replace this with the absolute path to anoma.) *)
let anoma_bin = Filename.concat (Filename.dirname Sys.executable_name) "anoma"

(* Util, Output, Secure are reached bare: Util/Output via `open Geneweb`,
   Secure as a top-level module (this tree has no Geneweb.Secure). *)

(* ---- validation -------------------------------------------------------- *)

let valid_codes = [ "M"; "B"; "D"; "E"; "CO"; "EO" ]

(* a fragment line must be exactly "left: code" on ONE line; reject anything
   with control chars. We build the line ourselves (never echo raw request text
   into the shared file), so the .ok cannot be injected into. *)
let clean_key s =
  let s = String.trim s in
  if s = "" || String.length s > 300 then None
  else if String.exists (fun c -> c = '\n' || c = '\r' || Char.code c < 32) s
  then None
  else Some s

let sanitize_wizard s =
  String.map
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' -> c
      | _ -> '_')
    (if s = "" then "anonymous" else s)

(* ---- io helpers -------------------------------------------------------- *)

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> try close_in ic with Sys_error _ -> ())
    (fun () -> really_input_string ic (in_channel_length ic))

let ensure_dir d =
  if not (Sys.file_exists d) then
    try Unix.mkdir d 0o755 with Unix.Unix_error _ -> ()

let html_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string b "&amp;"
      | '<' -> Buffer.add_string b "&lt;"
      | '>' -> Buffer.add_string b "&gt;"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* atomic single-line append: O_APPEND keeps concurrent line-sized writes from
   interleaving, and each wizard has their own file anyway. *)
let append_fragment ~base_dir ~base ~wizard ~line =
  let okd = Filename.concat base_dir (base ^ ".ok.d") in
  ensure_dir okd;
  let path = Filename.concat okd (wizard ^ ".ok") in
  let fd =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644
  in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      let s = line ^ "\n" in
      ignore (Unix.write_substring fd s 0 (String.length s)))

(* ---- m=ANOMA : serve the report (config -> string option -> bool) ----- *)

(* Location of the report anoma writes; keep in sync with anoma's -out, e.g.
     gwwarn <base> -bd <base_dir> -in <log> -out <base_dir>/<base>_warnings.html *)
let report_path conf =
  Filename.concat (Secure.base_dir ()) (conf.bname ^ "_warnings.html")

(* minimal JS-string escaping for a wizard login placed inside "..." *)
let js_escape s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\\' -> Buffer.add_string b "\\\\"
      | '"' -> Buffer.add_string b "\\\""
      | '\n' | '\r' -> ()
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(* replace the first occurrence of [needle] in [hay] with [rep] *)
let replace_first hay needle rep =
  let hl = String.length hay and nl = String.length needle in
  let rec find i =
    if i + nl > hl then -1
    else if String.sub hay i nl = needle then i
    else find (i + 1)
  in
  let i = find 0 in
  if i < 0 then hay
  else String.sub hay 0 i ^ rep ^ String.sub hay (i + nl) (hl - i - nl)

let anoma conf _ =
  (if not conf.wizard then Output.print_sstring conf "forbidden: wizard only\n"
   else
     let path = report_path conf in
     match try Some (read_whole_file path) with _ -> None with
     | Some html ->
         (* tell the report which wizard is viewing, for the "me" filter *)
         let html =
           replace_first html "var GWWARN_ME=\"\""
             (Printf.sprintf "var GWWARN_ME=\"%s\"" (js_escape conf.user))
         in
         (* [BIND] send as a bare HTML page. If your gwd needs an explicit
            content-type/status, set it here before printing. *)
         Output.print_sstring conf html
     | None -> Output.print_sstring conf ("anomalies report not found: " ^ path));
  true

(* ---- m=OK_ADD : wizard appends one verified entry --------------------- *)

let ok_add conf _ =
  (if not conf.wizard then Output.print_sstring conf "forbidden: wizard only\n"
   else
     match (Util.p_getenv conf.env "key", Util.p_getenv conf.env "code") with
     | Some key_raw, Some code_raw -> (
         let code = String.uppercase_ascii (String.trim code_raw) in
         match clean_key key_raw with
         | Some key when List.mem code valid_codes -> (
             try
               append_fragment ~base_dir:(Secure.base_dir ()) ~base:conf.bname
                 ~wizard:(sanitize_wizard conf.user)
                 ~line:(Printf.sprintf "%s: %s" key code);
               Output.print_sstring conf "OK\n"
             with e ->
               Output.print_sstring conf
                 ("error: " ^ Printexc.to_string e ^ "\n"))
         | _ -> Output.print_sstring conf "bad key or code\n")
     | _ -> Output.print_sstring conf "missing key or code\n");
  true

(* ---- m=OK_FILE : show the ignored file + pending fragments (read-only) - *)

(* The ignored file anoma consolidates, in the bases dir (anoma's -ok default),
   and its fragment directory <base>.ok.d holding not-yet-consolidated
   per-wizard contributions. Displayed as-is; never written — editing by hand
   is unsafe (whole-file races with anoma's fold and with wizard fragments). *)
let ok_path conf = Filename.concat (Secure.base_dir ()) (conf.bname ^ ".ok")

(* live "*.ok" fragments in [dir] as (wizard, path), sorted *)
let fragment_files dir =
  if Sys.file_exists dir && try Sys.is_directory dir with Sys_error _ -> false
  then
    Array.to_list (Sys.readdir dir)
    |> List.filter (fun n ->
        n <> ""
        && n.[0] <> '.'
        && Filename.check_suffix n ".ok"
        &&
        let p = Filename.concat dir n in
        Sys.file_exists p
        && not (try Sys.is_directory p with Sys_error _ -> false))
    |> List.sort compare
    |> List.map (fun n -> (Filename.remove_extension n, Filename.concat dir n))
  else []

let pre_of path =
  match try Some (read_whole_file path) with _ -> None with
  | Some txt -> "<pre>" ^ html_escape txt ^ "</pre>\n"
  | None -> ""

let ok_file conf _ =
  if not conf.wizard then Output.print_sstring conf "forbidden: wizard only\n"
  else begin
    let path = ok_path conf in
    let okd = path ^ ".d" in
    Output.print_sstring conf
      "<!DOCTYPE html>\n\
       <meta charset=\"utf-8\">\n\
       <title>ignored anomalies</title>\n";
    (* consolidated file *)
    Output.print_sstring conf
      (Printf.sprintf "<h2>%s</h2>\n" (html_escape (Filename.basename path)));
    (match try Some (read_whole_file path) with _ -> None with
    | Some txt ->
        Output.print_sstring conf ("<pre>" ^ html_escape txt ^ "</pre>\n")
    | None ->
        Output.print_sstring conf
          "<p><i>(no consolidated file yet — see pending contributions \
           below)</i></p>\n");
    (* pending per-wizard fragments, not yet folded in by anoma *)
    match fragment_files okd with
    | [] -> ()
    | frags ->
        Output.print_sstring conf
          "<h2>Pending wizard contributions (not yet consolidated)</h2>\n";
        Output.print_sstring conf
          "<p><a href=\"?m=OK_FOLD\">consolidate now</a> (fold into the \
           ignored file and archive)</p>\n";
        List.iter
          (fun (wizard, p) ->
            Output.print_sstring conf
              (Printf.sprintf "<h3>%s</h3>\n" (html_escape wizard));
            Output.print_sstring conf (pre_of p))
          frags
  end;
  true

(* ---- m=OK_FOLD : consolidate fragments on demand (wizard) ------------- *)

(* Runs the tested anoma consolidation instead of duplicating it:
     anoma <base> -bd <base_dir>          (no -in => fold only)
   which folds <base>.ok.d/*.ok into <base>.ok (claiming each atomically) and
   archives them. Command uses only server-side values (base name, base dir),
   never request input, so there is nothing to inject. *)
let ok_fold conf _ =
  if not conf.wizard then Output.print_sstring conf "forbidden: wizard only\n"
  else begin
    let cmd =
      (* no -in => anoma just consolidates the fragments and exits *)
      Printf.sprintf "%s %s -bd %s 2>&1" (Filename.quote anoma_bin)
        (Filename.quote conf.bname)
        (Filename.quote (Secure.base_dir ()))
    in
    Output.print_sstring conf
      "<!DOCTYPE html>\n\
       <meta charset=\"utf-8\">\n\
       <title>consolidate</title>\n\
       <h2>Consolidation</h2>\n\
       <pre>";
    (try
       let ic = Unix.open_process_in cmd in
       (try
          while true do
            Output.print_sstring conf (html_escape (input_line ic));
            Output.print_sstring conf "\n"
          done
        with End_of_file -> ());
       ignore (Unix.close_process_in ic)
     with e ->
       Output.print_sstring conf ("error: " ^ html_escape (Printexc.to_string e)));
    Output.print_sstring conf "</pre>\n";
    Output.print_sstring conf
      "<p><a href=\"?m=OK_FILE\">view ignored file</a></p>\n"
  end;
  true

(* ---- registration (same convention as forum/no_index) ----------------- *)

let () =
  Registration.register ~name:"gwwarn" []
    [
      ("ANOMA", anoma);
      ("OK_ADD", ok_add);
      ("OK_FILE", ok_file);
      ("OK_FOLD", ok_fold);
    ]
