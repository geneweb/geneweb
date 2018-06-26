(* camlp5o *)

let file_flags = [Open_wronly; Open_append; Open_creat; Open_text; Open_nonblock]
let file_mode = 0o644

let fallback_to_stderr = ref true
let file = ref ""

(* generates Some (out_channel, flush) *)

let open_no_file ~fallback_to_stderr =
  if fallback_to_stderr then
    Some (stderr, fun () -> flush stderr)
  else
    None

let open_log () =
  if !file = "" then
    open_no_file ~fallback_to_stderr:!fallback_to_stderr
  else
    match
      try Some (open_out_gen file_flags file_mode !file) with Sys_error _ -> None
    with
    | Some oc ->
      Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stderr;
      Some (oc, fun () -> close_out oc)
    | None ->
      file := "";
      open_no_file ~fallback_to_stderr:!fallback_to_stderr

let with_file ~file f =
  match
    try Some (open_out_gen file_flags file_mode file) with Sys_error _ -> None
  with
  | None -> ()
  | Some oc ->
    try f oc; close_out oc with e -> close_out oc; raise e

(* Calls f with an out_channel, only if logging is needed *)
let with_log f =
  match open_log () with
  | None -> ()
  | Some (oc, flush) ->
    try f oc; flush () with e -> flush (); raise e

(* Calls f with an out_channel option, always *)
let with_log_opt f =
  match open_log () with
  | None -> f None
  | Some (oc, flush) ->
    try f (Some oc); flush () with e -> flush (); raise e
