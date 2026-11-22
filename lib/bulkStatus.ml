(* Marker of a long write task (bulk renaming, data check, ...) holding the
   lock of a base, so that a request finding the base locked can tell the
   wizard to be patient instead of waiting silently. The marker sits next to
   the lock file: "base.lck" / "base.bulk". *)

type t = { command : string; user : string; started : float; pid : int }

let file bpath = Filename.remove_extension bpath ^ ".bulk"

(* A marker older than this is ignored whatever its process: protection
   against a marker left by a killed process when its pid cannot be checked
   (Windows) or has been reused. *)
let max_age = 12. *. 3600.

let write bpath t =
  let fname = file bpath in
  let tmp = fname ^ ".tmp" in
  try
    Secure.with_open_out_bin tmp (fun oc ->
        Printf.fprintf oc "%s\n%s\n%.0f\n%d\n" t.command t.user t.started t.pid);
    Sys.rename tmp fname
  with Sys_error _ -> ()

let remove bpath = try Sys.remove (file bpath) with Sys_error _ -> ()

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | exception Unix.Unix_error (_, _, _) -> true
  | exception Invalid_argument _ -> true

let read bpath =
  match Secure.with_open_in_bin (file bpath) In_channel.input_all with
  | exception Sys_error _ -> None
  | s -> (
      match String.split_on_char '\n' s with
      | command :: user :: started :: pid :: _ -> (
          match (float_of_string_opt started, int_of_string_opt pid) with
          | Some started, Some pid
            when Unix.time () -. started < max_age && alive pid ->
              Some { command; user; started; pid }
          | Some _, Some _ | None, _ | _, None -> None)
      | _ -> None)

let with_marker bpath ~command ~user k =
  write bpath { command; user; started = Unix.time (); pid = Unix.getpid () };
  Fun.protect ~finally:(fun () -> remove bpath) k
