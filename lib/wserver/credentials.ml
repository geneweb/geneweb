module U = Geneweb_unix
module Logs = Geneweb_logs.Logs

type t = { egid : int; euid : int }

let root = 0

let drop_groups t =
  if t.euid = root then Unix.setgroups [| Unix.getgid () |]

let drop_temporary t =
  let rgid = Unix.getgid () and ruid = Unix.getuid () in
  if t.egid <> rgid then U.setregid (-1) rgid;
  if t.euid <> ruid then U.setreuid (-1) ruid;
  (* Verify that the changes were successful. *)
  assert (
    (t.egid = rgid || Unix.getegid () = rgid)
    && (t.euid = ruid || Unix.geteuid () = ruid))

let init () =
  let egid = Unix.getegid () in
  let euid = Unix.geteuid () in
  let t = { egid; euid } in
  drop_groups t;
  drop_temporary t;
  t

let restore t =
  U.seteuid t.euid;
  U.setegid t.egid;
  (* We cannot portably restore the supplementary groups. For instance,
     macOS requires the effective user ID to be root to call [Unix.setgroups].
     Since we do not use them, we drop them definitively as soon as possible
     in [init]. *)
  Logs.debug (fun k ->
      k "New credentials: %d %d" (Unix.geteuid ()) (Unix.getegid ()))

let succeed f =
  try
    f ();
    true
  with _ -> false

let drop t =
  let rgid = Unix.getgid () and ruid = Unix.getuid () in
  (* The order in which we drop privileges is crucial for security.
     The reverse order is a severe security issue because the current process
     could regain privileges by restoring the real group ID with [Uni.setgid].
     For more details, see section POS36-C in the POSIX standard. *)
  if t.egid <> rgid then U.setregid rgid rgid;
  if t.euid <> ruid then U.setreuid ruid ruid;
  (* Verify that old privileges cannot be restored with best-effort tests. *)
  if t.egid <> rgid then (
    assert (not @@ succeed (fun () -> U.setegid t.euid));
    assert (Unix.getegid () = rgid));
  if t.euid <> ruid then (
    assert (not @@ succeed (fun () -> U.seteuid t.egid));
    assert (Unix.geteuid () = ruid))
