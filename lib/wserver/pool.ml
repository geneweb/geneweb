(* TODO: After switching to Logs, the two lines below will directly call the Logs
      library.

   let src = Logs.Src.create ~doc:"Pool" __MODULE__
   module Log = (val Logs.src_log src : Logs.LOG) *)

module Logs = Geneweb_logs.Logs

type worker = { pid : int } [@@unboxed]
type t = { workers : (worker, unit) Hashtbl.t } [@@unboxed]

let succeed f =
  try
    f ();
    true
  with _ -> false

(* The purpose of this function is to permanently drop privileges. We assume
   that the maiin gwd process is running with setuid capabilities, and is
   launched by a user or group with fewer privileges than the owner of the
   executable. We replace the effective user/group ID with the real user/group
   ID. *)
let drop_privileges () =
  let module U = Geneweb_unix in
  let ngid = Unix.getgid () and nuid = Unix.getuid () in
  let ogid = Unix.getegid () and ouid = Unix.geteuid () in
  (* The order in which we drop privileges is crucial for security.
     The reverse order is a severe security issue because the current process
     could regain privileges by restoring the real group ID with [Uni.setgid].
     For more details, see section POS36-C in the POSIX standard. *)
  if ngid <> ogid then if U.linux then U.setregid ngid ngid else U.setegid ngid;
  if nuid <> ouid then if U.linux then U.setreuid nuid nuid else U.seteuid nuid;
  (* Verify that old privileges cannot be restored with best-effort tests. *)
  if ngid <> ogid then (
    assert (not @@ succeed (fun () -> U.setegid ogid));
    assert (Unix.getegid () = ngid));
  if nuid <> ouid then (
    assert (not @@ succeed (fun () -> U.seteuid ouid));
    assert (Unix.geteuid () = nuid))

let add_worker t k =
  match Unix.fork () with
  | 0 ->
      (try
         (* XXX: Dropping privileges must be the worker's first action.
            DO NOT MOVE this line without a good reason. *)
         drop_privileges ();
         while true do
           k @@ Unix.getpid ()
         done
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         Logs.err (fun k -> k "%a" Util.pp_exception (e, bt)));
      exit 1
  | pid ->
      Logs.debug (fun k -> k "Creating worker %d" pid);
      Hashtbl.replace t.workers { pid } ()

let cleanup { workers } =
  Logs.debug (fun k -> k "Cleanup workers...");
  Hashtbl.iter
    (fun { pid } () ->
      Logs.debug (fun k -> k "Kill worker %d" pid);
      try Unix.kill pid Sys.sigterm with _ -> ())
    workers

let wait_any_child () = Geneweb_unix.waitpid_noeintr [] (-1) |> fst

let start n k =
  if (not Sys.unix) || n < 1 then invalid_arg "start";
  let t = { workers = Hashtbl.create n } in
  let parent_pid = Unix.getpid () in
  let finally () = if Unix.getpid () = parent_pid then cleanup t in
  Fun.protect ~finally @@ fun () ->
  for _ = 0 to n - 1 do
    add_worker t k
  done;
  while true do
    let pid = wait_any_child () in
    (* We never run out of children because each time a worker terminates,
       it is immediately replaced with a new one. *)
    assert (pid > 0);
    match Hashtbl.find t.workers { pid } with
    | () ->
        Logs.debug (fun k -> k "Worker %d is dead, replace it" pid);
        add_worker t k
    | exception Not_found -> assert false
  done
