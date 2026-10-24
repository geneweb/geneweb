let src = Logs.Src.create ~doc:"Pool" "POOL"

module Log = (val Logs.src_log src : Logs.LOG)

type worker = { pid : int } [@@unboxed]
type t = { workers : (worker, unit) Hashtbl.t } [@@unboxed]

let add_worker t k =
  match Unix.fork () with
  | 0 ->
      while true do
        k @@ Unix.getpid ()
      done
  | pid ->
      Log.debug (fun k -> k "Creating worker %d" pid);
      Hashtbl.replace t.workers { pid } ()

let cleanup { workers } =
  Log.debug (fun k -> k "Cleanup workers...");
  Hashtbl.iter
    (fun { pid } () ->
      Log.debug (fun k -> k "Kill worker %d" pid);
      try Unix.kill pid Sys.sigterm with _ -> ())
    workers

let wait_any_child () = My_unix.waitpid_noeintr [] (-1)

let signal_name signal =
  let names =
    [
      (Sys.sigsegv, "SIGSEGV");
      (Sys.sigbus, "SIGBUS");
      (Sys.sigabrt, "SIGABRT");
      (Sys.sigkill, "SIGKILL");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigfpe, "SIGFPE");
      (Sys.sigill, "SIGILL");
    ]
  in
  match List.assoc_opt signal names with
  | Some name -> name
  | None -> Printf.sprintf "signal %d" signal

let report_dead_worker pid status =
  match status with
  | Unix.WEXITED 0 -> Log.debug (fun k -> k "Worker %d is dead, replace it" pid)
  | Unix.WEXITED code ->
      Log.err (fun k -> k "Worker %d exited with code %d, replace it" pid code)
  | Unix.WSIGNALED signal ->
      Log.err (fun k ->
          k
            "Worker %d killed by %s while serving its last request (see the \
             request logged by this pid just before), replace it"
            pid (signal_name signal))
  | Unix.WSTOPPED signal ->
      Log.err (fun k ->
          k "Worker %d stopped by %s, replace it" pid (signal_name signal))

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
    let pid, status = wait_any_child () in
    (* We never run out of children because each time a worker terminates,
       it is immediately replaced with a new one. *)
    assert (pid > 0);
    match Hashtbl.find t.workers { pid } with
    | () ->
        report_dead_worker pid status;
        add_worker t k
    | exception Not_found -> assert false
  done
