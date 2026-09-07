let src = Logs.Src.create ~doc:"Pool" "POOL"

module Log = (val Logs.src_log src : Logs.LOG)

(* Implement a deadman strategy to detect parent termination.

   A pipe is established between the parent and the child process. The child
   monitors the read-end of the pipe with [watcher]. The parent leaks the
   file descriptors. If the parent dies, the operating system closes the
   writ-end, unblocking the watcher. *)
module Deadman : sig
  type t

  val create : unit -> t

  val watch : t -> unit
  (** [watch d] watches the deadman [d]. If the parent is dead, the child is
      immediately exited. *)
end = struct
  type t = { pid : int; fd_in : Unix.file_descr; fd_out : Unix.file_descr }

  let create () =
    let pid = Unix.getpid () in
    let fd_in, fd_out = Unix.pipe ~cloexec:true () in
    { pid; fd_in; fd_out }

  let watcher fd =
    let l, _, _ = My_unix.select_noeintr [ fd ] [] [] (-1.) in
    assert (l = [ fd ]);
    Unix._exit 1

  let watch { pid; fd_in; fd_out } =
    if pid = Unix.getpid () then
      failwith "watch: cannot watch yourself Narcissus!"
    else (
      Unix.close fd_out;
      (* Some signals are used by the OCaml runtime and one shouldn't block
         them. Currently, GeneWeb uses two process-wide signals:
         - [Sys.sigalrm] is used for timeout per request.
         - [Sys.sighup] is used to reload log files. *)
      let signals = [ Sys.sigalrm; Sys.sighup ] in
      let old_mask = Unix.sigprocmask SIG_BLOCK signals in
      let finally () =
        ignore (Unix.sigprocmask SIG_SETMASK old_mask : int list)
      in
      Fun.protect ~finally @@ fun () ->
      ignore (Thread.create watcher fd_in : Thread.t))
end

type worker = int
type t = { workers : (worker, unit) Hashtbl.t; deadman : Deadman.t }

let add_worker t k =
  match Unix.fork () with
  | 0 ->
      Deadman.watch t.deadman;
      while true do
        k @@ Unix.getpid ()
      done;
      assert false
  | pid ->
      (* Intentionally leaking the deadman descriptors in the parent process.
         - [fd_out] is kept open as its closure is used as a signal for the
           child of the parent termination.
         - [fd_in] is kept open to be able to create new worker with the
           same deadman. *)
      Hashtbl.add t.workers pid ();
      Log.debug (fun k -> k "Creating worker %d" pid)

let wait_any_child () = My_unix.waitpid_noeintr [] (-1) |> fst

let start n k =
  if (not Sys.unix) || n < 1 then invalid_arg "start";
  let deadman = Deadman.create () in
  let t = { workers = Hashtbl.create n; deadman } in
  for _ = 0 to n - 1 do
    add_worker t k
  done;
  while true do
    match wait_any_child () with
    | exception Unix.Unix_error (ECHILD, _, _) ->
        (* We never run out of children because each time a worker terminates,
           it is immediately replaced with a new one. *)
        assert false
    | pid -> (
        match Hashtbl.find t.workers pid with
        | () ->
            Log.debug (fun k -> k "Worker %d is dead, replace it" pid);
            Hashtbl.remove t.workers pid;
            add_worker t k
        | exception Not_found -> assert false)
  done
