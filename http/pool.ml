let src = Logs.Src.create ~doc:"Pool" "POOL"

module Log = (val Logs.src_log src : Logs.LOG)

type worker = int
type t = { workers : (worker, unit) Hashtbl.t } [@@unboxed]

(* Fork the current process using a dead man approach to detect
   parent termination.

   A pipe is established between the parent and the child process. The child
   monitors the read-end of the pipe. If the parent process dies, the operating
   system closes the write-end, unblocking the [Unix.select] call. *)
let fork child parent =
  let fd_in, fd_out = Unix.pipe ~cloexec:true () in
  let check_dead_man () =
    let fds, _, _ = My_unix.select_noeintr [ fd_in ] [] [] (-1.) in
    assert (fds == [ fd_in ]);
    Unix._exit 1
  in
  match Unix.fork () with
  | 0 ->
      Unix.close fd_out;
      (* One doesn't need to join this watchdog thread at the end of the child
         process, as the process will exit in [child]. *)
      let (_ : Thread.t) = Thread.create check_dead_man () in
      child ()
  | pid ->
      (* Intentionally keep [fd_out] open. It must remain inaccessible to
         the rest of the parent and will be closed automatically by the
         operating system if the parent dies. *)
      Unix.close fd_in;
      parent pid

let add_worker t k =
  let child () =
    while true do
      k @@ Unix.getpid ()
    done
  in
  let parent pid =
    Log.debug (fun k -> k "Creating worker %d" pid);
    Hashtbl.replace t.workers pid ()
  in
  fork child parent

let wait_any_child () = My_unix.waitpid_noeintr [] (-1) |> fst

let start n k =
  if (not Sys.unix) || n < 1 then invalid_arg "start";
  let t = { workers = Hashtbl.create n } in
  for _ = 0 to n - 1 do
    add_worker t k
  done;
  while true do
    let pid = wait_any_child () in
    (* We never run out of children because each time a worker terminates,
       it is immediately replaced with a new one. *)
    assert (pid > 0);
    match Hashtbl.find t.workers pid with
    | () ->
        Log.debug (fun k -> k "Worker %d is dead, replace it" pid);
        add_worker t k
    | exception Not_found -> assert false
  done
