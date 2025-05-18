(* TODO: After switching to Logs, the two lines below will directly call the Logs
      library.

   let src = Logs.Src.create ~doc:"Pool" __MODULE__
   module Log = (val Logs.src_log src : Logs.LOG) *)

module Logs = Geneweb_logs.Logs

type worker = { pid : int } [@@unboxed]
type t = { workers : (worker, unit) Hashtbl.t } [@@unboxed]

let pp_exception ppf (e, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception raised in %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let lines =
    String.split_on_char '\n' @@ Printexc.raw_backtrace_to_string bt
  in
  Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header (Unix.getpid ()) (Printexc.to_string e)
    Fmt.(list ~sep:cut string)
    lines

let add_worker t k =
  match Unix.fork () with
  | 0 ->
      (try
         while true do
           k @@ Unix.getpid ()
         done
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         Logs.info (fun k -> k "%a" pp_exception (e, bt)));
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

let wait_any_child () = My_unix.waitpid_noeintr [] (-1) |> fst

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
