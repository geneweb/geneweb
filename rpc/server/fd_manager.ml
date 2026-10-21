type 'a limit = Unlimited | Limit of 'a

(* We cannot use directly polynomorphic equality/hash on Lwt file descriptors,
   as these values can change while the underlying Unix file descriptors remain
   unchanged. *)
module H = Hashtbl.Make (struct
  type t = Lwt_unix.file_descr

  let equal fd1 fd2 =
    Lwt_unix.unix_file_descr fd1 = Lwt_unix.unix_file_descr fd2

  let hash fd = Hashtbl.hash @@ Lwt_unix.unix_file_descr fd
end)

type t = {
  handles : float H.t;
      (* Map of active file descriptors to the last time
         a ping was received on it. *)
  max_connection : int limit;
  idle_timeout : float limit;
}

let limit_of_option = function Some v -> Limit v | None -> Unlimited

let safe_close fd =
  match Lwt_unix.state fd with
  | Opened -> Lwt_unix.close fd
  | _ -> Lwt.return ()

let add { handles; max_connection; _ } fd =
  match max_connection with
  | Limit i when H.length handles >= i ->
      let%lwt () = safe_close fd in
      Lwt.return false
  | _ ->
      let now = Unix.gettimeofday () in
      H.replace handles fd now;
      Lwt.return true

let close { handles; _ } fd =
  H.remove handles fd;
  safe_close fd

let close_idle ({ handles; idle_timeout; _ } as t) =
  let now = Unix.gettimeofday () in
  let dead =
    H.fold
      (fun fd tm dead ->
        match (Lwt_unix.state fd, idle_timeout) with
        | Lwt_unix.(Aborted _ | Closed), _ -> fd :: dead
        | Opened, Limit v when now -. tm > v -> fd :: dead
        | _ -> dead)
      handles []
  in
  let l = List.map (close t) dead in
  Lwt.join l

let ping { handles; idle_timeout; _ } fd =
  match idle_timeout with
  | Unlimited -> ()
  | Limit _ ->
      let now = Unix.gettimeofday () in
      H.replace handles fd now

let make ?max_connection ?idle_timeout () =
  let max_connection = limit_of_option max_connection in
  let idle_timeout = limit_of_option idle_timeout in
  { handles = H.create 17; max_connection; idle_timeout }

let log_manager_exn exn =
  Logs.err (fun k ->
      k "Uncaught exception in the file descriptor manager:@ %a" Util.pp_exn exn);
  raise exn

open Lwt.Infix

let loop ?(sleep = 5.) t =
  match t.idle_timeout with
  | Limit _ ->
      let rec loop_idle () =
        close_idle t >>= fun () -> Lwt_unix.sleep sleep >>= loop_idle
      in
      Lwt.dont_wait loop_idle log_manager_exn
  | Unlimited -> ()
