module SS = Set.Make (String)
module MS = Map.Make (String)

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr addr) port

let pp_exception ppf (exn, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception uncaught in process %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let exn = Printexc.to_string exn in
  let pid = Unix.getpid () in
  if Printexc.backtrace_status () then
    let bt = Printexc.raw_backtrace_to_string bt in
    Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header pid exn Fmt.lines bt
  else Fmt.pf ppf "@[%a@ %s@]" pp_header pid exn

let pp_duration ppf f =
  let min = Int.of_float f / 60 and sec = mod_float f 60. in
  if min > 0 then Fmt.pf ppf "%dm%ds" min (Int.of_float sec)
  else
    let ms, sec = Float.modf sec in
    if sec > 0. then
      Fmt.pf ppf "%ds%dms" (Int.of_float sec) (Int.of_float (1_000. *. ms))
    else Fmt.pf ppf "%dms" (Int.of_float (1_000. *. ms))

open Lwt.Infix

let with_timer f x =
  let start = Unix.gettimeofday () in
  let%lwt r = f x in
  let stop = Unix.gettimeofday () in
  Lwt.return (r, stop -. start)

(* This wrapper cannot prevent [f] to stuck on blocking operations and
   doing heavy computations without calling Lwt.pause. *)
let with_timeout ~timeout f x =
  let task = f x >>= fun r -> Lwt.return @@ `Return r in
  if timeout = 0. then task
  else
    let timer = Lwt_unix.sleep timeout >>= fun () -> Lwt.return `Timeout in
    Lwt.pick [ Lwt.protected task; timer ]

(* Similar implementation of Fun.protect but compatible with Lwt. *)
let protect ~finally f =
  try%lwt f () >>= finally with exn -> finally () >>= Lwt.reraise exn
