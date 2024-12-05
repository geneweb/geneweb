module SS = Set.Make (String)
module MS = Map.Make (String)

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    let ppf = Fmt.with_buffer ~like b in
    Fmt.set_style_renderer ppf `Ansi_tty;
    ( ppf,
      fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }

let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_raw_string

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX s -> Fmt.string ppf s
  | ADDR_INET (addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr addr) port

let pp_exn ppf exn =
  let s = Printexc.to_string exn in
  if Printexc.backtrace_status () then
    Fmt.pf ppf "%s@ backtrace:@ %s" s (Printexc.get_backtrace ())
  else Fmt.string ppf s

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
