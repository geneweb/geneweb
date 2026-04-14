let pp_exception ppf (exn, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception uncaught in process %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let exn = Printexc.to_string exn in
  let pid = Unix.getpid () in
  if Printexc.backtrace_status () then
    let bt = Printexc.raw_backtrace_to_string bt in
    Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header pid exn Fmt.lines bt
  else Fmt.pf ppf "@[%a@ %s@]" pp_header pid exn
