let pp_exception ppf (e, bt) =
  let pp_header ppf pid = Fmt.pf ppf "Exception uncaught in process %d:" pid in
  let pp_header = Fmt.(styled (`Fg `Red) pp_header) in
  let lines =
    String.split_on_char '\n' @@ Printexc.raw_backtrace_to_string bt
  in
  Fmt.pf ppf "@[%a@ %s@ %a@]" pp_header (Unix.getpid ()) (Printexc.to_string e)
    Fmt.(list ~sep:cut string)
    lines
