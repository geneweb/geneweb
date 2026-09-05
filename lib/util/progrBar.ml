(* $Id: progrBar.ml,v 5.4 2007-02-01 10:28:55 ddr Exp $ *)

(* bar size in characters *)
let size = 60
let draw_rep = 5
let draw = "|/-\\"
let empty = ref '.'
let full = ref '#'
let draw_len = String.length draw
let pb_cnt = size * draw_rep * draw_len
let default_width = 60
let default_empty = '.'
let default_full = '#'

(* Progress bars redraw in place using carriage returns. That only makes sense
   on a terminal: when output is redirected to a file (e.g. comm.log) the
   redraws are never overwritten and pile up into one huge line. So the bar is
   gated on whether the relevant channel is a tty.

   The two APIs target different channels: [with_bar]/[progress] print to a
   formatter (in practice [Format.std_formatter], i.e. stdout), while the
   deprecated [start]/[run]/[finish] print to stderr. Hence the two checks. *)
let stdout_is_tty = Unix.isatty Unix.stdout
let stderr_is_tty = Unix.isatty Unix.stderr

type t = {
  ppf : Format.formatter;
  width : int;
  empty : char;
  full : char;
  disabled : bool;
  tty : bool;
  mutable last_output : float;
}

let progress t current total =
  if (not t.disabled) && t.tty then
    let now = Unix.gettimeofday () in
    if now -. t.last_output < 0.2 then ()
    else (
      t.last_output <- now;
      let filled = current * t.width / total in
      let bar =
        Format.asprintf "%s%s"
          (String.make filled t.full)
          (String.make (t.width - filled) t.empty)
      in
      Format.fprintf t.ppf "\r[%s] %d%%" bar (current * 100 / total);
      Format.pp_print_flush t.ppf ())

let finish_bar t =
  if t.tty then
    Format.fprintf t.ppf "\r[%s] 100%%@." (String.make t.width t.full)
  else Format.fprintf t.ppf "[%s] 100%%@." (String.make t.width t.full)

let with_bar ?(width = default_width) ?(empty = default_empty)
    ?(full = default_full) ?(disabled = false) ?(tty = stdout_is_tty) ppf f =
  let t = { ppf; width; empty; full; disabled; tty; last_output = 0. } in
  if not disabled then (
    Format.pp_print_flush t.ppf ();
    Fun.protect ~finally:(fun () -> finish_bar t) @@ fun () -> f t)
  else f t

let start () =
  if stderr_is_tty then (
    for _i = 1 to size do
      Printf.eprintf "%c" !empty
    done;
    Printf.eprintf "\013")

let run cnt max_cnt =
  if stderr_is_tty then (
    let pb_cnt = if max_cnt < pb_cnt then size * draw_len else pb_cnt in
    let already_disp = cnt * size / max_cnt in
    let to_disp = (cnt + 1) * size / max_cnt in
    for _i = already_disp + 1 to to_disp do
      Printf.eprintf "%c" !full
    done;
    let already_disp = cnt * pb_cnt / max_cnt in
    let to_disp = (cnt + 1) * pb_cnt / max_cnt in
    (if cnt = max_cnt - 1 then Printf.eprintf " \008"
     else if to_disp > already_disp then
       let k = to_disp mod draw_len in
       let k = if k < 0 then draw_len + k else k in
       Printf.eprintf "%c\008" draw.[k]);
    flush stderr)

let suspend () =
  if stderr_is_tty then (
    Printf.eprintf "%c\n" !full;
    flush stderr)

let restart cnt max_cnt =
  start ();
  for i = 0 to cnt do
    run i max_cnt
  done

let finish () =
  if stderr_is_tty then (
    Printf.eprintf "\n";
    flush stderr)
