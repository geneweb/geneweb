(* $Id: progrBar.ml,v 5.4 2007-02-01 10:28:55 ddr Exp $ *)

let size = 60
let draw_rep = 5
let draw = "|/-\\"
let empty = ref '.'
let full = ref '#'

let draw_len = String.length draw
let pb_cnt = size * draw_rep * draw_len

let start () =
  for i = 1 to size do Printf.eprintf "%c" !empty done; Printf.eprintf "\013"

let run cnt max_cnt =
  let (pb_cnt, draw_rep) =
    if max_cnt < pb_cnt then size * draw_len, 1 else pb_cnt, draw_rep
  in
  let already_disp = cnt * size / max_cnt in
  let to_disp = (cnt + 1) * size / max_cnt in
  for i = already_disp + 1 to to_disp do Printf.eprintf "%c" !full done;
  let already_disp = cnt * pb_cnt / max_cnt in
  let to_disp = (cnt + 1) * pb_cnt / max_cnt in
  if cnt = max_cnt - 1 then Printf.eprintf " \008"
  else if to_disp > already_disp then
    begin let k = to_disp mod draw_len in
      let k = if k < 0 then draw_len + k else k in
      Printf.eprintf "%c\008" draw.[k]
    end;
  flush stderr

let suspend () = Printf.eprintf "%c\n" !full; flush stderr

let restart cnt max_cnt = start (); for i = 0 to cnt do run i max_cnt done

let finish () = Printf.eprintf "\n"; flush stderr
