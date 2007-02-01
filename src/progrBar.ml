(* $Id: progrBar.ml,v 5.4 2007-02-01 10:28:55 ddr Exp $ *)

value size = 60;
value draw_rep = 5;
value draw = "|/-\\";
value empty = ref '.';
value full = ref '#';

value draw_len = String.length draw;
value pb_cnt = size * draw_rep * draw_len;

value start () =
  do {
    for i = 1 to size do { Printf.eprintf "%c" empty.val };
    Printf.eprintf "\013"
  }
;

value run cnt max_cnt =
  let (pb_cnt, draw_rep) =
    if max_cnt < pb_cnt then (size * draw_len, 1) else (pb_cnt, draw_rep)
  in
  do {
    let already_disp = cnt * size / max_cnt in
    let to_disp = (cnt + 1) * size / max_cnt in
    for i = already_disp + 1 to to_disp do {
      Printf.eprintf "%c" full.val
    };
    let already_disp = cnt * pb_cnt / max_cnt in
    let to_disp = (cnt + 1) * pb_cnt / max_cnt in
    if cnt = max_cnt - 1 then Printf.eprintf " \008"
    else if to_disp > already_disp then
      let k = to_disp mod draw_len in
      let k = if k < 0 then draw_len + k else k in
      Printf.eprintf "%c\008" draw.[k]
    else ();
    flush stderr;
  }
;

value suspend () = do {
  Printf.eprintf "%c\n" full.val;
  flush stderr;
};

value restart cnt max_cnt = do {
  start ();
  for i = 0 to cnt do { run i max_cnt };
};

value finish () = do {
  Printf.eprintf "\n";
  flush stderr;
};
