(* $Id: restrict.ml,v 4.1 2001-04-22 03:31:16 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)
(* Build restrict file marking persons of less than 100 years *)

open Def;
open Gutil;

value cnt = ref 0;

value rec mark_descendants base hidden ip =
  if hidden.(Adef.int_of_iper ip) then ()
  else do {
    hidden.(Adef.int_of_iper ip) := True;
    incr cnt;
    let u = uoi base ip in
    for i = 0 to Array.length u.family - 1 do {
      let des = doi base u.family.(i) in
      for i = 0 to Array.length des.children - 1 do {
        mark_descendants base hidden des.children.(i)
      }
    };
  }
;

value restrict bname base =
  let _ = base.data.unions.array () in
  let _ = base.data.descends.array () in
  let today =
    let tm = Unix.localtime (Unix.time ()) in
    {day = tm.Unix.tm_mday; month = succ tm.Unix.tm_mon;
     year = tm.Unix.tm_year + 1900; prec = Sure; delta = 0}
  in
  let hidden = Array.create base.data.persons.len False in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      if not hidden.(i) then
        let p = base.data.persons.get i in
        match Adef.od_of_codate p.birth with
        [ Some (Dgreg d _) ->
            if (temps_ecoule d today).year <= 100 then
              mark_descendants base hidden p.cle_index
            else ()
        | _ -> () ]
      else ()
    };
    Printf.printf "total = %d\n" cnt.val;
    flush stdout;
    let oc = open_out (Filename.concat (bname ^ ".gwb") "restrict") in
    output_value oc hidden;
    close_out oc;
  }
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  let cnt = ref 0 in
  do {
    Argl.parse speclist (fun s -> bname.val := s) usage;
    let base = Iobase.input bname.val in
    restrict bname.val base
  }
;

Printexc.catch main ();
