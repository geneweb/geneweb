(* camlp4r *)
(* $Id: titres.ml,v 1.4 2006-10-30 09:37:58 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;

module Buff = Buff.Make (struct value buff = ref (String.create 80); end);

value lower_utf_8 s =
  loop 0 0 where rec loop i len =
    if i >= String.length s then Buff.get len
    else
      let c =
        if Name.nbc s.[i] = 1 then Char.lowercase s.[i]
	else s.[i]
      in
      loop (i + 1) (Buff.store len c)
;

value titres bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      List.iter
        (fun t ->
	   printf "%s/%s\n" (lower_utf_8 (sou base t.t_ident))
	     (lower_utf_8 (sou base t.t_place)))
        (get_titles p)
    };
    flush stdout;
  }
;

value bname = ref "";

value speclist =
  []
;
value anonfun i = bname.val := i;
value usage = "Usage: titres base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    titres bname.val
  }
;

main ();
