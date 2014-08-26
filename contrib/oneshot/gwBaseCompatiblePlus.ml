(* camlp4r *)

open Def;
open Gwdb;
open Printf;


value bname = ref "";

value speclist = [];
value anonfun i = bname.val := i;
value usage = "Usage: gwBaseCompatiblePlus base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let base = Gwdb.open_base bname.val in
    if nb_of_persons base > 0 then
      try
        for i = 0 to nb_of_persons base - 1 do {
          let p = poi base (Adef.iper_of_int i) in
          let _ = sou base (get_birth_note p) in
          let _ = sou base (get_baptism_note p) in
          let _ = sou base (get_death_note p) in
          let _ = sou base (get_burial_note p) in
          let _ = get_pevents p in
          ()
        }
      with
      [ _ ->
        do {
          Printf.eprintf "GeneWeb base not compatible\n";
          flush stderr;
          exit 2
        }]
    else ()
  }
;

main ();






