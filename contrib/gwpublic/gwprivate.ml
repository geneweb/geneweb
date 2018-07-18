(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Gwaccess;

value list_ind = ref "";
value ind = ref "";
value bname = ref "";
value everybody = ref False;

value speclist =
  [("-everybody", Arg.Set everybody, "set flag public to everybody [slow option]");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-list-ind", Arg.String (fun s -> list_ind.val := s), "<file> file to the list of persons")]
;
value anonfun i = bname.val := i;
value usage = "Usage: private [-everybody] [-ind key] [-list-ind file] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    lock (Mutil.lock_file bname.val) with
    [ Accept ->
        if everybody.val then Gwaccess.access_everybody Def.Private bname.val
        else if list_ind.val = "" then Gwaccess.access_some Def.Private bname.val ind.val
        else Gwaccess.access_some_list Def.Private bname.val list_ind.val
    | Refuse -> do {
        Printf.eprintf "Base is locked. Waiting... ";
        flush stderr;
        lock_wait (Mutil.lock_file bname.val) with
        [ Accept -> do {
            Printf.eprintf "Ok\n";
            flush stderr;
            if everybody.val then Gwaccess.access_everybody Def.Private bname.val
            else if list_ind.val = "" then Gwaccess.access_some Def.Private bname.val ind.val
            else Gwaccess.access_some_list Def.Private bname.val list_ind.val
          }
        | Refuse -> do {
            Printf.printf "\nSorry. Impossible to lock base.\n";
            flush stdout;
            exit 2
          } ]
    } ];
  }
;

main ();
