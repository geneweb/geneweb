(* camlp4r *)

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
value usage = "Usage: public [-everybody] [-ind key] [-list-ind file] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    if everybody.val then Gwaccess.access_everybody Public bname.val
    else if list_ind.val = "" then Gwaccess.access_some Public bname.val ind.val
    else Gwaccess.access_some_list Public bname.val list_ind.val
  }
;

main ();
