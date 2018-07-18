(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Gwaccess

let list_ind = ref ""
let ind = ref ""
let bname = ref ""
let everybody = ref false

let speclist =
  ["-everybody", Arg.Set everybody,
   "set flag public to everybody [slow option]";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-list-ind", Arg.String (fun s -> list_ind := s),
   "<file> file to the list of persons"]
let anonfun i = bname := i
let usage = "Usage: private [-everybody] [-ind key] [-list-ind file] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  match
    Lock.control (Mutil.lock_file !bname) false
      (fun () ->
         if !everybody then Gwaccess.access_everybody Def.Private !bname
         else if !list_ind = "" then
           Gwaccess.access_some Def.Private !bname !ind
         else Gwaccess.access_some_list Def.Private !bname !list_ind)
  with
    Some x -> x
  | None ->
      Printf.eprintf "Base is locked. Waiting... ";
      flush stderr;
      match
        Lock.control (Mutil.lock_file !bname) true
          (fun () ->
             Printf.eprintf "Ok\n";
             flush stderr;
             if !everybody then Gwaccess.access_everybody Def.Private !bname
             else if !list_ind = "" then
               Gwaccess.access_some Def.Private !bname !ind
             else Gwaccess.access_some_list Def.Private !bname !list_ind)
      with
        Some x -> x
      | None ->
          Printf.printf "\nSorry. Impossible to lock base.\n";
          flush stdout;
          exit 2

let _ = main ()
