(* camlp4r *)
(* $Id: public.ml,v 4.26 2007/01/19 09:03:02 deraugla Exp $ *)

open Def;
open Gwdb;
open Printf;


value input_person file = do {
  let pl = ref [] in
  match try Some (open_in file) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        try
          while True do {
            let line = input_line ic in
            pl.val := [ line :: pl.val ]
          }
        with [ End_of_file -> () ];
        close_in ic
      }
  | None ->
      do {
        Printf.eprintf "Error while opening file %s\n" file;
        flush stderr;
      }] ;
  List.rev pl.val
};

value private_everybody bname =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if get_access p <> Private then
        let p = {(gen_person_of_person p) with access = Private} in
        patch_person base p.key_index p
      else ();
    };
    commit_patches base;
  }
;

value private_some bname key =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      do {
         if get_access p <> Private then
         let p = {(gen_person_of_person p) with access = Private} in
         patch_person base p.key_index p
         else ();
         commit_patches base;
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        (*
           Si on appel private_some sur une liste et qu'il
           y'a une mauvaise clé, alors on quitte tout le
           script, c'est un peu radical.
        *)
        (*exit 2*)
      } ]
;

value private_some_list bname file =
  if Sys.file_exists file then
    let pl = input_person file in
    List.iter (private_some bname) pl
  else
    do {
      Printf.eprintf "File does not exist : %s\n" file;
      flush stderr;
      exit 2
    }
;

value list_ind = ref "";
value ind = ref "";
value bname = ref "";
value everybody = ref False;

value speclist =
  [("-everybody", Arg.Set everybody, "set flag private to everybody [option
  lente!]");
   ("-ind", Arg.String (fun x -> ind.val := x),
    "individual key");
   ("-list-ind", Arg.String (fun s -> list_ind.val := s), "<file> file to the list of persons")]
;
value anonfun i = bname.val := i;
value usage = "Usage: private [-everybody] [-ind key] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    if everybody.val then private_everybody bname.val
    else if list_ind.val = "" then private_some bname.val ind.val
    else private_some_list bname.val list_ind.val
  }
;

main ();
