(* camlp4r *)

open Def;
open Gwdb;
open Printf;


value image_start_with_gallery img =
  let gallery = "http://www.geneanet.org/gallery/" in
  Mutil.start_with gallery img
;

value remove_image_everybody bname trace =
  let base = Gwdb.open_base bname in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let p = poi base (Adef.iper_of_int i) in
      if image_start_with_gallery (sou base (get_image p)) then do {
        if trace then printf "%s\n" (Gutil.designation base p)
        else ();
        let empty = Gwdb.insert_string base "" in
        let p = {(gen_person_of_person p) with image = empty} in
        patch_person base p.key_index p
      }
      else ();
    };
    commit_patches base;
  }
;

value remove_image_some bname key trace =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
  [ [ip] ->
      let p = poi base ip in
      do { 
         if image_start_with_gallery (sou base (get_image p)) then do {
           if trace then printf "%s\n" (Gutil.designation base p)
           else ();
           let empty = Gwdb.insert_string base "" in
           let p = {(gen_person_of_person p) with image = empty} in
           patch_person base p.key_index p
         }
         else ();
         commit_patches base;
      }
  | _ ->
      do {
        Printf.eprintf "Bad key %s\n" key;
        flush stderr;
        exit 2
      } ]
;

value ind = ref "";
value bname = ref "";
value everybody = ref False;
value trace = ref False;

value speclist =
   [("-everybody", Arg.Set everybody, "remove image for everybody");
   ("-ind", Arg.String (fun x -> ind.val := x), "individual key");
   ("-t", Arg.Set trace, "trace changed persons") ]
;
value anonfun i = bname.val := i;
value usage = "Usage: gwRemoveImgGallery [-everybody] [-ind key] base";

value main () =
  do {
    Arg.parse speclist anonfun usage;
    if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
    let gcc = Gc.get () in
    gcc.Gc.max_overhead := 100;
    Gc.set gcc;
    if everybody.val then remove_image_everybody bname.val trace.val
    else remove_image_some bname.val ind.val trace.val
  }
;

main ();
