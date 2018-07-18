(* camlp4r *)

open Def
open Gwdb
open Printf


let image_start_with_gallery img =
  let gallery = "http://www.geneanet.org/gallery/" in
  Mutil.start_with gallery img

let remove_image_everybody bname trace =
  let base = Gwdb.open_base bname in
  for i = 0 to nb_of_persons base - 1 do
    let p = poi base (Adef.iper_of_int i) in
    if image_start_with_gallery (sou base (get_image p)) then
      begin
        if trace then printf "%s\n" (Gutil.designation base p);
        let empty = Gwdb.insert_string base "" in
        let p = {(gen_person_of_person p) with image = empty} in
        patch_person base p.key_index p
      end
  done;
  commit_patches base

let remove_image_some bname key trace =
  let base = Gwdb.open_base bname in
  match Gutil.person_ht_find_all base key with
    [ip] ->
      let p = poi base ip in
      if image_start_with_gallery (sou base (get_image p)) then
        begin
          if trace then printf "%s\n" (Gutil.designation base p);
          let empty = Gwdb.insert_string base "" in
          let p = {(gen_person_of_person p) with image = empty} in
          patch_person base p.key_index p
        end;
      commit_patches base
  | _ ->
      match Gutil.person_of_string_dot_key base key with
        Some ip ->
          let p = poi base ip in
          if get_access p <> Private then
            begin let p = {(gen_person_of_person p) with access = Private} in
              patch_person base p.key_index p
            end;
          commit_patches base
      | None -> Printf.eprintf "Bad key %s\n" key; flush stderr; exit 2

let ind = ref ""
let bname = ref ""
let everybody = ref false
let trace = ref false

let speclist =
  ["-everybody", Arg.Set everybody, "remove image for everybody";
   "-ind", Arg.String (fun x -> ind := x), "individual key";
   "-t", Arg.Set trace, "trace changed persons"]
let anonfun i = bname := i
let usage = "Usage: gwRemoveImgGallery [-everybody] [-ind key] base"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" then begin Arg.usage speclist usage; exit 2 end;
  let gcc = Gc.get () in
  gcc.Gc.max_overhead <- 100;
  Gc.set gcc;
  if !everybody then remove_image_everybody !bname !trace
  else remove_image_some !bname !ind !trace

let _ = main ()
