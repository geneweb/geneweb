(* Permet de corriger une base par exemple en ajoutant le *)
(* domicile en tant qu'évènement sur les personnes.       *)
(*
   Sytaxe du fichier lu :
     nom;prénom;occ
     texte sur une ligne
     ligne_vide
*)

open Geneweb
open Def
open Gwdb

let trace = ref false

let read_file fname =
  let split_name name =
    match String.split_on_char ';' name with
      [sn; fn; occ; _] ->
        let occ =
          try int_of_string occ with
            Failure _ ->
              Printf.eprintf "*** Error int_of_string: %s\n" occ;
              flush stderr;
              exit 2
        in
        sn, fn, occ
    | _ -> Printf.eprintf "*** Error key: %s\n" name; flush stderr; exit 2
  in
  let list = ref [] in
  begin match (try Some (open_in fname) with Sys_error _ -> None) with
    Some ic ->
      begin try
        while true do
          let name = input_line ic in
          let note = input_line ic in
          let _empty = input_line ic in
          let (sn, fn, occ) = split_name name in
          list := ((sn, fn, occ), note) :: !list
        done
      with End_of_file -> ()
      end;
      close_in ic
  | None -> ()
  end;
  List.rev !list

let gen_replace all s s1 s2 =
  let buff = Buffer.create 30 in
  let () = Buffer.clear buff in
  let rec loop i =
    if i >= String.length s then Buffer.contents buff
    else if s.[i] = s1.[0] then
      try
        let sub = String.sub s i (String.length s1) in
        if sub = s1 then
          begin
            Buffer.add_string buff s2;
            if all then loop (i + String.length s1) else Buffer.contents buff
          end
        else begin Buffer.add_char buff s.[i]; loop (i + 1) end
      with Invalid_argument _ -> Buffer.add_char buff s.[i]; loop (i + 1)
    else begin Buffer.add_char buff s.[i]; loop (i + 1) end
  in
  loop 0

let replace_all = gen_replace true
let replace_first = gen_replace false

let update_database_with_alias base fname =
  let list = read_file fname in
  let changed = ref false in
  let nb_modified = ref 0 in
  List.iter
    (fun ((sn, fn, occ), note) ->
       match Gwdb.person_of_key base fn sn occ with
         Some ip ->
           let p = poi base ip in
           if !trace then
             begin
               Printf.eprintf "Modifiy person : %s\n" (Gutil.designation base p);
               flush stderr
             end;
           let note = Gwdb.insert_string base note in
           let first_names_aliases = get_first_names_aliases p @ [note] in
           let gp =
             {(gen_person_of_person p) with first_names_aliases =
               first_names_aliases}
           in
           patch_person base gp.key_index gp;
           changed := true;
           incr nb_modified
       | None ->
           Printf.eprintf "Person not in the database anymore : %s.%d %s\n" fn occ
             sn;
           flush stderr)
    list;
  if !changed then
    begin
      commit_patches base;
      Printf.eprintf "Number of modified persons: %d\n" !nb_modified;
      flush stderr
    end


(**/**)


let bname = ref ""
let fname = ref ""

let speclist =
  ["-f", Arg.String (fun x -> fname := x), "file to read info from";
   "-t", Arg.Set trace, "trace modified person"]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base <file>"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" || !fname = "" then
    begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false
    ~onerror:(fun () ->
        Printf.eprintf "Cannot lock database. Try again.\n";
        flush stderr)
      (fun () ->
         let base = Gwdb.open_base !bname in
         update_database_with_alias base !fname)

let _ = main ()










