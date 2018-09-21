(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

(* Permet de corriger une base par exemple en ajoutant le *)
(* domicile en tant qu'évènement sur les personnes.       *)
(*
   Sytaxe du fichier lu :
     nom;prénom;occ
     texte sur une ligne
     ligne_vide
*)


open Def
open Gwdb


let trace = ref false


let split str sep =
  let i = String.index str sep in
  let s = String.sub str 0 i in
  let sn = String.sub str (i + 1) (String.length str - i - 1) in s, sn

let explode str sep =
  let rec loop s accu =
    try let (s, sn) = split s sep in loop sn (s :: accu) with
      Not_found -> s :: accu
  in
  List.rev (loop str [])

let read_file fname =
  let split_name name =
    match explode name ';' with
      [sn; fn; occ] ->
        let occ =
          try int_of_string occ with
            Failure _ ->
              Printf.eprintf "*** Error int_of_string: %s" occ; flush stderr; exit 2
        in
        sn, fn, occ
    | _ -> Printf.eprintf "*** Error key: %s" name; flush stderr; exit 2
  in
  let list = ref [] in
  let () =
    match try Some (open_in fname) with Sys_error _ -> None with
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
  in
  List.rev !list

let toc_list = [] (* ?? *)

let lines_list_of_string s =
  let rec loop no_toc lines len i =
    if i = String.length s then
      List.rev (if len = 0 then lines else Buff.get len :: lines), no_toc
    else if s.[i] = '\n' then
      let line = Buff.get len in
      let no_toc = List.mem line toc_list || no_toc in
      loop no_toc (line :: lines) 0 (i + 1)
    else loop no_toc lines (Buff.store len s.[i]) (i + 1)
  in
  fst (loop false [] 0 0)

let update_database_with_file base fname =
  let empty = Gwdb.insert_string base "" in
  let list = read_file fname in
  List.iter
    (fun ((sn, fn, occ), note) ->
       match Gwdb.person_of_key base fn sn occ with
         Some ip ->
           let p = poi base ip in
           if !trace then
             begin
               Printf.eprintf "Modifiy person : %s" (Gutil.designation base p);
               flush stderr
             end;
           let evt =
             {epers_name = Epers_Residence; epers_date = Adef.cdate_None;
              epers_place = Gwdb.insert_string base note;
              epers_reason = empty; epers_note = empty; epers_src = empty;
              epers_witnesses = [| |]}
           in
           let pnote = sou base (get_notes p) in
           let lines = lines_list_of_string pnote in
           let notes =
             List.fold_right
               (fun line accu -> if line = note then accu else line :: accu)
               lines []
           in
           let notes = String.concat "\n" notes in
           let pevents = get_pevents p @ [evt] in
           let gp =
             {(gen_person_of_person p) with pevents = pevents;
              notes = Gwdb.insert_string base notes}
           in
           patch_person base gp.key_index gp
       | None ->
           Printf.eprintf "Person not in the database anymore : %s.%d %s" fn occ sn;
           flush stderr)
    list


(**/**)


let bname = ref ""
let fname = ref ""

let speclist =
  ["-f", Arg.String (fun x -> fname := x), "file to read info from";
   "-t", Arg.String (fun x -> fname := x), "trace modified person"]
let anonfun i = bname := i
let usage = "Usage: " ^ Sys.argv.(0) ^ " base <file>"

let main () =
  Arg.parse speclist anonfun usage;
  if !bname = "" || !fname = "" then
    begin Arg.usage speclist usage; exit 2 end;
  Lock.control (Mutil.lock_file !bname) false
    ~onerror:Lock.print_try_again
    (fun () ->
       let base = Gwdb.open_base !bname in
       let () = update_database_with_file base !fname in close_base base)

let _ = main ()
