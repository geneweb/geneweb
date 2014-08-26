(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: gw_fix_base.ml,v 0.01 2014-02-21 16:26:45 flh Exp $ *)

(* Permet de corriger une base par exemple en ajoutant le *)
(* domicile en tant qu'évènement sur les personnes.       *)
(*
   Sytaxe du fichier lu :
     nom;prénom;occ
     texte sur une ligne
     ligne_vide
*)


open Def;
open Gwdb;
open Printf;


value trace = ref False;


value split str sep =
  let i = String.index str sep in
  let s = String.sub str 0 i in
  let sn = String.sub str (i + 1) (String.length str - i - 1) in
  (s, sn)
;

value explode str sep =
  let rec loop s accu =
    try
      let (s, sn) = split s sep in
      loop sn [s :: accu]
    with [ Not_found -> [s :: accu] ]
  in
  List.rev (loop str [])
;

value read_file fname = do {
  let split_name name =
    match explode name ';' with
    [ [sn; fn; occ; _] ->
        let occ =
          try int_of_string occ
          with [ Failure "int_of_string" ->
            do {
              eprintf "*** Error int_of_string: %s\n" occ;
              flush stderr;
              exit 2;
            }]
        in
        (sn, fn, occ)
    | _ ->
        do {
          eprintf "*** Error key: %s\n" name;
          flush stderr;
          exit 2;
        }]
  in
  let list = ref [] in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do {
        try
          while True do {
            let name = input_line ic in
            let note = input_line ic in
            let _empty = input_line ic in
            let (sn, fn, occ) = split_name name in
            list.val := [((sn, fn, occ), note) :: list.val]
          }
        with [ End_of_file -> () ];
        close_in ic
      }
  | None -> () ];
  List.rev list.val
};

value gen_replace all s s1 s2 =
  let buff = Buffer.create 30 in
  let () = Buffer.clear buff in
  loop 0 where rec loop i =
    if i >= String.length s then Buffer.contents buff
    else if s.[i] = s1.[0] then
      try
        let sub = String.sub s i (String.length s1) in
        if sub = s1 then do {
          Buffer.add_string buff s2;
          if all then loop (i + String.length s1)
          else Buffer.contents buff
        }
        else do {
          Buffer.add_char buff s.[i];
          loop (i + 1)
        }
      with [ Invalid_argument _ ->
        do {
          Buffer.add_char buff s.[i];
          loop (i + 1)
        } ]
   else do {
     Buffer.add_char buff s.[i];
     loop (i + 1)
   }
;

value replace_all = gen_replace True;
value replace_first = gen_replace False;

value update_database_with_domicile base fname = do {
  let empty = Gwdb.insert_string base "" in
  let list = read_file fname in
  let changed = ref False in
  let nb_modified = ref 0 in
  List.iter
    (fun ((sn, fn, occ), note) ->
       do {
         match Gwdb.person_of_key base fn sn occ with
         [ Some ip ->
             do {
               let p = poi base ip in
               if trace.val then do {
                 eprintf "Modifiy person : %s\n" (Gutil.designation base p);
                 flush stderr
               }
               else ();
               let evt =
                 {epers_name = Epers_Residence; epers_date = Adef.codate_None;
                  epers_place = Gwdb.insert_string base note;
                  epers_reason = empty; epers_note = empty;
                  epers_src = empty; epers_witnesses = [| |]}
               in
               let pnote = sou base (get_notes p) in
               let flex_dom = "<br/>-Domicile: " ^ note ^ "<br/>" in
               let notes = replace_all pnote flex_dom "" in
               let notes = Gwdb.insert_string base notes in
               let pevents = (get_pevents p) @ [evt] in
               let gp =
                 {(gen_person_of_person p) with pevents = pevents; notes = notes}
               in
               patch_person base gp.key_index gp;
               changed.val := True;
               incr nb_modified
             }
         | None ->
             do {
               eprintf "Person not in the database anymore : %s.%d %s\n" fn occ sn;
               flush stderr
             } ]
       })
    list;
  if changed.val then do {
    commit_patches base;
    eprintf "Number of modified persons: %d\n" nb_modified.val;
    flush stderr
  }
  else ()
};

value update_database_with_alias base fname = do {
  let list = read_file fname in
  let changed = ref False in
  let nb_modified = ref 0 in
  List.iter
    (fun ((sn, fn, occ), note) ->
       do {
         match Gwdb.person_of_key base fn sn occ with
         [ Some ip ->
             do {
               let p = poi base ip in
               if trace.val then do {
                 eprintf "Modifiy person : %s\n" (Gutil.designation base p);
                 flush stderr
               }
               else ();
               let note = Gwdb.insert_string base note in
               let first_names_aliases = (get_first_names_aliases p) @ [note] in
               let gp =
                 {(gen_person_of_person p) with first_names_aliases = first_names_aliases}
               in
               patch_person base gp.key_index gp;
               changed.val := True;
               incr nb_modified
             }
         | None ->
             do {
               eprintf "Person not in the database anymore : %s.%d %s\n" fn occ sn;
               flush stderr
             } ]
       })
    list;
  if changed.val then do {
    commit_patches base;
    eprintf "Number of modified persons: %d\n" nb_modified.val;
    flush stderr
  }
  else ()
};


(**/**)


value bname = ref "";
value fname = ref "";

value speclist =
  [ ("-f", Arg.String (fun x -> fname.val := x), "file to read info from");
    ("-t", Arg.Set trace, "trace modified person")]
;
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " base <file>";

value main () = do {
  Arg.parse speclist anonfun usage;
  if bname.val = "" || fname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
  lock Mutil.lock_file bname.val with
  [ Accept ->
      let base = Gwdb.open_base bname.val in
      update_database_with_domicile base fname.val
  | Refuse -> do {
      eprintf "Cannot lock database. Try again.\n";
      flush stderr;
    } ]
};

main ();
