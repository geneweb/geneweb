(* camlp5r ../../src/pa_lock.cmo *)
(* $Id: gwFixY.ml,v 0.01 2015-04-29 09:48:20 flh Exp $ *)

open Def;
open Gwdb;
open Printf;

(* Copie de util.ml *)
value gen_only_printable or_nl s =
  let s' = String.create (String.length s) in
  do {
    for i = 0 to String.length s - 1 do {
      s'.[i] :=
        if Mutil.utf_8_db.val && Char.code s.[i] > 127 then s.[i]
        else
          match s.[i] with
          [ ' '..'~' | '\160'..'\255' -> s.[i]
          | '\n' -> if or_nl then '\n' else ' '
          | _ -> ' ' ]
    };
    Gutil.strip_spaces s'
  }
;

value only_printable = gen_only_printable False;


(**/**)

value trace = ref False;

value fix_occu_y base = do {
  let changed = ref False in
  let nb_ind_modified = ref 0 in
  let nb_fam_modified = ref 0 in
  let regexp_one = Str.regexp_string "Y," in
  let regexp_two = Str.regexp_string "Y<br>" in
  for i = 0 to nb_of_persons base - 1 do {
    let p = poi base (Adef.iper_of_int i) in
    let updt = ref False in
    let occu = sou base (get_occupation p) in
    let () =
      if Str.string_match regexp_one occu 0 then updt.val := True
      else ()
    in
    let new_occu = Str.global_replace regexp_one "" occu in
    let new_occu = only_printable new_occu in
    let new_occu = Gwdb.insert_string base new_occu in
    let new_pevents =
      List.map
        (fun evt ->
          let note = sou base evt.epers_note in
          let () =
            if Str.string_match regexp_one note 0 ||
               Str.string_match regexp_two note 0
            then updt.val := True
            else ()
          in
          let new_note =
            Str.global_replace regexp_two "" note
          in
          let new_note =
            Str.global_replace regexp_one "" new_note
          in
          let new_note = only_printable new_note in
          let new_note = Gwdb.insert_string base new_note in
          {(evt) with epers_note = new_note})
        (get_pevents p)
    in
    if updt.val then do {
      if trace.val then do {
        eprintf "Modifiy person : %s\n" (Gutil.designation base p);
        flush stderr
      }
      else ();
      let gp =
        {(gen_person_of_person p) with occupation = new_occu;
          pevents = new_pevents}
      in
      patch_person base gp.key_index gp;
      changed.val := True;
      incr nb_ind_modified
    }
    else ()
  };
  for i = 0 to nb_of_families base - 1 do {
    let fam = foi base (Adef.ifam_of_int i) in
    let updt = ref False in
    let new_fevents =
      List.map
        (fun evt ->
          let note = sou base evt.efam_note in
          let () =
            if Str.string_match regexp_one note 0 ||
               Str.string_match regexp_two note 0
            then updt.val := True
            else ()
          in
          let new_note =
            Str.global_replace regexp_two "" note
          in
          let new_note =
            Str.global_replace regexp_one "" new_note
          in
          let new_note = only_printable new_note in
          let new_note = Gwdb.insert_string base new_note in
          {(evt) with efam_note = new_note})
        (get_fevents fam)
    in
    if updt.val then do {
      if trace.val then do {
        let fath = poi base (get_father fam) in
        let moth = poi base (get_mother fam) in
        eprintf "Modifiy family : %s %s\n"
          (Gutil.designation base fath) (Gutil.designation base moth);
        flush stderr
      }
      else ();
      let gf =
        {(gen_family_of_family fam) with fevents = new_fevents}
      in
      patch_family base gf.fam_index gf;
      changed.val := True;
      incr nb_fam_modified
    }
    else ()
  };
  if changed.val then do {
    commit_patches base;
    eprintf "Number of modified persons: %d\n" nb_ind_modified.val;
    eprintf "Number of modified families: %d\n" nb_fam_modified.val;
    flush stderr
  }
  else ()
};


(**/**)

value bname = ref "";

value speclist = [("-t", Arg.Set trace, "trace modified person")] ;
value anonfun i = bname.val := i;
value usage = "Usage: " ^ Sys.argv.(0) ^ " base";

value main () = do {
  Arg.parse speclist anonfun usage;
  if bname.val = "" then do { Arg.usage speclist usage; exit 2; } else ();
  lock Mutil.lock_file bname.val with
  [ Accept ->
      let base = Gwdb.open_base bname.val in
      fix_occu_y base
  | Refuse -> do {
      eprintf "Cannot lock database. Try again.\n";
      flush stderr;
    } ]
};

main ();
