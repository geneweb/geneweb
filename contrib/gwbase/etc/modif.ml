(* $Id: modif.ml,v 4.4 2005-01-17 12:53:08 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Printf;

value has_infos p =
  match p.death with [ Death _ _ -> True | _ -> False ]
;

value modif base =
  let changes = ref False in
  do {
    let (get_person, _) = Gwdb.persons_array base in
    for i = 0 to (Gwdb.nb_of_persons base) - 1 do {
      let p = get_person i in
      let fn = Gwdb.p_first_name base p in
      let sn = Gwdb.p_surname base p in
      if match fn with [ "ne" -> True | _ -> False ] then do {
        eprintf ".";
        flush stderr;
        let fn' = match p.sex with [ Male -> "Nn" | Female -> "Ne" | _ -> "N" ] in
        let sn' = sn in
        try
          let p' = person_ht_find_unique base fn' sn' p.occ in
          do {
            eprintf "\nconflit %s avec %s..." (designation base p)
              (designation base (poi base p'));
            flush stderr;
            let occ' = Gutil.find_free_occ base fn' sn' 0 in
            eprintf " rempl occ par %d\n" occ';
            flush stderr;
            p.occ := occ';
            raise Not_found; (* comme �a, �a fait le reste... *)
          }
        with
        [ Not_found ->
  	  do {
              p.first_name := Gwdb.insert_string base fn';
              p.surname := Gwdb.insert_string base sn';
              base.func.patch_person p.cle_index p;
              base.func.patch_name (Name.lower (fn' ^ " " ^ sn')) p.cle_index;
              changes.val := True;
            } ];
      }
      else ()
    };
    if changes.val then do {
      base.func.commit_patches ();
      eprintf "
Attention: il n'est pas s�r que les index aient �t� compl�tement mis � jour.
Pour faire bien, il faudrait v�rifier si le programme le fait correctement.
Dans le doute, il est probablement pr�f�rable de faire gwu et gwc (nettoyage)
pour compl�ter le travail et avoir une base correcte.

On peut encore annuler les modifs en supprimant le fichier patches,
s'il �tait vide au d�part.";
      flush stderr;
    }
    else ();
    eprintf "\n";
    flush stderr;
  }
;

value bname = ref "";
value usage = "usage: " ^ Sys.argv.(0) ^ " <base>";
value speclist = [];

value main () =
  let cnt = ref 0 in
  do {
    Arg.parse speclist (fun s -> bname.val := s) usage;
    let base = Iobase.input bname.val in
    modif base;
  }
;

main ();
