(* $Id: modif.ml,v 4.4 2005-01-17 12:53:08 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def
open Printf

let has_infos p =
  match p.death with
    Death (_, _) -> true
  | _ -> false

let modif base =
  let changes = ref false in
  let (get_person, _) = Gwdb.persons_array base in
  for i = 0 to Gwdb.nb_of_persons base - 1 do
    let p = get_person i in
    let fn = Gwdb.p_first_name base p in
    let sn = Gwdb.p_surname base p in
    if match fn with
         "ne" -> true
       | _ -> false
    then
      begin
        eprintf ".";
        flush stderr;
        let fn' =
          match p.sex with
            Male -> "Nn"
          | Female -> "Ne"
          | _ -> "N"
        in
        let sn' = sn in
        try
          let p' = person_ht_find_unique base fn' sn' p.occ in
          eprintf "\nconflit %s avec %s..." (designation base p)
            (designation base (poi base p'));
          flush stderr;
          let occ' = Gutil.find_free_occ base fn' sn' 0 in
          eprintf " rempl occ par %d\n" occ';
          flush stderr;
          p.occ <- occ';
          raise Not_found
        with Not_found ->
          p.first_name <- Gwdb.insert_string base fn';
          p.surname <- Gwdb.insert_string base sn';
          base.func.patch_person p.cle_index p;
          base.func.patch_name (Name.lower (fn' ^ " " ^ sn')) p.cle_index;
          changes := true
      end
  done;
  if !changes then
    begin
      base.func.commit_patches ();
      eprintf "
Attention: il n'est pas sûr que les index aient été complètement mis à jour.
Pour faire bien, il faudrait vérifier si le programme le fait correctement.
Dans le doute, il est probablement préférable de faire gwu et gwc (nettoyage)
pour compléter le travail et avoir une base correcte.

On peut encore annuler les modifs en supprimant le fichier patches,
s'il était vide au départ.";
      flush stderr
    end;
  eprintf "\n";
  flush stderr

let bname = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " <base>"
let speclist = []

let main () =
  let cnt = ref 0 in
  Arg.parse speclist (fun s -> bname := s) usage;
  let base = Iobase.input !bname in modif base

let _ = main ()
