(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb
open Util

let digest_children base ipl =
  List.fold_left
    (fun _s ip ->
      let p = poi base ip in
      sou base (get_first_name p)
      ^ "\n"
      ^ sou base (get_surname p)
      ^ "\n"
      ^ string_of_int (get_occ p)
      ^ "\n")
    "" ipl
  |> Mutil.digest

let check_digest conf digest =
  match p_getenv conf.env "digest" with
  | Some ini_digest -> if digest <> ini_digest then Update.error_digest conf
  | None -> ()

exception ChangeChildrenConflict of person * person
exception FirstNameMissing of iper

let check_conflict base p key new_occ ipl =
  let name = Name.lower key in
  List.iter
    (fun ip ->
      let p1 = poi base ip in
      if
        get_iper p1 <> get_iper p
        && Name.lower (p_first_name base p1 ^ " " ^ p_surname base p1) = name
        && get_occ p1 = new_occ
      then raise @@ ChangeChildrenConflict (p, p1))
    ipl

let change_child conf base parent_surname changed ip =
  let p = poi base ip in
  let var = "c" ^ string_of_iper (get_iper p) in
  let new_first_name =
    match p_getenv conf.env (var ^ "_first_name") with
    | Some x -> only_printable x
    | _ -> p_first_name base p
  in
  let new_surname =
    match p_getenv conf.env (var ^ "_surname") with
    | Some x ->
        let x = only_printable x in
        if x = "" then parent_surname else x
    | _ -> p_surname base p
  in
  let new_occ =
    match p_getint conf.env (var ^ "_occ") with Some x -> x | _ -> 0
  in
  if new_first_name = "" then raise (FirstNameMissing ip)
  else if
    new_first_name <> p_first_name base p
    || new_surname <> p_surname base p
    || new_occ <> get_occ p
  then (
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = Gutil.person_ht_find_all base key in
    check_conflict base p key new_occ ipl;
    Image.rename_portrait conf base p (new_first_name, new_surname, new_occ);
    (* On ajoute les enfants dans le type Change_children_name       *)
    (* pour la future mise à jour de l'historique et du fichier gwf. *)
    let changed =
      ( (p_first_name base p, p_surname base p, get_occ p, ip),
        (new_first_name, new_surname, new_occ, ip) )
      :: changed
    in
    let p =
      {
        (gen_person_of_person p) with
        first_name = Gwdb.insert_string base new_first_name;
        surname = Gwdb.insert_string base new_surname;
        occ = new_occ;
      }
    in
    patch_person base ip p;
    changed)
  else changed

let change_children conf base parent_surname =
  List.fold_left
    (fun changed ip -> change_child conf base parent_surname changed ip)
    []
