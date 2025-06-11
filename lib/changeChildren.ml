(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

let digest_children base ipl =
  List.fold_left
    (fun _s ip ->
      let p = Driver.poi base ip in
      Driver.sou base (Driver.get_first_name p)
      ^ "\n"
      ^ Driver.sou base (Driver.get_surname p)
      ^ "\n"
      ^ string_of_int (Driver.get_occ p)
      ^ "\n")
    "" ipl
  |> Mutil.digest

let check_digest conf digest =
  match p_getenv conf.env "digest" with
  | Some ini_digest -> if digest <> ini_digest then Update.error_digest conf
  | None -> ()

exception ChangeChildrenConflict of Driver.person * Driver.person
exception FirstNameMissing of Driver.iper

let check_conflict base p key new_occ ipl =
  let name = Name.lower key in
  List.iter
    (fun ip ->
      let p1 = Driver.poi base ip in
      if
        Driver.get_iper p1 <> Driver.get_iper p
        && Name.lower
             (Driver.p_first_name base p1 ^ " " ^ Driver.p_surname base p1)
           = name
        && Driver.get_occ p1 = new_occ
      then raise @@ ChangeChildrenConflict (p, p1))
    ipl

let change_child conf base parent_surname changed ip =
  let p = Driver.poi base ip in
  let var = "c" ^ Driver.Iper.to_string (Driver.get_iper p) in
  let new_first_name =
    match p_getenv conf.env (var ^ "_first_name") with
    | Some x -> only_printable x
    | _ -> Driver.p_first_name base p
  in
  let new_surname =
    match p_getenv conf.env (var ^ "_surname") with
    | Some x ->
        let x = only_printable x in
        if x = "" then parent_surname else x
    | _ -> Driver.p_surname base p
  in
  let new_occ =
    match p_getint conf.env (var ^ "_occ") with Some x -> x | _ -> 0
  in
  if new_first_name = "" then raise (FirstNameMissing ip)
  else if
    new_first_name <> Driver.p_first_name base p
    || new_surname <> Driver.p_surname base p
    || new_occ <> Driver.get_occ p
  then (
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = Gutil.person_ht_find_all base key in
    check_conflict base p key new_occ ipl;
    Image.rename_portrait_and_blason conf base p
      (new_first_name, new_surname, new_occ);
    (* On ajoute les enfants dans le type Change_children_name       *)
    (* pour la future mise à jour de l'historique et du fichier gwf. *)
    let changed =
      ( ( Driver.p_first_name base p,
          Driver.p_surname base p,
          Driver.get_occ p,
          ip ),
        (new_first_name, new_surname, new_occ, ip) )
      :: changed
    in
    let p =
      {
        (Driver.gen_person_of_person p) with
        first_name = Driver.insert_string base new_first_name;
        surname = Driver.insert_string base new_surname;
        occ = new_occ;
      }
    in
    Driver.patch_person base ip p;
    changed)
  else changed

let change_children conf base parent_surname =
  List.fold_left
    (fun changed ip -> change_child conf base parent_surname changed ip)
    []
