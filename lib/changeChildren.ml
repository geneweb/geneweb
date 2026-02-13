(* Copyright (c) 1998-2018 INRIA *)

let digest_children base ipl =
  List.fold_left
    (fun _s ip ->
      let p = Gwdb.poi base ip in
      Gwdb.sou base (Gwdb.get_first_name p)
      ^ "\n"
      ^ Gwdb.sou base (Gwdb.get_surname p)
      ^ "\n"
      ^ string_of_int (Gwdb.get_occ p)
      ^ "\n")
    "" ipl
  |> Ext_string.digest

let check_digest conf digest =
  match Util.p_getenv conf.Config.env "digest" with
  | Some ini_digest -> if digest <> ini_digest then Update.error_digest conf
  | None -> ()

exception ChangeChildrenConflict of Gwdb.person * Gwdb.person
exception FirstNameMissing of Gwdb.iper

let check_conflict base p key new_occ ipl =
  let name = Name.lower key in
  List.iter
    (fun ip ->
      let p1 = Gwdb.poi base ip in
      if
        Gwdb.get_iper p1 <> Gwdb.get_iper p
        && Name.lower (Gwdb.p_first_name base p1 ^ " " ^ Gwdb.p_surname base p1)
           = name
        && Gwdb.get_occ p1 = new_occ
      then raise @@ ChangeChildrenConflict (p, p1))
    ipl

let change_child conf base parent_surname changed ip =
  let p = Gwdb.poi base ip in
  let var = "c" ^ Gwdb.string_of_iper (Gwdb.get_iper p) in
  let new_first_name =
    match Util.p_getenv conf.Config.env (var ^ "_first_name") with
    | Some x -> Ext_string.only_printable x
    | None -> Gwdb.p_first_name base p
  in
  let new_surname =
    match Util.p_getenv conf.Config.env (var ^ "_surname") with
    | Some x ->
        let x = Ext_string.only_printable x in
        if x = "" then parent_surname else x
    | None -> Gwdb.p_surname base p
  in
  let new_occ =
    match Util.p_getint conf.Config.env (var ^ "_occ") with
    | Some x -> x
    | None -> 0
  in
  if new_first_name = "" then raise (FirstNameMissing ip)
  else if
    new_first_name <> Gwdb.p_first_name base p
    || new_surname <> Gwdb.p_surname base p
    || new_occ <> Gwdb.get_occ p
  then (
    let key = new_first_name ^ " " ^ new_surname in
    let ipl = Gutil.person_ht_find_all base key in
    check_conflict base p key new_occ ipl;
    Image.rename_portrait conf base p (new_first_name, new_surname, new_occ);
    let new_p =
      {
        (Gwdb.gen_person_of_person p) with
        first_name = Gwdb.insert_string base new_first_name;
        surname = Gwdb.insert_string base new_surname;
        occ = new_occ;
      }
    in
    (* On ajoute les enfants dans le type Change_children_name       *)
    (* pour la future mise à jour de l'historique et du fichier gwf. *)
    let changed = (Gwdb.gen_person_of_person p, new_p) :: changed in
    Gwdb.patch_person base ip new_p;
    changed)
  else changed

let change_children conf base parent_surname =
  List.fold_left
    (fun changed ip -> change_child conf base parent_surname changed ip)
    []
