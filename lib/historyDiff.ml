(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

type gen_record = {
  date : Adef.safe_string;
  wizard : Adef.safe_string;
  gen_p : (Driver.iper, Driver.iper, string) gen_person;
  gen_f : (Driver.iper, Driver.ifam, string) gen_family list;
  gen_c : Driver.iper array list;
}

(* Le nom du fichier historique (à partir de la clé personne). *)
let history_file fn sn occ =
  let space_to_unders = Mutil.tr ' ' '_' in
  let f = space_to_unders (Name.lower fn) in
  let s = space_to_unders (Name.lower sn) in
  f ^ "." ^ string_of_int occ ^ "." ^ s

(* history directory path *)
let history_d conf =
  let path =
    match List.assoc_opt "history_path" conf.base_env with
    | Some path when path <> "" -> path
    | _ -> "history_d"
  in
  if Filename.is_relative path then
    let bname =
      if Filename.check_suffix conf.bname ".gwb" then conf.bname
      else conf.bname ^ ".gwb"
    in
    Filename.concat (Util.bpath bname) path
  else path

(* Le chemin du fichier historique dans le dossier history_d. *)
let history_path conf fname =
  if String.length fname >= 6 then
    let dirs =
      [ history_d conf; String.make 1 fname.[0]; String.make 1 fname.[1] ]
    in
    List.fold_right Filename.concat dirs fname
  else Filename.concat (history_d conf) fname

(* Créé tous les dossiers intermédiaires. *)
let create_history_dirs conf fname =
  if String.length fname >= 6 then
    let dirs =
      [ history_d conf; String.make 1 fname.[0]; String.make 1 fname.[1] ]
    in
    Filesystem.create_dir ~parent:true (List.fold_left Filename.concat "" dirs)

(* ************************************************************************ *)
(*  [Fonc] write_history_file : config -> string -> gen_record -> unit      *)

(* ************************************************************************ *)

(** [Description] : Enregistre la personne dans son fichier historique. [Args] :
    - fname : le chemin du fichier
    - gr : le contenu de la personne [Retour] : Néant [Rem] : Non exporté en
      clair hors de ce module. *)
let write_history_file conf person_file fname gr =
  (* On créé toujours les dossiers nécessaires (changement de clé ...). *)
  let () = create_history_dirs conf person_file in
  let ext_flags =
    [ Open_wronly; Open_append; Open_creat; Open_binary; Open_nonblock ]
  in
  match
    try Some (Secure.open_out_gen ext_flags 0o644 fname)
    with Sys_error _ -> None
  with
  | Some oc ->
      output_value oc (gr : gen_record);
      close_out oc
  | None -> ()

(* ************************************************************************ *)
(* [Fonc] make_gen_record :
            config -> base -> bool -> gen_person -> gen_record *)

(* ************************************************************************ *)

(** [Description] : Crée un gen_record à partir d'une personne. [Args] :
    - conf : configuratino de la base
    - base : base de donnée
    - first : booléen pour savoir si c'est la première entrée de l'historique.
      Si c'est le cas, on ne connait pas la date de modification, donc on met
      "environ" une seconde avant.
    - gen_p : gen_person [Retour] :
    - gen_record [Rem] : Non exporté en clair hors de ce module. *)
let make_gen_record conf base first gen_p =
  let date =
    let conf =
      (* On évite les calculs savant pour la date (ss - 1 avec une date *)
      (* autour de minuit ...). C'est simplement une indication.        *)
      if first then
        let hh, mm, ss = conf.time in
        { conf with time = (hh, mm, min 0 ss) }
      else conf
    in
    Util.sprintf_today conf
  in
  let p = Driver.poi base gen_p.key_index in
  let fam = Driver.get_family p in
  (* On fait en sorte qu'il y a une 'bijection' *)
  (* entre les familles et les enfants.         *)
  let gen_f, gen_c =
    Array.fold_right
      (fun ifam (accu_fam, accu_child) ->
        let fam = Driver.foi base ifam in
        let children = Driver.get_children fam in
        let gen_f = Driver.gen_family_of_family fam in
        (Util.string_gen_family base gen_f :: accu_fam, children :: accu_child))
      fam ([], [])
  in
  {
    date;
    wizard = (Util.escape_html conf.user :> Adef.safe_string);
    gen_p;
    gen_f;
    gen_c;
  }

(* ************************************************************************ *)
(*  [Fonc] record_diff : config -> base -> base_changed -> unit             *)

(* ************************************************************************ *)

(** [Description] : Met à jour le fichier historique d'une personne. [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - changed : le type de modification (voir def.mli) [Retour] : Néant [Rem] :
      Exporté en clair hors de ce module. *)
let record_diff conf base changed =
  match List.assoc_opt "history_diff" conf.base_env with
  | Some "yes" when not conf.manitou -> (
      let print_ind_add p =
        let person_file = history_file p.first_name p.surname p.occ in
        let fname = history_path conf person_file in
        let gr = make_gen_record conf base false p in
        write_history_file conf person_file fname gr
      in
      let print_ind_mod o p =
        let o_person_file = history_file o.first_name o.surname o.occ in
        let person_file = history_file p.first_name p.surname p.occ in
        let ofname = history_path conf o_person_file in
        let fname = history_path conf person_file in
        (* La clé a changé, on reprend l'ancien historique. *)
        (if o_person_file <> person_file && Sys.file_exists ofname then
           try
             let () = create_history_dirs conf person_file in
             Sys.rename ofname fname
           with Sys_error _ -> ());
        let gr = make_gen_record conf base false p in
        if Sys.file_exists fname then
          write_history_file conf person_file fname gr
        else
          let o_gr = make_gen_record conf base true o in
          write_history_file conf person_file fname o_gr;
          write_history_file conf person_file fname gr
      in
      match changed with
      | U_Add_person p -> print_ind_add p
      | U_Modify_person (o, p) -> print_ind_mod o p
      | U_Delete_person _ -> ()
      | U_Merge_person (_, o, p) ->
          let o_person_file = history_file o.first_name o.surname o.occ in
          let person_file = history_file p.first_name p.surname p.occ in
          let fname = history_path conf person_file in
          let gr = make_gen_record conf base false p in
          (* La clé a changé avec la fusion, on reprend l'ancien historique. *)
          if o_person_file <> person_file then (
            let ofname = history_path conf o_person_file in
            (try
               let () = create_history_dirs conf person_file in
               Sys.rename ofname fname
             with Sys_error _ -> ());
            write_history_file conf person_file fname gr)
          else write_history_file conf person_file fname gr
      | U_Delete_family (_p, _f) -> ()
      | U_Add_family (p, f)
      | U_Modify_family (p, _, f)
      | U_Merge_family (p, _, _, f)
      | U_Add_parent (p, f) ->
          let p_file = history_file p.first_name p.surname p.occ in
          let p_fname = history_path conf p_file in
          let cpl = Driver.foi base f.fam_index in
          let isp = Gutil.spouse p.key_index cpl in
          let sp = Driver.poi base isp in
          let sp_file =
            history_file
              (Driver.sou base (Driver.get_first_name sp))
              (Driver.sou base (Driver.get_surname sp))
              (Driver.get_occ sp)
          in
          let sp_fname = history_path conf sp_file in
          let gen_sp = Driver.gen_person_of_person sp in
          let gen_sp = Util.string_gen_person base gen_sp in
          let gr = make_gen_record conf base false p in
          write_history_file conf p_file p_fname gr;
          let gr = make_gen_record conf base false gen_sp in
          write_history_file conf sp_file sp_fname gr;
          (* Création des fichiers pour les enfants ajoutés. *)
          Array.iter
            (fun ip ->
              let p = Driver.poi base ip in
              let person_file =
                history_file
                  (Driver.sou base (Driver.get_first_name p))
                  (Driver.sou base (Driver.get_surname p))
                  (Driver.get_occ p)
              in
              let fname = history_path conf person_file in
              if Sys.file_exists fname then ()
              else
                let gen_p = Driver.gen_person_of_person p in
                let gen_p = Util.string_gen_person base gen_p in
                let gr = make_gen_record conf base false gen_p in
                write_history_file conf person_file fname gr)
            (Driver.get_children cpl)
      | U_Change_children_name (_, list) ->
          List.iter
            (fun ((ofn, osn, oocc, _oip), (fn, sn, occ, ip)) ->
              let o_person_file = history_file ofn osn oocc in
              let person_file = history_file fn sn occ in
              if o_person_file <> person_file then (
                let ofname = history_path conf o_person_file in
                let fname = history_path conf person_file in
                (try Sys.rename ofname fname with Sys_error _ -> ());
                let p = Driver.poi base ip in
                let p =
                  Futil.map_person_ps
                    (fun p -> p)
                    (Driver.sou base)
                    (Driver.gen_person_of_person p)
                in
                let gr = make_gen_record conf base false p in
                write_history_file conf person_file fname gr))
            list
      | U_Multi (o, p, modified_key) ->
          if modified_key then print_ind_mod o p else print_ind_add p
      | _ -> ())
  | _ -> ()

(* avec zip ? *)
(*
  let history = ref [] in
  let fname = history_path conf fname in
  if extract_zfile fname then
    do {
      read_history_file fname
      Sys.remove fname
    }
  else ();
  history.val
*)

(* ************************************************************************ *)
(*  [Fonc] load_person_history : config -> string -> gen_record list        *)

(* ************************************************************************ *)

(** [Description] : Charge la liste des modifications pour une personne.
    L'avantage est que les versions les plus récentes se trouvent en tête de
    liste. [Args] :
    - conf : configuration de la base
    - fname : le nom du fichier historique [Retour] :
    - gen_record list [Rem] : Non exporté en clair hors de ce module. *)
let load_person_history conf fname =
  let history = ref [] in
  let fname = history_path conf fname in
  (match try Some (Secure.open_in_bin fname) with Sys_error _ -> None with
  | Some ic ->
      (try
         while true do
           let v : gen_record = input_value ic in
           history := v :: !history
         done
       with End_of_file -> ());
      close_in ic
  | None -> ());
  !history
