open Def
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

(* Optimisation de find_sosa_aux :                                           *)
(* - ajout d'un cache pour conserver les descendants du sosa que l'on calcul *)
(* - on sauvegarde la dernière génération où l'on a arrêté le calcul pour    *)
(*   ne pas reprendre le calcul depuis la racine                             *)

(* Type pour ne pas créer à chaque fois un tableau tstab et mark *)
type sosa_t = {
  tstab : (Driver.iper, int) Collection.Marker.t;
  mark : (Driver.iper, bool) Collection.Marker.t;
  mutable last_zil : (Driver.iper * Sosa.t) list;
  sosa_ht : (Driver.iper, (Sosa.t * Driver.person) option) Hashtbl.t;
}

let init_sosa_t conf base sosa_ref =
  try
    let tstab = Util.create_topological_sort conf base in
    let mark = Driver.iper_marker (Driver.ipers base) false in
    let last_zil = [ (Driver.get_iper sosa_ref, Sosa.one) ] in
    let sosa_ht = Hashtbl.create 5003 in
    Hashtbl.add sosa_ht (Driver.get_iper sosa_ref) (Some (Sosa.one, sosa_ref));
    Some { tstab; mark; last_zil; sosa_ht }
  with Consang.TopologicalSortError _ -> None

let find_sosa_aux conf base a p t_sosa =
  let cache = ref [] in
  let has_ignore = ref false in
  let ht_add ht k v new_sosa =
    match try Hashtbl.find ht k with Not_found -> v with
    | Some (z, _) -> if not (Sosa.gt new_sosa z) then Hashtbl.replace ht k v
    | _ -> ()
  in
  let rec gene_find = function
    | [] -> Def.Left []
    | (ip, z) :: zil ->
        let _ = cache := (ip, z) :: !cache in
        if ip = Driver.get_iper a then Right z
        else if Collection.Marker.get t_sosa.mark ip then gene_find zil
        else (
          Collection.Marker.set t_sosa.mark ip true;
          if
            Collection.Marker.get t_sosa.tstab (Driver.get_iper a)
            <= Collection.Marker.get t_sosa.tstab ip
          then
            let _ = has_ignore := true in
            gene_find zil
          else
            let asc = Util.pget conf base ip in
            match Driver.get_parents asc with
            | Some ifam -> (
                let cpl = Driver.foi base ifam in
                let z = Sosa.twice z in
                match gene_find zil with
                | Left zil ->
                    Left
                      ((Driver.get_father cpl, z)
                      :: (Driver.get_mother cpl, Sosa.inc z 1)
                      :: zil)
                | Right z -> Right z)
            | None -> gene_find zil)
  in
  let rec find zil =
    match
      try gene_find zil
      with Invalid_argument msg when msg = "index out of bounds" ->
        Update.delete_topological_sort conf base;
        Left []
    with
    | Left [] ->
        let _ =
          List.iter
            (fun (ip, _) -> Collection.Marker.set t_sosa.mark ip false)
            !cache
        in
        None
    | Left zil ->
        let _ =
          if !has_ignore then ()
          else (
            List.iter
              (fun (ip, z) -> ht_add t_sosa.sosa_ht ip (Some (z, p)) z)
              zil;
            t_sosa.last_zil <- zil)
        in
        find zil
    | Right z ->
        let _ =
          List.iter
            (fun (ip, _) -> Collection.Marker.set t_sosa.mark ip false)
            !cache
        in
        Some (z, p)
  in
  find t_sosa.last_zil

let find_sosa conf base a sosa_ref t_sosa =
  match sosa_ref with
  | Some p ->
      if Driver.get_iper a = Driver.get_iper p then Some (Sosa.one, p)
      else
        let u = Util.pget conf base (Driver.get_iper a) in
        if Util.has_children base u then
          try Hashtbl.find t_sosa.sosa_ht (Driver.get_iper a)
          with Not_found -> find_sosa_aux conf base a p t_sosa
        else None
  | None -> None

(* [Type]: (iper, Sosa.t) Hashtbl.t *)
let sosa_ht = Hashtbl.create 5003

(* ************************************************************************ *)
(*  [Fonc] build_sosa_tree_ht : config -> base -> person -> unit            *)

(* ************************************************************************ *)

(** [Description] : Construit à partir d'une personne la base, la liste de tous
    ses ancêtres directs et la stocke dans une hashtbl. La clé de la table est
    l'iper de la personne et on lui associe son numéro de sosa. Les sosa
    multiples ne sont représentés qu'une seule fois par leur plus petit numéro
    sosa. [Args] :
    - conf : configuration de la base
    - base : base de donnée [Retour] :
    - unit [Rem] : Exporté en clair hors de ce module. *)
let build_sosa_tree_ht conf base person =
  Driver.load_ascends_array base;
  Driver.load_couples_array base;
  let nb_persons = Driver.nb_of_persons base in
  let mark =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) false
  in
  (* Tableau qui va stocker au fur et à mesure les ancêtres de person. *)
  (* Attention, on créé un tableau de la longueur de la base + 1 car on *)
  (* commence à l'indice 1 !                                            *)
  let sosa_accu = Array.make (nb_persons + 1) (Sosa.zero, Driver.Iper.dummy) in
  Array.set sosa_accu 1 (Sosa.one, Driver.get_iper person);
  let rec loop i len =
    if i > nb_persons then ()
    else
      let sosa_num, ip = Array.get sosa_accu i in
      (* Si la personne courante n'a pas de numéro de sosa, alors il n'y *)
      (* a plus d'ancêtres car ils ont été ajoutés par ordre croissant.  *)
      if Sosa.eq sosa_num Sosa.zero then ()
      else (
        Hashtbl.add sosa_ht ip sosa_num;
        let asc = Util.pget conf base ip in
        (* Ajoute les nouveaux ascendants au tableau des ancêtres. *)
        match Driver.get_parents asc with
        | Some ifam ->
            let cpl = Driver.foi base ifam in
            let z = Sosa.twice sosa_num in
            let len =
              if not @@ Collection.Marker.get mark (Driver.get_father cpl) then (
                Array.set sosa_accu (len + 1) (z, Driver.get_father cpl);
                Collection.Marker.set mark (Driver.get_father cpl) true;
                len + 1)
              else len
            in
            let len =
              if not @@ Collection.Marker.get mark (Driver.get_mother cpl) then (
                Array.set sosa_accu (len + 1)
                  (Sosa.inc z 1, Driver.get_mother cpl);
                Collection.Marker.set mark (Driver.get_mother cpl) true;
                len + 1)
              else len
            in
            loop (i + 1) len
        | None -> loop (i + 1) len)
  in
  loop 1 1

(* ************************************************************************ *)
(*  [Fonc] build_sosa_ht : config -> base -> unit                           *)

(* ************************************************************************ *)

(** [Description] : Fait appel à la construction de la liste de tous les
    ancêtres directs de la souche de l'arbre [Args] :
    - conf : configuration de la base
    - base : base de donnée [Retour] :
    - unit [Rem] : Exporté en clair hors de ce module. *)
let build_sosa_ht conf base =
  match Util.find_sosa_ref conf base with
  | Some sosa_ref -> build_sosa_tree_ht conf base sosa_ref
  | None -> ()

(* ******************************************************************** *)
(*  [Fonc] next_sosa : Sosa.t -> Sosa.t               *)

(* ******************************************************************** *)

(** [Description] : Recherche le sosa suivant [Args] :
    - s : sosa [Retour] :
    - Sosa.t : retourne Sosa.zero s'il n'y a pas de sosa suivant *)
let next_sosa s =
  (* La clé de la table est l'iper de la personne et on lui associe son numéro
     de sosa. On inverse pour trier sur les sosa *)
  let sosa_list = Hashtbl.fold (fun k v acc -> (v, k) :: acc) sosa_ht [] in
  let sosa_list =
    List.sort (fun (s1, _) (s2, _) -> Sosa.compare s1 s2) sosa_list
  in
  let rec find_n x lst =
    match lst with
    | [] -> (Sosa.zero, Driver.Iper.dummy)
    | (so, _) :: tl ->
        if Sosa.eq so x then
          match tl with
          | [] -> (Sosa.zero, Driver.Iper.dummy)
          | tl :: _tll -> tl
        else find_n x tl
  in
  let so, ip = find_n s sosa_list in
  (so, ip)

let prev_sosa s =
  let sosa_list = Hashtbl.fold (fun k v acc -> (v, k) :: acc) sosa_ht [] in
  let sosa_list =
    List.sort (fun (s1, _) (s2, _) -> Sosa.compare s1 s2) sosa_list
  in
  let sosa_list = List.rev sosa_list in
  let rec find_n x lst =
    match lst with
    | [] -> (Sosa.zero, Driver.Iper.dummy)
    | (so, _) :: tl ->
        if Sosa.eq so x then
          match tl with
          | [] -> (Sosa.zero, Driver.Iper.dummy)
          | tl :: _tll -> tl
        else find_n x tl
  in
  let so, ip = find_n s sosa_list in
  (so, ip)

(* ******************************************************************** *)
(*  [Fonc] get_sosa_person : config -> person -> Sosa.t          *)

(* ******************************************************************** *)

(** [Description] : Recherche si la personne passée en argument a un numéro de
    sosa. [Args] :
    - p : personne dont on cherche si elle a un numéro sosa [Retour] :
    - Sosa.t : retourne Sosa.zero si la personne n'a pas de numéro de sosa, ou
      retourne son numéro de sosa sinon [Rem] : Exporté en clair hors de ce
      module. *)
let get_sosa_person p =
  try Hashtbl.find sosa_ht (Driver.get_iper p) with Not_found -> Sosa.zero

(* ******************************************************************** *)
(*  [Fonc] get_single_sosa : config -> base -> person -> Sosa.t          *)

(* ******************************************************************** *)

(** [Description] : Recherche si la personne passée en argument a un numéro de
    sosa. [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p : personne dont on cherche si elle a un numéro sosa [Retour] :
    - Sosa.t : retourne Sosa.zero si la personne n'a pas de numéro de sosa, ou
      retourne son numéro de sosa sinon [Rem] : Exporté en clair hors de ce
      module. *)
let get_single_sosa conf base p =
  match Util.find_sosa_ref conf base with
  | None -> Sosa.zero
  | Some p_sosa as sosa_ref -> (
      match init_sosa_t conf base p_sosa with
      | None -> Sosa.zero
      | Some t_sosa -> (
          match find_sosa conf base p sosa_ref t_sosa with
          | Some (z, _) -> z
          | None -> Sosa.zero))

(* ************************************************************************ *)
(*  [Fonc] print_sosa : config -> base -> person -> bool -> unit            *)

(* ************************************************************************ *)

(** [Description] : Affiche le picto sosa ainsi que le lien de calcul de
    relation entre la personne et le sosa 1 (si l'option cancel_link n'est pas
    activée). [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - p : la personne que l'on veut afficher
    - link : ce booléen permet d'afficher ou non le lien sur le picto sosa. Il
      n'est pas nécessaire de mettre le lien si on a déjà affiché cette
      personne. [Retour] :
    - unit [Rem] : Exporté en clair hors de ce module. *)
let print_sosa conf base p link =
  let sosa_num = get_sosa_person p in
  if Sosa.gt sosa_num Sosa.zero then
    match Util.find_sosa_ref conf base with
    | Some r ->
        (if not link then ()
         else
           let sosa_link =
             let i1 = Driver.Iper.to_string (Driver.get_iper p) in
             let i2 = Driver.Iper.to_string (Driver.get_iper r) in
             let b2 = Sosa.to_string sosa_num in
             "m=RL&i1=" ^ i1 ^ "&i2=" ^ i2 ^ "&b1=1&b2=" ^ b2
           in
           Output.print_sstring conf {|<a href="|};
           Output.print_string conf (Util.commd conf);
           Output.print_string conf (sosa_link |> Adef.safe);
           Output.print_sstring conf {|"> |});
        let title =
          if Util.is_hide_names conf r && not (Util.authorized_age conf base r)
          then ""
          else
            let direct_ancestor =
              Name.strip_c (Driver.p_first_name base r) '"'
              ^ " "
              ^ Name.strip_c (Driver.p_surname base r) '"'
            in
            Printf.sprintf
              (Util.fcapitale (Util.ftransl conf "direct ancestor of %s"))
              direct_ancestor
            ^ Printf.sprintf ", Sosa: %s"
                (Sosa.to_string_sep
                   (Util.transl conf "(thousand separator)")
                   sosa_num)
        in
        Output.print_sstring conf {|<img class="mb-1" src="|};
        Output.print_sstring conf (Util.images_prefix conf);
        Output.print_sstring conf {|/sosa.png" alt="sosa" title="|};
        Output.print_string conf (title |> Adef.safe);
        Output.print_sstring conf {|"> |};
        if not link then () else Output.print_sstring conf "</a> "
    | None -> ()
