open Config
open Def
open Gwdb
open Util

module PersMap = Map.Make (struct
  type t = istr

  let compare = compare
end)

module PersSet = Set.Make (struct
  type t = person

  let compare p1 p2 = compare (get_iper p1) (get_iper p2)
end)

module StringSet = Set.Make (String)

module IstrSet = Set.Make (struct
  type t = istr

  let compare = compare
end)

(* if env parameter "all" is "on", then we should search for places
   without suburb.
   Later, we should update places values while keeping the suburb value.
*)

let get_data conf =
  match p_getenv conf.env "data" with
  | Some "occu" -> ([ get_occupation ], [], [], [])
  | Some "place" ->
      ( [ get_birth_place; get_baptism_place; get_death_place; get_burial_place ],
        [ (fun evt -> evt.epers_place) ],
        [ get_marriage_place ],
        [ (fun evt -> evt.efam_place) ] )
  | Some "src" ->
      ( [
          get_birth_src;
          get_baptism_src;
          get_death_src;
          get_burial_src;
          get_psources;
        ],
        [ (fun evt -> evt.epers_src) ],
        [ get_marriage_src; get_fsources ],
        [ (fun evt -> evt.efam_src) ] )
  | Some "fn" -> ([ get_first_name ], [], [], [])
  | Some "sn" -> ([ get_surname ], [], [], [])
  | _ -> ([], [], [], [])

let get_all_data conf base =
  let get_p, get_pe, get_f, get_fe = get_data conf in
  let aux : 'a. 'a -> IstrSet.t -> ('a -> istr) -> IstrSet.t =
   fun arg acc get ->
    let istr = get arg in
    if not (is_empty_string istr) then IstrSet.add istr acc else acc
  in
  let acc =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = pget conf base i in
        let acc = List.fold_left (aux p) acc get_p in
        let pevents = get_pevents p in
        List.fold_left
          (fun acc fn -> List.fold_left (fun acc e -> aux e acc fn) acc pevents)
          acc get_pe)
      IstrSet.empty (Gwdb.ipers base)
  in
  let acc =
    if get_f = [] && get_fe = [] then acc
    else
      Gwdb.Collection.fold
        (fun acc i ->
          let f = foi base i in
          let acc = List.fold_left (aux f) acc get_f in
          let fevents = get_fevents f in
          List.fold_left
            (fun acc fn ->
              List.fold_left (fun acc e -> aux e acc fn) acc fevents)
            acc get_fe)
        acc (Gwdb.ifams base)
  in
  IstrSet.elements acc

let get_person_from_data conf base =
  let get_p, get_pe, get_f, get_fe = get_data conf in
  let istr = Gwdb.istr_of_string @@ (List.assoc "key" conf.env :> string) in
  Printf.eprintf "get_person_from_data: key= %s, %s\n" (string_of_istr istr)
    (sou base istr);
  let add acc (istr : istr) p =
    try PersMap.add istr (PersSet.add p @@ PersMap.find istr acc) acc
    with Not_found -> PersMap.add istr (PersSet.add p PersSet.empty) acc
  in
  let aux (fn : PersSet.t PersMap.t -> istr -> PersSet.t PersMap.t) arg acc get
      =
    let istr' = get arg in
    if istr = istr' then fn acc istr else acc
  in
  let acc =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = pget conf base i in
        let add acc istr = add acc istr p in
        let acc = List.fold_left (aux add p) acc get_p in
        let pevents = get_pevents p in
        List.fold_left
          (fun acc fn ->
            List.fold_left (fun acc e -> aux add e acc fn) acc pevents)
          acc get_pe)
      PersMap.empty (Gwdb.ipers base)
  in
  let acc =
    if get_f = [] && get_fe = [] then acc
    else
      Gwdb.Collection.fold
        (fun acc i ->
          let f = foi base i in
          let add acc istr =
            add
              (add acc istr (pget conf base (get_father f)))
              istr
              (pget conf base (get_mother f))
          in
          let acc = List.fold_left (aux add f) acc get_f in
          let fevents = get_fevents f in
          List.fold_left
            (fun acc fn ->
              List.fold_left (fun acc e -> aux add e acc fn) acc fevents)
            acc get_fe)
        acc (Gwdb.ifams base)
  in
  PersMap.fold
    (fun istr pset acc -> (istr, PersSet.elements pset) :: acc)
    acc []

let combine_by_ini ini list =
  let len = Utf8.length ini + 1 in
  Mutil.groupby
    ~key:(fun (_, s) -> Alln.ini len @@ Place.without_suburb s)
    ~value:(fun x -> x)
    list

(* ************************************************************************** *)
(*  [Fonc] reduce_cpl_list : int -> ('a, 'b list) list -> ('a, 'b list) list  *)

(* ************************************************************************** *)

(** [Description] : Retourne la sous liste telle que la somme des longueurs
                    des ('b list) soit égale à size.
    [Args] :
      - size : la taille de la liste retournée
      - list : la liste originale
    [Retour] :
      - list : la nouvelle liste dont la somme des ('b list) est égale à size
    [Rem] : Non exporté en clair hors de ce module.                           *)
let reduce_cpl_list size list =
  let rec loop size cnt reduced_list list =
    if cnt >= size then reduced_list
    else
      match list with
      | [] -> reduced_list
      | (a, sl) :: l ->
          if List.length sl >= size - cnt then
            (a, Util.reduce_list (size - cnt) sl) :: reduced_list
          else loop size (cnt + List.length sl) ((a, sl) :: reduced_list) l
  in
  loop size 0 [] list

(* ************************************************************************** *)
(* [Fonc] update_person : conf -> base -> string -> string -> person ->
                            gen_person iper istr *)

(* ************************************************************************** *)

(** [Description] : Met à jour le/les champ(s) de la personne.
    [Args] :
      - conf : configuration de la base
      - base : base de donnée
      - old  : l'ancien contenu
      - new_input : le nouveau contenu
      - p : person
    [Retour] :
      - gen_person iper istr : gen_person avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
let update_person conf base old new_input p =
  match p_getenv conf.env "data" with
  | Some "occu" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let occupation = get_occupation p in
      let s_occupation = sou base occupation in
      let occupation = if old = s_occupation then new_istr else occupation in
      { (gen_person_of_person p) with occupation }
  | Some "place" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let pl_bi = get_birth_place p in
      let s_bi = sou base pl_bi in
      let pl_bp = get_baptism_place p in
      let s_bp = sou base pl_bp in
      let pl_de = get_death_place p in
      let s_de = sou base pl_de in
      let pl_bu = get_burial_place p in
      let s_bu = sou base pl_bu in
      let birth_place = if old = s_bi then new_istr else pl_bi in
      let baptism_place = if old = s_bp then new_istr else pl_bp in
      let death_place = if old = s_de then new_istr else pl_de in
      let burial_place = if old = s_bu then new_istr else pl_bu in
      let pevents =
        List.map
          (fun evt ->
            let pl_evt = evt.epers_place in
            let s_evt = sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with epers_place = place })
          (get_pevents p)
      in
      {
        (gen_person_of_person p) with
        birth_place;
        baptism_place;
        death_place;
        burial_place;
        pevents;
      }
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let src_bi = get_birth_src p in
      let s_bi = sou base src_bi in
      let src_bp = get_baptism_src p in
      let s_bp = sou base src_bp in
      let src_de = get_death_src p in
      let s_de = sou base src_de in
      let src_bu = get_burial_src p in
      let s_bu = sou base src_bu in
      let src_p = get_psources p in
      let s_p = sou base src_p in
      let birth_src = if old = s_bi then new_istr else src_bi in
      let baptism_src = if old = s_bp then new_istr else src_bp in
      let death_src = if old = s_de then new_istr else src_de in
      let burial_src = if old = s_bu then new_istr else src_bu in
      let psources_src = if old = s_p then new_istr else src_p in
      let pevents =
        List.map
          (fun evt ->
            let src_evt = evt.epers_src in
            let s_evt = sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with epers_src = src })
          (get_pevents p)
      in
      {
        (gen_person_of_person p) with
        birth_src;
        baptism_src;
        death_src;
        burial_src;
        psources = psources_src;
        pevents;
      }
  | Some "fn" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let first_name = get_first_name p in
      let s_first_name = sou base first_name in
      let s_first_name_lower = Name.lower s_first_name in
      let new_input_lower = Name.lower new_input in
      let first_name, occ =
        if new_input_lower = s_first_name_lower then (new_istr, get_occ p)
        else if old = s_first_name then
          ( new_istr,
            Gutil.find_free_occ base (sou base new_istr)
              (sou base (get_surname p)) )
        else (first_name, get_occ p)
      in
      let first_names_aliases = get_first_names_aliases p in
      let first_names_aliases =
        if p_getenv conf.env "first_name_aliases" = Some "yes" then
          let has_first_name_alias =
            List.fold_left
              (fun has_first_name alias ->
                has_first_name
                || s_first_name_lower = Name.lower (sou base alias))
              false first_names_aliases
          in
          if has_first_name_alias then first_names_aliases
          else get_first_name p :: first_names_aliases
        else first_names_aliases
      in
      { (gen_person_of_person p) with first_name; occ; first_names_aliases }
  | Some "sn" ->
      let new_istr = Gwdb.insert_string base (only_printable new_input) in
      let surname = get_surname p in
      let s_surname = sou base surname in
      let s_surname_lower = Name.lower s_surname in
      let new_input_lower = Name.lower new_input in
      let surname, occ =
        if new_input_lower = s_surname_lower then (new_istr, get_occ p)
        else if old = s_surname then
          ( new_istr,
            Gutil.find_free_occ base
              (sou base (get_first_name p))
              (sou base new_istr) )
        else (surname, get_occ p)
      in
      let surnames_aliases = get_surnames_aliases p in
      let surnames_aliases =
        if p_getenv conf.env "surname_aliases" = Some "yes" then
          let has_surname_alias =
            List.fold_left
              (fun has_surname alias ->
                has_surname || s_surname_lower = Name.lower (sou base alias))
              false surnames_aliases
          in
          if has_surname_alias then surnames_aliases
          else get_surname p :: surnames_aliases
        else surnames_aliases
      in
      { (gen_person_of_person p) with surname; occ; surnames_aliases }
  | _ -> gen_person_of_person p

(* ************************************************************************** *)
(* [Fonc] update_family : conf -> base -> string -> string -> person ->
                            gen_family ifam istr *)

(* ************************************************************************** *)

(** [Description] : Met à jour le/les champ(s) de la famille.
    [Args] :
      - conf      : configuration de la base
      - base      : base de donnée
      - old       : l'ancien contenu
      - new_input : le nouveau contenu
      - fam       : family
    [Retour] :
      - gen_family ifam istr : gen_family avec les champs modifiés
    [Rem] : Non exporté en clair hors de ce module.                           *)
let update_family conf base old new_istr fam =
  match p_getenv conf.env "data" with
  | Some "place" ->
      let new_istr = Gwdb.insert_string base (only_printable new_istr) in
      let p_ma = get_marriage_place fam in
      let s_ma = sou base p_ma in
      let marriage_place = if old = s_ma then new_istr else p_ma in
      let fevents =
        List.map
          (fun evt ->
            let pl_evt = evt.efam_place in
            let s_evt = sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with efam_place = place })
          (get_fevents fam)
      in
      { (gen_family_of_family fam) with marriage_place; fevents }
  | Some "src" ->
      let new_istr = Gwdb.insert_string base (only_printable new_istr) in
      let src_ma = get_marriage_src fam in
      let s_ma = sou base src_ma in
      let src_f = get_fsources fam in
      let s_f = sou base src_f in
      let marriage_src = if old = s_ma then new_istr else src_ma in
      let fsources = if old = s_f then new_istr else src_f in
      let fevents =
        List.map
          (fun evt ->
            let src_evt = evt.efam_src in
            let s_evt = sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with efam_src = src })
          (get_fevents fam)
      in
      { (gen_family_of_family fam) with marriage_src; fsources; fevents }
  | _ -> gen_family_of_family fam

(* ********************************************************************** *)
(* [Fonc] update_person_list :
            config -> base -> string -> (string * person) list -> int
              -> int -> unit *)

(* ********************************************************************** *)

(** [Description] :
    [Args] :
      - conf      : configuration
      - base      : base
      - new_input : le nouveau contenu
      - list      : la liste des (clé, person list)
      - nb_pers   : le nombre de personnes concernées par la mise à jour
      - max_updates = le nombre maximum de persons que l'on met à jour
    [Retour] :
      - unit
    [Rem] : Non exporté en clair hors de ce module.                       *)
let update_person_list conf base new_input list nb_pers max_updates =
  let test_family =
    match get_data conf with _, _, [], [] -> false | _ -> true
  in
  let action =
    match p_getenv conf.env "data" with
    | Some "occu" -> "co"
    | Some "place" -> "cp"
    | Some "src" -> "cs"
    | Some "fn" -> "fn"
    | Some "sn" -> "sn"
    | _ -> ""
  in
  let list =
    if nb_pers > max_updates then reduce_cpl_list max_updates list else list
  in
  let cnt = ref 0 in
  List.iter
    (fun (old, perl) ->
      (* Mise à jour de toutes les personnes concernées. *)
      List.iter
        (fun p ->
          if
            sou base (get_first_name p) <> "?"
            || sou base (get_surname p) <> "?"
          then (
            incr cnt;
            let o_p = Util.string_gen_person base (gen_person_of_person p) in
            let np = update_person conf base old new_input p in
            (if action = "fn" || action = "sn" then
             let pi = np.key_index in
             let op = poi base pi in
             let sp =
               Futil.map_person_ps (fun ip -> ip) (fun istr -> sou base istr) np
             in
             Image.rename_portrait conf base op
               (sp.first_name, sp.surname, sp.occ));
            patch_person base np.key_index np;
            if test_family then
              Array.iter
                (fun ifam ->
                  let fam = foi base ifam in
                  let nfam = update_family conf base old new_input fam in
                  patch_family base nfam.fam_index nfam)
                (get_family p);
            (* On met aussi à jour l'historique. *)
            let changed =
              U_Multi
                ( o_p,
                  Util.string_gen_person base np,
                  if action = "fn" || action = "sn" then true else false )
            in
            History.record conf base changed action))
        perl)
    list;
  Util.commit_patches conf base;
  (* On appelle explicitement notify_change car la base est modifiée.  *)
  (* On fait cet appel à la fin de chaque mise à jour de la liste des  *)
  (* personnes, car si l'administrateur de la base ne modifie pas tous *)
  (* les évènements liés à cette donnée, on ne sera pas mis au courant *)
  (* que la base à été mise à jour.                                    *)
  History.notify conf base action;
  !cnt

let move_particle base s =
  Util.surname_without_particle base s ^ Util.surname_particle base s

(** Get all the data and filter them if ["s"] is defined in [conf.env] *)
let build_list conf base =
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini = Option.value ~default:"" (p_getenv conf.env "s") in
  let list = get_all_data conf base in
  if ini <> "" then
    Mutil.filter_map
      (fun istr ->
        let str = sou base istr |> move_particle base in
        if Mutil.start_with_wildcard ini 0 @@ Place.without_suburb str then
          Some (istr, str)
        else None)
      list
  else
    List.filter_map
      (fun istr -> Some (istr, sou base istr |> move_particle base))
      (List.rev list)

let build_list_short conf list =
  let ini = Option.value ~default:"" (p_getenv conf.env "s") in
  (* Construit la liste des string commençant par ini. *)
  (* Pour certaines données comme les sources, on peut *)
  (* avoir beaucoup de sources qui commencent par les  *)
  (* mêmes lettres. On calcul alors à partir de quelle *)
  (* lettre de ini, les sources sont différentes.      *)
  (* ex: eta -> etat -> etat_ -> ... -> etat_civil     *)
  let rec build_ini l i =
    let inis =
      List.rev_map
        (fun (_, s) ->
          let s = Place.without_suburb s in
          if String.length s > i then String.sub s 0 (Utf8.next s i)
          else s ^ String.make (i + 1 - String.length s) '_')
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left
           (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    (* Astuce pour gérer les espaces. *)
    let inis = List.rev_map (fun p -> Mutil.tr ' ' '_' p) inis in
    let inis = remove_dup inis in
    match inis with
    | [ ini ] -> build_ini list (String.length ini)
    | list -> List.sort Gutil.alphabetic_order list
  in
  build_ini list (String.length ini)

let build_list_long conf list : (string * (istr * string) list) list =
  let ini = Option.value ~default:"" (p_getenv conf.env "s") in
  let list = combine_by_ini ini list in
  List.sort (fun (ini1, _) (ini2, _) -> Gutil.alphabetic_order ini1 ini2) list
