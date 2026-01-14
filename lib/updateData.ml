let get_data_kind_from_env env =
  Option.bind (Util.p_getenv env "data") (fun value ->
      match value with
      | "occu" -> Some `occupation
      | "place" -> Some `place
      | "src" -> Some `source
      | "fn" -> Some `firstname
      | "sn" -> Some `lastname
      | _ -> None)

let get_data = function
  | Some `occupation -> ([ Gwdb.get_occupation ], [], [], [])
  | Some `place ->
      ( [
          Gwdb.get_birth_place;
          Gwdb.get_baptism_place;
          Gwdb.get_death_place;
          Gwdb.get_burial_place;
        ],
        [ (fun evt -> Gwdb.get_pevent_place evt) ],
        [ Gwdb.get_marriage_place ],
        [ (fun evt -> Gwdb.get_fevent_place evt) ] )
  | Some `source ->
      ( [
          Gwdb.get_birth_src;
          Gwdb.get_baptism_src;
          Gwdb.get_death_src;
          Gwdb.get_burial_src;
          Gwdb.get_psources;
        ],
        [ (fun evt -> Gwdb.get_pevent_src evt) ],
        [ Gwdb.get_marriage_src; Gwdb.get_fsources ],
        [ (fun evt -> Gwdb.get_fevent_src evt) ] )
  | Some `firstname -> ([ Gwdb.get_first_name ], [], [], [])
  | Some `surname -> ([ Gwdb.get_surname ], [], [], [])
  | _ -> ([], [], [], [])

let get_all_data conf base =
  let get_p, get_pe, get_f, get_fe =
    get_data @@ get_data_kind_from_env conf.Config.env
  in
  let aux : 'a. 'a -> Gwdb.IstrSet.t -> ('a -> Gwdb.istr) -> Gwdb.IstrSet.t =
   fun arg acc get ->
    let istr = get arg in
    if not (Gwdb.is_empty_string istr) then Gwdb.IstrSet.add istr acc else acc
  in
  let acc =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = Util.pget conf base i in
        let acc = List.fold_left (aux p) acc get_p in
        let pevents = Gwdb.get_pevents p in
        List.fold_left
          (fun acc fn -> List.fold_left (fun acc e -> aux e acc fn) acc pevents)
          acc get_pe)
      Gwdb.IstrSet.empty (Gwdb.ipers base)
  in
  let acc =
    if get_f = [] && get_fe = [] then acc
    else
      Gwdb.Collection.fold
        (fun acc i ->
          let f = Gwdb.foi base i in
          let acc = List.fold_left (aux f) acc get_f in
          let fevents = Gwdb.get_fevents f in
          List.fold_left
            (fun acc fn ->
              List.fold_left (fun acc e -> aux e acc fn) acc fevents)
            acc get_fe)
        acc (Gwdb.ifams base)
  in
  Gwdb.IstrSet.elements acc

let get_person_from_data conf base =
  let get_p, get_pe, get_f, get_fe =
    get_data @@ get_data_kind_from_env conf.Config.env
  in
  let istr = Gwdb.istr_of_string @@ (List.assoc "key" conf.env :> string) in
  let add acc (istr : Gwdb.istr) p =
    match Gwdb.IstrMap.find_opt istr acc with
    | Some persons -> Gwdb.IstrMap.add istr (Gwdb.PersonSet.add p persons) acc
    | None ->
        Gwdb.IstrMap.add istr (Gwdb.PersonSet.add p Gwdb.PersonSet.empty) acc
  in
  let aux
      (fn :
        Gwdb.PersonSet.t Gwdb.IstrMap.t ->
        Gwdb.istr ->
        Gwdb.PersonSet.t Gwdb.IstrMap.t) arg acc get =
    let istr' = get arg in
    if istr = istr' then fn acc istr else acc
  in
  let acc =
    Gwdb.Collection.fold
      (fun acc i ->
        let p = Util.pget conf base i in
        let add acc istr = add acc istr p in
        let acc = List.fold_left (aux add p) acc get_p in
        let pevents = Gwdb.get_pevents p in
        List.fold_left
          (fun acc fn ->
            List.fold_left (fun acc e -> aux add e acc fn) acc pevents)
          acc get_pe)
      Gwdb.IstrMap.empty (Gwdb.ipers base)
  in
  let acc =
    if get_f = [] && get_fe = [] then acc
    else
      Gwdb.Collection.fold
        (fun acc i ->
          let f = Gwdb.foi base i in
          let add acc istr =
            add
              (add acc istr (Util.pget conf base (Gwdb.get_father f)))
              istr
              (Util.pget conf base (Gwdb.get_mother f))
          in
          let acc = List.fold_left (aux add f) acc get_f in
          let fevents = Gwdb.get_fevents f in
          List.fold_left
            (fun acc fn ->
              List.fold_left (fun acc e -> aux add e acc fn) acc fevents)
            acc get_fe)
        acc (Gwdb.ifams base)
  in
  Gwdb.IstrMap.fold
    (fun istr pset acc -> (istr, Gwdb.PersonSet.elements pset) :: acc)
    acc []

let combine_by_ini ~ignore_case ini list =
  let len = Utf8.length ini + 1 in
  Ext_list.groupby
    ~key:(fun (_, s) ->
      let normalize = if ignore_case then Utf8.capitalize else Fun.id in
      normalize @@ Alln.ini len @@ Place.without_suburb s)
    ~value:Fun.id list

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
            (a, Ext_list.take sl (size - cnt)) :: reduced_list
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
  match get_data_kind_from_env conf.Config.env with
  | Some `occupation ->
      let new_istr =
        Gwdb.insert_string base (Ext_string.only_printable new_input)
      in
      let occupation = Gwdb.get_occupation p in
      let s_occupation = Gwdb.sou base occupation in
      let occupation = if old = s_occupation then new_istr else occupation in
      { (Gwdb.gen_person_of_person p) with occupation }
  | Some `place ->
      let new_istr =
        Gwdb.insert_string base (Ext_string.only_printable new_input)
      in
      let pl_bi = Gwdb.get_birth_place p in
      let s_bi = Gwdb.sou base pl_bi in
      let pl_bp = Gwdb.get_baptism_place p in
      let s_bp = Gwdb.sou base pl_bp in
      let pl_de = Gwdb.get_death_place p in
      let s_de = Gwdb.sou base pl_de in
      let pl_bu = Gwdb.get_burial_place p in
      let s_bu = Gwdb.sou base pl_bu in
      let birth_place = if old = s_bi then new_istr else pl_bi in
      let baptism_place = if old = s_bp then new_istr else pl_bp in
      let death_place = if old = s_de then new_istr else pl_de in
      let burial_place = if old = s_bu then new_istr else pl_bu in
      let pevents =
        List.map
          (fun evt ->
            let evt = Gwdb.gen_pevent_of_pers_event evt in
            let pl_evt = evt.epers_place in
            let s_evt = Gwdb.sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with epers_place = place })
          (Gwdb.get_pevents p)
      in
      {
        (Gwdb.gen_person_of_person p) with
        birth_place;
        baptism_place;
        death_place;
        burial_place;
        pevents;
      }
  | Some `source ->
      let new_istr =
        Gwdb.insert_string ~format:`Html base
          (Ext_string.only_printable new_input)
      in
      let src_bi = Gwdb.get_birth_src p in
      let s_bi = Gwdb.sou base src_bi in
      let src_bp = Gwdb.get_baptism_src p in
      let s_bp = Gwdb.sou base src_bp in
      let src_de = Gwdb.get_death_src p in
      let s_de = Gwdb.sou base src_de in
      let src_bu = Gwdb.get_burial_src p in
      let s_bu = Gwdb.sou base src_bu in
      let src_p = Gwdb.get_psources p in
      let s_p = Gwdb.sou base src_p in
      let birth_src = if old = s_bi then new_istr else src_bi in
      let baptism_src = if old = s_bp then new_istr else src_bp in
      let death_src = if old = s_de then new_istr else src_de in
      let burial_src = if old = s_bu then new_istr else src_bu in
      let psources_src = if old = s_p then new_istr else src_p in
      let pevents =
        List.map
          (fun evt ->
            let src_evt = evt.Def.epers_src in
            let s_evt = Gwdb.sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with epers_src = src })
          (Gwdb.get_pevents p |> List.map Gwdb.gen_pevent_of_pers_event)
      in
      {
        (Gwdb.gen_person_of_person p) with
        birth_src;
        baptism_src;
        death_src;
        burial_src;
        psources = psources_src;
        pevents;
      }
  | Some `firstname ->
      let new_istr =
        Gwdb.insert_string base (Ext_string.only_printable new_input)
      in
      let first_name = Gwdb.get_first_name p in
      let s_first_name = Gwdb.sou base first_name in
      let s_first_name_lower = Name.lower s_first_name in
      let new_input_lower = Name.lower new_input in
      let first_name, occ =
        if new_input_lower = s_first_name_lower then (new_istr, Gwdb.get_occ p)
        else if old = s_first_name then
          ( new_istr,
            Gutil.find_free_occ base (Gwdb.sou base new_istr)
              (Gwdb.sou base (Gwdb.get_surname p)) )
        else (first_name, Gwdb.get_occ p)
      in
      let first_names_aliases = Gwdb.get_first_names_aliases p in
      let first_names_aliases =
        if Util.p_getenv conf.env "first_name_aliases" = Some "yes" then
          let has_first_name_alias =
            List.fold_left
              (fun has_first_name alias ->
                has_first_name
                || s_first_name_lower = Name.lower (Gwdb.sou base alias))
              false first_names_aliases
          in
          if has_first_name_alias then first_names_aliases
          else Gwdb.get_first_name p :: first_names_aliases
        else first_names_aliases
      in
      {
        (Gwdb.gen_person_of_person p) with
        first_name;
        occ;
        first_names_aliases;
      }
  | Some `surname ->
      let new_istr =
        Gwdb.insert_string base (Ext_string.only_printable new_input)
      in
      let surname = Gwdb.get_surname p in
      let s_surname = Gwdb.sou base surname in
      let s_surname_lower = Name.lower s_surname in
      let new_input_lower = Name.lower new_input in
      let surname, occ =
        if new_input_lower = s_surname_lower then (new_istr, Gwdb.get_occ p)
        else if old = s_surname then
          ( new_istr,
            Gutil.find_free_occ base
              (Gwdb.sou base (Gwdb.get_first_name p))
              (Gwdb.sou base new_istr) )
        else (surname, Gwdb.get_occ p)
      in
      let surnames_aliases = Gwdb.get_surnames_aliases p in
      let surnames_aliases =
        if Util.p_getenv conf.env "surname_aliases" = Some "yes" then
          let has_surname_alias =
            List.fold_left
              (fun has_surname alias ->
                has_surname
                || s_surname_lower = Name.lower (Gwdb.sou base alias))
              false surnames_aliases
          in
          if has_surname_alias then surnames_aliases
          else Gwdb.get_surname p :: surnames_aliases
        else surnames_aliases
      in
      { (Gwdb.gen_person_of_person p) with surname; occ; surnames_aliases }
  | _ -> Gwdb.gen_person_of_person p

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
  match get_data_kind_from_env conf.Config.env with
  | Some `place ->
      let new_istr =
        Gwdb.insert_string base (Ext_string.only_printable new_istr)
      in
      let p_ma = Gwdb.get_marriage_place fam in
      let s_ma = Gwdb.sou base p_ma in
      let marriage_place = if old = s_ma then new_istr else p_ma in
      let fevents =
        List.map
          (fun evt ->
            let evt = Gwdb.gen_fevent_of_fam_event evt in
            let pl_evt = evt.efam_place in
            let s_evt = Gwdb.sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with efam_place = place })
          (Gwdb.get_fevents fam)
      in
      { (Gwdb.gen_family_of_family fam) with marriage_place; fevents }
  | Some `source ->
      let new_istr =
        Gwdb.insert_string ~format:`Html base
          (Ext_string.only_printable new_istr)
      in
      let src_ma = Gwdb.get_marriage_src fam in
      let s_ma = Gwdb.sou base src_ma in
      let src_f = Gwdb.get_fsources fam in
      let s_f = Gwdb.sou base src_f in
      let marriage_src = if old = s_ma then new_istr else src_ma in
      let fsources = if old = s_f then new_istr else src_f in
      let fevents =
        List.map
          (fun evt ->
            let evt = Gwdb.gen_fevent_of_fam_event evt in
            let src_evt = evt.efam_src in
            let s_evt = Gwdb.sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with efam_src = src })
          (Gwdb.get_fevents fam)
      in
      { (Gwdb.gen_family_of_family fam) with marriage_src; fsources; fevents }
  | _ -> Gwdb.gen_family_of_family fam

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
  let data_kind = get_data_kind_from_env conf.Config.env in
  let test_family =
    match get_data data_kind with _, _, [], [] -> false | _ -> true
  in
  let action =
    match data_kind with
    | Some `occupation -> "co"
    | Some `place -> "cp"
    | Some `source -> "cs"
    | Some `firstname -> "fn"
    | Some `surname -> "sn"
    | _ -> ""
  in
  let list =
    if nb_pers > max_updates then reduce_cpl_list max_updates list else list
  in
  List.iter
    (fun (old, perl) ->
      (* Mise à jour de toutes les personnes concernées. *)
      List.iter
        (fun p ->
          let o_p = Gwdb.gen_person_of_person p in
          let np = update_person conf base old new_input p in
          (if action = "fn" || action = "sn" then
           let pi = np.key_index in
           let op = Gwdb.poi base pi in
           let sp =
             Futil.map_person_ps
               (fun ip -> ip)
               (fun ?format:_ istr -> Gwdb.sou base istr)
               np
           in
           Image.rename_portrait conf base op (sp.first_name, sp.surname, sp.occ));
          Gwdb.patch_person base np.key_index np;
          if test_family then
            Array.iter
              (fun ifam ->
                let fam = Gwdb.foi base ifam in
                let nfam = update_family conf base old new_input fam in
                Gwdb.patch_family base nfam.fam_index nfam)
              (Gwdb.get_family p);
          (* On met aussi à jour l'historique. *)
          let changed =
            Def.U_Multi
              (o_p, np, if action = "fn" || action = "sn" then true else false)
          in
          History.record conf base changed action)
        perl)
    list;
  Util.commit_patches conf base;
  (* On appelle explicitement notify_change car la base est modifiée.  *)
  (* On fait cet appel à la fin de chaque mise à jour de la liste des  *)
  (* personnes, car si l'administrateur de la base ne modifie pas tous *)
  (* les évènements liés à cette donnée, on ne sera pas mis au courant *)
  (* que la base à été mise à jour.                                    *)
  History.notify conf base action

(** Get all the data and filter them if ["s"] is defined in [conf.env] *)
let build_list ~ignore_case conf base =
  (* Paramètre pour savoir par quoi commence la chaine. *)
  let ini = Option.value ~default:"" (Util.p_getenv conf.Config.env "s") in
  let list =
    let get_data_from_database ~conf base =
      List.map (fun string_id -> `String_id string_id) (get_all_data conf base)
    in
    match get_data_kind_from_env conf.Config.env with
    | None -> get_data_from_database ~conf base
    | Some data_kind ->
        let with_cache () =
          Gwdb.nb_of_persons base > Caches.node_threshold
          && Caches.has_cache ~conf ~mode:data_kind ~with_up_to_date_state:true
               ()
        in
        if not @@ with_cache () then get_data_from_database ~conf base
        else
          Ext_list.map_sort_uniq
            (fun string -> `String string)
            (Caches.complete_with_patch data_kind base
               (Fun.negate @@ String.equal "")
               (Caches.read_cache ~conf data_kind))
  in
  let to_string_id_string ~base = function
    | `String_id istr -> Some (istr, Gwdb.sou base istr)
    | `String string ->
        let string_id = Gwdb.find_opt_string_istr base string in
        Option.map (fun string_id -> (string_id, string)) string_id
  in
  if ini <> "" then
    List.filter_map
      (fun element ->
        Option.bind (to_string_id_string ~base element) (fun (istr, str) ->
            if
              Utf8.start_with_wildcard ~ignore_case ini 0
              @@ Place.without_suburb str
            then Some (istr, str)
            else None))
      list
  else List.filter_map (to_string_id_string ~base) list

let build_list_short conf list =
  let ini = Option.value ~default:"" (Util.p_getenv conf.Config.env "s") in
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
          (* Astuce pour gérer les espaces. *)
          Ext_string.tr ' ' '_'
            (if String.length s > i then String.sub s 0 (Utf8.next s i)
            else s ^ String.make (i + 1 - String.length s) '_'))
        l
    in
    let inis = List.sort_uniq Utf8.alphabetic_order inis in
    match inis with
    | [ ini ] -> build_ini list (String.length ini)
    | list -> list
  in
  build_ini list (String.length ini)

let build_list_long ~ignore_case conf list :
    (string * (Gwdb.istr * string) list) list =
  let ini = Option.value ~default:"" (Util.p_getenv conf.Config.env "s") in
  let list = combine_by_ini ~ignore_case ini list in
  List.sort (fun (ini1, _) (ini2, _) -> Utf8.alphabetic_order ini1 ini2) list
