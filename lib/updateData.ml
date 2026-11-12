open Config
open Def
open Util
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil
module Dict = Geneweb_db.Dict

module PersSet = Set.Make (struct
  type t = Driver.person

  let compare p1 p2 = compare (Driver.get_iper p1) (Driver.get_iper p2)
end)

module StringSet = Set.Make (String)

(* if env parameter "all" is "on", then we should search for places
   without suburb.
   Later, we should update places values while keeping the suburb value.
*)

let dict conf = Option.bind (p_getenv conf.env "data") Dict.of_string

let get_person_from_data conf base =
  let istr = Driver.Istr.of_string @@ (List.assoc "key" conf.env :> string) in
  match dict conf with
  | None -> []
  | Some d ->
      let ipers, ifams = Driver.dict_records base d istr in
      let add acc ip = PersSet.add (pget conf base ip) acc in
      let acc =
        List.fold_left
          (fun acc ifam ->
            let f = Driver.foi base ifam in
            add (add acc (Driver.get_father f)) (Driver.get_mother f))
          (List.fold_left add PersSet.empty ipers)
          ifams
      in
      if PersSet.is_empty acc then [] else [ (istr, PersSet.elements acc) ]

(* The book of places ordered from the last term ("France, Manche,
   Granville"), asked by [o=r]. It reads the section
   [Dict.Place_last] of the index; an index written before it has none, and
   the book stays in the alphabetic order until "gwc -reindex" writes it. *)
let rev_param conf = p_getenv conf.env "o" = Some "r"

let rev_order conf base =
  p_getenv conf.env "data" = Some "place"
  && rev_param conf
  && Driver.dict_has base Dict.Place_last

(* the text the navigation by 1, 2... characters runs on *)
let nav_key conf base =
  if rev_order conf base then Dict.last_key else Place.without_suburb

let rec compare_terms l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | t1 :: l1, t2 :: l2 ->
      let c = Gutil.alphabetic_order t1 t2 in
      if c <> 0 then c else compare_terms l1 l2

let combine_by_ini key ini list =
  let len = Utf8.length ini + 1 in
  Mutil.groupby
    ~key:(fun (_, s) ->
      let s = key s in
      if Utf8.length s >= len then Utf8.sub s 0 len else s)
    ~value:(fun x -> x)
    list

(* ************************************************************************** *)
(* [Fonc] update_person : conf -> base -> string -> string -> person ->
                            gen_person iper istr *)

(* ************************************************************************** *)

(** [Description] : Met à jour le/les champ(s) de la personne. [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - old : l'ancien contenu
    - new_input : le nouveau contenu
    - p : person [Retour] :
    - gen_person iper istr : gen_person avec les champs modifiés [Rem] : Non
      exporté en clair hors de ce module. *)
let update_person conf base old new_input p =
  match p_getenv conf.env "data" with
  | Some "occu" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let occupation = Driver.get_occupation p in
      let s_occupation = Driver.sou base occupation in
      let occupation = if old = s_occupation then new_istr else occupation in
      { (Driver.gen_person_of_person p) with occupation }
  | Some "place" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let pl_bi = Driver.get_birth_place p in
      let s_bi = Driver.sou base pl_bi in
      let pl_bp = Driver.get_baptism_place p in
      let s_bp = Driver.sou base pl_bp in
      let pl_de = Driver.get_death_place p in
      let s_de = Driver.sou base pl_de in
      let pl_bu = Driver.get_burial_place p in
      let s_bu = Driver.sou base pl_bu in
      let birth_place = if old = s_bi then new_istr else pl_bi in
      let baptism_place = if old = s_bp then new_istr else pl_bp in
      let death_place = if old = s_de then new_istr else pl_de in
      let burial_place = if old = s_bu then new_istr else pl_bu in
      let pevents =
        List.map
          (fun evt ->
            let pl_evt = evt.epers_place in
            let s_evt = Driver.sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with epers_place = place })
          (Driver.get_pevents p)
      in
      {
        (Driver.gen_person_of_person p) with
        birth_place;
        baptism_place;
        death_place;
        burial_place;
        pevents;
      }
  | Some "src" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let src_bi = Driver.get_birth_src p in
      let s_bi = Driver.sou base src_bi in
      let src_bp = Driver.get_baptism_src p in
      let s_bp = Driver.sou base src_bp in
      let src_de = Driver.get_death_src p in
      let s_de = Driver.sou base src_de in
      let src_bu = Driver.get_burial_src p in
      let s_bu = Driver.sou base src_bu in
      let src_p = Driver.get_psources p in
      let s_p = Driver.sou base src_p in
      let birth_src = if old = s_bi then new_istr else src_bi in
      let baptism_src = if old = s_bp then new_istr else src_bp in
      let death_src = if old = s_de then new_istr else src_de in
      let burial_src = if old = s_bu then new_istr else src_bu in
      let psources_src = if old = s_p then new_istr else src_p in
      let pevents =
        List.map
          (fun evt ->
            let src_evt = evt.epers_src in
            let s_evt = Driver.sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with epers_src = src })
          (Driver.get_pevents p)
      in
      {
        (Driver.gen_person_of_person p) with
        birth_src;
        baptism_src;
        death_src;
        burial_src;
        psources = psources_src;
        pevents;
      }
  | Some "fn" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let first_name = Driver.get_first_name p in
      let s_first_name = Driver.sou base first_name in
      let s_first_name_lower = Name.lower s_first_name in
      let new_input_lower = Name.lower new_input in
      let first_name, occ =
        if new_input_lower = s_first_name_lower then (new_istr, Driver.get_occ p)
        else if old = s_first_name then
          ( new_istr,
            Gutil.find_free_occ base (Driver.sou base new_istr)
              (Driver.sou base (Driver.get_surname p)) )
        else (first_name, Driver.get_occ p)
      in
      let first_names_aliases = Driver.get_first_names_aliases p in
      let first_names_aliases =
        if p_getenv conf.env "first_name_aliases" = Some "yes" then
          let has_first_name_alias =
            List.fold_left
              (fun has_first_name alias ->
                has_first_name
                || s_first_name_lower = Name.lower (Driver.sou base alias))
              false first_names_aliases
          in
          if has_first_name_alias then first_names_aliases
          else Driver.get_first_name p :: first_names_aliases
        else first_names_aliases
      in
      {
        (Driver.gen_person_of_person p) with
        first_name;
        occ;
        first_names_aliases;
      }
  | Some "sn" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let surname = Driver.get_surname p in
      let s_surname = Driver.sou base surname in
      let s_surname_lower = Name.lower s_surname in
      let new_input_lower = Name.lower new_input in
      let surname, occ =
        if new_input_lower = s_surname_lower then (new_istr, Driver.get_occ p)
        else if old = s_surname then
          ( new_istr,
            Gutil.find_free_occ base
              (Driver.sou base (Driver.get_first_name p))
              (Driver.sou base new_istr) )
        else (surname, Driver.get_occ p)
      in
      let surnames_aliases = Driver.get_surnames_aliases p in
      let surnames_aliases =
        if p_getenv conf.env "surname_aliases" = Some "yes" then
          let has_surname_alias =
            List.fold_left
              (fun has_surname alias ->
                has_surname
                || s_surname_lower = Name.lower (Driver.sou base alias))
              false surnames_aliases
          in
          if has_surname_alias then surnames_aliases
          else Driver.get_surname p :: surnames_aliases
        else surnames_aliases
      in
      { (Driver.gen_person_of_person p) with surname; occ; surnames_aliases }
  | Some "fna" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_fna = Driver.get_first_names_aliases p in
      let first_names_aliases =
        List.fold_left
          (fun acc a ->
            if old = Driver.sou base a then new_istr :: acc else a :: acc)
          [] old_fna
      in
      { (Driver.gen_person_of_person p) with first_names_aliases }
  | Some "sna" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_sna = Driver.get_surnames_aliases p in
      let surnames_aliases =
        List.fold_left
          (fun acc a ->
            if old = Driver.sou base a then new_istr :: acc else a :: acc)
          [] old_sna
      in
      { (Driver.gen_person_of_person p) with surnames_aliases }
  | Some "alias" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_aliases = Driver.get_aliases p in
      let aliases =
        List.fold_left
          (fun acc a ->
            if old = Driver.sou base a then new_istr :: acc else a :: acc)
          [] old_aliases
      in
      { (Driver.gen_person_of_person p) with aliases }
  | Some "pubn" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let public_name = new_istr in
      { (Driver.gen_person_of_person p) with public_name }
  | Some "qual" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_qualifiers = Driver.get_qualifiers p in
      let qualifiers =
        List.map
          (fun q -> if old = Driver.sou base q then new_istr else q)
          old_qualifiers
      in
      { (Driver.gen_person_of_person p) with qualifiers }
  | Some "title" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_titles = Driver.get_titles p in
      let titles =
        List.map
          (fun t ->
            if old = Driver.sou base t.t_ident then
              { t with t_ident = new_istr }
            else t)
          old_titles
      in
      { (Driver.gen_person_of_person p) with titles }
  | Some "domain" ->
      let new_istr = Driver.insert_string base (only_printable new_input) in
      let old_titles = Driver.get_titles p in
      let titles =
        List.map
          (fun t ->
            if old = Driver.sou base t.t_place then
              { t with t_place = new_istr }
            else t)
          old_titles
      in
      { (Driver.gen_person_of_person p) with titles }
  | _ -> Driver.gen_person_of_person p

(* ************************************************************************** *)
(* [Fonc] update_family : conf -> base -> string -> string -> person ->
                            gen_family ifam istr *)

(* ************************************************************************** *)

(** [Description] : Met à jour le/les champ(s) de la famille. [Args] :
    - conf : configuration de la base
    - base : base de donnée
    - old : l'ancien contenu
    - new_input : le nouveau contenu
    - fam : family [Retour] :
    - gen_family ifam istr : gen_family avec les champs modifiés [Rem] : Non
      exporté en clair hors de ce module. *)
let update_family conf base old new_istr fam =
  match p_getenv conf.env "data" with
  | Some "place" ->
      let new_istr = Driver.insert_string base (only_printable new_istr) in
      let p_ma = Driver.get_marriage_place fam in
      let s_ma = Driver.sou base p_ma in
      let marriage_place = if old = s_ma then new_istr else p_ma in
      let fevents =
        List.map
          (fun evt ->
            let pl_evt = evt.efam_place in
            let s_evt = Driver.sou base pl_evt in
            let place = if old = s_evt then new_istr else pl_evt in
            { evt with efam_place = place })
          (Driver.get_fevents fam)
      in
      { (Driver.gen_family_of_family fam) with marriage_place; fevents }
  | Some "src" ->
      let new_istr = Driver.insert_string base (only_printable new_istr) in
      let src_ma = Driver.get_marriage_src fam in
      let s_ma = Driver.sou base src_ma in
      let src_f = Driver.get_fsources fam in
      let s_f = Driver.sou base src_f in
      let marriage_src = if old = s_ma then new_istr else src_ma in
      let fsources = if old = s_f then new_istr else src_f in
      let fevents =
        List.map
          (fun evt ->
            let src_evt = evt.efam_src in
            let s_evt = Driver.sou base src_evt in
            let src = if old = s_evt then new_istr else src_evt in
            { evt with efam_src = src })
          (Driver.get_fevents fam)
      in
      { (Driver.gen_family_of_family fam) with marriage_src; fsources; fevents }
  | _ -> Driver.gen_family_of_family fam

(* ********************************************************************** *)
(* [Fonc] update_person_list :
            config -> base -> string -> (string * person list) list -> int *)

(* ********************************************************************** *)

(** [Description] : [Args] :
    - conf : configuration
    - base : base
    - new_input : le nouveau contenu
    - list : la liste des (clé, person list) [Retour] :
    - le nombre de personnes mises à jour *)
let update_person_list conf base new_input list =
  let test_family =
    Option.fold ~none:false ~some:Dict.has_family_fields (dict conf)
  in
  let action =
    match p_getenv conf.env "data" with
    | Some "occu" -> "mo"
    | Some "place" -> "mq"
    | Some "src" -> "ms"
    | Some "fn" -> "fn"
    | Some "sn" -> "sn"
    | Some "alias" -> "ma"
    | Some "pubn" -> "mu"
    | Some "qual" -> "mx"
    | Some "title" -> "mt"
    | Some "domain" -> "md"
    | Some "fna" -> "fa"
    | Some "sna" -> "sa"
    | _ -> ""
  in
  let cnt = ref 0 in
  List.iter
    (fun (old, perl) ->
      (* Mise à jour de toutes les personnes concernées. *)
      List.iter
        (fun p ->
          incr cnt;
          let gp = Driver.gen_person_of_person p in
          let o_p = Util.string_gen_person base gp in
          let np = update_person conf base old new_input p in
          if action = "fn" || action = "sn" then
            Image.rename_portrait_and_blason conf base p
              (Driver.sou base np.first_name, Driver.sou base np.surname, np.occ);
          if np <> gp then Driver.patch_person base np.key_index np;
          if test_family then
            Array.iter
              (fun ifam ->
                let fam = Driver.foi base ifam in
                let nfam = update_family conf base old new_input fam in
                if nfam <> Driver.gen_family_of_family fam then
                  Driver.patch_family base nfam.fam_index nfam)
              (Driver.get_family p);
          (* On met aussi à jour l'historique. *)
          if not (Util.is_empty_name p) then
            let changed = U_Multi (o_p, Util.string_gen_person base np) in
            History.record conf base changed action)
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

let book_entry conf base =
  let ini = Option.value ~default:"" (p_getenv_notrim conf.env "s") in
  let data_type = p_getenv conf.env "data" in
  let has_particle = data_type = Some "sn" || data_type = Some "domain" in
  let is_name_type = data_type = Some "fn" || data_type = Some "sn" in
  let key = nav_key conf base in
  fun istr ->
    let str = Driver.sou base istr in
    if is_name_type && str = "?" then None
    else
      let str = if has_particle then move_particle base str else str in
      if String.starts_with ~prefix:ini (key str) then Some (istr, str)
      else None

let build_list_short conf base list =
  let ini = Option.value ~default:"" (p_getenv_notrim conf.env "s") in
  let key = nav_key conf base in
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
          let s = key s in
          if String.length s > i then String.sub s 0 (Utf8.next s i) else s)
        l
    in
    (* Fonction pour supprimer les doublons. *)
    let remove_dup list =
      StringSet.elements
        (List.fold_left
           (fun accu ini -> StringSet.add ini accu)
           StringSet.empty list)
    in
    let inis = remove_dup inis in
    match inis with
    | [ ini ] ->
        let new_i = String.length ini in
        if new_i <= i then [ ini ] else build_ini list new_i
    | list -> List.sort Gutil.alphabetic_order list
  in
  build_ini list (String.length ini)

let build_list_long conf base list : (string * (Driver.istr * string) list) list
    =
  let ini = Option.value ~default:"" (p_getenv_notrim conf.env "s") in
  let list = combine_by_ini (nav_key conf base) ini list in
  List.sort (fun (ini1, _) (ini2, _) -> Gutil.alphabetic_order ini1 ini2) list

type book = {
  count : int;
  groups : (string * int * (Driver.istr * string) list) list;
  records : (Driver.istr, int) Hashtbl.t;
}

let book_max_results conf =
  match
    Option.bind
      (List.assoc_opt "book_max_results" conf.base_env)
      int_of_string_opt
  with
  | Some n when n > 0 -> n
  | Some _ | None -> 500

let book_bulk_confirm conf =
  match
    Option.bind
      (List.assoc_opt "book_bulk_confirm" conf.base_env)
      int_of_string_opt
  with
  | Some n when n > 0 -> n
  | Some _ | None -> 150

(* [from] and [to]: a page of the index of the prefix [s], the groups from
   [from] to [to] listed together *)
let book_range conf =
  match (p_getenv_notrim conf.env "from", p_getenv_notrim conf.env "to") with
  | Some a, Some b -> Some (a, b)
  | _ -> None

(* The groups of an index cut into the pages listed by one click: consecutive
   groups of less than [book_max_results] entries in all. A group of more
   entries is a page of its own, which opens its own index. *)
let book_pages conf groups =
  let max = book_max_results conf in
  let close cur acc = if cur = [] then acc else List.rev cur :: acc in
  let rec loop acc cur sum = function
    | [] -> List.rev (close cur acc)
    | ((_, n, _) as g) :: l ->
        if n >= max then loop ([ g ] :: close cur acc) [] 0 l
        else if sum + n >= max then loop (close cur acc) [ g ] n l
        else loop acc (g :: cur) (sum + n) l
  in
  loop [] [] 0 groups

let book conf base =
  match dict conf with
  | None -> { count = 0; groups = []; records = Hashtbl.create 1 }
  | Some d -> (
      let d =
        if d = Dict.Place && rev_order conf base then Dict.Place_last else d
      in
      let p = Option.value ~default:"" (p_getenv_notrim conf.env "s") in
      let hidden =
        match d with
        | Dict.First_name | Dict.Surname ->
            List.filter_map
              (fun istr ->
                let s = Driver.sou base istr in
                if
                  String.starts_with ~prefix:p s
                  && fst (Driver.dict_records base d istr) <> []
                then Some s
                else None)
              [ Driver.Istr.empty; Driver.Istr.quest ]
        | Dict.(
            ( First_name_alias | Surname_alias | Public_name | Qualifier | Alias
            | Occupation | Place | Source | Title | Domain | Place_last )) ->
            []
      in
      let index () =
        List.filter_map
          (fun (g, n) ->
            let n = if List.mem g hidden then n - 1 else n in
            if n > 0 then Some (g, n, []) else None)
          (Driver.dict_groups base d p)
        |> List.sort (fun (g1, _, _) (g2, _, _) -> Gutil.alphabetic_order g1 g2)
      in
      let listed entries =
        let entry = book_entry conf base in
        let records = Hashtbl.create 256 in
        let list =
          List.filter_map
            (fun (istr, n) ->
              if Driver.Istr.is_empty istr then None
              else (
                Hashtbl.replace records istr n;
                entry istr))
            entries
        in
        {
          count = List.length list;
          groups =
            List.map
              (fun (ini, l) -> (ini, List.length l, l))
              (build_list_long conf base list);
          records;
        }
      in
      match book_range conf with
      | Some (a, b) ->
          let rec from = function
            | (g, _, _) :: _ as l when g = a -> l
            | _ :: l -> from l
            | [] -> []
          in
          let rec until acc = function
            | ((g, _, _) as x) :: l ->
                if g = b then List.rev (x :: acc) else until (x :: acc) l
            | [] -> List.rev acc
          in
          listed
            (List.concat_map
               (fun (g, _, _) ->
                 if g = p then Driver.dict_exact_count base d p
                 else Driver.dict_entries_count base d g)
               (until [] (from (index ()))))
      | None ->
          let count = Driver.dict_size base d p - List.length hidden in
          if count < book_max_results conf then
            listed (Driver.dict_entries_count base d p)
          else { count; groups = index (); records = Hashtbl.create 1 })
