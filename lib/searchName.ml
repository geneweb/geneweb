(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

type spi = {
  spi : Gwdb.string_person_index;
  mutable st : [ `First | `Current of istr ];
}

let spi_of_fn base =
  let spi = Gwdb.persons_of_first_name base in
  { spi; st = `First }

let spi_of_sn base =
  let spi = Gwdb.persons_of_surname base in
  { spi; st = `First }

let start_with base pfx s =
  let particles = Gwdb.base_particles base in
  let p = Mutil.get_particle particles s in
  let len_particle = String.length p in
  let s = String.sub s len_particle (String.length s - len_particle) in
  Mutil.start_with pfx 0 s

let ipers_of_prefix base spi prefix =
  let istr_o =
    try
      match spi.st with
      | `First -> Some (Gwdb.spi_first spi.spi prefix)
      | `Current istr ->
          let istr' = Gwdb.spi_next spi.spi istr in
          if Gwdb.compare_istr istr istr' <> 0 then Some istr' else None
    with Not_found -> None
  in
  Option.bind istr_o (fun istr ->
      spi.st <- `Current istr;
      let s = Gwdb.sou base istr in
      if start_with base prefix s then Some (Gwdb.spi_find spi.spi istr)
      else None)

let all_names_of_prefix base spi prefix =
  try
    let istr = Gwdb.spi_first spi.spi prefix in
    let rec aux istr l =
      let s = Gwdb.sou base istr in
      if start_with base prefix s then
        let l = istr :: l in
        try
          let istr' = Gwdb.spi_next spi.spi istr in
          if Gwdb.compare_istr istr istr' <> 0 then aux istr' l else l
        with Not_found -> l
      else l
    in
    aux istr []
  with Not_found -> []

let persons_of_prefix conf base spi prefix max =
  let ipers = ipers_of_prefix base spi prefix in
  let rec aux n l ipers =
    match ipers with
    | _iper :: _ipers when n <= 0 -> l
    | iper :: ipers ->
        let p = Gwdb.poi base iper in
        if Util.authorized_age conf base p then aux (n - 1) (p :: l) ipers
        else aux n l ipers
    | _ -> l
  in
  match ipers with
  | Some ipers -> Some (List.rev (aux max [] ipers))
  | None -> None

let n_persons_of_prefix n conf base spi prefix =
  let rec aux n l =
    match persons_of_prefix conf base spi prefix n with
    | Some persons ->
        let len = List.length persons in
        if len > n then
          let persons = Ext_list.take persons n in
          l @ persons
        else aux (n - len) (l @ persons)
    | None -> l
  in
  aux n []

let persons_of_prefixes max conf base fn_pfx sn_pfx =
  let sn_spi = spi_of_sn base in
  let all_fn_pfx = all_names_of_prefix base (spi_of_fn base) fn_pfx in
  let fn_pfx_set = Util.IstrSet.of_list all_fn_pfx in
  let rec aux n l =
    let sn_ipers = ipers_of_prefix base sn_spi sn_pfx in
    let rec aux' n l ipers =
      if n = 0 then l
      else
        match ipers with
        | iper :: ipers ->
            let p = Gwdb.poi base iper in
            let fn = Gwdb.get_first_name p in
            if IstrSet.mem fn fn_pfx_set && Util.authorized_age conf base p then
              aux' (n - 1) (p :: l) ipers
            else aux' n l ipers
        | _ -> aux n l
    in
    match sn_ipers with Some ipers -> aux' n l ipers | None -> l
  in
  List.rev (aux max [])

let persons_starting_with ~conf ~base ~first_name_prefix ~surname_prefix ~limit
    =
  let l =
    match (first_name_prefix, surname_prefix) with
    | "", "" -> []
    | _, "" ->
        let spi = spi_of_fn base in
        n_persons_of_prefix limit conf base spi first_name_prefix
    | "", _ ->
        let spi = spi_of_sn base in
        n_persons_of_prefix limit conf base spi surname_prefix
    | _, _ ->
        persons_of_prefixes limit conf base first_name_prefix surname_prefix
  in
  let cmp_s proj p1 p2 =
    Utf8.compare (Gwdb.sou base (proj p1)) (Gwdb.sou base (proj p2))
  in
  List.sort
    (fun p1 p2 ->
      let sn = cmp_s Gwdb.get_surname p1 p2 in
      if sn <> 0 then sn else cmp_s Gwdb.get_first_name p1 p2)
    l

let empty_sn_or_fn base p =
  is_empty_string (get_surname p)
  || is_quest_string (get_surname p)
  || is_empty_string (get_first_name p)
  || is_quest_string (get_first_name p)
  || Name.lower (sou base (get_surname p)) = ""
  || Name.lower (sou base (get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if
    List.exists
      (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then true
  else false

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (p_first_name base p) in
  let sn = Name.strip_lower (p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      if person_is_approx_key base p k then p :: pl
      else if person_is_misc_name conf base p k then p :: pl
      else pl)
    pl []

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | Some p, Some n ->
      if n <> Sosa.zero then
        match
          Util.branch_of_sosa conf base n (pget conf base @@ get_iper p)
        with
        | Some (p :: _) -> [ p ]
        | _ -> []
      else []
  | _ -> []

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
  | Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let list, _ =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base))
          get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              let p = pget conf base ip in
              if search_reject_p conf base p then pl
              else
                let fn1 =
                  Name.abbrev (Name.lower (sou base (get_first_name p)))
                in
                if List.mem fn (cut_words fn1) then p :: pl else pl)
            pl ipl)
        [] list
  | None -> []

let search_key_aux aux conf base an =
  let acc = Gutil.person_not_a_key_find_all base an in
  let an, acc =
    if acc = [] then
      match Util.name_with_roman_number an with
      | Some an1 ->
          let acc = Gutil.person_ht_find_all base an1 in
          if acc = [] then (an, []) else (an1, acc)
      | None -> (an, acc)
    else (an, acc)
  in
  let acc =
    Mutil.filter_map
      (fun i ->
        let p = Util.pget conf base i in
        if search_reject_p conf base p then None else Some p)
      acc
  in
  let acc = aux conf base acc an in
  Gutil.sort_uniq_person_list base acc

let search_partial_key =
  search_key_aux (fun conf base acc an ->
      if acc = [] then search_by_name conf base an else acc)

let search_approx_key = search_key_aux select_approx_key

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | Some ip ->
      let p = Util.pget conf base ip in
      if search_reject_p conf base p then [] else [ p ]
  | None -> []

(* main *)

type search_type =
  | Sosa
  | Key
  | Surname
  | FirstName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let search conf base an search_order specify unknown =
  let rec loop l =
    match l with
    | [] -> unknown conf an
    | Sosa :: l -> (
        let pl = search_by_sosa conf base an in
        match pl with
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | _ -> loop l)
    | Key :: l -> (
        let pl = search_by_key conf base an in
        match pl with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl)
    | Surname :: l -> (
        let pl = Some.search_surname conf base an in
        match pl with
        | [] -> loop l
        | _ -> Some.search_surname_print conf base unknown an)
    | FirstName :: l -> (
        let pl = Some.search_first_name conf base an in
        match pl with
        | [] -> loop l
        | _ -> Some.search_first_name_print conf base an)
    | ApproxKey :: l -> (
        let pl = search_approx_key conf base an in
        match pl with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl)
    | PartialKey :: l -> (
        let pl = search_partial_key conf base an in
        match pl with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl)
    | DefaultSurname :: _ -> Some.search_surname_print conf base unknown an
  in
  loop search_order

(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)

(* ************************************************************************ *)

(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
      trouver la meilleure combinaison de résultat pour afficher la réponse
      la plus probable.
    [Args] :
      - conf : configuration de la base
      - base : base
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
let print conf base specify unknown =
  let real_input label =
    match p_getenv conf.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  match (real_input "p", real_input "n") with
  | Some fn, Some sn ->
      let order = [ Key; ApproxKey; PartialKey ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | Some fn, None ->
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | None, Some sn ->
      let order =
        [ Sosa; Key; Surname; ApproxKey; PartialKey; DefaultSurname ]
      in
      search conf base sn order specify unknown
  | None, None -> Hutil.incorrect_request conf
