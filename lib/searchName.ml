(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let persons_of_stream conf base filter stream max =
  let rec aux n l ipers =
    match ipers with
    | _iper :: _ipers when n <= 0 -> l
    | iper :: ipers ->
        let p = Gwdb.poi base iper in
        if Util.authorized_age conf base p && filter p then
          aux (n - 1) (p :: l) ipers
        else aux n l ipers
    | _ -> l
  in
  if max <= 0 then None
  else
    try
      let ipers = Stream.next stream in
      Some (List.rev (aux max [] ipers))
    with Stream.Failure -> None

let n_persons_of_stream n conf base filter stream =
  let rec consume n l =
    match persons_of_stream conf base filter stream n with
    | Some persons ->
        let len = List.length persons in
        if len > n then
          let persons = Ext_list.take persons n in
          List.rev_append persons l
        else consume (n - len) (List.rev_append persons l)
    | None -> l
  in
  List.rev (consume n [])

let strip_particle base s =
  let particles = Gwdb.base_particles base in
  let p = Mutil.get_particle particles s in
  let len_particle = String.length p in
  String.sub s len_particle (String.length s - len_particle)

let start_with base pfx s =
  let s = Name.lower (strip_particle base s) in
  Mutil.start_with pfx 0 s

let persons_of_prefixes_stream max conf base filter fn_pfx sn_pfx =
  let sn_stream = Gwdb.persons_stream_of_surname_prefix base sn_pfx in
  let fn_map = Hashtbl.create 100 in
  let match_fn_istr istr =
    match Hashtbl.find_opt fn_map istr with
    | Some value -> value
    | None ->
        let value =
          start_with base
            (Name.lower (strip_particle base fn_pfx))
            (Gwdb.sou base istr)
        in
        Hashtbl.add fn_map istr value;
        value
  in
  let rec consume n results =
    try
      let sn_ipers = Stream.next sn_stream in
      let rec aux n results ipers =
        if n = 0 then results
        else
          match ipers with
          | iper :: ipers ->
              let p = Gwdb.poi base iper in
              let fn = Gwdb.get_first_name p in
              if match_fn_istr fn && Util.authorized_age conf base p && filter p
              then aux (n - 1) (p :: results) ipers
              else aux n results ipers
          | _ -> consume n results
      in
      aux n results sn_ipers
    with Stream.Failure -> results
  in
  List.rev (consume max [])

let persons_starting_with ~conf ~base ~filter ~first_name_prefix ~surname_prefix
    ~limit =
  let l =
    match (first_name_prefix, surname_prefix) with
    | "", "" -> []
    | _, "" ->
        let stream =
          Gwdb.persons_stream_of_first_name_prefix base first_name_prefix
        in
        n_persons_of_stream limit conf base filter stream
    | "", _ ->
        let stream =
          Gwdb.persons_stream_of_surname_prefix base surname_prefix
        in
        n_persons_of_stream limit conf base filter stream
    | _, _ ->
        persons_of_prefixes_stream limit conf base filter first_name_prefix
          surname_prefix
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
