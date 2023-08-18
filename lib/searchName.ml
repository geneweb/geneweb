(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

(* TODO use function from Util instead? *)
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

let split_normalize case s =
  cut_words (Name.abbrev (if case then s else Name.lower s))

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | None, _ | _, None -> None
  | Some p, Some n ->
      if n <> Sosa.zero then
        match
          Util.branch_of_sosa conf base n (pget conf base @@ get_iper p)
        with
        | Some (p :: _) -> Some p
        | _ -> None
      else None

(* TODO use function from Util instead? *)
let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let search_by_name conf base n =
  (* TODO use f here? why only split on the first ' '? *)
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
  | None -> []
  | Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let p_of_sn_l, _ =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base))
          get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              match Util.pget_opt conf base ip with
              | None -> pl
              | Some p ->
                  let fn1_l =
                    split_normalize true (sou base (get_first_name p))
                  in
                  let fn2_l =
                    split_normalize true (sou base (get_public_name p))
                  in
                  if List.mem fn fn1_l || List.mem fn fn2_l then p :: pl else pl)
            pl ipl)
        [] p_of_sn_l

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
  let acc = Mutil.filter_map (fun i -> Util.pget_opt conf base i) acc in
  let acc = aux conf base acc an in
  Gutil.sort_uniq_person_list base acc

let search_approx_key = search_key_aux select_approx_key

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
  | None -> None
  | Some ip -> Util.pget_opt conf base ip

(* main *)

type search_type =
  | Sosa
  | Key
  | Surname
  | FirstName
  | FullName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let search_for_multiple_fn conf base fn pl =
  let test label = p_getenv conf.env label = Some "on" in
  let exact = test "exact" in
  let case = test "case" in
  let order = test "order" in
  let all = test "all" in
  let is_same_or_substr x xx = if exact then x = xx else Mutil.contains xx x in
  let is_sublist l1 l2 =
    let rec check_from_pos l1 l2_remaining =
      match (l1, l2_remaining) with
      | [], _ -> true (* Empty list is a sublist *)
      | _, [] -> false (* Ran out of l2 before finding match *)
      | h1 :: t1, h2 :: t2 ->
          if is_same_or_substr h1 h2 then
            (* First element matches *)
            check_from_pos t1 t2
          else false
    in
    let rec try_all_positions l1 = function
      | [] -> false
      | _ :: t2 as l2 -> check_from_pos l1 l2 || try_all_positions l1 t2
    in
    match l1 with [] -> true | _ -> try_all_positions l1 l2
  in
  let ok fn_l fn1_l =
    if all && order then is_sublist fn_l fn1_l
    else if all then
      List.for_all
        (fun fn -> List.exists (fun fn1 -> is_same_or_substr fn fn1) fn1_l)
        fn_l
    else
      List.exists
        (fun fn -> List.exists (fun fn1 -> is_same_or_substr fn fn1) fn1_l)
        fn_l
  in
  let fn_l = cut_words fn in
  let fn_l = List.map (fun fn -> if case then fn else Name.lower fn) fn_l in
  List.fold_left
    (fun pl p ->
      if search_reject_p conf base p then pl
      else
        let fn1_l = get_first_name p |> sou base |> split_normalize case in
        let fn2_l = get_public_name p |> sou base |> split_normalize case in
        if ok fn_l fn1_l || ok fn_l fn2_l then p :: pl else pl)
    [] pl

let search conf base an search_order specify unknown =
  let rec loop l =
    match l with
    | [] -> unknown conf an
    | Sosa :: l -> (
        match search_by_sosa conf base an with
        | None -> loop l
        | Some p ->
            record_visited conf (get_iper p);
            Perso.print conf base p)
    | Key :: l -> (
        match search_by_key conf base an with
        | None -> loop l
        | Some p ->
            record_visited conf (get_iper p);
            Perso.print conf base p)
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
    | FullName :: l -> (
        let fn =
          match p_getenv conf.env "p" with
          | Some fn -> Name.lower fn
          | None -> ""
        in
        let sn =
          match p_getenv conf.env "n" with
          | Some sn -> Name.lower sn
          | None -> ""
        in
        let fn, sn =
          if fn = "" then
            (* we assume fn1 fn2 sn. For other cases, use fn, sn explicitely *)
            (* TODO check for particles and cut before particle *)
            (* see if    Name.abbrev (Name.lower sn)    is Ok *)
            (* or use split_normalize here? *)
            match String.rindex_opt sn ' ' with
            | Some i ->
                ( String.sub sn 0 i,
                  String.sub sn (i + 1) (String.length sn - i - 1) )
            | _ -> ("", sn)
          else (fn, sn)
        in
        let conf =
          { conf with env = ("surname", Adef.encoded sn) :: conf.env }
        in
        (* find all bearers of sn using advanced_search *)
        let list, _len = AdvSearchOk.advanced_search conf base max_int in
        match list with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | pl -> (
            (* check first_names or public_names in list of persons *)
            let pl = search_for_multiple_fn conf base fn pl in
            match pl with
            | [] -> loop l
            | [ p ] ->
                record_visited conf (get_iper p);
                Perso.print conf base p
            | pl -> specify conf base an pl))
    | ApproxKey :: l -> (
        let pl = search_approx_key conf base an in
        match pl with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl)
    | PartialKey :: l -> (
        let pl = search_by_name conf base an in
        match pl with
        | [] -> (
            (* try advanced search *)
            (* TODO use split_normalize here? why only split on the first ' '? *)
            let n1 = Name.abbrev (Name.lower an) in
            let fn, sn =
              match String.index_opt n1 ' ' with
              | Some i ->
                  ( String.sub n1 0 i,
                    String.sub n1 (i + 1) (String.length n1 - i - 1) )
              | _ -> ("", n1)
            in
            let conf =
              { conf with env = ("surname", Adef.encoded sn) :: conf.env }
            in
            let p_of_sn_l, _len =
              AdvSearchOk.advanced_search conf base max_int
            in
            match p_of_sn_l with
            | [] -> loop l
            | [ p ] ->
                record_visited conf (get_iper p);
                Perso.print conf base p
            | pl -> (
                let pl = search_for_multiple_fn conf base fn pl in
                match pl with
                | [] -> loop l
                | [ p ] ->
                    record_visited conf (get_iper p);
                    Perso.print conf base p
                | pl -> specify conf base an pl))
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
      let order = [ Key; FullName ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | Some fn, None ->
      let fn =
        match String.rindex_opt fn '.' with
        | Some i -> String.sub fn 0 i
        | None -> fn
      in
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | None, Some sn ->
      Printf.eprintf "None, Some sn: %s\n" sn;
      let order =
        [ Sosa; Key; FullName; Surname; ApproxKey; PartialKey; DefaultSurname ]
      in
      search conf base sn order specify unknown
  | None, None ->
      Hutil.incorrect_request conf ~comment:"Missing first_name and surname"
