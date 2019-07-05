(* $Id: searchName.ml,v 1.00 2014-07-08 11:26:10 flh Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

(* tools *)

(*
value gen_person_is_std_key misc conf base p k =
  let k = Name.strip_lower k in
  if k = Name.strip_lower (p_first_name base p ^ " " ^ p_surname base p) then
    True
  else if
    misc &&
    List.exists (fun n -> Name.strip n = k)
      (person_misc_names base p (nobtit conf base))
  then
    True
  else False
;

value person_is_std_key_approx = gen_person_is_std_key True;
value person_is_std_key_exact = gen_person_is_std_key False;

value select_std_eq conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_std_key_exact conf base p k then [p :: pl] else pl)
    pl []
;

value select_std_approx conf base pl k =
  List.fold_right
    (fun p pl ->
       if person_is_std_key_approx conf base p k then [p :: pl] else pl)
    pl []
;
*)

let empty_surname_or_firsntame base p =
  is_empty_string (get_surname p) || is_quest_string (get_surname p) ||
  is_empty_string (get_first_name p) || is_quest_string (get_first_name p) ||
  Name.lower (sou base (get_surname p)) = "" ||
  Name.lower (sou base (get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if List.exists (fun n -> Name.strip n = k)
       (person_misc_names base p (nobtit conf base))
  then
    true
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

let cut_words str =
  let rec loop beg i =
    if i < String.length str then
      match str.[i] with
        ' ' ->
          if beg = i then loop (succ beg) (succ i)
          else String.sub str beg (i - beg) :: loop (succ i) (succ i)
      | _ -> loop beg (succ i)
    else if beg = i then []
    else [String.sub str beg (i - beg)]
  in
  loop 0 0

let try_find_with_one_first_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
    Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let (list, _) =
        Some.persons_of_fsname conf base base_strings_of_surname
          (spi_find (persons_of_surname base)) get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
           List.fold_left
             (fun pl ip ->
                let p = pget conf base ip in
                let fn1 =
                  Name.abbrev (Name.lower (sou base (get_first_name p)))
                in
                if List.mem fn (cut_words fn1) then p :: pl else pl)
             pl ipl)
        [] list
  | None -> []

let compact_list base xl =
  let pl = Gutil.sort_person_list base xl in
  List.fold_right
    (fun p pl ->
       match pl with
         p1 :: _ when get_iper p = get_iper p1 -> pl
       | _ -> p :: pl)
    pl []

let name_with_roman_number str =
  let rec loop found len i =
    if i = String.length str then if found then Some (Buff.get len) else None
    else
      match str.[i] with
        '0'..'9' as c ->
          let (n, i) =
            let rec loop n i =
              if i = String.length str then n, i
              else
                match str.[i] with
                  '0'..'9' as c ->
                    loop (10 * n + Char.code c - Char.code '0') (i + 1)
                | _ -> n, i
            in
            loop (Char.code c - Char.code '0') (i + 1)
          in
          loop true (Buff.mstore len (Mutil.roman_of_arabian n)) i
      | c -> loop found (Buff.store len c) (i + 1)
  in
  loop false 0 0

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match sosa_ref, sosa_nb with
    Some p, Some n ->
      if n <> Sosa.zero then
        match Util.branch_of_sosa conf base n (pget conf base @@ get_iper p) with
          Some (p :: _) -> [p]
        | _ -> []
      else []
  | _ -> []

(*
value gen_search_approx_key std_key conf base an =
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
      [ Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then (an, []) else (an1, ipl)
      | None -> (an, ipl) ]
    else (an, ipl)
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in
         if is_hidden p then l else [p :: l])
    [] ipl
  in
  let spl =
    if std_key then select_std_approx conf base pl an
    else select_std_eq conf base pl an
  in
  let pl =
    if std_key then
      if spl = [] then
        if pl = [] then try_find_with_one_first_name conf base an else pl
      else spl
    else spl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then [p :: pl]
           else pl)
        pl []
    else pl
  in
  compact_list conf base pl
;
*)

let search_partial_key conf base an =
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  let pl =
    if pl = [] then try_find_with_one_first_name conf base an else pl
  in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  compact_list base pl

let search_approx_key conf base an =
  let ipl = Gutil.person_not_a_key_find_all base an in
  let (an, ipl) =
    if ipl = [] then
      match name_with_roman_number an with
        Some an1 ->
          let ipl = Gutil.person_ht_find_all base an1 in
          if ipl = [] then an, [] else an1, ipl
      | None -> an, ipl
    else an, ipl
  in
  let pl =
    List.fold_left
      (fun l ip ->
         let p = pget conf base ip in if is_hidden p then l else p :: l)
      [] ipl
  in
  let pl = select_approx_key conf base pl an in
  let pl =
    if not conf.wizard && not conf.friend then
      List.fold_right
        (fun p pl ->
           if not (is_hide_names conf p) || Util.authorized_age conf base p
           then
             p :: pl
           else pl)
        pl []
    else pl
  in
  let pl =
    List.fold_right
      (fun p pl -> if empty_surname_or_firsntame base p then pl else p :: pl)
      pl []
  in
  compact_list base pl

(* recherche par clé, i.e. prenom.occ nom *)
let search_by_key conf base an =
  match Gutil.person_of_string_key base an with
    Some ip ->
      let pl = let p = pget conf base ip in if is_hidden p then [] else [p] in
      if not conf.wizard && not conf.friend then
        List.fold_right
          (fun p pl ->
             if not (is_hide_names conf p) || Util.authorized_age conf base p
             then
               p :: pl
             else pl)
          pl []
      else pl
  | None -> []

(* main *)

type search_type =
    Sosa
  | Key
  | Surname
  | FirstName
  | ApproxKey
  | PartialKey
  | DefaultSurname

let search conf base an search_order specify unknown =
  let rec loop l =
    match l with
      [] -> unknown conf an
    | Sosa :: l ->
        let pl = search_by_sosa conf base an in
        begin match pl with
          [p] ->
            record_visited conf (get_iper p); Perso.print conf base p
        | _ -> loop l
        end
    | Key :: l ->
        let pl = search_by_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); Perso.print conf base p
        | pl -> specify conf base an pl
        end
    | Surname :: l ->
        let pl = Some.search_surname conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            conf.cancel_links <- false;
            Some.search_surname_print conf base unknown an
        end
    | FirstName :: l ->
        let pl = Some.search_first_name conf base an in
        begin match pl with
          [] -> loop l
        | _ ->
            conf.cancel_links <- false;
            Some.search_first_name_print conf base an
        end
    | ApproxKey :: l ->
        let pl = search_approx_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); Perso.print conf base p
        | pl -> specify conf base an pl
        end
    | PartialKey :: l ->
        let pl = search_partial_key conf base an in
        begin match pl with
          [] -> loop l
        | [p] ->
            record_visited conf (get_iper p); Perso.print conf base p
        | pl -> specify conf base an pl
        end
    | DefaultSurname :: _ ->
        conf.cancel_links <- false;
        Some.search_surname_print conf base unknown an
  in
  loop search_order


(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)
(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
      trouver la meilleure combinaison de résultat pour afficher la réponse
      la plus probable.
    [Args] :
      - conf : configuration de la base
      - base : base
    [Retour] : Néant
    [Rem] : Exporté en clair hors de ce module.                             *)
(* ************************************************************************ *)
let print conf base specify unknown =
  let real_input label =
    match p_getenv conf.env label with
      Some s -> if s = "" then None else Some s
    | None -> None
  in
  match real_input "p", real_input "n" with
    Some fn, Some sn ->
      let order = [Key; ApproxKey; PartialKey] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | Some fn, None ->
      let order = [FirstName] in search conf base fn order specify unknown
  | None, Some sn ->
      let order =
        [Sosa; Key; Surname; ApproxKey; PartialKey; DefaultSurname]
      in
      search conf base sn order specify unknown
  | None, None -> Hutil.incorrect_request conf
