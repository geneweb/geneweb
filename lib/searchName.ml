(* Copyright (c) 1998-2007 INRIA *)

open Config
open Util
module Sosa = Geneweb_sosa
module Driver = Geneweb_db.Driver
module Gutil = Geneweb_db.Gutil

(* TODO use function from Util instead? *)
let empty_sn_or_fn base p =
  Driver.Istr.is_empty (Driver.get_surname p)
  || Driver.Istr.is_quest (Driver.get_surname p)
  || Driver.Istr.is_empty (Driver.get_first_name p)
  || Driver.Istr.is_quest (Driver.get_first_name p)
  || Name.lower (Driver.sou base (Driver.get_surname p)) = ""
  || Name.lower (Driver.sou base (Driver.get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  if
    List.exists
      (fun n -> Name.strip n = k)
      (Driver.person_misc_names base p (nobtit conf base))
  then true
  else false

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Driver.p_first_name base p) in
  let sn = Name.strip_lower (Driver.p_surname base p) in
  if k = fn ^ sn && fn <> "" && sn <> "" then true else false

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      if person_is_approx_key base p k then p :: pl
      else if person_is_misc_name conf base p k then p :: pl
      else pl)
    pl []

let split_normalize case s =
  let s = Name.abbrev s in
  let s = if case then s else Name.lower s in
  cut_words s

(* search functions *)

let search_by_sosa conf base an =
  let sosa_ref = Util.find_sosa_ref conf base in
  let sosa_nb = try Some (Sosa.of_string an) with _ -> None in
  match (sosa_ref, sosa_nb) with
  | None, _ | _, None -> None
  | Some p, Some n when n <> Sosa.zero -> (
      match
        Util.branch_of_sosa conf base n (pget conf base @@ Driver.get_iper p)
      with
      | Some (p :: _) -> Some p
      | _ -> None)
  | _ -> None

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Util.authorized_age conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index n1 ' ' with
  | exception Not_found -> []
  | i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let p_of_sn_l, _ =
        Some.persons_of_fsname conf base Driver.base_strings_of_surname
          (Driver.spi_find (Driver.persons_of_surname base))
          Driver.get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              match Util.pget_opt conf base ip with
              | None -> pl
              | Some p ->
                  let fn1_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_first_name p))
                  in
                  let fn2_l =
                    split_normalize true
                      (Driver.sou base (Driver.get_public_name p))
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

(* FIXME this set of options needs deeper review
   for semantic and implementation *)
type opts = {
  order : bool; (* first_names should be in same order as typed *)
  all : bool; (* all first_names typed should be present *)
  case : bool; (* maintain case and accents when comparing *)
  exact : bool; (* fuzzy match (for the time being starts_with *)
  all_in : bool; (* all first_names should match one of the typed first_names *)
}

let match_fn_lists fn_l fn1_l opts =
  let lower fn_l =
    List.map (fun fn -> if opts.case then fn else Name.lower fn) fn_l
  in
  let equal fn1 fn2 =
    if opts.exact then String.compare fn1 fn2 = 0 else Mutil.contains fn2 fn1
  in
  let _list_equal equal fn1_l fn2_l =
    try List.length fn1_l = List.length fn2_l && List.for_all2 equal fn1_l fn2_l
    with Invalid_argument _ -> false
  in
  let rec is_subsequence l1 l2 =
    match (l1, l2) with
    | [], _ -> true
    | _, [] -> false
    | x :: xs, y :: ys ->
        if equal x y then is_subsequence xs ys else is_subsequence l1 ys
  in
  let fn_l = lower fn_l in
  let fn1_l = lower fn1_l in
  match (fn_l, opts.all, opts.order) with
  | [], _, _ -> true
  | [ fn ], _, _ when List.compare_length_with fn1_l 1 = 0 ->
      equal fn (List.hd fn1_l)
  | _, true, true -> is_subsequence fn_l fn1_l
  | _, true, false -> List.for_all (fun fn -> List.mem fn fn1_l) fn_l
  | _, false, _ ->
      List.exists (fun fn -> List.exists (fun fn1 -> equal fn fn1) fn1_l) fn_l

let search_for_multiple_fn conf base fn pl opts =
  (* Check if l1 is a contiguous sublist of l2 *)
  let fn_l = cut_words fn in
  List.fold_left
    (fun pl p ->
      if search_reject_p conf base p then pl
      else
        let fn1_l =
          Driver.get_first_name p |> Driver.sou base
          |> split_normalize opts.case
        in
        let fn2_l =
          Driver.get_public_name p |> Driver.sou base
          |> split_normalize opts.case
        in
        if match_fn_lists fn_l fn1_l opts || match_fn_lists fn_l fn2_l opts then
          p :: pl
        else pl)
    [] pl

let search conf base an search_order specify unknown =
  let test label = p_getenv conf.env label = Some "on" in
  let test_not label = p_getenv conf.env label = Some "off" in
  let opts =
    {
      exact = not (test_not "p_exact");
      case = test "p_case";
      order = test "p_order";
      all = not (test_not "p_all");
      all_in = false;
    }
  in
  let rec loop l =
    match l with
    | [] -> SrcfileDisplay.print_welcome conf base
    | Sosa :: l -> (
        match search_by_sosa conf base an with
        | None -> loop l
        | Some p ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p)
    | Key :: l -> (
        match search_by_key conf base an with
        | None -> loop l
        | Some p ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p)
    | Surname :: l -> (
        let pl = Some.search_surname conf base an in
        match pl with
        | [] -> loop l
        | _ -> Some.search_surname_print conf base unknown an)
    | FirstName :: l -> (
        let fn_l = cut_words an in
        let save_env = conf.env in
        (* was let _pl = Some.search_first_name conf base an in *)
        let conf =
          {
            conf with
            env =
              ("first_name", Adef.encoded an)
              :: ("exact_first_name", Adef.encoded "on")
              :: save_env;
          }
        in
        (* find all bearers of sn with all exact = "on" fn using advanced_search *)
        let pl1, _len = AdvSearchOk.advanced_search conf base max_int in
        (* filter out with match_fn_list *)
        let pl1 =
          List.fold_left
            (fun acc p ->
              let fn1 = Driver.sou base (Driver.get_first_name p) in
              let fn1_l = cut_words fn1 in
              if fn1 = "" then acc
              else if match_fn_lists fn_l fn1_l opts then p :: acc
              else acc)
            [] pl1
        in
        let conf =
          {
            conf with
            env =
              ("first_name", Adef.encoded an)
              :: ("exact_first_name", Adef.encoded "off")
              :: save_env;
          }
        in
        (* find additional bearers of sn with with exact = "off" fn using advanced_search *)
        let pl1_ht = Hashtbl.create 40 in
        List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
        let pl2, _len = AdvSearchOk.advanced_search conf base max_int in
        (* filter out with match_fn_lists *)
        let pl2 =
          List.fold_left
            (fun acc p ->
              let fn1 = Driver.sou base (Driver.get_first_name p) in
              let fn1_l = cut_words fn1 in
              if fn1 = "" then acc
              else if match_fn_lists fn_l fn1_l opts then p :: acc
              else acc)
            [] pl2
        in
        (* remove from pl2 persons already in pl1 *)
        let pl2 =
          List.fold_left
            (fun acc p ->
              if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
            [] pl2
        in
        (* split pl1 into exact matches (pl1) and partial match (pl3) *)
        let pl1, pl3 =
          List.fold_left
            (fun (acc1, acc3) p ->
              let fn1 = Driver.sou base (Driver.get_public_name p) in
              let fn1_l = cut_words fn1 in
              if fn1 = "" then (p :: acc1, acc3)
              else if match_fn_lists fn_l fn1_l opts then (acc1, p :: acc3)
              else (p :: acc1, acc3))
            ([], []) pl1
        in
        let pl1_ht = Hashtbl.create 40 in
        List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
        (* remove from pl2 persons already in pl1 *)
        let pl2 =
          List.fold_left
            (fun acc p ->
              if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
            [] pl2
        in
        (* remove from pl3 persons already in pl1 *)
        let pl3 =
          List.fold_left
            (fun acc p ->
              if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc else p :: acc)
            [] pl3
        in
        let pl2_ht = Hashtbl.create 40 in
        List.iter (fun p -> Hashtbl.add pl2_ht (Driver.get_iper p) "") pl2;
        (* remove from pl3 persons already in pl2 *)
        let pl3 =
          List.fold_left
            (fun acc p ->
              if Hashtbl.mem pl2_ht (Driver.get_iper p) then acc else p :: acc)
            [] pl3
        in
        match (pl1, pl2, pl3) with
        | [], [], [] -> loop l
        | [ p ], [], [] | [], [ p ], [] | [], [], [ p ] ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p
        | _ ->
            let str = Mutil.StrSet.empty in
            let str = Mutil.StrSet.add an str in
            let tit2 =
              transl conf "other possibilities" |> Utf8.capitalize_fst
            in
            let tit3 = transl conf "with spouse name" |> Utf8.capitalize_fst in
            Some.first_name_print_list conf base an str
              [ ("", pl1); (tit2, pl2); (tit3, pl3) ]
        (*specify conf base an pl1 (pl2 @ pl3) []*))
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
            let an = Name.lower an in
            if sn = "" then
              match String.rindex_opt an ' ' with
              | Some i ->
                  ( String.sub an 0 i,
                    String.sub an (i + 1) (String.length an - i - 1) )
              | _ -> ("", an)
            else (fn, sn)
          else (fn, sn)
        in
        let conf =
          {
            conf with
            env =
              ("surname", Adef.encoded sn)
              :: ("exact_surname", Adef.encoded "on")
              :: conf.env;
          }
        in
        (* find all bearers of sn using advanced_search *)
        let list, _len = AdvSearchOk.advanced_search conf base max_int in
        match list with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p
        | pl -> (
            (* check first_names or public_names in list of persons *)
            let opts1 = { opts with all_in = true } in
            let pl1 = search_for_multiple_fn conf base fn pl opts1 in
            let opts2 = { opts with all_in = false } in
            let pl2 = search_for_multiple_fn conf base fn pl opts2 in
            let pl1_ht = Hashtbl.create 40 in
            List.iter (fun p -> Hashtbl.add pl1_ht (Driver.get_iper p) "") pl1;
            let pl2 =
              List.fold_left
                (fun acc p ->
                  if Hashtbl.mem pl1_ht (Driver.get_iper p) then acc
                  else p :: acc)
                [] pl2
            in
            let get_spouse iper ifam =
              let f = Driver.foi base ifam in
              if iper = Driver.get_father f then
                Driver.poi base (Driver.get_mother f)
              else Driver.poi base (Driver.get_father f)
            in
            (* find bearers of surname *)
            let find_pl3 =
              not (List.assoc_opt "public_name_as_fn" conf.base_env = Some "no")
            in
            let pl3 =
              if find_pl3 then Some.search_surname conf base sn else []
            in
            let pl3 =
              List.fold_left
                (fun acc ip ->
                  Array.fold_left
                    (fun acc ifam -> get_spouse ip ifam :: acc)
                    acc
                    (Driver.get_family (Driver.poi base ip)))
                [] pl3
            in
            let pl3 = search_for_multiple_fn conf base fn pl3 opts in
            match (pl1, pl2, pl3) with
            | [], [], [] -> loop l
            | [ p ], [], [] | [], [ p ], [] | [], [], [ p ] ->
                record_visited conf (Driver.get_iper p);
                Perso.print conf base p
            | _ -> specify conf base an pl1 pl2 pl3))
    | ApproxKey :: l -> (
        let pl = search_approx_key conf base an in
        match pl with
        | [] -> loop l
        | [ p ] ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl [] [])
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
                record_visited conf (Driver.get_iper p);
                Perso.print conf base p
            | pl -> (
                let opts1 = { opts with all_in = true } in
                let pl1 = search_for_multiple_fn conf base fn pl opts1 in
                let opts2 = { opts with all_in = false } in
                let pl2 = search_for_multiple_fn conf base fn pl opts2 in
                let pl2 =
                  List.fold_left
                    (fun acc p -> if List.mem p pl1 then acc else p :: acc)
                    [] pl2
                in
                match pl1 with
                | [] -> loop l
                | [ p ] ->
                    record_visited conf (Driver.get_iper p);
                    Perso.print conf base p
                | pl1 -> specify conf base an pl1 pl2 []))
        | [ p ] ->
            record_visited conf (Driver.get_iper p);
            Perso.print conf base p
        | pl -> specify conf base an pl [] [])
    | DefaultSurname :: _ -> Some.search_surname_print conf base unknown an
  in
  loop search_order

(* ************************************************************************ *)
(*  [Fonc] print : conf -> string -> unit                                   *)

(* ************************************************************************ *)

(** [Description] : Recherche qui n'utilise que 2 inputs. On essai donc de
    trouver la meilleure combinaison de résultat pour afficher la réponse la
    plus probable. [Args] :
    - conf : configuration de la base
    - base : base [Retour] : Néant [Rem] : Exporté en clair hors de ce module.
*)
let print conf base specify unknown =
  let real_input label =
    match p_getenv conf.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  match (real_input "pn", real_input "p", real_input "n") with
  | None, Some fn, Some sn ->
      let order = [ Key; FullName; ApproxKey; PartialKey; DefaultSurname ] in
      search conf base (fn ^ " " ^ sn) order specify unknown
  | None, Some fn, None ->
      let fn =
        match String.rindex_opt fn '.' with
        | Some i -> String.sub fn 0 i
        | None -> fn
      in
      let order = [ FirstName ] in
      search conf base fn order specify unknown
  | Some pn, None, None -> (
      let order =
        [ Sosa; Key; FullName; ApproxKey; PartialKey; DefaultSurname ]
      in
      let i = try String.index pn '/' with Not_found -> -1 in
      if i = -1 then search conf base pn order specify unknown
      else
        let j = try String.index_from pn (i + 1) '/' with Not_found -> -1 in
        let oc =
          if j = -1 then "" else String.sub pn (j + 1) (String.length pn - j - 1)
        in
        let j = if j = -1 then 0 else String.length pn - j in
        let fn = String.sub pn 0 i |> String.trim in
        let sn =
          String.sub pn (i + 1) (String.length pn - i - 1 - j) |> String.trim
        in
        match (fn, sn, oc) with
        | fn, "", "" when fn <> "" ->
            let order = [ FirstName ] in
            search conf base fn order specify unknown
        | "", sn, "" when sn <> "" ->
            let order = [ Surname; ApproxKey; DefaultSurname ] in
            search conf base sn order specify unknown
        | _ ->
            let order =
              [ Sosa; Key; FullName; ApproxKey; PartialKey; DefaultSurname ]
            in
            let env =
              List.map
                (fun (k, v) ->
                  match k with
                  | "p" -> (k, Adef.encoded fn)
                  | "n" -> (k, Adef.encoded sn)
                  | "oc" -> (k, Adef.encoded oc)
                  | _ -> (k, v))
                conf.env
            in
            let env =
              if List.mem_assoc "oc" env && oc <> "" then env
              else ("oc", Adef.encoded oc) :: env
            in
            let conf = { conf with env } in
            search conf base
              (Printf.sprintf "%s%s %s" (String.sub pn 0 i)
                 (if oc = "" then "" else "." ^ oc)
                 (String.sub pn (i + 1) (String.length pn - i - 1 - j)))
              order specify unknown)
  | None, None, Some sn ->
      let order = [ Surname; ApproxKey; DefaultSurname ] in
      search conf base sn order specify unknown
  | _ -> SrcfileDisplay.print_welcome conf base
