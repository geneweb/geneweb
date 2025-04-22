(* Copyright (c) 1998-2007 INRIA *)

let empty_sn_or_fn base p =
  Gwdb.is_empty_string (Gwdb.get_surname p)
  || Gwdb.is_quest_string (Gwdb.get_surname p)
  || Gwdb.is_empty_string (Gwdb.get_first_name p)
  || Gwdb.is_quest_string (Gwdb.get_first_name p)
  || Name.lower (Gwdb.sou base (Gwdb.get_surname p)) = ""
  || Name.lower (Gwdb.sou base (Gwdb.get_first_name p)) = ""

let person_is_misc_name conf base p k =
  let k = Name.strip_lower k in
  List.exists
    (fun n -> Name.strip n = k)
    (Gwdb.person_misc_names base p (Util.nobtit conf base))

let person_is_approx_key base p k =
  let k = Name.strip_lower k in
  let fn = Name.strip_lower (Gwdb.p_first_name base p) in
  let sn = Name.strip_lower (Gwdb.p_surname base p) in
  k = fn ^ sn && fn <> "" && sn <> ""

let select_approx_key conf base pl k =
  List.fold_right
    (fun p pl ->
      if person_is_approx_key base p k then p :: pl
      else if person_is_misc_name conf base p k then p :: pl
      else pl)
    pl []

(* search functions *)

let search_by_sosa ~conf ~base ~sosa =
  if Sosa.eq sosa Sosa.zero then None
  else
    Option.bind (Util.find_sosa_ref conf base) (fun sosa_ref ->
        Util.p_of_sosa conf base sosa sosa_ref)

let search_reject_p conf base p =
  empty_sn_or_fn base p
  || (Util.is_hide_names conf p && not (Person.is_visible conf base p))

let search_by_name conf base n =
  let n1 = Name.abbrev (Name.lower n) in
  match String.index_opt n1 ' ' with
  | Some i ->
      let fn = String.sub n1 0 i in
      let sn = String.sub n1 (i + 1) (String.length n1 - i - 1) in
      let list, _ =
        Search_name_display.persons_of_fsname conf base
          Gwdb.base_strings_of_surname
          (Gwdb.spi_find (Gwdb.persons_of_surname base))
          Gwdb.get_surname sn
      in
      List.fold_left
        (fun pl (_, _, ipl) ->
          List.fold_left
            (fun pl ip ->
              let p = Util.pget conf base ip in
              if search_reject_p conf base p then pl
              else
                let fn1 =
                  Name.abbrev
                    (Name.lower (Gwdb.sou base (Gwdb.get_first_name p)))
                in
                if List.mem fn (Ext_string.cut_words fn1) then p :: pl else pl)
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
    List.filter_map
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
  Option.bind (Gutil.person_of_string_key base an) (fun ip ->
      let p = Util.pget conf base ip in
      if search_reject_p conf base p then None else Some p)

let print_fiche conf base p =
  Util.record_visited conf (Gwdb.get_iper p);
  Perso.print conf base p

let search_sosa conf base s =
  match Sosa.of_string s with
  | Some sosa -> Option.to_list (search_by_sosa ~conf ~base ~sosa)
  | None -> []

let search_key conf base s = Option.to_list (search_by_key conf base s)

let print conf base specify unknown =
  let real_input label =
    match Util.p_getenv conf.Config.env label with
    | Some s -> if s = "" then None else Some s
    | None -> None
  in
  let bind s xs f =
    match xs with
    | [ p ] -> print_fiche conf base p
    | [] -> f ()
    | pl -> specify conf base s pl
  in
  match (real_input "p", real_input "n") with
  | Some fn, Some sn ->
      let s = fn ^ " " ^ sn in
      let ( >>= ) = bind s in
      search_key conf base s >>= fun () ->
      search_approx_key conf base s >>= fun () ->
      search_partial_key conf base s >>= fun () -> unknown conf s
  | Some fn, None ->
      let fres = Search_name_display.search_first_name conf base fn in
      if Search_name_display.fn_search_result_is_empty fres then unknown conf fn
      else Search_name_display.search_first_name_print conf base fres fn
  | None, Some sn ->
      let ( >>= ) = bind sn in
      search_sosa conf base sn >>= fun () ->
      search_key conf base sn >>= fun () ->
      let sres = Search_name_display.search_surname conf base sn in
      if not (Search_name_display.sn_search_result_is_empty sres) then
        Search_name_display.search_surname_print conf base unknown sres sn
      else
        let ( >>= ) = bind sn in
        search_approx_key conf base sn >>= fun () ->
        search_partial_key conf base sn >>= fun () -> unknown conf sn
  | None, None -> Hutil.incorrect_request conf
