(* camlp5r ./pa_html.cmo *)
(* $Id: advSearchOk.ml,v 5.14 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value get_number var key env = p_getint env (var ^ "_" ^ key);

value reconstitute_date_dmy conf var =
  match get_number var "yyyy" conf.env with
  [ Some y ->
      match get_number var "mm" conf.env with
      [ Some m ->
          match get_number var "dd" conf.env with
          [ Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some {day = d; month = m; year = y; prec = Sure; delta = 0}
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some {day = 0; month = m; year = y; prec = Sure; delta = 0}
              else None ]
      | None -> Some {day = 0; month = 0; year = y; prec = Sure; delta = 0} ]
  | None -> None ]
;

value reconstitute_date conf var =
  match reconstitute_date_dmy conf var with
  [ Some d -> Some (Dgreg d Dgregorian)
  | None -> None ]
;

value name_eq x y = Name.abbrev (Name.lower x) = Name.abbrev (Name.lower y);

value rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i = ' ' then skip_spaces x (i + 1)
  else i
;

value rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i
;

value string_incl x y =
  loop 0 where rec loop j_ini =
    if j_ini = String.length y then False
    else
      let rec loop1 i j =
        if i = String.length x then
          if j = String.length y then True
          else (String.unsafe_get y j = ' ' || String.unsafe_get y (j-1) = ' ')
        else if
          j < String.length y &&
          String.unsafe_get x i = String.unsafe_get y j then
          loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
      in
      loop1 0 j_ini
;

value name_incl x y =
  let x = Name.abbrev (Name.lower x) in
  let y = Name.abbrev (Name.lower y) in
  string_incl x y
;

value advanced_search conf base max_answers =
  let hs = Hashtbl.create 73 in
  let hd = Hashtbl.create 73 in
  let gets x =
    try Hashtbl.find hs x with
    [ Not_found ->
        let v =
          match p_getenv conf.env x with
          [ Some v -> v
          | None -> "" ]
        in
        do { Hashtbl.add hs x v; v } ]
  in
  let test x cmp =
    let y = gets x in
    if y = "" then True else cmp y
  in
  let test_auth p x cmp =
    let y = gets x in
    if y = "" then True else if fast_auth_age conf p then cmp y else False
  in
  let test_date p x df =
    let (d1, d2) =
      try Hashtbl.find hd x with
      [ Not_found ->
          let v =
            (reconstitute_date conf (x ^ "1"),
             reconstitute_date conf (x ^ "2"))
          in
          do { Hashtbl.add hd x v; v } ]
    in
    match (d1, d2) with
    [ (Some d1, Some d2) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf p ->
            if CheckItem.strictly_before d d1 then False
            else if CheckItem.strictly_before d2 d then False
            else True
        | _ -> False ]
    | (Some d1, _) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf p ->
            if CheckItem.strictly_before d d1 then False else True
        | _ -> False ]
    | (_, Some d2) ->
        match df () with
        [ Some (Dgreg _ _ as d) when fast_auth_age conf p ->
            if CheckItem.strictly_after d d2 then False else True
        | _ -> False ]
    | _ -> True ]
  in
  let test_marriage p x y =
    let (d1, d2) =
      try Hashtbl.find hd x with
      [ Not_found ->
          let v =
            (reconstitute_date conf (x ^ "1"),
             reconstitute_date conf (x ^ "2"))
          in
          do { Hashtbl.add hd x v; v } ]
    in
    let y = gets y in
    let test_date_place df =
      List.exists
        (fun ifam ->
           let fam = foi base ifam in
           let father = poi base (get_father fam) in
           let mother = poi base (get_mother fam) in
           if fast_auth_age conf father && fast_auth_age conf mother then
             if y = "" then df (Adef.od_of_codate (get_marriage fam))
             else
               name_incl y (sou base (get_marriage_place fam)) &&
                 df (Adef.od_of_codate (get_marriage fam))
           else False)
        (Array.to_list (get_family p))
    in
    match (d1, d2) with
    [ (Some d1, Some d2) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) ->
              if CheckItem.strictly_before d d1 then False
              else if CheckItem.strictly_before d2 d then False
              else True
          | _ -> False ])
    | (Some d1, _) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) when fast_auth_age conf p ->
              if CheckItem.strictly_before d d1 then False else True
          | _ -> False ])
    | (_, Some d2) ->
        test_date_place
          (fun
          [ Some (Dgreg _ _ as d) when fast_auth_age conf p ->
              if CheckItem.strictly_after d d2 then False else True
          | _ -> False ])
    | _ ->
        if y = "" then True
        else
          List.exists
            (fun ifam ->
              let fam = foi base ifam in
              let father = poi base (get_father fam) in
              let mother = poi base (get_mother fam) in
              if fast_auth_age conf father && fast_auth_age conf mother then
                name_incl y (sou base (get_marriage_place fam))
              else False )
            (Array.to_list (get_family p)) ]
  in
  let list = ref [] in
  let len = ref 0 in
  let test_person p =
    if test "sex"
         (fun
          [ "M" -> get_sex p = Male
          | "F" -> get_sex p = Female
          | _ -> True ]) &&
       test_date p "birth" (fun () -> Adef.od_of_codate (get_birth p)) &&
       test_date p "bapt" (fun () -> Adef.od_of_codate (get_baptism p)) &&
       test_auth p "death"
         (fun d ->
            match (d, get_death p) with
            [ ("Dead", NotDead | DontKnowIfDead) -> False
            | ("Dead", _) -> True
            | ("NotDead", NotDead) -> True
            | ("NotDead", _) -> False
            | _ -> True ]) &&
       test_date p "death"
         (fun () ->
            match get_death p with
            [ Death _ cd -> Some (Adef.date_of_cdate cd)
            | _ -> None ]) &&
       test_date p "burial"
         (fun () ->
            match get_burial p with
            [ Buried cod -> Adef.od_of_codate cod
            | Cremated cod -> Adef.od_of_codate cod
            | _ -> None ]) &&
       test "first_name" (fun x -> name_eq x (p_first_name base p)) &&
       test "surname" (fun x -> name_eq x (p_surname base p)) &&
       test "married"
         (fun
          [ "Y" -> get_family p <> [| |]
          | "N" -> get_family p = [| |]
          | _ -> True ]) &&
       test_marriage p "marriage" "marriage_place" &&
       test_auth p "birth_place"
         (fun x -> name_incl x (sou base (get_birth_place p))) &&
       test_auth p "bapt_place"
         (fun x -> name_incl x (sou base (get_baptism_place p))) &&
       test_auth p "death_place"
         (fun x -> name_incl x (sou base (get_death_place p))) &&
       test_auth p "burial_place"
         (fun x -> name_incl x (sou base (get_burial_place p))) &&
       test_auth p "occu"
         (fun x -> name_incl x (sou base (get_occupation p)))
    then do {
      list.val := [p :: list.val]; incr len;
    }
    else ()
  in
  do {
    if gets "first_name" <> "" || gets "surname" <> "" then
      let (slist, _) =
        if gets "first_name" <> "" then
          Some.persons_of_fsname conf base base_strings_of_first_name
            (spi_find (persons_of_first_name base)) get_first_name
            (gets "first_name")
        else
          Some.persons_of_fsname conf base base_strings_of_surname
            (spi_find (persons_of_surname base)) get_surname
            (gets "surname")
      in
      let slist = List.fold_right (fun (_, _, l) sl -> l @ sl) slist [] in
      loop slist where rec loop =
        fun
        [ [] -> ()
        | [ip :: l] ->
            if len.val > max_answers then ()
            else do {
              test_person (pget conf base ip);
              loop l
            } ]
    else
      for i = 0 to nb_of_persons base - 1 do {
        if len.val > max_answers then ()
        else test_person (pget conf base (Adef.iper_of_int i))
      };
    (List.rev list.val, len.val)
  }
;

value print_result conf base max_answers (list, len) =
  let list =
    if len > max_answers then
      Util.reduce_list max_answers list
    else list
  in
  if len = 0 then
    Wserver.wprint "%s\n" (capitale (transl conf "no match"))
  else
    (* Construction de la table des sosa de la base *)
    let () = Perso.build_sosa_ht conf base in
    tag "ul" begin
      List.iter
        (fun p ->
           do {
             html_li conf;
             Perso.print_sosa conf base p True;
             Wserver.wprint "\n%s" (referenced_person_text conf base p);
             Wserver.wprint "%s" (Date.short_dates_text conf base p);
             stag "em" begin
               specify_homonymous conf base p False;
             end;
           })
        list;
      if len > max_answers then do { html_li conf; Wserver.wprint "...\n"; }
      else ();
    end
;

value searching_fields conf base =
  let test_string x =
    match p_getenv conf.env x with
    [ Some v -> if v <> "" then True else False
    | None -> False ]
  in
  let test_date x =
    match
      (reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2"))
    with
    [ (Some d1, Some d2) -> True
    | (Some d1, _) -> True
    | (_, Some d2) -> True
    | _ -> False ]
  in
  let gets x =
    match p_getenv conf.env x with
    [ Some v -> v
    | None -> "" ]
  in
  let getd x =
    (reconstitute_date conf (x ^ "1"), reconstitute_date conf (x ^ "2"))
  in
  let sex =
    match gets "sex" with
    [ "M" -> 0
    | "F" -> 1
    | _ -> 2 ]
  in
  (* Fonction pour tester un simple champ texte (e.g: first_name). *)
  let string_field x search =
    if test_string x then search ^ " " ^ gets x
    else search
  in
  (* Fonction pour tester un "bloc date" (e.g: birth, birth_place). *)
  let date_field x y z search =
    let sep = if search <> "" then ", " else "" in
    let search =
      if test_string x || test_date y then
        search ^ sep ^ (transl_nth conf z sex)
      else search
    in
    let search =
      match getd y with
      [ (Some d1, Some d2) ->
          Printf.sprintf "%s %s %s %s %s"
            search (transl conf "between (date)")
            (Date.string_of_date conf d1)
            (transl conf "and")
            (Date.string_of_date conf d2)
      | (Some d1, _) ->
          Printf.sprintf "%s %s %s"
            search (transl conf "after (date)") (Date.string_of_date conf d1)
      | (_, Some d2) ->
          Printf.sprintf "%s %s %s"
            search (transl conf "before (date)") (Date.string_of_date conf d2)
      | _ -> search ]
    in
    let search =
      if test_string x then
        search ^ " " ^ transl conf "in (place)" ^ " " ^ gets x
      else search
    in
    search
  in
  let search = "" in
  let search = string_field "first_name" search in
  let search = string_field "surname" search in
  let search = date_field "birth_place" "birth" "born" search in
  let search = date_field "bapt_place" "bapt" "baptized" search in
  let search = date_field "marriage_place" "marriage" "married" search in
  let search = date_field "death_place" "death" "died" search in
  let search = date_field "burial_place" "burial" "buried" search in
  (* C'est vraiment pas très heureux ce test... *)
  let search =
    if not (test_string "marriage_place" || test_date "marriage") then
      let sep = if search <> "" then ", " else "" in
      if gets "married" = "Y" then
        search ^ sep ^ transl conf "having a family"
      else if gets "married" = "N" then
        search ^ sep ^ transl conf "having no family"
      else search
    else search
  in
  let search =
    let sep = if search <> "" then "," else "" in
    string_field "occu" (search ^ sep)
  in
  search
;

value print conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "advanced request"))
  in
  let max_answers =
    match p_getint conf.env "max" with
    [ Some n -> n
    | None -> 100 ]
  in
  do {
    header conf title;
    tag "p" begin
      Wserver.wprint "%s: %s."
        (capitale (transl conf "searching all")) (searching_fields conf base);
    end;
    let list = advanced_search conf base max_answers in
    print_result conf base max_answers list;
    trailer conf;
  }
;
