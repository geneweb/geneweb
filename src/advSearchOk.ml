(* camlp4r ./pa_html.cmo ./def.syn.cmo *)
(* $Id: advSearchOk.ml,v 2.1 1999-03-08 11:18:19 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

value get_number var key env = p_getint env (var ^ "_" ^ key);

value reconstitute_date conf var =
  match get_number var "yyyy" conf.env with
  [ Some y ->
      match get_number var "mm" conf.env with
      [ Some m ->
          match get_number var "dd" conf.env with
          [ Some d ->
              if d >= 1 && d <= 31 && m >= 1 && m <= 12 then
                Some {day = d; month = m; year = y; prec = Sure}
              else None
          | None ->
              if m >= 1 && m <= 12 then
                Some {day = 0; month = m; year = y; prec = Sure}
              else None ]
      | None ->
          Some {day = 0; month = 0; year = y; prec = Sure} ]
  | None -> None ]
;

value name_eq x y =
  Name.abbrev (Name.lower x) = Name.abbrev (Name.lower y)
;

value rec skip_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i == ' ' then skip_spaces x (i + 1)
  else i
;

value rec skip_no_spaces x i =
  if i = String.length x then i
  else if String.unsafe_get x i != ' ' then skip_no_spaces x (i + 1)
  else i
;

value string_incl x y =
  loop 0 where rec loop j_ini =
    if j_ini == String.length y then False
    else
      loop1 0 j_ini where rec loop1 i j =
        if i == String.length x then
          if j == String.length y then True
          else String.unsafe_get y j == ' '
        else if
          j < String.length y &&
          String.unsafe_get x i == String.unsafe_get y j
        then loop1 (i + 1) (j + 1)
        else loop (skip_spaces y (skip_no_spaces y j_ini))
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
        do Hashtbl.add hs x v; return v ]
  in
  let test x cmp =
    let y = gets x in
    if y = "" then True else cmp y
  in
  let test_auth p x cmp =
    let y = gets x in
    if y = "" then True
    else if age_autorise conf base p then cmp y
    else False
  in
  let test_date p x df =
    let (d1, d2) =
      try Hashtbl.find hd x with
      [ Not_found ->
          let v =
            (reconstitute_date conf (x ^ "1"),
             reconstitute_date conf (x ^ "2"))
          in
          do Hashtbl.add hd x v; return v ]
    in
    match (d1, d2) with
    [ (Some d1, Some d2) ->
        match df () with
        [ Some d when age_autorise conf base p ->
            if d strictement_avant d1 then False
            else if d strictement_apres d2 then False
            else True
        | _ -> False ]
    | (Some d1, _) ->
        match df () with
        [ Some d when age_autorise conf base p ->
            if d strictement_avant d1 then False else True
        | _ -> False ]
    | (_, Some d2) ->
        match df () with
        [ Some d when age_autorise conf base p ->
            if d strictement_apres d2 then False else True
        | _ -> False ]
    | _ -> True ]
  in
  let list = ref [] in
  let len = ref 0 in
  let test_person p =
    if test "first_name" (fun x -> name_eq x (sou base p.first_name))
    && test "surname" (fun x -> name_eq x (sou base p.surname))
    && test "sex"
         (fun              
          [ "M" -> p.sex = Masculine
          | "F" -> p.sex = Feminine
          | _ -> True ])
    && test "married"
         (fun              
          [ "Y" -> p.family <> [| |]
          | "N" -> p.family = [| |]
          | _ -> True ])
    && test_auth p "birth_place"
         (fun x -> name_incl x (sou base p.birth_place))
    && test_date p "birth" (fun () -> Adef.od_of_codate p.birth)
    && test_auth p "baptism_place"
         (fun x -> name_incl x (sou base p.baptism_place))
    && test_date p "baptism" (fun () -> Adef.od_of_codate p.baptism)
    && test_auth p "death"
         (fun d ->
            match (d, p.death) with
            [ ("Dead", NotDead | DontKnowIfDead) -> False
            | ("Dead", _) -> True
            | ("NotDead", NotDead) -> True
            | ("NotDead", _) -> False
            | _ -> True ])
    && test_auth p "death_place"
         (fun x -> name_incl x (sou base p.death_place))
    && test_date p "death"
         (fun () ->
            match p.death with
            [ Death _ cd -> Some (Adef.date_of_cdate cd)
            | _ -> None ])
    && test_auth p "burial_place"
          (fun x -> name_incl x (sou base p.burial_place))
    && test_date p "burial"
         (fun () ->
            match p.burial with
            [ Buried cod -> Adef.od_of_codate cod
            | Cremated cod -> Adef.od_of_codate cod
            | _ -> None ])
    && test_auth p "occu" (fun x -> name_incl x (sou base p.occupation))
    then
      do list.val := [p :: list.val];
         incr len;
      return ()
    else ()
  in
  do if gets "first_name" <> "" || gets "surname" <> "" then
       let (slist, _) =
         if gets "first_name" <> "" then
           Some.persons_of_fsname base base.func.persons_of_first_name.find
             (fun x -> x.first_name) (gets "first_name")
         else
           Some.persons_of_fsname base base.func.persons_of_surname.find
             (fun x -> x.surname) (gets "surname")
       in
       let slist = List.fold_right (fun (_, _, l) sl -> l @ sl) slist [] in
       List.iter (fun ip -> test_person (poi base ip)) slist
     else
       for i = 0 to base.data.persons.len - 1 do
         if len.val > max_answers then ()
         else test_person (base.data.persons.get i);
       done;
  return (List.rev list.val, len.val)
;

value print_result conf base max_answers (list, len) =
  if len > max_answers then
    do Wserver.wprint (fcapitale (ftransl conf "more than %d answers"))
         max_answers;
       Wserver.wprint "\n";
       html_p conf;
    return ()
  else if len == 0 then
    Wserver.wprint "%s\n" (capitale (transl conf "no match"))
  else
    tag "ul" begin
      List.iter
        (fun p ->
           do html_li conf;
              afficher_personne_referencee conf base p;
              Date.afficher_dates_courtes conf base p;
           return ())
        list;
      if len > max_answers then
        do html_li conf; Wserver.wprint "...\n"; return ()
      else ();
    end
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
  do header conf title;
     let list = advanced_search conf base max_answers in
     print_result conf base max_answers list;
     trailer conf;
  return ()
;
