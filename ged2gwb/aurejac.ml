(* $Id: aurejac.ml,v 2.1 1999-03-08 11:17:47 ddr Exp $ *)
(* Copyright (c) INRIA *)
(* Find titles had hoc for genealogy Aurejac *)

value trace = False;

value rec s_skip_spaces s i =
  if i == String.length s then i
  else if s.[i] = ' ' then s_skip_spaces s (i + 1)
  else i
;
value string_sub_unless_dot s ibeg i =
  if i > ibeg + 1 && s.[i - 1] = '.' then
    (String.sub s ibeg (i - ibeg - 1), i - 1)
  else (String.sub s ibeg (i - ibeg), i)
;

value rec s_ident s ibeg i =
  if i == String.length s then
    if i == ibeg then raise Not_found
    else string_sub_unless_dot s ibeg i
  else
    match s.[i] with
    [ 'a'..'z' | 'A'..'Z' | 'À'..'Ö' | 'Ø'..'ö' | 'ø'..'ÿ' | '-' | '.' ->
        s_ident s ibeg (i + 1)
    | ''' -> (String.sub s ibeg (i + 1 - ibeg), i + 1)
    | _ ->
        if i == ibeg then raise Not_found
        else string_sub_unless_dot s ibeg i ]
;  
value rec s_skip_opt_nth s ibeg i =
  if i == String.length s then
    if i == ibeg then i else raise Not_found
  else
    match s.[i] with
    [ '0'..'9' -> s_skip_opt_nth s ibeg (i + 1)
    | _ ->
        if i == ibeg then i
        else
          let (id, i1) = s_ident s i i in
          match id with
          [ "er" | "ème" -> i1
          | _ -> raise Not_found ] ]
;
value s_complement s i id =
  match id with
  [ "de" | "des" | "d'" | "en" | "du" ->
      let i = s_skip_spaces s i in
      let (id2, i) = s_ident s i i in
      let (particle, complement, i) =
        if id = "de" then
          match id2 with
          [ "la" | "La" | "l'" | "L'" ->
              let i = s_skip_spaces s i in
              let (complement, i) = s_ident s i i in
              ("de " ^ id2, complement, i)
         | _ -> (id, id2, i) ]
        else (id, id2, i)
      in
      match complement.[0] with
      [ 'A'..'Z' | 'À'..'Ö' ->
          let complement =
            if particle.[String.length particle - 1] = ''' then
              particle ^ complement
            else particle ^ " " ^ complement
          in
          let (complement, i) =
            if complement.[String.length complement - 1] = ''' then
              let (id, i) = s_ident s i i in
              (complement ^ id, i)
            else (complement, i)
          in
          (complement, i)
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
;

value try_find_title s i =
  let i = s_skip_spaces s i in
  let i = s_skip_opt_nth s i i in
  let i = s_skip_spaces s i in
  let (title, i) =
    let (title, i) = s_ident s i i in
    if title = "premier" || title = "Premier" then
     let i = s_skip_spaces s i in
      s_ident s i i
    else (title, i)
  in
  let i = s_skip_spaces s i in
  let (title, id, i) =
    let (cotitle, i) = s_ident s i i in
    match cotitle with
    [ "de" | "des" | "d'" | "en" | "du" -> (title, cotitle, i)
    | _ ->
        match cotitle.[0] with
        [ 'A'..'Z' | 'À'..'Ö' ->
            let i = s_skip_spaces s i in
            let (id, i) = s_ident s i i in
            (title ^ " " ^ cotitle, id, i)
        | _ -> raise Not_found ] ]
  in
  let (place, i) = s_complement s i id in
  let (place, i) =
    try
      let i = s_skip_spaces s i in
      let (id, i) = s_ident s i i in
      let (comp, i) = s_complement s i id in
      (place ^ " " ^ comp, i)
    with [ Not_found -> (place, i) ]
  in
  let title = String.uncapitalize title in
  do if trace then
       do Printf.printf "%s : %s\n" title place; flush stdout; return ()
     else ();
  return
  (title, place)
;

value start_id =
  ["Profession"; "Abbesse"; "Châtelaine"; "Comtesse"; "Dame"; "Héritière";
   "Reine"; "Vicomtesse"]
;

value find_titles s =
  match try Some (s_ident s 0 0) with [ Not_found -> None ] with
  [ Some (id, i) when List.mem id start_id ->
      do if trace then do Printf.printf "  %s\n" s; flush stdout; return ()
         else ();
      return
      let i = s_skip_spaces s i in
      let i =
        if id = "Profession" then
          if i < String.length s && s.[i] == ':' then s_skip_spaces s (i + 1)
          else i
        else 0
      in
    try [try_find_title s i] with
    [ Not_found ->
        try
          let i = String.index s ',' + 1 in
          [try_find_title s i]
        with
        [ Not_found ->
            if trace then do Printf.printf "failed\n"; flush stdout; return []
            else [] ] ]
  | _ -> [] ]
;
