(* $Id: lextitle.ml,v 3.1 2000-07-17 11:54:28 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Config;

value lextitles = ref None;

value input_item ic =
  loop [] where rec loop item =
    match try Some (input_line ic) with [ End_of_file -> None ] with
    [ Some line ->
        let len = String.length line in
        if len < 5 || line.[2] <> ':' || line.[3] <> ' ' then
          (List.rev item, False)
        else
          let lang = String.sub line 0 2 in
          let transl = String.sub line 4 (len - 4) in
          loop [(lang, transl) :: item]
    | None -> (List.rev item, True) ]
;

value unfreeze_lextitles conf base_dir =
  match lextitles.val with
  [ Some x -> x
  | None ->
      let fname =
        List.fold_right Filename.concat [base_dir; "lang"] "lextitles.txt"
      in
      let lt =
        match try Some (open_in fname) with [ Sys_error _ -> None ] with
        [ Some ic ->
            loop [] where rec loop lt =
              let (item, eof) = input_item ic in
              let lt = if item <> [] then [item :: lt] else lt in
              if eof then do close_in ic; return List.rev lt
              else loop lt
        | None -> [] ]
      in
      do lextitles.val := Some lt; return lt ]
;

value match_beginning x y =
  loop 0 0 where rec loop i j =
    if i == String.length x then None
    else if j == String.length y - 1 then
      Some (String.sub x i (String.length x - i))
    else if x.[i] = y.[j] then loop (i + 1) (j + 1)
    else None
;

value match_transl transl (ide, est) =
  let (ide1, est1) =
    try
      let i = String.index transl ':' in
      (String.sub transl 0 i,
       String.sub transl (i + 1) (String.length transl - i - 1))
    with
    [ Not_found -> (transl, "") ]
  in
  let v1 =
    if ide1 = "" || ide1 = ide then Some ide
    else if ide1.[String.length ide1 - 1] = '*' then
      match match_beginning ide ide1 with
      [ Some v -> Some v
      | None -> None ]
    else None
  in
  let v2 =
    if est1 = "" || est1 = est then Some est
    else if est1.[String.length est1 - 1] = '*' then
      match match_beginning est est1 with
      [ Some v -> Some v
      | None -> None ]
    else None
  in
  match (v1, v2) with
  [ (Some v1, Some v2) -> Some (v1, v2)
  | _ -> None ]
;

value bind_str x y =
  if y = "" then y
  else
    let len = String.length y in
    if y.[len - 1] = '*' then String.sub y 0 (len - 1) ^ x
    else y
;

value bind transl (ide, est) =
  let (ide1, est1) =
    try
      let i = String.index transl ':' in
      (String.sub transl 0 i,
       String.sub transl (i + 1) (String.length transl - i - 1))
    with
    [ Not_found -> (transl, "") ]
  in
  let ide1 = bind_str ide ide1 in
  let est1 = bind_str est est1 in
  let ide1 = if ide1 = "" then None else Some ide1 in
  let est1 = if est1 = "" then None else Some est1 in
  (ide1, est1)
;

value match_item lang (ide, est) item =
  let transl =
    loop item where rec loop =
      fun
      [ [(_, transl) :: item] ->
          match match_transl transl (ide, est) with
          [ Some env -> Some env
          | None -> loop item ]
      | [] -> None ]
  in
  match transl with
  [ Some env ->
      find_lang item where rec find_lang =
        fun
        [ [(lang1, transl1) :: item] ->
            if lang = lang1 || lang1 = "**" then Some (bind transl1 env)
            else find_lang item
        | [] -> None ]
  | None -> None ]
;

value do_translate lang (ide, est) =
  loop where rec loop =
    fun
    [ [item :: item_list] ->
        match match_item lang (ide, est) item with
        [ Some (ide, est) -> (ide, est, item_list)
        | None -> loop item_list ]
    | [] -> (None, None, []) ]
;

value translate conf base_dir (ide, est) =
  let lextitles = unfreeze_lextitles conf base_dir in
  let (ide1, est1, rest) = do_translate conf.lang (ide, est) lextitles in
  let (ide1, est1) =
    match (ide1, est1) with
    [ (Some t, None) ->
        let (_, est1, _) = do_translate conf.lang ("", est) rest in
        (ide1, est1)
    | (None, Some t) ->
        let (ide1, _, _) = do_translate conf.lang (ide, "") rest in
        (ide1, est1)
    | _ -> (ide1, est1) ]
  in
  let ide1 =
    match ide1 with
    [ Some ide1 -> ide1
    | None -> ide ]
  in
  let est1 =
    match est1 with
    [ Some est1 -> est1
    | None -> est ]
  in
  (ide1, est1)
;
