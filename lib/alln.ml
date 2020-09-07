(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let default_max_cnt = 2000

(* selection *)

let select_names conf base is_surnames ini _need_whole_list =
  let iii =
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  let (list, len) =
    let start_k = Mutil.tr '_' ' ' ini in
    try
      let istr = spi_first iii start_k in
        let rec loop istr len list =
          let s = Translate.eval (Mutil.nominative (sou base istr)) in
          let k = Util.name_key base s in
          if Mutil.start_with ~wildcard:true ini 0 k then
            let (list, len) =
              if s <> "?" then
                let my_list = spi_find iii istr in
                let my_list =
                  if conf.use_restrict then
                    List.fold_left begin fun l ip ->
                      if is_restricted conf base ip then l else ip :: l
                    end [] my_list
                  else if conf.hide_names then
                    List.fold_left begin fun l ip ->
                      if get_access (poi base ip) = Def.Private
                      then l
                      else ip :: l
                    end [] my_list
                  else my_list
                in
                let cnt = List.length my_list in
                if cnt = 0 then list, len
                else
                  match list with
                  | (k1, s1, cnt1) :: list1 ->
                    if k = k1 then (k1, s1, cnt1 + cnt) :: list1, len
                    else (k, s, cnt) :: list, (len + 1)
                  | [] -> [k, s, cnt], (len + 1)
              else list, len
            in
            match spi_next iii istr with
            | istr -> loop istr len list
            | exception Not_found -> list, len
          else list, len
        in
        loop istr 0 []
    with Not_found -> [], 0
  in
  let (list, len) =
    let lim =
      match p_getint conf.env "atleast" with
        Some x -> x
      | None -> 0
    in
    List.fold_left
      (fun (list, len) (k, s, cnt) ->
         if cnt >= lim then (k, s, cnt) :: list, len else list, len - 1)
      ([], len) list
  in
  list, len
