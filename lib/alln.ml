(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

let default_max_cnt = 2000

(* selection *)

type t =
  | Result of (string * string * int) list
  | Specify of string list

let first_letters base is_surnames =
  let iii =
    if is_surnames then persons_of_surname base
    else persons_of_first_name base
  in
  try
    let rec loop istr list =
      let s = Translate.eval (Mutil.nominative (sou base istr)) in
      let k = Util.name_key base s in
      let c = Utf8.sub k 0 1 in
      let list =
        match list with
        | hd :: _ -> if hd = c then list else c :: list
        | [] -> [c]
      in
      match spi_next iii istr with
      | istr -> loop istr list
      | exception Not_found -> list
    in loop (spi_first iii "") []
  with Not_found -> []

(** [select_names conf base is_surnames ini limit]
    Select up to [limit] first names/surnames starting with [ini].
    If more values are available, return [Specify] with different
    possible prefixes.
    Otherwise, return the list of values as (key * text * person count).
  *)
let select_names conf base is_surnames ini limit =
  let inilen = Utf8.length ini + 1 in
  let cut k = Utf8.sub k 0 (min (Utf8.length k) inilen) in
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
                let ips = spi_find iii istr in
                let cnt =
                  (* Optimization:
                   * In the case of [Specify _]:
                   * [cnt] is not used except for zero equality test
                   * so we can use List.exists in order to avoid useless operations *)
                  match list with
                  | Specify _ ->
                    if ips = [] then 0
                    else if conf.use_restrict then
                      if List.exists (fun i -> not @@ is_restricted conf base i) ips
                      then 1
                      else 0
                    else if conf.hide_names then
                      if List.exists (fun i -> Util.authorized_age conf base (poi base i)) ips
                      then 1
                      else 0
                    else 1
                  | Result _ ->
                    if conf.use_restrict then
                      List.fold_left begin fun acc i ->
                        if is_restricted conf base i
                        then acc
                        else acc + 1
                      end 0 ips
                      else if conf.hide_names then
                        List.fold_left begin fun acc i ->
                          if Util.authorized_age conf base (poi base i)
                          then acc + 1
                          else acc
                        end 0 ips
                    else List.length ips
                in
                if cnt = 0 then list, len
                else match list with
                  | Result ((k1, s1, cnt1) :: tl) when k = k1 ->
                    Result ((k1, s1, cnt1 + cnt) :: tl), len
                  | Result acc ->
                    if len >= limit
                    then
                      let k = cut k in
                      match
                        List.sort_uniq
                          (fun a b -> compare b a)
                          (List.map (fun (k, _, _) -> cut k) acc)
                      with
                      | (hd :: _) as acc when hd = k -> Specify acc, len + 1
                      | acc -> Specify (k :: acc), len + 1
                    else Result ((k, s, cnt) :: acc), len + 1
                  | Specify (k1 :: tl) ->
                    let k = cut k in
                    (if k = k1 then list else Specify (k :: k1 :: tl)), len + 1
                  | Specify [] -> Specify [cut k], 1
              else list, len
            in
            match spi_next iii istr with
            | istr -> loop istr len list
            | exception Not_found -> list, len
          else list, len
        in
        loop istr 0 (Result [])
    with Not_found -> (Result []), 0
  in
  let (list, len) =
    match list with
    | Specify _ -> list, len
    | Result acc ->
      match p_getint conf.env "atleast" with
      | None -> list, len
      | Some min ->
        let acc, len =
          List.fold_left begin fun (list, len) (k, s, cnt) ->
            if cnt >= min then (k, s, cnt) :: list, len else list, len - 1
          end ([], len) acc
        in Result acc, len
  in
  list, len
