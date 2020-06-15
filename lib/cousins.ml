(* Copyright (c) 1998-2007 INRIA *)

open Def
open Gwdb
open Util

let default_max_cnt = 2000

let children_of base u =
  Array.fold_right
    (fun ifam list ->
       let des = foi base ifam in
       Array.fold_right List.cons (get_children des) list)
    (get_family u) []

let children_of_fam base ifam =
  Array.to_list (get_children @@ foi base ifam)

let siblings_by conf base iparent ip =
  let list = children_of base (pget conf base iparent) in List.filter ((<>) ip) list

let merge_siblings l1 l2 =
  let l =
    let rec rev_merge r =
      function
        (v, _ as x) :: l ->
          rev_merge (if List.mem_assoc v r then r else x :: r) l
      | [] -> r
    in
    rev_merge (List.rev l1) l2
  in
  List.rev l

let siblings conf base ip =
  match get_parents (pget conf base ip) with
    Some ifam ->
      let cpl = foi base ifam in
      let fath_sib =
        List.map (fun ip -> ip, (get_father cpl, Male))
          (siblings_by conf base (get_father cpl) ip)
      in
      let moth_sib =
        List.map (fun ip -> ip, (get_mother cpl, Female))
          (siblings_by conf base (get_mother cpl) ip)
      in
      merge_siblings fath_sib moth_sib
  | None -> []

let rec has_desc_lev conf base lev u =
  if lev <= 1 then true
  else
    Array.exists
      (fun ifam ->
         let des = foi base ifam in
         Array.exists
           (fun ip -> has_desc_lev conf base (lev - 1) (pget conf base ip))
           (get_children des))
      (get_family u)

let br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1

(* Algorithms *)

let sibling_has_desc_lev conf base lev (ip, _) =
  has_desc_lev conf base lev (pget conf base ip)

let sosa_of_persons conf base =
  let rec loop n =
    function
      [] -> n
    | ip :: list ->
        loop (if get_sex (pget conf base ip) = Male then 2 * n else 2 * n + 1)
          list
  in
  loop 1
