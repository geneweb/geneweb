(* Copyright (c) 1998-2007 INRIA *)

let default_max_cnt = 2000

let max_cousin_level conf base p =
  let default_max_cousin_lvl = 6 in
  let max_lvl =
    try
      Option.value ~default:default_max_cousin_lvl
        (Option.map int_of_string
           (List.assoc_opt "max_cousins_level" conf.Config.base_env))
    with Failure _ -> default_max_cousin_lvl
  in
  Util.max_ancestor_level conf base (Gwdb.get_iper p) max_lvl + 1

let children_of base u =
  Array.fold_right
    (fun ifam list ->
      let des = Gwdb.foi base ifam in
      Array.fold_right List.cons (Gwdb.get_children des) list)
    (Gwdb.get_family u) []

let children_of_fam base ifam =
  Array.to_list (Gwdb.get_children @@ Gwdb.foi base ifam)

let siblings_by conf base iparent ip =
  let list = children_of base (Util.pget conf base iparent) in
  List.filter (( <> ) ip) list

let merge_siblings l1 l2 =
  let l =
    let rec rev_merge r = function
      | [] -> r
      | ((v, _) as x) :: l ->
          rev_merge (if List.mem_assoc v r then r else x :: r) l
    in
    rev_merge (List.rev l1) l2
  in
  List.rev l

let siblings conf base ip =
  match Gwdb.get_parents (Util.pget conf base ip) with
  | None -> []
  | Some ifam ->
      let cpl = Gwdb.foi base ifam in
      let fath_sib =
        List.map
          (fun ip -> (ip, (Gwdb.get_father cpl, Def.Male)))
          (siblings_by conf base (Gwdb.get_father cpl) ip)
      in
      let moth_sib =
        List.map
          (fun ip -> (ip, (Gwdb.get_mother cpl, Def.Female)))
          (siblings_by conf base (Gwdb.get_mother cpl) ip)
      in
      merge_siblings fath_sib moth_sib

let rec has_desc_lev conf base lev u =
  if lev <= 1 then true
  else
    Array.exists
      (fun ifam ->
        let des = Gwdb.foi base ifam in
        Array.exists
          (fun ip -> has_desc_lev conf base (lev - 1) (Util.pget conf base ip))
          (Gwdb.get_children des))
      (Gwdb.get_family u)

let br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1

(* Algorithms *)

let sibling_has_desc_lev conf base lev (ip, _) =
  has_desc_lev conf base lev (Util.pget conf base ip)
