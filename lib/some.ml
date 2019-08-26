(* Copyright (c) 1998-2007 INRIA *)

open Config
open Gwdb
open Util

module StrSet = Mutil.StrSet

let persons_of_fsname base base_strings_of_fsname find x =
  (* list of strings index corresponding to the crushed lower first name
     or surname "x" *)
  let istrl = base_strings_of_fsname base x in
  (* selecting the persons who have this first name or surname *)
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
         let str = Mutil.nominative (sou base istr) in
         if Name.crush_lower str = x
         then match find istr with [] -> l | iperl -> (str, istr, iperl) :: l
         else l)
      istrl []
  in
  let (l, name_inj) =
    let (l1, name_inj) =
      let x = Name.lower x in
      List.fold_right
        (fun (str, istr, iperl) l ->
           if x = Name.lower str
           then (str, istr, iperl) :: l
           else l)
        l [],
      Name.lower
    in
    let (l1, name_inj) =
      if l1 = [] then
        let x = Name.strip_lower x in
        List.fold_right
          (fun (str, istr, iperl) l ->
             if x = Name.strip_lower str then (str, istr, iperl) :: l else l)
          l [],
        Name.strip_lower
      else l1, name_inj
    in
    if l1 = [] then l, Name.crush_lower else l1, name_inj
  in
  l, name_inj

let rec merge_insert (sstr, (strl, iperl) as x) =
  function
    (sstr1, (strl1, iperl1) as y) :: l ->
      if sstr < sstr1 then x :: y :: l
      else if sstr > sstr1 then y :: merge_insert x l
      else (sstr, (StrSet.union strl strl1, iperl @ iperl1)) :: l
  | [] -> [x]

let persons_of_absolute_name_aux conf base base_strings_of persons_of get_field x =
  let istrl = base_strings_of base x in
  List.fold_right
    (fun istr l ->
       let str = sou base istr in
       if str = x then
         let iperl = spi_find (persons_of base) istr in
         let iperl =
           List.fold_left
             (fun iperl iper ->
                if eq_istr (get_field (pget conf base iper)) istr then
                  iper :: iperl
                else iperl)
             [] iperl
         in
         if iperl = [] then l else (str, istr, iperl) :: l
       else l)
    istrl []

let persons_of_absolute_first_name conf base x =
  persons_of_absolute_name_aux
    conf
    base
    base_strings_of_first_name
    persons_of_first_name
    get_first_name
    x

let search_aux conf base of_absolute base_strings_of persons_of x =
  let list, name_inj =
    if p_getenv conf.env "t" = Some "A"
    then of_absolute conf base x, (fun x -> x)
    else if x = "" then [], (fun _ -> assert false)
    else persons_of_fsname base base_strings_of (spi_find (persons_of base)) x
  in
  let list =
    List.map
      (fun (str, _, iperl) ->
         Name.lower str, (StrSet.add str StrSet.empty, iperl))
      list
  in
  let list = List.fold_right merge_insert list [] in
  list, name_inj

let has_children_with_that_name conf base des name =
  let compare_name n1 n2 =
    if p_getenv conf.env "t" = Some "A" then n1 = n2
    else Name.lower n1 = Name.lower n2
  in
  List.exists
    (fun ip -> compare_name (p_surname base (pget conf base ip)) name)
    (Array.to_list (get_children des))

let insert_at_position_in_family children ip ipl =
  let rec loop child_list ipl =
    match child_list, ipl with
      ip1 :: ipl1, ip2 :: ipl2 ->
        if ip1 = ip2 then if ip = ip1 then ipl else ip2 :: loop ipl1 ipl2
        else if ip = ip1 then ip1 :: ipl
        else loop ipl1 ipl
    | _ :: _, [] -> [ip]
    | [], _ -> assert false
  in
  loop (Array.to_list children) ipl

type 'a branch_head = { bh_ancestor : 'a; bh_well_named_ancestors : 'a list }

let select_ancestors conf base name_inj ipl =
  let str_inj s = name_inj (sou base s) in
  List.fold_left
    (fun bhl ip ->
       let p = pget conf base ip in
       match get_parents p with
         Some ifam ->
           let fam = foi base ifam in
           let ifath = get_father fam in
           let imoth = get_mother fam in
           let fath = pget conf base ifath in
           let moth = pget conf base imoth in
           let s = str_inj (get_surname p) in
           if str_inj (get_surname fath) <> s &&
              str_inj (get_surname moth) <> s
           then
             let rec loop =
               function
                 bh :: bhl ->
                   if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
                     let bh =
                       {bh with bh_well_named_ancestors =
                         insert_at_position_in_family (get_children fam) ip
                           bh.bh_well_named_ancestors}
                     in
                     bh :: bhl
                   else bh :: loop bhl
               | [] -> [{bh_ancestor = ifath; bh_well_named_ancestors = [ip]}]
             in
             loop bhl
           else bhl
       | _ ->
           let bh = {bh_ancestor = ip; bh_well_named_ancestors = []} in
           bh :: bhl)
    [] ipl

let persons_of_absolute_surname conf base x =
  persons_of_absolute_name_aux
    conf
    base
    base_strings_of_surname
    persons_of_surname
    get_surname
    x

(** [(result, injection) = search_surname conf base x]
    where [result] is the list of matching iper list
    and [injection] is the function used to normalize
    strings during comparison.
*)
let search_surname conf base x
  : (string * (StrSet.t * iper list)) list * (string -> string) =
  search_aux conf base
    persons_of_absolute_surname
    base_strings_of_surname
    persons_of_surname
    x

let search_first_name conf base x : (string * (StrSet.t * iper list)) list =
  fst @@
  search_aux conf base
    persons_of_absolute_first_name
    base_strings_of_first_name
    persons_of_first_name
    x

(* Helpers *)

let match_first_name base exact fn_list p =
  match fn_list with
  | [] -> true
  | _ ->
    let list =
      Gwdb.get_first_name p
      |> Util.split_fname_is base
      |> List.map Name.lower
    in
    if exact
    then List.sort compare list = List.sort compare fn_list
    else List.for_all (fun s -> List.mem s list) fn_list

let match_surname base exact sn_list p =
  match sn_list with
  | [] -> true
  | _ ->
    let list =
      Gwdb.get_surname p
      |> Util.split_sname_is base
      |> List.map Name.lower
    in
    if exact
    then List.sort compare list = List.sort compare sn_list
    else List.for_all (fun s -> List.mem s list) sn_list

(** [ipers search_result]
    Return the list of ipers from a first name or surname search. *)
let ipers : (string * (StrSet.t * iper list)) list -> iper list =
  List.fold_left
    begin fun acc (_, (_, ipl)) ->
      List.fold_left (fun acc i -> if List.mem i acc then acc else i :: acc) acc ipl
    end []

let branches conf base (inj : string -> string) (ipl : iper list) : person branch_head list =
  select_ancestors conf base inj ipl
  |> List.map begin fun bh ->
    { bh_ancestor = pget conf base bh.bh_ancestor
    ; bh_well_named_ancestors =
        List.map (pget conf base) bh.bh_well_named_ancestors
    }
  end
