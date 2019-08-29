(* Copyright (c) 1998-2007 INRIA *)

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

let search_aux base base_strings_of persons_of x =
  let list, name_inj =
    if x = "" then [], (fun _ -> assert false)
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
       | Some ifam ->
         let fam = foi base ifam in
         let ifath = get_father fam in
         let imoth = get_mother fam in
         let fath = pget conf base ifath in
         let moth = pget conf base imoth in
         let s = str_inj (get_surname p) in
         if str_inj (get_surname fath) <> s && str_inj (get_surname moth) <> s
         then
           let rec loop =
             function
             | bh :: bhl -> if bh.bh_ancestor = ifath || bh.bh_ancestor = imoth then
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

(** [(result, injection) = search_surname conf base x]
    where [result] is the list of matching iper list
    and [injection] is the function used to normalize
    strings during comparison.
*)
let search_surname base x
  : (string * (StrSet.t * iper list)) list * (string -> string) =
  search_aux base
    base_strings_of_surname
    persons_of_surname
    x

let search_first_name base x : (string * (StrSet.t * iper list)) list =
  fst @@
  search_aux base
    base_strings_of_first_name
    persons_of_first_name
    x

(* Helpers *)

let match_first_name base ~exact fn_list p =
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

let match_surname base ~exact sn_list p =
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

(** [search_surname base sn]
    Decompose [sn] and use [search_surname].
*)
let search_surnames base s =
  match Mutil.split_sname s |> List.map Name.lower with
  | [] -> [], fun x -> x
  | hd :: tl ->
    let (list, inj) = search_surname base hd in
    ( Util.filter_map
        begin fun (string, (s, ipl)) ->
          let ipl = List.filter (fun i -> match_surname base ~exact:false tl (poi base i)) ipl in
          if [] <> ipl then Some (string, (s, ipl)) else None
        end list
    , inj )

(** [search_first_names base fn]
    Decompose [fn] and use [search_first_name].
*)
let search_first_names base s =
  match Mutil.split_fname s |> List.map Name.lower with
  | [] -> []
  | hd :: tl ->
    let list = search_first_name base hd in
    Util.filter_map
      begin fun (string, (s, ipl)) ->
        let ipl = List.filter (fun i -> match_first_name base ~exact:false tl (poi base i)) ipl in
        if [] <> ipl then Some (string, (s, ipl)) else None
      end list

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
