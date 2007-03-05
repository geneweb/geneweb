(* $Id: futil.ml,v 5.5 2007-03-05 05:18:23 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Adef;
open Def;

value map_title_strings f t =
  let t_name =
    match t.t_name with
    [ Tmain -> Tmain
    | Tname s -> Tname (f s)
    | Tnone -> Tnone ]
  in
  let t_ident = f t.t_ident in
  let t_place = f t.t_place in
  {t_name = t_name; t_ident = t_ident; t_place = t_place;
   t_date_start = t.t_date_start; t_date_end = t.t_date_end; t_nth = t.t_nth}
;

value map_relation_ps fp fs r =
  {r_type = r.r_type;
   r_fath =
     match r.r_fath with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_moth =
     match r.r_moth with
     [ Some x -> Some (fp x)
     | None -> None ];
   r_sources = fs r.r_sources}
;

value map_person_ps fp fs p =
  {first_name = fs p.first_name; surname = fs p.surname; occ = p.occ;
   image = fs p.image;
   first_names_aliases = List.map fs p.first_names_aliases;
   surnames_aliases = List.map fs p.surnames_aliases;
   public_name = fs p.public_name; qualifiers = List.map fs p.qualifiers;
   titles = List.map (map_title_strings fs) p.titles;
   rparents = List.map (map_relation_ps fp fs) p.rparents;
   related = p.related; aliases = List.map fs p.aliases;
   occupation = fs p.occupation; sex = p.sex; access = p.access;
   birth = p.birth; birth_place = fs p.birth_place;
   birth_src = fs p.birth_src; baptism = p.baptism;
   baptism_place = fs p.baptism_place; baptism_src = fs p.baptism_src;
   death = p.death; death_place = fs p.death_place;
   death_src = fs p.death_src; burial = p.burial;
   burial_place = fs p.burial_place; burial_src = fs p.burial_src;
   notes = fs p.notes; psources = fs p.psources; key_index = p.key_index}
;

value map_union_f ff u = {family = Array.map ff u.family};

value map_family_ps fp fs fam =
  {marriage = fam.marriage; marriage_place = fs fam.marriage_place;
   marriage_src = fs fam.marriage_src; witnesses = Array.map fp fam.witnesses;
   relation = fam.relation; divorce = fam.divorce; comment = fs fam.comment;
   origin_file = fs fam.origin_file; fsources = fs fam.fsources;
   fam_index = fam.fam_index}
;

value parent multi parent =
  if not multi then Adef.parent parent else Adef.multi_parent parent
;

value map_couple_p multi_parents fp cpl =
  parent multi_parents (Array.map fp (parent_array cpl))
;

value map_descend_p fp des = {children = Array.map fp des.children};

value gen_person_misc_names first_name surname public_name qualifiers aliases
    first_names_aliases surnames_aliases titles husbands
    father_titles_places =
  let first_name = Mutil.nominative first_name in
  let surname = Mutil.nominative surname in
  if first_name = "?" || surname = "?" then []
  else
    let public_names =
      let titles_names =
        let tnl = ref [] in
        do {
          List.iter
            (fun t ->
               match t.t_name with
               [ Tmain | Tnone -> ()
               | Tname x -> tnl.val := [x :: tnl.val] ])
            titles;
          tnl.val
        }
      in
      if public_name = "" || titles = [] then titles_names
      else [public_name :: titles_names]
    in
    let first_names =
      let pn =
        if public_name <> "" && titles = [] then [public_name :: public_names]
        else public_names
      in
      [first_name :: first_names_aliases @ pn]
    in
    let surnames =
      [surname ::
       Mutil.surnames_pieces surname @ surnames_aliases @ qualifiers]
    in
    let surnames =
      List.fold_left
        (fun list (husband_surname, husband_surnames_aliases) ->
           let husband_surname = Mutil.nominative husband_surname in
           if husband_surname = "?" then husband_surnames_aliases @ list
           else
             [husband_surname ::
              Mutil.surnames_pieces husband_surname @
                husband_surnames_aliases @ list])
        surnames husbands
    in
    let list = public_names in
    let list =
      List.fold_left
        (fun list f ->
           List.fold_left (fun list s -> [f ^ " " ^ s :: list]) list surnames)
        list first_names
    in
    let list =
      let first_names = [first_name :: first_names_aliases] in
      List.fold_left
        (fun list t ->
           let s = t.t_place in
           if s = "" then list
           else
             let first_names =
               match t.t_name with
               [ Tname f -> [f :: first_names]
               | Tmain | Tnone ->
                   let f = public_name in
                   if f = "" then first_names else [f :: first_names] ]
             in
             List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
               first_names)
        list titles
    in
    let list =
      if father_titles_places = [] then list
      else
        let first_names = [first_name :: first_names_aliases] in
        List.fold_left
          (fun list s ->
             if s = "" then list
             else
               List.fold_left (fun list f -> [f ^ " " ^ s :: list]) list
                 first_names)
          list father_titles_places
    in
    let list = List.rev_append aliases list in
    let fn = Name.lower (first_name ^ " " ^ surname) in
    List.fold_left
      (fun list s ->
         let s = Name.lower s in
         if s = fn || List.mem s list then list else [s :: list])
      [] list
;

value rec eq_lists eq l1 l2 =
  match (l1, l2) with
  [ ([x1 :: l1], [x2 :: l2]) -> eq x1 x2 && eq_lists eq l1 l2
  | ([], []) -> True
  | _ -> False ]
;

value eq_title_names eq tn1 tn2 =
  match (tn1, tn2) with
  [ (Tname i1, Tname i2) -> eq i1 i2
  | (Tmain, Tmain) | (Tnone, Tnone) -> True
  | _ -> False ]
;

value eq_titles eq t1 t2 =
  eq_title_names eq t1.t_name t2.t_name && eq t1.t_ident t2.t_ident &&
  eq t1.t_place t2.t_place && t1.t_date_start = t2.t_date_start &&
  t1.t_date_end = t2.t_date_end && t1.t_nth = t2.t_nth
;
