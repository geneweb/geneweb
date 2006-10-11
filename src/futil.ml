(* $Id: futil.ml,v 5.1 2006-10-11 19:52:35 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

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
