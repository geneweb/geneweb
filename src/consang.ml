(* $Id: consang.ml,v 4.4 2001-06-08 14:33:27 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

(* Algorithm relationship and links from Didier Remy *)

open Check;
open Def;
open Gutil;

type anc_stat = [ MaybeAnc | IsAnc ];

(* relationship:
   - elim_ancestor
        to prune displayed relationships
   - anc_stat1, anc_stat2
        optimization to answer faster when ancestors list is exhausted
   - lens1, lens2:
        the third parameter (list iper) of the list has been added to
        be able to reconstitute the branch in case of the sex of the
        persons in the branch is important to display the relationship
        text
*)

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int * list iper);
    lens2 : mutable list (int * int * list iper);
    inserted : mutable int;
    elim_ancestors : mutable bool;
    anc_stat1 : mutable anc_stat;
    anc_stat2 : mutable anc_stat }
;

type relationship_info =
  { tstab : array int;
    reltab : array relationship;
    queue : mutable array (list int) }
;

value no_consang = Adef.fix (-1);

value half x = x *. 0.5;

value mark = ref 0;
value new_mark () = do { incr mark; mark.val };

exception TopologicalSortError of person;

(* Return tab such as: i is an ancestor of j => tab.(i) > tab.(j) *)
(* This complicated topological sort has the important following properties:
   - only "ascends" has to be loaded; no need to load "union" and "descend"
     which use much memory space.
   - the value of tab is minimum; it is important for the optimization of
     relationship computation (stopping the computation when the ancestor
     list of one of the person is exhausted).
*)

value topological_sort base =
  let tab = Array.create base.data.persons.len 0 in
  let todo = ref [] in
  let cnt = ref 0 in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let a = base.data.ascends.get i in
      match a.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let ifath = Adef.int_of_iper cpl.father in
          let imoth = Adef.int_of_iper cpl.mother in
          do {
            tab.(ifath) := tab.(ifath) + 1; tab.(imoth) := tab.(imoth) + 1
          }
      | _ -> () ]
    };
    for i = 0 to base.data.persons.len - 1 do {
      if tab.(i) == 0 then todo.val := [i :: todo.val] else ()
    };
    let rec loop tval list =
      if list = [] then ()
      else
        let new_list =
          List.fold_left
            (fun new_list i ->
               let a = base.data.ascends.get i in
               do {
                 tab.(i) := tval;
                 incr cnt;
                 match a.parents with
                 [ Some ifam ->
                     let cpl = coi base ifam in
                     let ifath = Adef.int_of_iper cpl.father in
                     let imoth = Adef.int_of_iper cpl.mother in
                     do {
                       tab.(ifath) := tab.(ifath) - 1;
                       tab.(imoth) := tab.(imoth) - 1;
                       let new_list =
                         if tab.(ifath) == 0 then [ifath :: new_list]
                         else new_list
                       in
                       let new_list =
                         if tab.(imoth) == 0 then [imoth :: new_list]
                         else new_list
                       in
                       new_list
                     }
                 | _ -> new_list ]
               })
            [] list
        in
        loop (tval + 1) new_list
    in
    loop 0 todo.val;
    if cnt.val <> base.data.persons.len then
      Gutil.check_noloop base
        (fun
         [ OwnAncestor p -> raise (TopologicalSortError p)
         | _ -> assert False ])
    else ();
    tab
  }
;

value phony_rel =
  {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = []; lens2 = [];
   inserted = 0; elim_ancestors = False; anc_stat1 = MaybeAnc;
   anc_stat2 = MaybeAnc}
;

value make_relationship_info base tstab =
  let tab = Array.create base.data.persons.len phony_rel in
  {tstab = tstab; reltab = tab; queue = [| |]}
;

value rec insert_branch_len_rec ((len, n, ip) as x) =
  fun
  [ [] -> [(len, n, [ip])]
  | [((len1, n1, ipl1) as y) :: lens] ->
      if len == len1 then
        let n2 = n + n1 in
        let n2 = if n < 0 || n1 < 0 || n2 < 0 then -1 else n2 in
        [(len1, n2, [ip :: ipl1]) :: lens]
      else [y :: insert_branch_len_rec x lens] ]
;

value insert_branch_len ip lens (len, n, ipl) =
  insert_branch_len_rec (succ len, n, ip) lens
;

value consang_of p =
  if p.consang == no_consang then 0.0 else Adef.float_of_fix p.consang
;

value relationship_and_links base ri b ip1 ip2 =
  let i1 = Adef.int_of_iper ip1 in
  let i2 = Adef.int_of_iper ip2 in
  if i1 == i2 then (1.0, [])
  else do {
    let reltab = ri.reltab in
    let tstab = ri.tstab in
    let yes_inserted = new_mark () in
    let reset u =
      let tu = reltab.(u) in
      if tu == phony_rel then
        reltab.(u) :=
          {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = [];
           lens2 = []; inserted = yes_inserted; elim_ancestors = False;
           anc_stat1 = MaybeAnc; anc_stat2 = MaybeAnc}
      else do {
        tu.weight1 := 0.0;
        tu.weight2 := 0.0;
        tu.relationship := 0.0;
        tu.lens1 := [];
        tu.lens2 := [];
        tu.inserted := yes_inserted;
        tu.elim_ancestors := False;
        tu.anc_stat1 := MaybeAnc;
        tu.anc_stat2 := MaybeAnc
      }
    in
    let qi = ref (min tstab.(i1) tstab.(i2)) in
    let qmax = ref (-1) in
    let insert u =
      let v = tstab.(u) in
      do {
        reset u;
        if v >= Array.length ri.queue then
          let len = Array.length ri.queue in
          ri.queue := Array.append ri.queue (Array.create (v + 1 - len) [])
        else ();
        if qmax.val < 0 then do {
          for i = qi.val to v - 1 do { ri.queue.(i) := [] };
          qmax.val := v;
          ri.queue.(v) := [u]
        }
        else do {
          if v > qmax.val then do {
            for i = qmax.val + 1 to v do { ri.queue.(i) := [] }; qmax.val := v
          }
          else ();
          ri.queue.(v) := [u :: ri.queue.(v)]
        }
      }
    in
    let relationship = ref 0.0 in
    let nb_anc1 = ref 1 in
    let nb_anc2 = ref 1 in
    let tops = ref [] in
    let treat_parent ip_from u y =
      do {
        if reltab.(y).inserted <> yes_inserted then insert y else ();
        let ty = reltab.(y) in
        let p1 = half u.weight1 in
        let p2 = half u.weight2 in
        if u.anc_stat1 = IsAnc && ty.anc_stat1 <> IsAnc then do {
          ty.anc_stat1 := IsAnc; incr nb_anc1
        }
        else ();
        if u.anc_stat2 = IsAnc && ty.anc_stat2 <> IsAnc then do {
          ty.anc_stat2 := IsAnc; incr nb_anc2
        }
        else ();
        ty.weight1 := ty.weight1 +. p1;
        ty.weight2 := ty.weight2 +. p2;
        ty.relationship := ty.relationship +. p1 *. p2;
        if u.elim_ancestors then ty.elim_ancestors := True else ();
        if b && not ty.elim_ancestors then do {
          ty.lens1 :=
            List.fold_left (insert_branch_len ip_from) ty.lens1 u.lens1;
          ty.lens2 :=
            List.fold_left (insert_branch_len ip_from) ty.lens2 u.lens2
        }
        else ()
      }
    in
    let treat_ancestor u =
      let tu = reltab.(u) in
      let a = base.data.ascends.get u in
      let contribution =
        tu.weight1 *. tu.weight2 -. tu.relationship *. (1.0 +. consang_of a)
      in
      do {
        if tu.anc_stat1 == IsAnc then decr nb_anc1 else ();
        if tu.anc_stat2 == IsAnc then decr nb_anc2 else ();
        relationship.val := relationship.val +. contribution;
        if b && contribution <> 0.0 && not tu.elim_ancestors then do {
          tops.val := [u :: tops.val]; tu.elim_ancestors := True
        }
        else ();
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            do {
              treat_parent (Adef.iper_of_int u) tu
                (Adef.int_of_iper cpl.father);
              treat_parent (Adef.iper_of_int u) tu
                (Adef.int_of_iper cpl.mother)
            }
        | _ -> () ]
      }
    in
    insert i1;
    insert i2;
    reltab.(i1).weight1 := 1.0;
    reltab.(i2).weight2 := 1.0;
    reltab.(i1).lens1 := [(0, 1, [])];
    reltab.(i2).lens2 := [(0, 1, [])];
    reltab.(i1).anc_stat1 := IsAnc;
    reltab.(i2).anc_stat2 := IsAnc;
    while qi.val <= qmax.val && nb_anc1.val > 0 && nb_anc2.val > 0 do {
      List.iter treat_ancestor ri.queue.(qi.val); incr qi
    };
    (half relationship.val, tops.val)
  }
;
