(* Copyright (c) 1998-2007 INRIA *)

(* Algorithm relationship and links from Didier Remy *)

open Def
module Driver = Geneweb_db.Driver
module Collection = Geneweb_db.Collection

type anc_stat = MaybeAnc | IsAnc

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

type relationship = {
  mutable weight1 : float;
  mutable weight2 : float;
  mutable relationship : float;
  mutable lens1 : (int * int * Geneweb_db.Driver.iper list) list;
  mutable lens2 : (int * int * Geneweb_db.Driver.iper list) list;
  mutable inserted : int;
  mutable elim_ancestors : bool;
  mutable anc_stat1 : anc_stat;
  mutable anc_stat2 : anc_stat;
}

type relationship_info = {
  tstab : (Geneweb_db.Driver.iper, int) Geneweb_db.Collection.Marker.t;
  reltab :
    (Geneweb_db.Driver.iper, relationship) Geneweb_db.Collection.Marker.t;
  mutable queue : Geneweb_db.Driver.iper list array;
}

let half x = x *. 0.5

type visit =
  | NotVisited (* not visited person *)
  | BeingVisited
    (* visited person but visit of ascendants haven't been terminated *)
  | Visited (* visited person and his ascendants *)

let rec noloop_aux base error tab i =
  match Collection.Marker.get tab i with
  | NotVisited ->
      (match Driver.get_parents (Driver.poi base i) with
      | Some ifam ->
          let fam = Driver.foi base ifam in
          let fath = Driver.get_father fam in
          let moth = Driver.get_mother fam in
          Collection.Marker.set tab i BeingVisited;
          noloop_aux base error tab fath;
          noloop_aux base error tab moth
      | None -> ());
      Collection.Marker.set tab i Visited
  | BeingVisited -> error (OwnAncestor (Driver.poi base i))
  | Visited -> ()

(** It is highly recommended to load ascends and couples array before running
    [check_noloop] *)
let check_noloop base error =
  let tab = Driver.iper_marker (Driver.ipers base) NotVisited in
  Collection.iter (noloop_aux base error tab) (Driver.ipers base)

let check_noloop_for_person_list base error list =
  let tab = Driver.iper_marker (Driver.ipers base) NotVisited in
  List.iter (noloop_aux base error tab) list

exception TopologicalSortError of Geneweb_db.Driver.person

(* Return tab such as: i is an ancestor of j => tab.(i) > tab.(j) *)
(* This complicated topological sort has the important following properties:
   - only "ascends" has to be loaded; no need to load "union" and "descend"
     which use much memory space.
   - the value of tab is minimum; it is important for the optimization of
     relationship computation (stopping the computation when the ancestor
     list of one of the person is exhausted).
*)
let topological_sort base poi =
  let persons = Driver.ipers base in
  let tab = Driver.iper_marker (Driver.ipers base) 0 in
  let cnt = ref 0 in
  Collection.iter
    (fun i ->
      let a = poi base i in
      match Driver.get_parents a with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          let ifath = Driver.get_father cpl in
          let imoth = Driver.get_mother cpl in
          Collection.Marker.set tab ifath (Collection.Marker.get tab ifath + 1);
          Collection.Marker.set tab imoth (Collection.Marker.get tab imoth + 1)
      | _ -> ())
    persons;
  (* starting from the leaf vertex of graph (persons without childs) *)
  let todo =
    Collection.fold
      (fun acc i -> if Collection.Marker.get tab i = 0 then i :: acc else acc)
      [] persons
  in
  let rec loop tval list =
    if list = [] then ()
    else
      let new_list =
        List.fold_left
          (fun new_list i ->
            let a = poi base i in
            Collection.Marker.set tab i tval;
            incr cnt;
            match Driver.get_parents a with
            | Some ifam ->
                let cpl = Driver.foi base ifam in
                let ifath = Driver.get_father cpl in
                let imoth = Driver.get_mother cpl in
                Collection.Marker.set tab ifath
                  (Collection.Marker.get tab ifath - 1);
                Collection.Marker.set tab imoth
                  (Collection.Marker.get tab imoth - 1);
                let new_list =
                  if Collection.Marker.get tab ifath = 0 then ifath :: new_list
                  else new_list
                in
                if Collection.Marker.get tab imoth = 0 then imoth :: new_list
                else new_list
            | _ -> new_list)
          [] list
      in
      loop (tval + 1) new_list
  in
  loop 0 todo;
  if !cnt <> Driver.nb_of_persons base then
    check_noloop base (function
      | OwnAncestor p -> raise (TopologicalSortError p)
      | _ -> assert false);
  tab

let phony_rel =
  {
    weight1 = 0.0;
    weight2 = 0.0;
    relationship = 0.0;
    lens1 = [];
    lens2 = [];
    inserted = 0;
    elim_ancestors = false;
    anc_stat1 = MaybeAnc;
    anc_stat2 = MaybeAnc;
  }

let make_relationship_info base tstab =
  let tab =
    Geneweb_db.Driver.iper_marker (Geneweb_db.Driver.ipers base) phony_rel
  in
  { tstab; reltab = tab; queue = [||] }

let rec insert_branch_len_rec ((len, n, ip) as x) = function
  | [] -> [ (len, n, [ ip ]) ]
  | ((len1, n1, ipl1) as y) :: lens ->
      if len = len1 then
        let n2 = n + n1 in
        let n2 = if n < 0 || n1 < 0 || n2 < 0 then -1 else n2 in
        (len1, n2, ip :: ipl1) :: lens
      else y :: insert_branch_len_rec x lens

let insert_branch_len ip lens (len, n, _ipl) =
  insert_branch_len_rec (succ len, n, ip) lens

let consang_of p =
  if Driver.get_consang p = Adef.no_consang then 0.0
  else Adef.float_of_fix (Driver.get_consang p)

let mark = ref 0

let new_mark () =
  incr mark;
  !mark

let relationship_and_links base ri b ip1 ip2 =
  let i1 = ip1 in
  let i2 = ip2 in
  if i1 = i2 then (1.0, [])
  else
    let reltab = ri.reltab in
    let tstab = ri.tstab in
    let yes_inserted = new_mark () in
    let reset u =
      let tu = Collection.Marker.get reltab u in
      if tu == phony_rel then
        Collection.Marker.set reltab u
          {
            weight1 = 0.0;
            weight2 = 0.0;
            relationship = 0.0;
            lens1 = [];
            lens2 = [];
            inserted = yes_inserted;
            elim_ancestors = false;
            anc_stat1 = MaybeAnc;
            anc_stat2 = MaybeAnc;
          }
      else (
        tu.weight1 <- 0.0;
        tu.weight2 <- 0.0;
        tu.relationship <- 0.0;
        tu.lens1 <- [];
        tu.lens2 <- [];
        tu.inserted <- yes_inserted;
        tu.elim_ancestors <- false;
        tu.anc_stat1 <- MaybeAnc;
        tu.anc_stat2 <- MaybeAnc)
    in
    let qi =
      ref
        (min (Collection.Marker.get tstab i1) (Collection.Marker.get tstab i2))
    in
    let qmax = ref (-1) in
    let insert u =
      let v = Collection.Marker.get tstab u in
      reset u;
      (if v >= Array.length ri.queue then
         let len = Array.length ri.queue in
         ri.queue <- Array.append ri.queue (Array.make (v + 1 - len) []));
      if !qmax < 0 then (
        for i = !qi to v - 1 do
          ri.queue.(i) <- []
        done;
        qmax := v;
        ri.queue.(v) <- [ u ])
      else (
        if v > !qmax then (
          for i = !qmax + 1 to v do
            ri.queue.(i) <- []
          done;
          qmax := v);
        ri.queue.(v) <- u :: ri.queue.(v))
    in
    let relationship = ref 0.0 in
    let nb_anc1 = ref 1 in
    let nb_anc2 = ref 1 in
    let tops = ref [] in
    let treat_parent ip_from u y =
      if (Collection.Marker.get reltab y).inserted <> yes_inserted then insert y;
      let ty = Collection.Marker.get reltab y in
      let p1 = half u.weight1 in
      let p2 = half u.weight2 in
      if u.anc_stat1 = IsAnc && ty.anc_stat1 <> IsAnc then (
        ty.anc_stat1 <- IsAnc;
        incr nb_anc1);
      if u.anc_stat2 = IsAnc && ty.anc_stat2 <> IsAnc then (
        ty.anc_stat2 <- IsAnc;
        incr nb_anc2);
      ty.weight1 <- ty.weight1 +. p1;
      ty.weight2 <- ty.weight2 +. p2;
      ty.relationship <- ty.relationship +. (p1 *. p2);
      if u.elim_ancestors then ty.elim_ancestors <- true;
      if b && not ty.elim_ancestors then (
        ty.lens1 <- List.fold_left (insert_branch_len ip_from) ty.lens1 u.lens1;
        ty.lens2 <- List.fold_left (insert_branch_len ip_from) ty.lens2 u.lens2)
    in
    let treat_ancestor u =
      let tu = Collection.Marker.get reltab u in
      let a = Driver.poi base u in
      let contribution =
        (tu.weight1 *. tu.weight2) -. (tu.relationship *. (1.0 +. consang_of a))
      in
      if tu.anc_stat1 = IsAnc then decr nb_anc1;
      if tu.anc_stat2 = IsAnc then decr nb_anc2;
      relationship := !relationship +. contribution;
      if b && contribution <> 0.0 && not tu.elim_ancestors then (
        tops := u :: !tops;
        tu.elim_ancestors <- true);
      match Driver.get_parents a with
      | Some ifam ->
          let cpl = Driver.foi base ifam in
          treat_parent u tu (Driver.get_father cpl);
          treat_parent u tu (Driver.get_mother cpl)
      | _ -> ()
    in
    insert i1;
    insert i2;
    (Collection.Marker.get reltab i1).weight1 <- 1.0;
    (Collection.Marker.get reltab i2).weight2 <- 1.0;
    (Collection.Marker.get reltab i1).lens1 <- [ (0, 1, []) ];
    (Collection.Marker.get reltab i2).lens2 <- [ (0, 1, []) ];
    (Collection.Marker.get reltab i1).anc_stat1 <- IsAnc;
    (Collection.Marker.get reltab i2).anc_stat2 <- IsAnc;
    while !qi <= !qmax && !nb_anc1 > 0 && !nb_anc2 > 0 do
      List.iter treat_ancestor ri.queue.(!qi);
      incr qi
    done;
    (half !relationship, !tops)
