(* $Id: consang.ml,v 1.1 1998-09-01 14:32:05 ddr Exp $ *)

(* Algorithm relationship and links from Didier Remy *)

open Check;
open Def;
open Gutil;

type relationship =
  { weight1 : mutable float;
    weight2 : mutable float;
    relationship : mutable float;
    lens1 : mutable list (int * int);
    lens2 : mutable list (int * int);
    elim_ancestors : mutable bool;
    mark : mutable int }
;

type relationship_table =
  { id : array int;
    info : array relationship }
;

value no_consang = Adef.fix (-1);

value half x = x *. 0.5;

value mark = ref 0;
value new_mark () = do incr mark; return mark.val;

value topological_sort base =
  let tab = Array.create base.persons.len 0 in
  let todo = ref [] in
  let cnt = ref 0 in
  do for i = 0 to base.persons.len - 1 do
       let a = base.ascends.get i in
       match a.parents with
       [ Some ifam ->
           let cpl = coi base ifam in
           let ifath = Adef.int_of_iper cpl.father in
           let imoth = Adef.int_of_iper cpl.mother in
           do tab.(ifath) := tab.(ifath) + 1;
              tab.(imoth) := tab.(imoth) + 1;
           return ()
       | _ -> () ];
     done;
     for i = 0 to base.persons.len - 1 do
       if tab.(i) == 0 then todo.val := [i :: todo.val] else ();
     done;
     loop todo.val where rec loop =
       fun
       [ [i :: il] ->
           let a = base.ascends.get i in
           do todo.val := il;
              tab.(i) := cnt.val;
              incr cnt;
              match a.parents with
              [ Some ifam ->
                  let cpl = coi base ifam in
                  let ifath = Adef.int_of_iper cpl.father in
                  let imoth = Adef.int_of_iper cpl.mother in
                  do tab.(ifath) := tab.(ifath) - 1;
                     tab.(imoth) := tab.(imoth) - 1;
                     if tab.(ifath) == 0 then todo.val := [ifath :: todo.val]
                     else ();
                     if tab.(imoth) == 0 then todo.val := [imoth :: todo.val]
                     else ();
                  return ()
              | _ -> () ];           
           return loop todo.val
       | [] -> () ];
     if cnt.val <> base.persons.len then
       failwith
          ("topological sort: cnt " ^ string_of_int cnt.val ^ " len " ^
           string_of_int base.persons.len)
     else ();
  return tab
;

value make_relationship_table base =
  let id = topological_sort base in
  let phony =
    {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = []; lens2 = [];
     mark = 0; elim_ancestors = False}
  in
  let tab = Array.create base.persons.len phony in
  {id = id; info = tab}
;

value rec insert_branch_len_rec (len, n) =
  fun
  [ [] -> [(len, n)]
  | [(len1, n1) :: lens] ->
      if len == len1 then [(len1, n + n1) :: lens]
      else [(len1, n1) :: insert_branch_len_rec (len, n) lens] ]
;

value rec insert_branch_len lens (len, n) =
  insert_branch_len_rec (succ len, n) lens
;

value consang_of p =
  if p.consang == no_consang then 0.0 else Adef.float_of_fix p.consang
;

value leq = ref (fun []);
module Pq =
  Pqueue.Make (struct type t = int; value leq x y = leq.val x y; end)
;

value relationship_and_links base {id = id; info = tab} b ip1 ip2 =
  let i1 = Adef.int_of_iper ip1 in
  let i2 = Adef.int_of_iper ip2 in
  if i1 == i2 then (1.0, [])
  else
    let reset u mark =
      tab.(u) :=
        {weight1 = 0.0; weight2 = 0.0; relationship = 0.0; lens1 = [];
         lens2 = []; mark = mark; elim_ancestors = False}
    in
    do leq.val := fun x y -> id.(x) < id.(y); return
    let q = ref Pq.empty in
    let inserted = new_mark () in
    let add u = do reset u inserted; q.val := Pq.add u q.val; return () in
    let relationship = ref 0.0 in
    let tops = ref [] in
    let treat u y =
      do if tab.(y).mark <> inserted then add y else (); return
      let ty = tab.(y) in
      let p1 = half u.weight1 in
      let p2 = half u.weight2 in
      do ty.weight1 := ty.weight1 +. p1;
         ty.weight2 := ty.weight2 +. p2;
         ty.relationship := ty.relationship +. p1 *. p2;
         if u.elim_ancestors then ty.elim_ancestors := True else ();
         if b && not ty.elim_ancestors then
           do ty.lens1 :=
                List.fold_left insert_branch_len ty.lens1 u.lens1;
              ty.lens2 :=
                List.fold_left insert_branch_len ty.lens2 u.lens2;
           return ()
         else ();
      return ()
    in
    do add i1;
       add i2;
       tab.(i1).weight1 := 1.0;
       tab.(i2).weight2 := 1.0;
       tab.(i1).lens1 := [(0, 1)];
       tab.(i2).lens2 := [(0, 1)];
       while not (Pq.is_empty q.val) do
         let (u, nq) = Pq.take q.val in
         do q.val := nq; return
         let tu = tab.(u) in
         let a = base.ascends.get u in
         let contribution =
           tu.weight1 *. tu.weight2 -.
           tu.relationship *. (1.0 +. consang_of a)
         in
         do relationship.val := relationship.val +. contribution;
            if b && contribution <> 0.0 && not tu.elim_ancestors then
              do tops.val := [u :: tops.val];
                 tu.elim_ancestors := True;
              return ()
            else ();
            match a.parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                do treat tu (Adef.int_of_iper cpl.father);
                   treat tu (Adef.int_of_iper cpl.mother);
                return ()
            | _ -> () ];
         return ();
       done;
    return (half relationship.val, tops.val)
;

value relationship base tab ip1 ip2 =
  fst (relationship_and_links base tab False ip1 ip2)
;

value compute_all_consang base from_scratch =
  let _ = base.ascends.array () in
  let _ = base.couples.array () in
  let _ = base.families.array () in
  do Printf.eprintf "Computing consanguinity..."; flush stderr; return
  let running = ref True in
  let tab = make_relationship_table base in
  let cnt = ref 0 in
  let most = ref (base.ascends.get 0) in
  do for i = 0 to base.ascends.len - 1 do
       let a = base.ascends.get i in
       do if from_scratch then a.consang := no_consang else (); return
       if a.consang == no_consang then incr cnt else ();
     done;
     while running.val do
       running.val := False;
       for i = 0 to base.ascends.len - 1 do
         let a = base.ascends.get i in
         if a.consang == no_consang then
           match a.parents with
           [ Some ifam ->
               let cpl = coi base ifam in
               let fath = aoi base cpl.father in
               let moth = aoi base cpl.mother in
               if fath.consang != no_consang && moth.consang != no_consang then
                 let consang = relationship base tab cpl.father cpl.mother in
                 let fam = foi base ifam in
                 for i = 0 to Array.length fam.children - 1 do
                   let ip = fam.children.(i) in
                   let a = aoi base ip in
                   do Printf.eprintf "%6d\008\008\008\008\008\008" cnt.val;
                      flush stderr;
                      decr cnt;
                      a.consang := Adef.fix_of_float consang;
                      if a.consang > most.val.consang then
                        do Printf.eprintf "\nMax consanguinity %g for %s... "
                             consang (denomination base (poi base ip));
                           flush stderr;
                        return most.val := a
                      else ();
                   return ();
                 done
               else running.val := True
           | None ->
               do Printf.eprintf "%6d\008\008\008\008\008\008" cnt.val;
                  flush stderr;
                  decr cnt;
               return a.consang := Adef.fix_of_float 0.0 ]
         else ();
       done;
     done;
     Printf.eprintf " done   \n";
     flush stderr;
  return ()
;
