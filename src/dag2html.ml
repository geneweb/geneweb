(* $Id: dag2html.ml,v 3.5 1999-12-03 03:25:19 ddr Exp $ *)

open Config;
open Def;
open Gutil;

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'x
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type table 'a = { table : mutable array (array (data 'a)) }
and data 'a = { elem : elem 'a; span : mutable span_id }
and elem 'a = [ Elem of 'a | Ghost of ghost_id | Nothing ]
and span_id = 'x
and ghost_id = 'x;

external span_id_of_int : int -> span_id = "%identity";
external int_of_span_id : span_id -> int = "%identity";
external ghost_id_of_int : int -> ghost_id = "%identity";
external int_of_ghost_id : ghost_id -> int = "%identity";

value new_span_id =
  let i = ref 0 in fun () -> do incr i; return span_id_of_int i.val
;

value new_ghost_id =
  let i = ref 0 in fun () -> do incr i; return ghost_id_of_int i.val
;

(* input dag *)

value get_dag_elems conf base =
  let module O = struct type t = iper; value compare = compare; end in
  let module S = Set.Make O in
  let set =
    loop S.empty 1 where rec loop set i =
      let s = string_of_int i in
      let po = Util.find_person_in_env conf base s in
      let so = Util.p_getenv conf.env ("s" ^ s) in
      match (po, so) with
      [ (Some p, Some s) ->
          let set =
            match Util.branch_of_sosa base p.cle_index (Num.of_string s) with
            [ Some ipsl ->
                List.fold_left (fun set (ip, _) -> S.add ip set) set ipsl
            | None -> set ]
          in
          loop set (i + 1)
      | _ -> set ]
  in
  S.elements set
;

value make_dag base list =
  let module O = struct type t = iper; value compare = compare; end in
  let module M = Map.Make O in
  let nodes = Array.of_list list in
  let map =
    loop M.empty 0 where rec loop map i =
      if i = Array.length nodes then map
      else loop (M.add nodes.(i) (idag_of_int i) map) (i + 1)
  in
  let nodes =
    Array.map
      (fun ip ->
(*
do Printf.eprintf "\no %s\n" (denomination base (poi base ip)); flush stderr; return
*)
         let pare =
           match (aoi base ip).parents with
           [ Some ifam ->
               let c = coi base ifam in
               let l = try [M.find c.father map] with [ Not_found -> [] ] in
               try [M.find c.mother map :: l] with [ Not_found -> l ]
           | None -> [] ]
         in
(*
do Printf.eprintf "parents\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) pare; flush stderr; return
*)
         let chil =
           let u = uoi base ip in
           Array.fold_left
             (fun chil ifam ->
                let des = doi base ifam in
                Array.fold_left
                  (fun chil ip ->
                     try [M.find ip map :: chil] with [ Not_found -> chil ])
                  chil des.children)
             [] u.family
         in
(*
do Printf.eprintf "children\n"; flush stderr; return
do List.iter (fun id -> Printf.eprintf "- %s\n" (denomination base (poi base nodes.(int_of_idag id)))) chil; flush stderr; return
*)
         {pare = pare; valu = ip; chil = chil})
      nodes
  in
  {dag = nodes}
;

(* print *)

value print_html_table conf base print_indi short d t =
  let jlast = Array.length t.table.(0) - 1 in
  let print_elem =
    fun
    [ Elem e -> print_indi d.dag.(int_of_idag e).valu
    | Ghost _ -> Wserver.wprint "|"
    | Nothing -> Wserver.wprint "&nbsp;" ]
  in
  let print_bar =
    fun
    [ Elem _ | Ghost _ -> Wserver.wprint "|"
    | Nothing -> Wserver.wprint "&nbsp;" ]
  in
  let print_line_elem print_elem k i =
    do Wserver.wprint "<tr>\n"; return
    let rec loop j =
      if j = Array.length t.table.(i) then ()
      else
        let x = t.table.(i).(j).elem in
        let next_j =
          loop (j + 1) where rec loop j =
            if j = Array.length t.table.(i) then j
            else if t.table.(i).(j).elem = x then loop (j + 1)
            else j
        in
        let colspan = 3 * (next_j - j) in
        do Wserver.wprint "<td>&nbsp;</td>\n";
           Wserver.wprint "<td colspan=%d align=center>" (colspan - 2);
           if t.table.(k).(j).elem = Nothing then Wserver.wprint "&nbsp;"
           else print_elem x;
           Wserver.wprint "</td>\n";
           Wserver.wprint "<td>&nbsp;</td>\n";
        return loop next_j
    in
    loop 0
  in
  let print_alone_bar i =
    do Wserver.wprint "<tr>\n"; return
    let rec loop j =
      if j = Array.length t.table.(i) then ()
      else
        let next_j =
          let x = t.table.(i).(j).span in
          let rec loop j =
            if j = Array.length t.table.(i) then j
            else if t.table.(i).(j).span = x then loop (j + 1)
            else j
          in
          loop (j + 1)
        in
        let colspan = 3 * (next_j - j) - 2 in
        do Wserver.wprint "<td>&nbsp;</td>\n";
           if t.table.(i + 1).(j).elem = Nothing then
             Wserver.wprint "<td colspan=%d>&nbsp;</td>" colspan
           else Wserver.wprint "<td colspan=%d align=center>|</td>\n" colspan;
           Wserver.wprint "<td>&nbsp;</td>\n";
        return loop next_j
    in
    loop 0
  in
  let exist_several_branches i k =
    loop 0 where rec loop j =
      if j = Array.length t.table.(i) then False
      else
        let x = t.table.(i).(j).span in
        let e = t.table.(k).(j).elem in
        loop1 (j + 1) where rec loop1 j =
          if j = Array.length t.table.(i) then False
          else if t.table.(i).(j).span <> x then loop j
          else if t.table.(k).(j).elem <> e then True
          else loop1 (j + 1)
  in
  let print_hbars i k =
    do Wserver.wprint "<tr>\n"; return
    let rec loop j =
      if j = Array.length t.table.(i) then ()
      else
        let next_j =
          let x = t.table.(i).(j).span in
          let rec loop j =
            if j = Array.length t.table.(i) then j
            else if t.table.(i).(j).span = x then loop (j + 1)
            else j
          in
          loop (j + 1)
        in
        let rec loop1 l =
          if l = next_j then loop next_j
          else
            let next_l =
              let y = t.table.(k).(l).elem in
              match y with
              [ Elem _ | Ghost _ ->
                  let rec loop l =
                    if l = Array.length t.table.(i) then l
                    else if t.table.(k).(l).elem = y then loop (l + 1)
                    else l
                  in
                  loop (l + 1)
              | _ -> l + 1 ]
            in
do if next_l > next_j then do Printf.eprintf "i %d j %d next_j %d l %d next_l %d\n" i j next_j l next_l; flush stderr; return ()
else (); return
            do assert (next_l <= next_j); return
            let colspan = 3 * (next_l - l) - 2 in
            do match t.table.(i + 1).(l).elem with
               [ Nothing ->
                   Wserver.wprint "<td colspan=%d>&nbsp;</td>\n" (colspan + 2)
               | _ ->
                   if l = j && next_l = next_j then
                     do Wserver.wprint "<td>&nbsp</td>\n";
                        Wserver.wprint "<td colspan=%d align=center>|</td>\n"
                          colspan;
                        Wserver.wprint "<td>&nbsp;</td>\n";
                     return ()
                   else if l = j then
                     do Wserver.wprint "<td>&nbsp;</td>\n";
                        Wserver.wprint "<td colspan=%d align=right>" colspan;
                        Wserver.wprint
                          "<hr noshade size=1 width=\"50%%\" align=right>";
                        Wserver.wprint "</td>\n";
                        Wserver.wprint "<td><hr noshade size=1></td>\n";
                     return ()
                   else if next_l = next_j then
                     do Wserver.wprint "<td><hr noshade size=1></td>\n";
                        Wserver.wprint "<td colspan=%d align=left>" colspan;
                        Wserver.wprint
                          "<hr noshade size=1 width=\"50%%\" align=left>";
                        Wserver.wprint "</td>\n";
                        Wserver.wprint "<td>&nbsp;</td>\n";
                     return ()
                   else
                     do Wserver.wprint "<td colspan=%d>" (colspan + 2);
                        Wserver.wprint "<hr noshade size=1></td>\n";
                     return () ];
            return loop1 next_l
        in
        loop1 j
    in
    loop 0
  in
  do Wserver.wprint "<center><table border=%d cellspacing=0 cellpadding=0>\n"
       conf.border;
     for i = 0 to Array.length t.table - 1 do
       print_line_elem print_elem i i;
       if i < Array.length t.table - 1 then
         do if short then () else print_line_elem print_bar (i + 1) i;
            if exist_several_branches i i then
              do print_hbars i i;
                 print_alone_bar i;
              return ()
            else ();
            if exist_several_branches i (i + 1) then
              do print_hbars i (i + 1);
                 if short then ()
                 else print_line_elem print_bar (i + 1) (i + 1);
              return ()
            else ();
         return ()
       else ();
     done;
     Wserver.wprint "</table></center>\n";
  return ()
;

(* transforming dag into table *)

value ancestors d =
  loop 0 where rec loop i =
    if i = Array.length d.dag then []
    else
      let n = d.dag.(i) in
      if n.pare = [] then [idag_of_int i :: loop (i + 1)] else loop (i + 1)
;

value get_children d parents =
  merge_children [] parents where rec merge_children children el =
    List.fold_right
      (fun (x, _) children ->
         match x with
         [ Elem e ->
             let e = d.dag.(int_of_idag e) in
             List.fold_right
               (fun c children ->
                  if List.mem c children then children else [c :: children])
               e.chil children
         | _ -> [] ])
      el children
;

value rec get_block t i j =
  if j = Array.length t.table.(i) then None
  else if j = Array.length t.table.(i) - 1 then
    let x = t.table.(i).(j) in Some ([(x.elem, 1)], 1, x.span)
  else
    let x = t.table.(i).(j) in
    let y = t.table.(i).(j + 1) in
    if y.span = x.span then
      match get_block t i (j + 1) with
      [ Some ([(x1, c1) :: list], mpc, span) ->
          let (list, mpc) =
            if x1 = x.elem then ([(x1, c1 + 1) :: list], max mpc (c1 + 1))
            else ([(x.elem, 1); (x1, c1) :: list], max mpc c1)
          in
          Some (list, mpc, span)
      | _ -> assert False ]
    else Some ([(x.elem, 1)], 1, x.span)
;

value group_by_common_children d list =
  let module O = struct type t = idag; value compare = compare; end in
  let module S = Set.Make O in
  let nlcsl =
    List.map
      (fun id ->
         let n = d.dag.(int_of_idag id) in
         let cs = List.fold_right S.add n.chil S.empty in ([id], cs))
      list
  in
  let nlcsl =
    loop nlcsl where rec loop =
      fun
      [ [] -> []
      | [(nl, cs) :: rest] ->
          let rec loop1 beg =
            fun
            [ [(nl1, cs1) :: rest1] ->
                if S.is_empty (S.inter cs cs1) then
                  loop1 [(nl1, cs1) :: beg] rest1
                else loop [(nl @ nl1, S.union cs cs1) :: List.rev beg @ rest1]
            | [] -> [(nl, cs) :: loop rest] ]
          in
          loop1 [] rest ]
  in
  List.fold_right
    (fun (nl, _) a ->
       let span = new_span_id () in
       List.fold_right (fun n a -> [{elem = Elem n; span = span} :: a]) nl a)
    nlcsl []
;

value insert_columns t nb j =
  let t1 = Array.create (Array.length t.table) [| |] in
  do for i = 0 to Array.length t.table - 1 do
       let line = t.table.(i) in
       let line1 = Array.create (Array.length line + nb) line.(0) in
       do t1.(i) := line1; return
       let rec loop k =
         if k = Array.length line then ()
         else
           do if k < j then line1.(k) := line.(k)
              else if k = j then
                for r = 0 to nb do line1.(k + r) := line.(k); done
              else line1.(k + nb) := line.(k);
           return loop (k + 1)
       in
       loop 0;
     done;
  return {table = t1}
;

value rec gcd a b =
  if a < b then gcd b a else if b = 0 then a else gcd b (a mod b)
;

value treat_new_row d t =
  let i = Array.length t.table - 1 in
  let rec loop t i j =
    match get_block t i j with
    [ Some (parents, max_parent_colspan, span) ->
        let children = get_children d parents in
        let children =
          if children = [] then [{elem = Nothing; span = new_span_id ()}]
          else
            List.map (fun n -> {elem = Elem n; span = new_span_id ()})
              children
        in
        let simple_parents_colspan =
          List.fold_left (fun x (_, c) -> x + c) 0 parents
        in
        if simple_parents_colspan mod List.length children = 0 then
          let j = j + simple_parents_colspan in
          let children =
            let cnt = simple_parents_colspan / List.length children in
            List.fold_right
              (fun d list ->
                 let rec loop cnt list =
                   if cnt = 0 then list else [d :: loop (cnt - 1) list]
                 in
                 loop cnt list)
              children []
          in
          let (t, children_rest) = loop t i j in (t, children @ children_rest)
        else
          let parent_colspan =
            List.fold_left
              (fun scm (_, c) -> let g = gcd scm c in scm / g * c)
              max_parent_colspan parents
          in
          let (t, parents, _) =
            List.fold_left
              (fun (t, parents, j) (x, c) ->
                 let to_add = parent_colspan / c - 1 in
                 let t = insert_columns t to_add j in
                 (t, [(x, parent_colspan) :: parents], j + parent_colspan))
              (t, [], j) parents
          in
          let parents = List.rev parents in
          let parents_colspan = max_parent_colspan * List.length parents in
          let children_colspan = List.length children in
          let g = gcd parents_colspan children_colspan in
          let (t, j) =
            let cnt = children_colspan / g in
            List.fold_left
              (fun (t, j) (_, c) ->
                 if c <> max_parent_colspan then failwith "not impl"
                 else
                   let rec loop cc t j =
                     if cc = 0 then (t, j)
                     else
                       let t = insert_columns t (cnt - 1) j in
                       let j = j + cnt in loop (cc - 1) t j
                   in
                   loop c t j)
              (t, j) parents
          in
          let children =
            let cnt = parents_colspan / g in
            List.fold_right
              (fun d list ->
                 let rec loop cnt list =
                   if cnt = 0 then list else [d :: loop (cnt - 1) list]
                 in
                 loop cnt list)
              children []
          in
          let (t, children_rest) = loop t i j in (t, children @ children_rest)
    | None -> (t, []) ]
  in
  loop t i 0
;

value down_it t i k y =
  do t.table.(Array.length t.table - 1).(k) := t.table.(i).(k);
     for r = i to Array.length t.table - 2 do
       t.table.(r).(k) :=
         {elem = Ghost (new_ghost_id ()); span = new_span_id ()};
     done;
  return ()
;

value equilibrate t =
  let ilast = Array.length t.table - 1 in
  let last = t.table.(ilast) in
  let len = Array.length last in
  let rec loop j =
    if j = len then ()
    else
      match last.(j).elem with
      [ Elem x ->
          let rec loop1 i =
            if i = ilast then loop (j + 1)
            else
              let rec loop2 k =
                if k = len then loop1 (i + 1)
                else
                  match t.table.(i).(k).elem with
                  [ Elem y when x = y -> do down_it t i k y; return loop 0
                  | _ -> loop2 (k + 1) ]
              in
              loop2 0
          in
          loop1 0
      | _ -> loop (j + 1) ]
  in
  loop 0
;

value group_elem t =
  for i = 0 to Array.length t.table - 2 do
    for j = 1 to Array.length t.table.(0) - 1 do
      let x =
        match t.table.(i+1).(j-1).elem with
        [ Elem x -> Some x
        | _ -> None ]
      in
      let y =
        match t.table.(i+1).(j).elem with
        [ Elem x -> Some x
        | _ -> None ]
      in
      match (x, y) with
      [ (Some x, Some y) when x = y ->
          t.table.(i).(j).span := t.table.(i).(j-1).span
      | _ -> () ];
    done;
  done
;

value group_ghost t =
  for i = 0 to Array.length t.table - 2 do
    for j = 1 to Array.length t.table.(0) - 1 do
      match (t.table.(i+1).(j-1).elem, t.table.(i+1).(j).elem) with
      [ (Ghost x, Ghost _) ->
          if t.table.(i).(j-1).elem = t.table.(i).(j).elem then
            t.table.(i+1).(j) :=
              {elem = Ghost x; span = t.table.(i+1).(j-1).span}
          else ()
      | _ -> () ];
    done;
  done
;

value group_children t =
  for i = 0 to Array.length t.table - 1 do
    let line = t.table.(i) in
    let len = Array.length line in
    for j = 1 to len - 1 do
      if line.(j).elem = line.(j - 1).elem then
        line.(j).span := line.(j - 1).span
      else ();
    done;
  done
;

value group_span_by_common_children d t =
  let module O = struct type t = idag; value compare = compare; end in
  let module S = Set.Make O in
  let i = Array.length t.table - 1 in
  let line = t.table.(i) in
  let rec loop j cs =
    if j = Array.length line then ()
    else
      match line.(j).elem with
      [ Elem id ->
          let n = d.dag.(int_of_idag id) in
          let curr_cs = List.fold_right S.add n.chil S.empty in
          if S.is_empty (S.inter cs curr_cs) then loop (j + 1) curr_cs
          else
            do line.(j).span := line.(j - 1).span; return
            loop (j + 1) (S.union cs curr_cs)
      | _ -> loop (j + 1) S.empty ]
  in
  loop 0 S.empty
;

value find_same_parents t i j1 j2 j3 j4 =
  loop i j1 j2 j3 j4 where rec loop i j1 j2 j3 j4 =
    if i = 0 then (i, j1, j2, j3, j4)
    else
      let x1 = t.(i - 1).(j1) in
      let x2 = t.(i - 1).(j2) in
      let x3 = t.(i - 1).(j3) in
      let x4 = t.(i - 1).(j4) in
      if x1.span = x4.span then (i, j1, j2, j3, j4)
      else
        let j1 =
          loop (j1 - 1) where rec loop j =
            if j < 0 then 0
            else if t.(i - 1).(j).span = x1.span then loop (j - 1)
            else j + 1
        in
        let j2 =
          loop (j2 + 1) where rec loop j =
            if j >= Array.length t.(i) then j - 1
            else if t.(i - 1).(j).span = x2.span then loop (j + 1)
            else j - 1
        in
        let j3 =
          loop (j3 - 1) where rec loop j =
            if j < 0 then 0
            else if t.(i - 1).(j).span = x3.span then loop (j - 1)
            else j + 1
        in
        let j4 =
          loop (j4 + 1) where rec loop j =
            if j >= Array.length t.(i) then j - 1
            else if t.(i - 1).(j).span = x4.span then loop (j + 1)
            else j - 1
        in
        loop (i - 1) j1 j2 j3 j4
;

value find_linked_children t i j1 j2 j3 j4 =
  loop i j1 j2 j3 j4 where rec loop i j1 j2 j3 j4 =
    if i = Array.length t - 1 then (j1, j2, j3, j4)
    else
      let x1 = t.(i).(j1) in
      let x2 = t.(i).(j2) in
      let x3 = t.(i).(j3) in
      let x4 = t.(i).(j4) in
      let j1 =
        loop (j1 - 1) where rec loop j =
          if j < 0 then 0
          else if t.(i).(j).span = x1.span then loop (j - 1)
          else j + 1
      in
      let j2 =
        loop (j2 + 1) where rec loop j =
          if j >= Array.length t.(i) then j - 1
          else if t.(i).(j).span = x2.span then loop (j + 1)
          else j - 1
      in
      let j3 =
        loop (j3 - 1) where rec loop j =
          if j < 0 then 0
          else if t.(i).(j).span = x3.span then loop (j - 1)
          else j + 1
      in
      let j4 =
        loop (j4 + 1) where rec loop j =
          if j >= Array.length t.(i) then j - 1
          else if t.(i).(j).span = x4.span then loop (j + 1)
          else j - 1
      in
      loop (i + 1) j1 j2 j3 j4
;

value exch_blocks t i1 i2 j1 j2 j3 j4 =
  do for i = i1 to i2 do
       let line = t.(i) in
       let saved = Array.copy line in
       do for j = j1 to j2 do line.(j4 - j2 + j) := saved.(j); done;
          for j = j3 to j4 do line.(j1 - j3 + j) := saved.(j); done;
       return ();
     done;
  return ()
;

value find_block_with_parents t i jj1 jj2 jj3 jj4 =
  loop i jj1 jj2 jj3 jj4 where rec loop ii jj1 jj2 jj3 jj4 =
    let (nii, njj1, njj2, njj3, njj4) =
      find_same_parents t i jj1 jj2 jj3 jj4
    in
    if nii <> ii || njj1 <> jj1 || njj2 <> jj2 || njj3 <> jj3 || njj4 <> jj4
    then
      let nii = min ii nii in
      let (jj1, jj2, jj3, jj4) =
        find_linked_children t nii njj1 njj2 njj3 njj4
      in
      if njj1 <> jj1 || njj2 <> jj2 || njj3 <> jj3 || njj4 <> jj4 then
        loop nii jj1 jj2 jj3 jj4
      else (nii, jj1, jj2, jj3, jj4)
    else (ii, jj1, jj2, jj3, jj4)
;

value push_to_right d t i j1 j2 =
  let line = t.(i) in
  let rec loop j =
    if j = j2 then j - 1
    else
      let jj1 =
        let x = line.(j - 1).elem in
        let rec same_value j =
          if j < 0 then 0
          else if line.(j).elem = x then same_value (j - 1)
          else j + 1
        in
        same_value (j - 2)
      in
      let jj2 = j - 1 in
      let jj3 = j in
      let jj4 =
        let x = line.(j).elem in
        let rec same_value j =
          if j >= Array.length line then j - 1
          else if line.(j).elem = x then same_value (j + 1)
          else j - 1
        in
        same_value (j + 1)
      in
      let (ii, jj1, jj2, jj3, jj4) =
        find_block_with_parents t i jj1 jj2 jj3 jj4
      in
      if jj4 < j2 && jj2 < jj3 then
        do exch_blocks t ii i jj1 jj2 jj3 jj4; return loop (jj4 + 1)
      else j1
  in
  loop (j1 + 1)
;

value push_to_left d t i j1 j2 =
  let line = t.(i) in
  let rec loop j =
    if j = j1 then j + 1
    else
      let jj1 =
        let x = line.(j).elem in
        let rec same_value j =
          if j < 0 then 0
          else if line.(j).elem = x then same_value (j - 1)
          else j + 1
        in
        same_value (j - 1)
      in
      let jj2 = j in
      let jj3 = j + 1 in
      let jj4 =
        let x = line.(j + 1).elem in
        let rec same_value j =
          if j >= Array.length line then j - 1
          else if line.(j).elem = x then same_value (j + 1)
          else j - 1
        in
        same_value (j + 2)
      in
      let (ii, jj1, jj2, jj3, jj4) =
        find_block_with_parents t i jj1 jj2 jj3 jj4
      in
      if jj1 > j1 && jj2 < jj3 then
        do exch_blocks t ii i jj1 jj2 jj3 jj4; return loop (jj1 - 1)
      else j2
  in
  loop (j2 - 1)
;

value fill_gap d t i j1 j2 =
  let t1 =
    let t1 = Array.copy t.table in
    do for i = 0 to Array.length t.table - 1 do
         t1.(i) := Array.copy t.table.(i);
         for j = 0 to Array.length t1.(i) - 1 do
           t1.(i).(j) :=
             {elem = t.table.(i).(j).elem; span = t.table.(i).(j).span};
         done;
       done;
    return t1
  in
  let j1 = push_to_right d t1 i j1 j2 in
  let j2 = push_to_left d t1 i j1 j2 in
  if j1 = j2 - 1 then
    let line = t1.(i-1) in
    let x = line.(j1).span in
    let y = line.(j2).span in
    do let rec loop j =
         if j >= Array.length line then ()
         else if line.(j).span = y then
           do line.(j).span := x;
              if i > 0 then t1.(i - 1).(j).span := t1.(i - 1).(j - 1).span
              else ();
           return loop (j + 1)
         else ()
       in
       loop j2;
    return Some ({table = t1}, True)
  else None
;

value treat_gaps d t =
  let i = Array.length t.table - 1 in
  let rec loop t j =
    let line = t.table.(i) in
    if j = Array.length line then t
    else
      match line.(j).elem with
      [ Elem _ as y ->
          if y = line.(j - 1).elem then loop t (j + 1)
          else
            let rec loop1 t j1 =
              if j1 < 0 then loop t (j + 1)
              else if y = line.(j1).elem then
                match fill_gap d t i j1 j with
                [ Some (t, ok) -> if ok then loop t 2 else loop t (j + 1)
                | None -> loop t (j + 1) ]
              else loop1 t (j1 - 1)
            in
            loop1 t (j - 2)
      | _ -> loop t (j + 1) ]
  in
  if Array.length t.table.(i) = 1 then t else loop t 2
;

value table_of_dag d =
  let a = ancestors d in
  let r = group_by_common_children d a in
  let t = {table = [| Array.of_list r |]} in
  let rec loop t =
    let (t, new_row) = treat_new_row d t in
    if List.for_all (fun x -> x.elem = Nothing) new_row then t
    else
      let t = {table = Array.append t.table [| Array.of_list new_row |]} in
      let _ = equilibrate t in
      let _ = group_elem t in
      let _ = group_ghost t in
      let _ = group_children t in
      let _ = group_span_by_common_children d t in
      let t = treat_gaps d t in
      loop t
  in
  loop t
;

(* invert *)

value invert_dag d =
  let d = {dag = Array.copy d.dag} in
  do for i = 0 to Array.length d.dag - 1 do
       let n = d.dag.(i) in
       d.dag.(i) :=
         {pare = List.map (fun x -> x) n.chil; valu = n.valu;
          chil = List.map (fun x -> x) n.pare};
     done;
  return d
;

value invert_table t =
  let t' = {table = Array.copy t.table} in
  let len = Array.length t.table in
  do for i = 0 to len - 1 do
       t'.table.(i) :=
         Array.init (Array.length t.table.(0))
           (fun j ->
              let d = t.table.(len - 1 - i).(j) in
              {elem = d.elem; span = d.span});
       if i < len - 1 then
         for j = 0 to Array.length t'.table.(i) - 1 do
           t'.table.(i).(j).span := t.table.(len - 2 - i).(j).span;
         done
       else ();
     done;
  return t'
;

(* test *)

value map_dag f d =
  let a =
    Array.map (fun d -> {pare = d.pare; valu = f d.valu; chil = d.chil}) d.dag
  in
  {dag = a}
;

value tag_dag d =
  let c = ref 'A' in
  map_dag
    (fun v ->
       let v = c.val in
       do c.val :=
            if c.val = 'Z' then 'a'
            else if c.val = 'z' then '1'
            else Char.chr (Char.code c.val + 1);
       return v)
    d
;

open Printf;

value print_table print_newline print_elem print_span t =
  for i = 0 to Array.length t.table - 1 do
    for j = 0 to Array.length t.table.(i) - 1 do
      print_elem t.table.(i).(j).elem;
    done;
    print_newline ();
    if i < Array.length t.table - 1 then
      do for j = 0 to Array.length t.table.(i) - 1 do
           print_span i j t.table.(i).(j).span;
         done;
         print_newline ();
      return ()
    else ();
  done
;

value print_char_table d t =
  let print_elem =
    fun
    [ Elem e -> eprintf "  %c" (d.dag.(int_of_idag e).valu)
    | Ghost x -> eprintf "  |" (*int_of_ghost_id x*)
    | Nothing -> eprintf "   " ]
  in
(*
  let print_span i j r =
    let n = int_of_span_id r in
    let c = Char.chr (Char.code 'a' + n mod 26) in
    eprintf "*%c" c
  in
*)
  let print_span i j r =
    if j > 0 && t.table.(i).(j-1).span = r then eprintf "---"
    else eprintf "  -"
  in
(**)
  print_table prerr_newline print_elem print_span t
;

(* main *)

value print_dag conf base d =
  let title _ =
    Wserver.wprint "%s" (Util.capitale (Util.transl conf "tree"))
  in
  let t = table_of_dag d in
  let print_indi ip =
    let p = poi base ip in
    do Wserver.wprint "%s" (Util.referenced_person_title_text conf base p);
       Wserver.wprint "%s" (Date.short_dates_text conf base p);
    return ()
  in
(*
do let d = tag_dag d in print_char_table d (table_of_dag d); flush stderr; return
*)
  do Util.header_no_page_title conf title;
     print_html_table conf base print_indi False d t;
     Util.trailer conf;
  return ()
;

value print conf base =
  let d = make_dag base (get_dag_elems conf base) in
  print_dag conf base d
;
