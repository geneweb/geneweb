(* $Id: dag2html.ml,v 3.40 2001-02-01 12:51:18 ddr Exp $ *)

(* Warning: this data structure for dags is not satisfactory, its
   consistency must always be checked, resulting on a complicated
   code difficult to debug *)

type dag 'a = { dag : mutable array (node 'a) }
and node 'a =
  { pare : mutable list idag; valu : 'a; chil : mutable list idag }
and idag = 'x
;

external int_of_idag : idag -> int = "%identity";
external idag_of_int : int -> idag = "%identity";

type table 'a = { table : mutable array (array (data 'a)) }
and data 'a = { elem : mutable elem 'a; span : mutable span_id }
and elem 'a = [ Elem of 'a | Ghost of ghost_id | Nothing ]
and span_id = 'x
and ghost_id = 'x
;

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

(* creating the html table structure *)

type align = [ LeftA | CenterA | RightA ];
type table_data = [ TDstring of string | TDhr of align ];
type html_table = array (array (int * align * table_data));

value html_table_struct indi_txt phony d t =
  let phony =
    fun
    [ Elem e -> phony d.dag.(int_of_idag e)
    | Ghost _ -> False
    | Nothing -> True ]
  in
  let jlast = Array.length t.table.(0) - 1 in
  let elem_txt =
    fun
    [ Elem e -> indi_txt d.dag.(int_of_idag e)
    | Ghost _ -> "|"
    | Nothing -> "&nbsp;" ]
  in
  let bar_txt =
    fun
    [ Elem _ | Ghost _ -> "|"
    | Nothing -> "&nbsp;" ]
  in
  let all_empty i =
    loop 0 where rec loop j =
      if j = Array.length t.table.(i) then True
      else
        match t.table.(i).(j).elem with
        [ Nothing -> loop (j + 1)
        | e -> if phony e then loop (j + 1) else False ]
  in
  let line_elem_txt i =
    let les =
      loop [] 0 where rec loop les j =
        if j = Array.length t.table.(i) then les
        else
          let x = t.table.(i).(j).elem in
          let next_j =
            loop (j + 1) where rec loop j =
              if j = Array.length t.table.(i) then j
              else if t.table.(i).(j).elem = x then loop (j + 1)
              else j
          in
          let colspan = 3 * (next_j - j) in
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          let les =
            let s =
              if t.table.(i).(j).elem = Nothing then "&nbsp;"
              else elem_txt x
            in
            [(colspan - 2, CenterA, TDstring s) :: les]
          in
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          loop les next_j
    in
    Array.of_list (List.rev les)
  in
  let vbars_txt k i =
    let les =
      loop [] 0 where rec loop les j =
        if j = Array.length t.table.(i) then les
        else
          let x = t.table.(i).(j).elem in
          let next_j =
            loop (j + 1) where rec loop j =
              if j = Array.length t.table.(i) then j
              else if t.table.(i).(j).elem = x then loop (j + 1)
              else j
          in
          let colspan = 3 * (next_j - j) in
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          let les =
            let s =
              if k > 0 && t.table.(k - 1).(j).elem = Nothing ||
                 t.table.(k).(j).elem = Nothing then
                "&nbsp;"
              else if phony t.table.(i).(j).elem then "&nbsp;"
              else bar_txt x
            in
            [(colspan - 2, CenterA, TDstring s) :: les]
          in
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          loop les next_j
    in
    Array.of_list (List.rev les)
  in
  let alone_bar_txt i =
    let les =
      loop [] 0 where rec loop les j =
        if j = Array.length t.table.(i) then les
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
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          let les =
            if t.table.(i).(j).elem = Nothing ||
               t.table.(i + 1).(j).elem = Nothing then
              [(colspan, LeftA, TDstring "&nbsp;") :: les]
            else
              let s =
                let all_ph =
                  loop j where rec loop j =
                    if j = next_j then True
                    else if phony t.table.(i + 1).(j).elem then loop (j + 1)
                    else False
                in
                if all_ph then "&nbsp;" else "|"
              in
              [(colspan, CenterA, TDstring s) :: les]
          in
          let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
          loop les next_j
    in
    Array.of_list (List.rev les)
  in
  let exist_several_branches i k =
    loop 0 where rec loop j =
      if j = Array.length t.table.(i) then False
      else
        let x = t.table.(i).(j).span in
        let e = t.table.(k).(j).elem in
        let rec loop1 j =
          if j = Array.length t.table.(i) then False
          else if t.table.(i).(j).elem = Nothing then loop j
          else if t.table.(i).(j).span <> x then loop j
          else if t.table.(k).(j).elem <> e then True
          else loop1 (j + 1)
        in
        loop1 (j + 1)
  in
  let hbars_txt i k =
    let les =
      loop [] 0 where rec loop les j =
        if j = Array.length t.table.(i) then les
        else
          let next_j =
            let e = t.table.(i).(j).elem in
            let x = t.table.(i).(j).span in
            let rec loop j =
              if j = Array.length t.table.(i) then j
              else if e = Nothing && t.table.(i).(j).elem = Nothing then
                loop (j + 1)
              else if t.table.(i).(j).span = x then loop (j + 1)
              else j
            in
            loop (j + 1)
          in
          let rec loop1 les l =
            if l = next_j then loop les next_j
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
(* happens sometimes: should be debugged *)
do if next_l > next_j then do Printf.eprintf "assert false i %d k %d l %d next_l %d next_j %d\n" i k l next_l next_j; flush stderr; return () else (); return
let next_l = min next_l next_j in
(*
              do assert (next_l <= next_j); return
*)
              let colspan = 3 * (next_l - l) - 2 in
              let les =
                match (t.table.(i).(l).elem, t.table.(i + 1).(l).elem) with
                [ (Nothing, _) | (_, Nothing) ->
                    [(colspan + 2, LeftA, TDstring "&nbsp;") :: les]
                | _ ->
                    let ph s =
                      if phony t.table.(k).(l).elem then TDstring "&nbsp;"
                      else s
                    in
                    if l = j && next_l = next_j then
                      let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
                      let s = ph (TDstring "|") in
                      let les = [(colspan, CenterA, s) :: les] in
                      let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
                      les
                    else if l = j then
                      let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
                      let s = ph (TDhr RightA) in
                      let les = [(colspan, RightA, s) :: les] in
                      let s = ph (TDhr CenterA) in
                      let les = [(1, LeftA, s) :: les] in
                      les
                    else if next_l = next_j then
                      let s = ph (TDhr CenterA) in
                      let les = [(1, LeftA, s) :: les] in
                      let s = ph (TDhr LeftA) in
                      let les = [(colspan, LeftA, s) :: les] in
                      let les = [(1, LeftA, TDstring "&nbsp;") :: les] in
                      les
                    else
                      let s = ph (TDhr CenterA) in
                      [(colspan + 2, LeftA, s) :: les] ]
              in loop1 les next_l
          in
          loop1 les j
    in
    Array.of_list (List.rev les)
  in
  let hts =
    loop [] 0 where rec loop hts i =
      if i = Array.length t.table then hts
      else if i = Array.length t.table - 1 && all_empty i then hts
      else
        let hts = [line_elem_txt i :: hts] in
        let hts =
          if i < Array.length t.table - 1 then
            let hts = [vbars_txt (i + 1) i :: hts] in
            let hts =
              if exist_several_branches i i then
                [alone_bar_txt i; hbars_txt i i :: hts]
              else hts
            in
            let hts =
              if exist_several_branches i (i + 1)
              && (i < Array.length t.table - 2 || not (all_empty (i + 1))) then
                [vbars_txt (i + 1) (i + 1); hbars_txt i (i + 1) :: hts]
              else hts
            in
            hts
          else hts
        in
        loop hts (i + 1)
        
  in
  Array.of_list (List.rev hts)
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

value copy_data d = {elem = d.elem; span = d.span};

value insert_columns t nb j =
  let t1 = Array.create (Array.length t.table) [| |] in
  do for i = 0 to Array.length t.table - 1 do
       let line = t.table.(i) in
       let line1 = Array.create (Array.length line + nb) line.(0) in
       do t1.(i) := line1; return
       let rec loop k =
         if k = Array.length line then ()
         else
           do if k < j then line1.(k) := copy_data line.(k)
              else if k = j then
                for r = 0 to nb do line1.(k + r) := copy_data line.(k); done
              else line1.(k + nb) := copy_data line.(k);
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
                   if cnt = 1 then [d :: list]
                   else [copy_data d :: loop (cnt - 1) list]
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
                 let t =
                   loop c t j where rec loop cc t j =
                     if cc = 0 then t
                     else
                       let t = insert_columns t to_add j in
                       loop (cc - 1) t (j + to_add + 1)
                 in
                 (t, [(x, parent_colspan) :: parents], j + parent_colspan))
              (t, [], j) parents
          in
          let parents = List.rev parents in
          let parents_colspan = parent_colspan * List.length parents in
          let children_colspan = List.length children in
          let g = gcd parents_colspan children_colspan in
          let (t, j) =
            let cnt = children_colspan / g in
            List.fold_left
              (fun (t, j) (_, c) ->
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

(* equilibrate:
   in the last line, for all elem A, make fall all As, which are located at
   its right side above, to its line,
                             A             |
   i.e. transform all        . into        |
                      A.......      A......A
*)

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

(* group_elem:
   transform all x y into x x
                 A A      A A *)

value group_elem t =
  for i = 0 to Array.length t.table - 2 do
    for j = 1 to Array.length t.table.(0) - 1 do
      let x =
        match t.table.(i + 1).(j - 1).elem with
        [ Elem x -> Some x
        | _ -> None ]
      in
      let y =
        match t.table.(i + 1).(j).elem with
        [ Elem x -> Some x
        | _ -> None ]
      in
      match (x, y) with
      [ (Some x, Some y) when x = y ->
          t.table.(i).(j).span := t.table.(i).(j - 1).span
      | _ -> () ];
    done;
  done
;

(* group_ghost:
                 x  x       x  x           |a |a      |a |a
   transform all |a |b into |a |a and all  x  y  into x  x
                 y  z       y  y           A  A       A  A  *)

value group_ghost t =
  for i = 0 to Array.length t.table - 2 do
    for j = 1 to Array.length t.table.(0) - 1 do
      match (t.table.(i + 1).(j - 1).elem, t.table.(i + 1).(j).elem) with
      [ (Ghost x, Ghost _) ->
          if t.table.(i).(j - 1).span = t.table.(i).(j).span then
            t.table.(i + 1).(j) :=
              {elem = Ghost x; span = t.table.(i + 1).(j - 1).span}
          else ()
      | _ -> () ];
      match (t.table.(i).(j - 1).elem, t.table.(i).(j).elem) with
      [ (Ghost x, Ghost _) ->
          if t.table.(i + 1).(j - 1).elem = t.table.(i + 1).(j).elem then
            t.table.(i).(j) :=
              {elem = Ghost x; span = t.table.(i).(j - 1).span}
          else ()
      | _ -> () ];
    done;
  done
;

(* group_children:
   transform all A A into A A
                 x y      x x *)

value group_children t =
  for i = 0 to Array.length t.table - 1 do
    let line = t.table.(i) in
    let len = Array.length line in
    for j = 1 to len - 1 do
      if line.(j).elem = line.(j - 1).elem && line.(j).elem <> Nothing then
        line.(j).span := line.(j - 1).span
      else ();
    done;
  done
;

(* group_span_by_common_children:
   in the last line, transform all
     A B into A B
     x y      x x
   if A and B have common children *)

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

value mirror_block t i1 i2 j1 j2 =
  for i = i1 to i2 do
    let line = t.(i) in
    let rec loop j1 j2 =
      if j1 >= j2 then ()
      else
        let v = line.(j1) in
        do line.(j1) := line.(j2); line.(j2) := v; return
        loop (j1 + 1) (j2 - 1)
    in
    loop j1 j2;
  done
;

value exch_blocks t i1 i2 j1 j2 j3 j4 =
  for i = i1 to i2 do
    let line = t.(i) in
    let saved = Array.copy line in
    do for j = j1 to j2 do line.(j4 - j2 + j) := saved.(j); done;
       for j = j3 to j4 do line.(j1 - j3 + j) := saved.(j); done;
    return ();
  done
;

value find_block_with_parents t i jj1 jj2 jj3 jj4 =
  loop i jj1 jj2 jj3 jj4 where rec loop ii jj1 jj2 jj3 jj4 =
    let (nii, njj1, njj2, njj3, njj4) =
      find_same_parents t i jj1 jj2 jj3 jj4
    in
    if nii <> ii || njj1 <> jj1 || njj2 <> jj2 || njj3 <> jj3 ||
       njj4 <> jj4 then
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
      let ini_jj1 =
        match line.(j - 1).elem with
        [ Nothing -> j - 1
        | x ->
            let rec same_value j =
              if j < 0 then 0
              else if line.(j).elem = x then same_value (j - 1)
              else j + 1
            in
            same_value (j - 2) ]
      in
      let jj1 = ini_jj1 in
      let jj2 = j - 1 in
      let jj3 = j in
      let jj4 =
        match line.(j).elem with
        [ Nothing -> j
        | x ->
            let rec same_value j =
              if j >= Array.length line then j - 1
              else if line.(j).elem = x then same_value (j + 1)
              else j - 1
            in
            same_value (j + 1) ]
      in
      let (ii, jj1, jj2, jj3, jj4) =
        find_block_with_parents t i jj1 jj2 jj3 jj4
      in
      if jj4 < j2 && jj2 < jj3 then
        do exch_blocks t ii i jj1 jj2 jj3 jj4; return loop (jj4 + 1)
      else if jj4 < j2 && jj1 = ini_jj1 && jj2 <= jj4 then
        do mirror_block t ii i jj1 jj4; return loop (jj4 + 1)
      else j - 1
  in
  loop (j1 + 1)
;

value push_to_left d t i j1 j2 =
  let line = t.(i) in
  let rec loop j =
    if j = j1 then j + 1
    else
      let jj1 =
        match line.(j).elem with
        [ Nothing -> j
        | x ->
            let rec same_value j =
              if j < 0 then 0
              else if line.(j).elem = x then same_value (j - 1)
              else j + 1
            in
            same_value (j - 1) ]
      in
      let jj2 = j in
      let jj3 = j + 1 in
      let ini_jj4 =
        match line.(j + 1).elem with
        [ Nothing -> j + 1
        | x ->
            let rec same_value j =
              if j >= Array.length line then j - 1
              else if line.(j).elem = x then same_value (j + 1)
              else j - 1
            in
            same_value (j + 2) ]
      in
      let jj4 = ini_jj4 in
      let (ii, jj1, jj2, jj3, jj4) =
        find_block_with_parents t i jj1 jj2 jj3 jj4
      in
      if jj1 > j1 && jj2 < jj3 then
        do exch_blocks t ii i jj1 jj2 jj3 jj4; return loop (jj1 - 1)
      else if jj1 > j1 && jj4 = ini_jj4 && jj3 >= jj1 then
        do mirror_block t ii i jj1 jj4; return loop (jj1 - 1)
      else j + 1
  in
  loop (j2 - 1)
;

value fill_gap d t i j1 j2 =
  let t1 =
    let t1 = Array.copy t.table in
    do for i = 0 to Array.length t.table - 1 do
         t1.(i) := Array.copy t.table.(i);
         for j = 0 to Array.length t1.(i) - 1 do
           t1.(i).(j) := copy_data t.table.(i).(j);
         done;
       done;
    return t1
  in
  let j2 = push_to_left d t1 i j1 j2 in
  let j1 = push_to_right d t1 i j1 j2 in
  if j1 = j2 - 1 then
    let line = t1.(i - 1) in
    let x = line.(j1).span in
    let y = line.(j2).span in
    do let rec loop y j =
         if j >= Array.length line then ()
         else if
           line.(j).span = y || t1.(i).(j).elem = t1.(i).(j - 1).elem then
           let y = line.(j).span in
           do line.(j).span := x;
              if i > 0 then t1.(i - 1).(j).span := t1.(i - 1).(j - 1).span
              else ();
           return loop y (j + 1)
         else ()
       in
       loop y j2;
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

value group_span_last_row t =
  let row = t.table.(Array.length t.table - 1) in
  let rec loop i =
    if i >= Array.length row then ()
    else
      do match row.(i).elem with
         [ Elem _ | Ghost _ as x ->
             if x = row.(i - 1).elem then row.(i).span := row.(i - 1).span
             else ()
         | _ -> () ];
      return loop (i + 1)
  in
  loop 1
;

value tablify no_optim no_group d =
  let a = ancestors d in
  let r = group_by_common_children d a in
  let t = {table = [| Array.of_list r |]} in
  let rec loop t =
    let (t, new_row) = treat_new_row d t in
    if List.for_all (fun x -> x.elem = Nothing) new_row then t
    else
      let t = {table = Array.append t.table [| Array.of_list new_row |]} in
      let t =
        if no_group then
          let _ = group_elem t in
          let _ = group_ghost t in
          let _ = group_children t in
          t
        else
          let _ = if no_optim then () else equilibrate t in
          let _ = group_elem t in
          let _ = group_ghost t in
          let _ = group_children t in
          let _ = group_span_by_common_children d t in
          let t = if no_optim then t else treat_gaps d t in
          let _ = group_span_last_row t in t
      in
      loop t
  in
  loop t
;

value fall d t =
  for i = 1 to Array.length t.table - 1 do
    let line = t.table.(i) in
    let rec loop j =
      if j = Array.length line then ()
      else
        match line.(j).elem with
        [ Ghost x ->
            let j2 =
              loop (j + 1) where rec loop j =
                if j = Array.length line then j - 1
                else
                  match line.(j).elem with
                  [ Ghost y when y = x -> loop (j + 1)
                  | _ -> j - 1 ]
            in
            let i1 =
              loop (i - 1) where rec loop i =
                if i < 0 then i + 1
                else
                  let line = t.table.(i) in
                  if (j = 0 || line.(j - 1).span <> line.(j).span) &&
                     (j2 = Array.length line - 1 ||
                      line.(j2 + 1).span <> line.(j2).span) then
                    loop (i - 1)
                  else i + 1
            in
            let i1 =
              if i1 = i then i1
              else if i1 = 0 then i1
              else if t.table.(i1).(j).elem = Nothing then i1
              else i
            in 
            do if i1 < i then
                 do for k = i downto i1 + 1 do
                      for j = j to j2 do
                        t.table.(k).(j).elem := t.table.(k - 1).(j).elem;
                        if k < i then
                          t.table.(k).(j).span := t.table.(k - 1).(j).span
                        else ();
                      done;
                    done;
                    for l = j to j2 do
                      if i1 = 0 || t.table.(i1 - 1).(l).elem = Nothing then
                        t.table.(i1).(l).elem := Nothing
                      else
                        t.table.(i1).(l) :=
                          if l = j ||
                             t.table.(i1 - 1).(l - 1).span <>
                               t.table.(i1 - 1).(l).span then
                            {elem = Ghost (new_ghost_id ());
                             span = new_span_id ()}
                          else copy_data t.table.(i1).(l - 1);
                    done;
                 return ()
               else ();
            return loop (j2 + 1)
        | _ -> loop (j + 1) ]
    in
    loop 0;
  done
;

value fall2_cool_right t i1 i2 i3 j1 j2 =
  let span = t.table.(i2 - 1).(j1).span in
  do for i = i2 - 1 downto 0 do
       for j = j1 to j2 - 1 do
         t.table.(i).(j) :=
           if i - i2 + i1 >= 0 then t.table.(i - i2 + i1).(j)
           else {elem = Nothing; span = new_span_id ()};
       done;
     done;
     for i = Array.length t.table - 1 downto 0 do
       for j = j2 to Array.length t.table.(i) - 1 do
         t.table.(i).(j) :=
           if i - i2 + i1 >= 0 then t.table.(i - i2 + i1).(j)
           else {elem = Nothing; span = new_span_id ()};
       done;
     done;
     let old_span = t.table.(i2 - 1).(j1).span in
(*
do Printf.eprintf "fall2_coll_right i1 %d i2 %d j %d i3 %d span %d old_span %d\n" i1 i2 j i3 (int_of_span_id span) (int_of_span_id old_span); flush stderr; return
*)
     loop j1 where rec loop j =
       if j = Array.length t.table.(i2 - 1) then ()
       else if t.table.(i2 - 1).(j).span = old_span then
         do t.table.(i2 - 1).(j).span := span; return loop (j + 1)
       else ();
  return ()  
;

value fall2_cool_left t i1 i2 i3 j1 j2 =
  let span = t.table.(i2 - 1).(j2).span in
  do for i = i2 - 1 downto 0 do
       for j = j1 + 1 to j2 do
         t.table.(i).(j) :=
           if i - i2 + i1 >= 0 then t.table.(i - i2 + i1).(j)
           else {elem = Nothing; span = new_span_id ()};
       done;
     done;
     for i = Array.length t.table - 1 downto 0 do
       for j = j1 downto 0 do
         t.table.(i).(j) :=
           if i - i2 + i1 >= 0 then t.table.(i - i2 + i1).(j)
           else {elem = Nothing; span = new_span_id ()};
       done;
     done;
     let old_span = t.table.(i2 - 1).(j2).span in
     loop j2 where rec loop j =
       if j < 0 then ()
       else if t.table.(i2 - 1).(j).span = old_span then
         do t.table.(i2 - 1).(j).span := span; return loop (j - 1)
       else ();
  return ()  
;

value do_fall2_right t i1 i2 j1 j2 =
  let i3 =
    loop_i (Array.length t.table - 1) where rec loop_i i =
      if i < 0 then 0
      else
        loop_j j2 where rec loop_j j =
          if j = Array.length t.table.(i) then loop_i (i - 1)
          else
            match t.table.(i).(j).elem with
            [ Nothing -> loop_j (j + 1)
            | _ -> i + 1 ]
  in
  let new_height = i3 + i2 - i1 in
  let t =
    if new_height > Array.length t.table then
      loop (new_height - Array.length t.table) t where rec loop cnt t =
        if cnt = 0 then t
        else
          let new_line =
            Array.init (Array.length t.table.(0))
              (fun i -> {elem = Nothing; span = new_span_id ()})
          in
          let t = {table = Array.append t.table [| new_line |]} in
          loop (cnt - 1) t
    else t
  in
  do fall2_cool_right t i1 i2 i3 j1 j2; return t
;

value do_fall2_left t i1 i2 j1 j2 =
  let i3 =
    loop_i (Array.length t.table - 1) where rec loop_i i =
      if i < 0 then 0
      else
        loop_j j1 where rec loop_j j =
          if j < 0 then loop_i (i - 1)
          else
            match t.table.(i).(j).elem with
            [ Nothing -> loop_j (j - 1)
            | _ -> i + 1 ]
  in
  let new_height = i3 + i2 - i1 in
  let t =
    if new_height > Array.length t.table then
      loop (new_height - Array.length t.table) t where rec loop cnt t =
        if cnt = 0 then t
        else
          let new_line =
            Array.init (Array.length t.table.(0))
              (fun i -> {elem = Nothing; span = new_span_id ()})
          in
          let t = {table = Array.append t.table [| new_line |]} in
          loop (cnt - 1) t
    else t
  in
  do fall2_cool_left t i1 i2 i3 j1 j2; return t
;

value try_fall2_right t i j =
  match t.table.(i).(j).elem with
  [ Ghost _ ->
      let i1 =
        loop (i - 1) where rec loop i =
          if i < 0 then 0
          else
            match t.table.(i).(j).elem with
            [ Ghost _ -> loop (i - 1)
            | _ -> i + 1 ]
      in
      let separated1 =
        loop (i1 - 1) where rec loop i =
          if i < 0 then True
          else if j > 0
               && t.table.(i).(j - 1).span = t.table.(i).(j).span then False
          else loop (i - 1)
      in
      let j2 =
        let x = t.table.(i).(j).span in
        loop (j + 1) where rec loop j2 =
          if j2 = Array.length t.table.(i) then j2
          else
            match t.table.(i).(j2) with
            [ {elem = Ghost _; span = y} when y = x -> loop (j2 + 1)
            | _ -> j2 ]
      in
      let separated2 =
        loop (i + 1) where rec loop i =
          if i = Array.length t.table then True
          else if j2 = Array.length t.table.(i) then False
          else if t.table.(i).(j2 - 1).span = t.table.(i).(j2).span then False
          else loop (i + 1)
      in
      if not separated1 || not separated2 then None
      else Some (do_fall2_right t i1 (i + 1) j j2)
  | _ -> None ]
;

value try_fall2_left t i j =
  match t.table.(i).(j).elem with
  [ Ghost _ ->
      let i1 =
        loop (i - 1) where rec loop i =
          if i < 0 then 0
          else
            match t.table.(i).(j).elem with
            [ Ghost _ -> loop (i - 1)
            | _ -> i + 1 ]
      in
      let separated1 =
        loop (i1 - 1) where rec loop i =
          if i < 0 then True
          else if j < Array.length t.table.(i) - 1
               && t.table.(i).(j).span = t.table.(i).(j + 1).span then False
          else loop (i - 1)
      in
      let j1 =
        let x = t.table.(i).(j).span in
        loop (j - 1) where rec loop j1 =
          if j1 < 0 then j1
          else
            match t.table.(i).(j1) with
            [ {elem = Ghost _; span = y} when y = x -> loop (j1 - 1)
            | _ -> j1 ]
      in
      let separated2 =
        loop (i + 1) where rec loop i =
          if i = Array.length t.table then True
          else if j1 < 0 then False
          else if t.table.(i).(j1).span = t.table.(i).(j1 + 1).span then False
          else loop (i + 1)
      in
      if not separated1 || not separated2 then None
      else Some (do_fall2_left t i1 (i + 1) j1 j)
  | _ -> None ]
;

value fall2_right t =
  loop_i (Array.length t.table - 1) t where rec loop_i i t =
    if i <= 0 then t
    else
      loop_j (Array.length t.table.(i) - 2) t where rec loop_j j t =
        if j < 0 then loop_i (i - 1) t
        else
          match try_fall2_right t i j with
          [ Some t -> loop_i (Array.length t.table - 1) t
          | None -> loop_j (j - 1) t ]
;

value fall2_left t =
  loop_i (Array.length t.table - 1) t where rec loop_i i t =
    if i <= 0 then t
    else
      loop_j 1 t where rec loop_j j t =
        if j >= Array.length t.table.(i) then loop_i (i - 1) t
        else
          match try_fall2_left t i j with
          [ Some t -> loop_i (Array.length t.table - 1) t
          | None -> loop_j (j + 1) t ]
;

value top_adjust t =
  let di =
    loop 0 where rec loop i =
      if i = Array.length t.table then i
      else
        let rec loop_j j =
          if j = Array.length t.table.(i) then loop (i + 1)
          else if t.table.(i).(j).elem <> Nothing then i
          else loop_j (j + 1)
        in
        loop_j 0
  in
  if di > 0 then
    do for i = 0 to Array.length t.table - 1 - di do
         t.table.(i) := t.table.(i + di);
       done;
    return {table = Array.sub t.table 0 (Array.length t.table - di)}
  else t
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

(* main *)

value table_of_dag no_optim invert no_group d =
  let d = if invert then invert_dag d else d in
  let t = tablify no_optim no_group d in
  let t = if invert then invert_table t else t in
  let _ = fall () t in
  let t = fall2_right t in
  let t = fall2_left t in
  let t = top_adjust t in
  t
;

value html_table_of_dag indi_txt phony invert no_group d =
  let t = table_of_dag False invert no_group d in
  html_table_struct indi_txt phony d t
;
