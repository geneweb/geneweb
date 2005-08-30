(* camlp4r *)
(* $Id: perso.ml,v 4.200 2005-08-30 17:56:23 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Gutil;
open Util;
open Config;
open TemplAst;

value max_im_wid = 240;
value max_im_hei = 240;
value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value has_children base u =
  List.exists
    (fun ifam -> let des = doi base ifam in Array.length des.children > 0)
    (Array.to_list u.family)
;

value string_of_marriage_text conf base fam =
  let marriage = Adef.od_of_codate fam.marriage in
  let marriage_place = sou base fam.marriage_place in
  let s =
    match marriage with
    [ Some d -> " " ^ Date.string_of_ondate conf d
    | _ -> "" ]
  in
  match marriage_place with
  [ "" -> s
  | _ -> s ^ ", " ^ string_with_macros conf [] marriage_place ^ "," ]
;

value string_of_title conf base and_txt p (nth, name, title, places, dates) =
  let href =
    "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
      code_varenv (sou base (List.hd places))
  in
  let (tit, est) = (sou base title, sou base (List.hd places)) in
  let s = tit ^ " " ^ est in
  let b = Buffer.create 50 in
  do {
    Buffer.add_string b (geneweb_link conf href s);
    let rec loop places =
      do {
        match places with
        [ [] -> ()
        | [_] -> Printf.bprintf b "\n%s " and_txt
        | _ -> Buffer.add_string b ",\n" ];
        match places with
        [ [place :: places] ->
            let href =
              "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
                code_varenv (sou base place)
            in
            let est = sou base place in
            do {
              Buffer.add_string b (geneweb_link conf href est);
              loop places
            }
        | _ -> () ]
      }
    in
    loop (List.tl places);
    let paren =
      match (nth, dates, name) with
      [ (n, _, _) when n > 0 -> True
      | (_, _, Tname _) -> True
      | (_, [(Some _, _) :: _], _) -> authorized_age conf base p
      | _ -> False ]
    in
    if paren then Buffer.add_string b "\n(" else ();
    let first =
      if nth > 0 then do {
        Buffer.add_string b
          (if nth >= 100 then string_of_int nth
           else transl_nth conf "nth" nth);
        False
      }
      else True
    in
    let first =
      match name with
      [ Tname n ->
          do {
            if not first then Buffer.add_string b " ," else ();
            Buffer.add_string b (sou base n);
            False
          }
      | _ -> first ]
    in
    if authorized_age conf base p && dates <> [(None, None)] then
      let _ =
        List.fold_left
          (fun first (date_start, date_end) ->
             do {
               if not first then Buffer.add_string b ",\n" else ();
               match date_start with
               [ Some d -> Buffer.add_string b (Date.string_of_date conf d)
               | None -> () ];
               match date_end with
               [ Some (Dgreg d _) ->
                   if d.month <> 0 then Buffer.add_string b " - "
                   else Buffer.add_string b "-"
               | _ -> () ];
               match date_end with
               [ Some d -> Buffer.add_string b (Date.string_of_date conf d)
               | None -> () ];
               False
             })
          first dates
      in
      ()
    else ();
    if paren then Buffer.add_string b ")" else ();
    Buffer.contents b
  }
;

value name_equiv n1 n2 =
  n1 = n2 || n1 = Tmain && n2 = Tnone || n1 = Tnone && n2 = Tmain
;

value nobility_titles_list conf p =
  let titles =
    List.fold_right
      (fun t l ->
         let t_date_start = Adef.od_of_codate t.t_date_start in
         let t_date_end = Adef.od_of_codate t.t_date_end in
         match l with
         [ [(nth, name, title, place, dates) :: rl]
           when
             not conf.is_rtl && nth = t.t_nth && name_equiv name t.t_name &&
             title = t.t_ident && place = t.t_place ->
             [(nth, name, title, place,
               [(t_date_start, t_date_end) :: dates]) ::
              rl]
         | _ ->
             [(t.t_nth, t.t_name, t.t_ident, t.t_place,
               [(t_date_start, t_date_end)]) ::
              l] ])
      p.titles []
  in
  List.fold_right
    (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
       match l with
       [ [(nth, name, title, places, dates) :: rl]
         when
           not conf.is_rtl && nth = t_nth && name_equiv name t_name &&
           title = t_ident && dates = t_dates ->
           [(nth, name, title, [t_place :: places], dates) :: rl]
       | _ -> [(t_nth, t_name, t_ident, [t_place], t_dates) :: l] ])
    titles []
;

(* obsolete; should be removed one day *)

value string_of_titles conf base cap and_txt p =
  let titles = nobility_titles_list conf p in
  List.fold_left
    (fun s t ->
       s ^ (if s = "" then "" else ",") ^ "\n" ^
       string_of_title conf base and_txt p t)
    "" titles
;

(* Version matching the Sosa number of the "ancestor" pages *)

value find_sosa_aux conf base a p =
  let tstab = Util.create_topological_sort conf base in
  let mark = Array.create base.data.persons.len False in
  let rec gene_find =
    fun
    [ [] -> Left []
    | [(z, ip) :: zil] ->
        if ip = a.cle_index then Right z
        else if mark.(Adef.int_of_iper ip) then gene_find zil
        else do {
          mark.(Adef.int_of_iper ip) := True;
          if tstab.(Adef.int_of_iper a.cle_index) <=
               tstab.(Adef.int_of_iper ip) then
            gene_find zil
          else
            let asc = aget conf base ip in
            match parents asc with
            [ Some ifam ->
                let cpl = coi base ifam in
                let z = Num.twice z in
                match gene_find zil with
                [ Left zil ->
                    Left [(z, father cpl); (Num.inc z 1, (mother cpl)) :: zil]
                | Right z -> Right z ]
            | None -> gene_find zil ]
        } ]
  in
  let rec find zil =
    match gene_find zil with
    [ Left [] -> None
    | Left zil -> find zil
    | Right z -> Some (z, p) ]
  in
  find [(Num.one, p.cle_index)]
;
(* Male version
value find_sosa_aux conf base a p =
  let mark = Array.create base.data.persons.len False in
  let rec find z ip =
    if ip = a.cle_index then Some z
    else if mark.(Adef.int_of_iper ip) then None
    else do {
      mark.(Adef.int_of_iper ip) := True;
      let asc = aget conf base ip in
      match asc.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let z = Num.twice z in
          match find z (father cpl) with
          [ Some z -> Some z
          | None -> find (Num.inc z 1) (mother cpl) ]
      | None -> None ]
    }
  in
  find Num.one p.cle_index
;
*)

value find_sosa conf base a sosa_ref_l =
  match Lazy.force sosa_ref_l with
  [ Some p ->
      if a.cle_index = p.cle_index then Some (Num.one, p)
      else
        let u = uget conf base a.cle_index in
        if has_children base u then find_sosa_aux conf base a p else None
  | None -> None ]
;

value max_ancestor_level conf base ip max_lev =
  let x = ref 0 in
  let mark = Array.create base.data.persons.len False in
  let rec loop level ip =
    if mark.(Adef.int_of_iper ip) then ()
    else do {
      mark.(Adef.int_of_iper ip) := True;
      x.val := max x.val level;
      if x.val = max_lev then ()
      else
        match parents (aget conf base ip) with
        [ Some ifam ->
            let cpl = coi base ifam in
            do {
              loop (succ level) (father cpl); loop (succ level) (mother cpl)
            }
        | _ -> () ]
    }
  in
  do { loop 0 ip; x.val }
;

value default_max_cousin_lev = 5;

value max_cousin_level conf base p =
  let max_lev =
    try int_of_string (List.assoc "max_cousins_level" conf.base_env) with
    [ Not_found | Failure _ -> default_max_cousin_lev ]
  in
  max_ancestor_level conf base p.cle_index max_lev + 1
;

value limit_anc_by_tree conf =
  match p_getint conf.base_env "max_anc_tree" with
  [ Some x -> max 1 x
  | None -> 7 ]
;

value limit_desc conf =
  match p_getint conf.base_env "max_desc_level" with
  [ Some x -> max 1 x
  | None -> 12 ]
;

value infinite = 10000;

value make_desc_level_table conf base max_level p =
  let mark = Array.create base.data.persons.len False in
  let levt = Array.create base.data.persons.len infinite in
  let rec fill ip u lev =
    if max_level == infinite && mark.(Adef.int_of_iper ip) then ()
    else do {
      mark.(Adef.int_of_iper ip) := True;
      if lev <= max_level then do {
        if lev < levt.(Adef.int_of_iper ip) then
          levt.(Adef.int_of_iper ip) := lev
        else ();
        Array.iter
          (fun ifam ->
             let ipl = (doi base ifam).children in
             Array.iter (fun ip -> fill ip (uget conf base ip) (succ lev)) ipl)
          u.family
      }
      else ()
    }
  in
  do { fill p.cle_index (uget conf base p.cle_index) 0; levt }
;

value desc_level_max conf base desc_level_table_l =
  let levt = Lazy.force desc_level_table_l in
  let x = ref 0 in
  do {
    for i = 0 to Array.length levt - 1 do {
      let lev = levt.(i) in
      if lev != infinite && x.val < lev then x.val := lev else ()
    };
    x.val
  }
;

value max_descendant_level conf base desc_level_table_l =
  min (limit_desc conf) (desc_level_max conf base desc_level_table_l)
;

(* ancestors by list *)

type generation_person =
  [ GP_person of Num.t and iper and option ifam
  | GP_same of Num.t and Num.t and iper
  | GP_interv of option (Num.t * Num.t * option (Num.t * Num.t))
  | GP_missing of Num.t and iper ]
;

value next_generation conf base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
         match gp with
         [ GP_person n ip _ ->
             let n_fath = Num.twice n in
             let n_moth = Num.inc n_fath 1 in
             let a = aget conf base ip in
             match parents a with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 [GP_person n_fath (father cpl) (Some ifam);
                  GP_person n_moth (mother cpl) (Some ifam) :: gpl]
             | None -> [GP_missing n ip :: gpl] ]
         | GP_interv None -> [gp :: gpl]
         | GP_interv (Some (n1, n2, x)) ->
             let x =
               match x with
               [ Some (m1, m2) -> Some (Num.twice m1, Num.twice m2)
               | None -> None ]
             in
             let gp = GP_interv (Some (Num.twice n1, Num.twice n2, x)) in
             [gp :: gpl]
         | _ -> gpl ])
      gpl []
  in
  let gpl =
    List.fold_left
      (fun gpl gp ->
         match gp with
         [ GP_person n ip _ ->
             let i = Adef.int_of_iper ip in
             let m = mark.(i) in
             if Num.eq m Num.zero then do { mark.(i) := n; [gp :: gpl] }
             else [GP_same n m ip :: gpl]
         | _ -> [gp :: gpl] ])
      [] gpl
  in
  List.rev gpl
;

value next_generation2 conf base mark gpl =
  let gpl =
    List.map
      (fun gp ->
         match gp with
         [ GP_same n m ip ->
             GP_interv (Some (n, Num.inc n 1, Some (m, Num.inc m 1)))
         | _ -> gp ])
      gpl
  in
  let gpl = next_generation conf base mark gpl in
  List.fold_right
    (fun gp gpl ->
       match (gp, gpl) with
       [ (GP_interv (Some (n1, n2, x)),
          [GP_interv (Some (n3, n4, y)) :: gpl1]) ->
           if Num.eq n2 n3 then
             let z =
               match (x, y) with
               [ (Some (m1, m2), Some (m3, m4)) ->
                   if Num.eq m2 m3 then Some (m1, m4) else None
               | _ -> None ]
             in
             [GP_interv (Some (n1, n4, z)) :: gpl1]
           else [GP_interv None :: gpl1]
       | (GP_interv _, [GP_interv _ :: gpl]) ->
           [GP_interv None :: gpl]
       | (GP_missing _ _, gpl) -> gpl
       | _ -> [gp :: gpl] ])
    gpl []
;

value sosa_is_present all_gp n1 =
  loop all_gp where rec loop =
    fun
    [ [GP_person n _ _ :: gpl]
    | [GP_same n _ _ :: gpl] -> if Num.eq n n1 then True else loop gpl
    | [gp :: gpl] -> loop gpl
    | [] -> False ]
;

value get_link all_gp ip =
  loop all_gp where rec loop =
    fun
    [ [(GP_person n ip0 _ as gp) :: gpl] ->
         if ip = ip0 then Some gp else loop gpl
    | [gp :: gpl] -> loop gpl
    | [] -> None ]
;

value parent_sosa conf base ip all_gp n parent =
  if sosa_is_present all_gp n then Num.to_string n
  else
    match parents (aget conf base ip) with
    [ Some ifam ->
        match get_link all_gp (parent (coi base ifam)) with
        [ Some (GP_person n _ _) -> Num.to_string n
        | _ -> "" ]
    | None -> "" ]
;

value will_print =
  fun
  [ GP_person _ _ _ -> True
  | GP_same _ _ _ -> True
  | _ -> False ]
;

value get_all_generations conf base p =
  let max_level =
    match p_getint conf.env "v" with
    [ Some v -> v + 1
    | None -> 0 ]
  in
  let mark = Array.create base.data.persons.len Num.zero in
  let rec get_generations level gpll gpl =
    let gpll = [gpl :: gpll] in
    if level < max_level then
      let next_gpl = next_generation conf base mark gpl in
      if List.exists will_print next_gpl then
        get_generations (level + 1) gpll next_gpl
      else gpll
    else gpll
  in
  let gpll = get_generations 1 [] [GP_person Num.one p.cle_index None] in
  let gpll = List.rev gpll in
  List.flatten gpll
;

(* Ancestors by tree:

  8 ? ? ? ? ? ? ?
   4   5   ?   7
     2       3 
         1

1) Build list of levels (t1 = True for parents flag, size 1)
   => [ [8At1 E E] [4Lt1 5Rt1 7At1] [2Lt1 3Rt1] [1Ct1] ] 

2) Enrich list of levels (parents flag, sizing)
   => [ [8At1 E E] [4Lt1 5Rf1 7Af1] [2Lt3 3Rt1] [1Ct5] ]

3) Display it
    For each cell:
      Top vertical bar if parents flag (not on top line)
      Person
      Person tree link (vertical bar) ) not on bottom line
      Horizontal line                 )

*)

type pos = [ Left | Right | Center | Alone ];
type cell = [ Cell of person and pos and bool and int | Empty ];

value rec enrich lst1 lst2 =
  match (lst1, lst2) with
  [ (_, []) -> []
  | ([], lst) -> lst
  | ([Cell _ Right _ s1 :: l1], [Cell p d u s2 :: l2]) ->
      [Cell p d u (s1 + s2 + 1) :: enrich l1 l2]
  | ([Cell _ Left _ s :: l1], [Cell p d u _ :: l2]) ->
      enrich l1 [Cell p d u s :: l2]
  | ([Cell _ _ _ s :: l1], [Cell p d u _ :: l2]) ->
     [Cell p d u s :: enrich l1 l2]
  | ([Empty :: l1], [Cell p d _ s :: l2]) -> [Cell p d False s :: enrich l1 l2]
  | ([_ :: l1], [Empty :: l2]) -> [Empty :: enrich l1 l2] ]
;
    
value is_empty = List.for_all (\== Empty);
     
value rec enrich_tree lst =
  match lst with
  [ [] -> []
  | [head :: tail] ->
      if is_empty head then enrich_tree tail
      else
        match tail with
        [ [] -> [head]
        | [thead :: ttail] ->
            [head :: enrich_tree [enrich head thead :: ttail]] ] ]
;

(* tree_generation_list
    conf: configuration parameters
    base: base name
    gv: number of generations
    p: person *)
value tree_generation_list conf base gv p =
  let next_gen pol =
    List.fold_right
      (fun po list ->
         match po with
         [ Empty -> [Empty :: list]
         | Cell p _ _ _ ->
             match parents (aget conf base p.cle_index) with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 let fath =
                   let p = pget conf base (father cpl) in
                   if know base p then Some p else None
                 in
                 let moth =
                   let p = pget conf base (mother cpl) in
                   if know base p then Some p else None
                 in
                 match (fath, moth) with
                 [ (Some f, Some m) ->
                     [Cell f Left True 1; Cell m Right True 1 :: list]
                 | (Some f, None) -> [Cell f Alone True 1 :: list]
                 | (None, Some m) -> [Cell m Alone True 1 :: list]
                 | (None, None) -> [Empty :: list] ]
             | _ -> [Empty :: list] ] ])
      pol []
  in
  let gen =
    loop (gv - 1) [Cell p Center True 1] [] where rec loop i gen list =
      if i == 0 then [gen :: list]
      else loop (i - 1) (next_gen gen) [gen :: list]
  in
  enrich_tree gen
;

value string_of_place conf base istr =
  string_with_macros conf [] (sou base istr)
;

(* Ancestors surnames list *)

value get_date_place conf base auth_for_all_anc p =
  if auth_for_all_anc || authorized_age conf base p then
    let d1 =
      match Adef.od_of_codate p.birth with
      [ None -> Adef.od_of_codate p.baptism
      | x -> x ]
    in
    let d1 =
      if d1 <> None then d1
      else
        List.fold_left
          (fun d ifam ->
             if d <> None then d
             else Adef.od_of_codate (foi base ifam).marriage)
          d1 (Array.to_list (uget conf base p.cle_index).family)
    in
    let d2 =
      match p.death with
      [ Death _ cd -> Some (Adef.date_of_cdate cd)
      | _ ->
          match p.burial with
          [ Buried cod -> Adef.od_of_codate cod
          | Cremated cod -> Adef.od_of_codate cod
          | _ -> None ] ]
    in
    let auth_for_all_anc =
      if auth_for_all_anc then True
      else
        match d2 with
        [ Some (Dgreg d _)
          when (time_gone_by d conf.today).year > conf.private_years ->
            True
        | _ -> False ]
    in
    let pl =
      let pl = "" in
      let pl = if pl <> "" then pl else sou base p.birth_place in
      let pl = if pl <> "" then pl else sou base p.baptism_place in
      let pl = if pl <> "" then pl else sou base p.death_place in
      let pl = if pl <> "" then pl else sou base p.burial_place in
      let pl =
        if pl <> "" then pl
        else
          List.fold_left
            (fun pl ifam ->
               if pl <> "" then pl
               else sou base (foi base ifam).marriage_place)
            pl (Array.to_list (uget conf base p.cle_index).family)
      in
      pl
    in
    ((d1, d2, pl), auth_for_all_anc)
  else ((None, None, ""), False)
;

value merge_date_place conf base surn ((d1, d2, pl), auth) p =
  let ((pd1, pd2, ppl), auth) = get_date_place conf base auth p in
  let nd1 =
    if pd1 <> None then pd1
    else if p.surname = surn then if pd2 <> None then pd2 else d1
    else None
  in
  let nd2 =
    if p.surname = surn then
      if d2 <> None then d2
      else if d1 <> None then d1
      else if pd1 <> None then pd2
      else pd1
    else if pd2 <> None then pd2
    else if pd1 <> None then pd1
    else d1
  in
  let pl = if ppl <> "" then ppl else if p.surname = surn then pl else "" in
  ((nd1, nd2, pl), auth)
;

value build_surnames_list conf base v p =
  let ht = Hashtbl.create 701 in
  let mark = Array.create base.data.persons.len 5 in
  let auth = conf.wizard || conf.friend in
  let add_surname sosa p surn dp =
    let r =
      try Hashtbl.find ht surn with
      [ Not_found ->
          let r = ref ((fst dp, p), []) in
          do { Hashtbl.add ht surn r; r } ]
    in
    r.val := (fst r.val, [sosa :: snd r.val])
  in
  let rec loop lev sosa p surn dp =
    if mark.(Adef.int_of_iper p.cle_index) = 0 then ()
    else if lev = v then
      if conf.hide_names && not (fast_auth_age conf p) then ()
      else add_surname sosa p surn dp
    else do {
      mark.(Adef.int_of_iper p.cle_index) :=
        mark.(Adef.int_of_iper p.cle_index) - 1;
      match parents (aget conf base p.cle_index) with
      [ Some ifam ->
          let cpl = coi base ifam in
          let fath = pget conf base (father cpl) in
          let moth = pget conf base (mother cpl) in
          do {
            if surn <> fath.surname && surn <> moth.surname then
              add_surname sosa p surn dp
            else ();
            let sosa = Num.twice sosa in
            if not (is_hidden fath) then
              let dp1 = merge_date_place conf base surn dp fath in
              loop (lev + 1) sosa fath fath.surname dp1
            else ();
            let sosa = Num.inc sosa 1 in
            if not (is_hidden moth) then
              let dp2 = merge_date_place conf base surn dp moth in
              loop (lev + 1) sosa moth moth.surname dp2
            else ();
          }
      | None -> add_surname sosa p surn dp ]
    }
  in
  do {
    loop 0 Num.one p p.surname (get_date_place conf base auth p);
    let list = ref [] in
    Hashtbl.iter
      (fun i dp ->
         let surn = sou base i in
         if surn <> "?" then list.val := [(surn, dp.val) :: list.val] else ())
      ht;
    List.sort
      (fun (s1, _) (s2, _) ->
         match
           Gutil.alphabetic_order (surname_end base s1) (surname_end base s2)
         with
         [ 0 ->
             Gutil.alphabetic_order (surname_begin base s1)
               (surname_begin base s2)
         | x -> x ])
      list.val
  }
;

(* Interpretation of template file *)

value rec compare_ls sl1 sl2 =
  match (sl1, sl2) with
  [ ([s1 :: sl1], [s2 :: sl2]) ->
      let c = Gutil.alphabetic_order s1 s2 in
      if c = 0 then compare_ls sl1 sl2 else c
  | ([_ :: _], []) -> 1
  | ([], [_ :: _]) -> -1
  | ([], []) -> 0 ]
;

module SortedList =
  Set.Make (struct type t = list string; value compare = compare_ls; end)
;

type ancestor_surname_info =
  (string * option date * option date * string * person * list Num.t * loc)
;

type env 'a =
  [ Vallgp of list generation_person
  | Vanc of generation_person
  | Vanc_surn of ancestor_surname_info
  | Vcell of cell
  | Vcelll of list cell
  | Vcnt of ref int
  | Vdesclevtab of Lazy.t (array int)
  | Vdmark of ref (array bool)
  | Vslist of ref SortedList.t
  | Vslistlm of list (list string)
  | Vind of person and ascend and union
  | Vfam of family and (iper * iper * iper) and descend and bool
  | Vrel of relation and option person
  | Vbool of bool
  | Vint of int
  | Vgpl of list generation_person
  | Vnldb of NotesLinks.notes_links_db
  | Vstring of string
  | Vsosa_ref of Lazy.t (option person)
  | Vsosa of option (Num.t * person)
  | Vtitle of person and title_item
  | Vlazyp of ref (option string)
  | Vlazy of Lazy.t (env 'a)
  | Vother of 'a
  | Vnone ]
and title_item =
  (int * gen_title_name istr * istr * list istr *
   list (option date * option date))
;

value get_env v env =
  try
    match List.assoc v env with
    [ Vlazy l -> Lazy.force l
    | x -> x ]
  with
  [ Not_found -> Vnone ]
;
value get_vother = fun [ Vother x -> Some x | _ -> None ];
value set_vother x = Vother x;

value not_impl func x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.\tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  ">Perso." ^ func ^ ": not impl " ^ desc ^ "<p>\n"
;

value extract_var sini s =
  let len = String.length sini in
  if String.length s > len && String.sub s 0 (String.length sini) = sini then
    String.sub s len (String.length s - len)
  else ""
;

value template_file = ref "perso.txt";

value warning_use_has_parents_before_parent (bp, ep) var r =
  IFDEF UNIX THEN do {
    Printf.eprintf "*** <W> %s" template_file.val;
    Printf.eprintf ", chars %d-%d" bp ep;
    Printf.eprintf "\
: since v5.00, must test \"has_parents\" before using \"%s\"\n"
      var;
    flush stderr;
    r
  }
  ELSE r END
;

value obsolete_list = ref [];

value obsolete (bp, ep) version var new_var r =
  if List.mem var obsolete_list.val then r
  else IFDEF UNIX THEN do {
    Printf.eprintf "*** <W> %s, chars %d-%d:" template_file.val bp ep;
    Printf.eprintf " \"%s\" obsolete since v%s%s\n" var version
      (if new_var = "" then "" else "; rather use \"" ^ new_var ^ "\"");
    flush stderr;
    obsolete_list.val := [var :: obsolete_list.val];
    r
  }
  ELSE r END
;

value string_of_num sep num =
  let len = ref 0 in
  do {
    Num.print (fun x -> len.val := Buff.mstore len.val x) sep num;
    Buff.get len.val
  }
;

value bool_val x = VVbool x;
value str_val x = VVstring x;

value gen_string_of_img_sz max_wid max_hei conf base env (p, _, _, p_auth) =
  if p_auth then
    let v = image_and_size conf base p (limited_image_size max_wid max_hei) in
    match v with
    [ Some (_, _, Some (width, height)) ->
        Format.sprintf " width=\"%d\" height=\"%d\"" width height
    | Some (_, _, None) -> Format.sprintf " height=\"%d\"" max_hei
    | None -> "" ]
  else ""
;
value string_of_image_size = gen_string_of_img_sz max_im_wid max_im_wid;
value string_of_image_small_size = gen_string_of_img_sz 100 75;

value make_ep conf base ip =
  let p = pget conf base ip in
  let a = aget conf base ip in
  let u = uget conf base ip in
  let p_auth = authorized_age conf base p in (p, a, u, p_auth)
;

value rec eval_var conf base env ep loc sl =
  try eval_simple_var conf base env ep sl with
  [ Not_found -> eval_compound_var conf base env ep loc sl ]
and eval_simple_var conf base env ep =
  fun
  [ [s] ->
      try bool_val (eval_simple_bool_var conf base env ep s) with
      [ Not_found -> str_val (eval_simple_str_var conf base env ep s) ]
  | _ -> raise Not_found ]
and eval_simple_bool_var conf base env (_, _, _, p_auth) =
  fun
  [ "are_divorced" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          match fam.divorce with
          [ Divorced _ -> True
          | _ -> False ]
      | _ -> raise Not_found ]
  | "are_engaged" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ -> fam.relation = Engaged
      | _ -> raise Not_found ]
  | "are_married" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          fam.relation = Married || fam.relation = NoSexesCheckMarried
      | _ -> raise Not_found ]
  | "are_not_married" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          fam.relation = NotMarried || fam.relation = NoSexesCheckNotMarried
      | _ -> raise Not_found ]
  | "are_separated" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          match fam.divorce with
          [ Separated -> True
          | _ -> False ]
      | _ -> raise Not_found ]
  | "browsing_with_sosa_ref" ->
      match get_env "sosa_ref" env with
      [ Vsosa_ref v -> Lazy.force v <> None
      | _ -> raise Not_found ]
  | "has_comment" ->
      match get_env "fam" env with
      [ Vfam fam _ _ m_auth -> m_auth && sou base fam.comment <> ""
      | _ -> raise Not_found ]
  | "has_relation_her" ->
      match get_env "rel" env with
      [ Vrel {r_moth = Some _} None -> True
      | _ -> False ]
  | "has_relation_him" ->
      match get_env "rel" env with
      [ Vrel {r_fath = Some _} None -> True
      | _ -> False ]
  | "has_sosa" ->
      match get_env "sosa" env with
      [ Vsosa (Some _) -> True
      | _ -> False ]
  | "has_witnesses" ->
      match get_env "fam" env with
      [ Vfam fam _ _ m_auth -> m_auth && Array.length fam.witnesses > 0
      | _ -> raise Not_found ]
  | "is_first" ->
      match get_env "first" env with
      [ Vbool x -> x
      | _ -> raise Not_found ]
  | "is_last" ->
      match get_env "last" env with
      [ Vbool x -> x
      | _ -> raise Not_found ]
  | "is_no_mention" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ -> fam.relation = NoMention
      | _ -> raise Not_found ]
  | "is_no_sexes_check" ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          fam.relation = NoSexesCheckNotMarried ||
          fam.relation = NoSexesCheckMarried
      | _ -> raise Not_found ]
  | "is_self" -> get_env "pos" env = Vstring "self"
  | "is_sibling_after" -> get_env "pos" env = Vstring "next"
  | "is_sibling_before" -> get_env "pos" env = Vstring "prev"
  | "lazy_printed" ->
      match get_env "lazy_print" env with
      [ Vlazyp r -> r.val = None
      | _ -> raise Not_found ]
  | s ->
      let v = extract_var "file_exists_" s in
      if v <> "" then
        let v = code_varenv v in
        let s = Srcfile.source_file_name conf v in
        Sys.file_exists s
      else raise Not_found ]
and eval_simple_str_var conf base env (_, _, _, p_auth) =
  fun
  [ "alias" ->
      match get_env "alias" env with
      [ Vstring s -> s
      | _ -> raise Not_found ]
  | "child_cnt" -> string_of_int_env "child_cnt" env
  | "comment" ->
      match get_env "fam" env with
      [ Vfam fam _ _ m_auth ->
          if m_auth then string_with_macros conf [] (sou base fam.comment)
          else ""
      | _ -> raise Not_found ]
  | "count" ->
      match get_env "count" env with
      [ Vcnt c -> string_of_int c.val
      | _ -> "" ]
  | "divorce_date" ->
      match get_env "fam" env with
      [ Vfam fam (_, _, isp) _ m_auth ->
          match fam.divorce with
          [ Divorced d ->
              let d = Adef.od_of_codate d in
              match d with
              [ Some d when m_auth ->
                  " <em>" ^ Date.string_of_ondate conf d ^ "</em>"
              | _ -> "" ]
          | _ -> raise Not_found ]
      | _ -> raise Not_found ]
  | "empty_sorted_list" ->
      match get_env "list" env with
      [ Vslist l -> do { l.val := SortedList.empty; "" }
      | _ -> raise Not_found ]
  | "family_cnt" -> string_of_int_env "family_cnt" env
  | "first_name_alias" ->
      match get_env "first_name_alias" env with
      [ Vstring s -> s
      | _ -> "" ]
  | "incr_count" ->
      match get_env "count" env with
      [ Vcnt c -> do { incr c; "" }
      | _ -> "" ]
  | "lazy_force" ->
      match get_env "lazy_print" env with
      [ Vlazyp r ->
          match r.val with
          [ Some s -> do { r.val := None; s }
          | None -> "" ]
      | _ -> raise Not_found ]
  | "level" ->
      match get_env "level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]  
 | "marriage_place" ->
      match get_env "fam" env with
      [ Vfam fam _ _ m_auth ->
          if m_auth then string_of_place conf base fam.marriage_place else ""
      | _ -> raise Not_found ]
  | "max_anc_level" ->
      match get_env "max_anc_level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | "max_cous_level" ->
      match get_env "max_cous_level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | "max_desc_level" ->
      match get_env "max_desc_level" env with
      [ Vint i -> string_of_int i
      | _ -> "" ]
  | "nobility_title" ->
      match get_env "nobility_title" env with
      [ Vtitle p t ->
          if p_auth then
            string_of_title conf base (transl_nth conf "and" 0) p t
          else ""
      | _ -> raise Not_found ]
  | "number_of_subitems" ->
      match get_env "item" env with
      [ Vslistlm [[s :: _] :: sll] ->
          let n =
            loop 1 sll where rec loop n =
              fun
              [ [[s1 :: _] :: sll] -> if s = s1 then loop (n + 1) sll else n
              | _ -> n ]
          in
          string_of_int n
      | _ -> raise Not_found ]
  | "on_marriage_date" ->
      match get_env "fam" env with
      [ Vfam fam _ _ m_auth ->
          match (m_auth, Adef.od_of_codate fam.marriage) with
          [ (True, Some s) -> Date.string_of_ondate conf s
          | _ -> "" ] 
      | _ -> raise Not_found ]
  | "origin_file" ->
      if conf.wizard then
        match get_env "fam" env with
        [ Vfam fam _ _ _ -> sou base fam.origin_file
        | _ -> "" ]
      else raise Not_found
  | "prefix_no_iz" ->
      commd {(conf) with henv = List.remove_assoc "iz" conf.henv}
  | "prefix_no_templ" ->
      let henv =
        List.fold_right
          (fun (k, v) henv -> if k = "templ" then henv else [(k, v) :: henv])
          conf.henv []
      in
      let c = conf.command ^ "?" in
      List.fold_left (fun c (k, v) -> c ^ k ^ "=" ^ v ^ ";") c
        (henv @ conf.senv)
  | "qualifier" ->
      match get_env "qualifier" env with
      [ Vstring nn -> nn
      | _ -> raise Not_found ]
  | "related_type" ->
      match get_env "rel" env with
      [ Vrel r (Some c) -> rchild_type_text conf r.r_type (index_of_sex c.sex)
      | _ -> raise Not_found ]
  | "relation_type" ->
      match get_env "rel" env with
      [ Vrel r None ->
          match (r.r_fath, r.r_moth) with
          [ (Some ip, None) -> relation_type_text conf r.r_type 0
          | (None, Some ip) -> relation_type_text conf r.r_type 1
          | (Some ip1, Some ip2) -> relation_type_text conf r.r_type 2
          | _ -> raise Not_found ]
      | _ -> raise Not_found ]
  | "reset_count" ->
      match get_env "count" env with
      [ Vcnt c -> do { c.val := 0; "" }
      | _ -> "" ]
  | "source_type" ->
       match get_env "src_typ" env with
       [ Vstring s -> s
       | _ -> raise Not_found ]
  | "surname_alias" ->
      match get_env "surname_alias" env with
      [ Vstring s -> s
      | _ -> raise Not_found ]
  | "witness_relation" ->
      match get_env "fam" env with
      [ Vfam _ (ip1, ip2, _) _ m_auth ->
          if not m_auth then ""
          else
            Printf.sprintf
              (ftransl conf "witness at marriage of %s and %s")
              (referenced_person_title_text conf base (pget conf base ip1))
              (referenced_person_title_text conf base (pget conf base ip2))
      | _ -> raise Not_found ]
  | s ->
      loop
        [("evar_",
          fun v ->
            match p_getenv (conf.env @ conf.henv) v with
            [ Some vv -> quote_escaped vv
            | None -> "" ]);
         (* warning: "cvar_" deprecated since 5.00; use "bvar." *)
         ("cvar_",
          fun v -> try List.assoc v conf.base_env with [ Not_found -> "" ])]
      where rec loop =
        fun
        [ [(pfx, f) :: pfx_list] ->
            let v = extract_var pfx s in
            if v <> "" then f v
            else loop pfx_list
        | [] -> raise Not_found ] ]
and eval_compound_var conf base env ((_, a, _, _) as ep) loc =
  fun 
  [ ["ancestor" :: sl] ->
      match get_env "ancestor" env with
      [ Vanc gp -> eval_ancestor_field_var conf base env gp loc sl
      | Vanc_surn info -> eval_anc_by_surnl_field_var conf base env ep info sl
      | _ -> raise Not_found ]
  | ["base"; "name"] -> VVstring conf.bname
  | ["base"; "nb_persons"] ->
      VVstring
        (string_of_num (Util.transl conf "(thousand separator)")
           (Num.of_int base.data.persons.len))
  | ["cell" :: sl] ->
      match get_env "cell" env with
      [ Vcell cell -> eval_cell_field_var conf base env ep cell loc sl
      | _ -> raise Not_found ]
  | ["child" :: sl] ->
      match get_env "child" env with
      [ Vind p a u ->
          let auth =
            match get_env "auth" env with
            [ Vbool True -> authorized_age conf base p
            | _ -> False ]
          in
          let ep = (p, a, u, auth) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | ["enclosing" :: sl] ->
      let rec loop =
        fun
        [ [("#loop", _) :: env] ->
            eval_person_field_var conf base env ep loc sl
        | [_ :: env] -> loop env
        | [] -> raise Not_found ]
      in
      loop env
  | ["family" :: sl] ->
      match get_env "fam" env with
      [ Vfam f c d m -> eval_family_field_var conf base env (f, c, d, m) loc sl
      | _ -> raise Not_found ]
  | ["father" :: sl] ->
      match parents a with
      [ Some ifam ->
          let cpl = coi base ifam in
          let ep = make_ep conf base (father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
          warning_use_has_parents_before_parent loc "father" (str_val "") ]
  | ["item" :: sl] ->
      match get_env "item" env with
      [ Vslistlm ell -> eval_item_field_var env ell sl
      | _ -> raise Not_found ]
  | ["mother" :: sl] ->
      match parents a with
      [ Some ifam ->
          let cpl = coi base ifam in
          let ep = make_ep conf base (mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
          warning_use_has_parents_before_parent loc "mother" (str_val "") ]
  | ["next_item" :: sl] ->
      match get_env "item" env with
      [ Vslistlm [_ :: ell] -> eval_item_field_var env ell sl
      | _ -> raise Not_found ]
  | ["parent" :: sl] ->
      match get_env "parent" env with
      [ Vind p a u ->
          let ep = (p, a, u, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | ["prev_item" :: sl] ->
      match get_env "prev_item" env with
      [ Vslistlm ell -> eval_item_field_var env ell sl
      | _ -> raise Not_found ]
  | ["prev_family" :: sl] ->
      match get_env "prev_fam" env with
      [ Vfam f c d m -> eval_family_field_var conf base env (f, c, d, m) loc sl
      | _ -> raise Not_found ]
  | ["pvar"; v :: sl] ->
      match find_person_in_env conf base v with
      [ Some p ->
          let ep = make_ep conf base p.cle_index in
          eval_person_field_var conf base env ep loc sl
      | None -> raise Not_found ]
  | ["related" :: sl] ->
      match get_env "rel" env with
      [ Vrel {r_type = rt} (Some p) ->
          eval_relation_field_var conf base env
            (index_of_sex p.sex, rt, p.cle_index, False) loc sl
      | _ -> raise Not_found ]
  | ["relation_her" :: sl] ->
      match get_env "rel" env with
      [ Vrel {r_moth = Some ip; r_type = rt} None ->
          eval_relation_field_var conf base env (1, rt, ip, True) loc sl
      | _ -> raise Not_found ]
  | ["relation_him" :: sl] ->
      match get_env "rel" env with
      [ Vrel {r_fath = Some ip; r_type = rt} None ->
          eval_relation_field_var conf base env (0, rt, ip, True) loc sl
      | _ -> raise Not_found ]
  | ["self" :: sl] -> eval_person_field_var conf base env ep loc sl
  | ["sosa" :: sl] ->
      match get_env "sosa" env with
      [ Vsosa x ->
          match x with
          [ Some (n, p) -> VVstring (eval_num conf n sl)
          | None -> VVstring "" ]
      | _ -> raise Not_found ]
  | ["sosa_ref" :: sl] ->
      match get_env "sosa_ref" env with
      [ Vsosa_ref v ->
          match Lazy.force v with
          [ Some p ->
              let ep = make_ep conf base p.cle_index in
              eval_person_field_var conf base env ep loc sl
          | None -> raise Not_found ]
      | _ -> raise Not_found ]
  | ["spouse" :: sl] ->
      match get_env "fam" env with
      [ Vfam _ (_, _, ip) _ _ ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | ["witness" :: sl] ->
      match get_env "witness" env with
      [ Vind p a u ->
          let ep = (p, a, u, authorized_age conf base p) in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | sl -> eval_person_field_var conf base env ep loc sl ]
and eval_item_field_var env ell =
  fun
  [ [s] ->
      try
        match ell with
        [ [el :: _] ->
            let v = int_of_string s in
            let r = try List.nth el (v - 1) with [ Failure _ -> "" ] in
            VVstring r
        | [] -> VVstring "" ]
      with
      [ Failure _ -> raise Not_found ]
  | _ -> raise Not_found ]
and eval_relation_field_var conf base env (i, rt, ip, is_relation) loc =
  fun
  [ ["type"] ->
       if is_relation then VVstring (relation_type_text conf rt i)
       else VVstring (rchild_type_text conf rt i)
  | sl ->
      let ep = make_ep conf base ip in
      eval_person_field_var conf base env ep loc sl ]
and eval_cell_field_var conf base env ep cell loc =
  fun
  [ ["colspan"] ->
      match cell with
      [ Empty -> VVstring "1"
      | Cell _ _ _ s -> VVstring (string_of_int s) ]
  | ["is_center"] ->
      match cell with
      [ Cell _ Center _ _ -> VVbool True
      | _ -> VVbool False ]
  | ["is_empty"] ->
      match cell with
      [ Empty -> VVbool True
      | _ -> VVbool False ]
  | ["is_left"] ->
      match cell with
      [ Cell _ Left _ _ -> VVbool True
      | _ -> VVbool False ]
  | ["is_right"] ->
      match cell with
      [ Cell _ Right _ _ -> VVbool True
      | _ -> VVbool False ]
  | ["is_top"] ->
      match cell with
      [ Cell _ _ False _ -> VVbool True
      | _ -> VVbool False ]
  | ["person" :: sl] ->
      match cell with
      [ Cell p _ _ _ ->
          let ep = make_ep conf base p.cle_index in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | _ -> raise Not_found ]
and eval_ancestor_field_var conf base env gp loc =
  fun
  [ ["family" :: sl] ->
      match gp with
      [ GP_person _ ip (Some ifam) ->
          let f = foi base ifam in
          let c = coi base ifam in
          let d = doi base ifam in
          let ifath = father c in
          let imoth = mother c in
          let ispouse = if ip = ifath then imoth else ifath in
          let c = (ifath, imoth, ispouse) in
          let m_auth =
            authorized_age conf base (poi base ifath) &&
            authorized_age conf base (poi base imoth)
          in
          eval_family_field_var conf base env (f, c, d, m_auth) loc sl
      | _ -> raise Not_found ]
  | ["father" :: sl] ->
      match gp with
      [ GP_person _ ip _ ->
          match (parents (aoi base ip), get_env "all_gp" env) with
          [ (Some ifam, Vallgp all_gp) ->
              let cpl = coi base ifam in
              match get_link all_gp (father cpl) with
              [ Some gp -> eval_ancestor_field_var conf base env gp loc sl
              | None -> raise Not_found ]
          | (_, _) -> raise Not_found ]
      | _ -> raise Not_found ]
  | ["father_sosa"] ->
      match (gp, get_env "all_gp" env) with
      [ (GP_person n ip _ | GP_same n _ ip, Vallgp all_gp) ->
          let n = Num.twice n in
          VVstring (parent_sosa conf base ip all_gp n father)
      | _ -> VVstring "" ]
  | ["interval"] ->
      match gp with
      [ GP_interv (Some (n1, n2, Some (n3, n4))) ->
          let n2 = Num.sub n2 Num.one in
          let n4 = Num.sub n4 Num.one in
          let sep = transl conf "(thousand separator)" in
          let r =
            Num.to_string_sep sep n1 ^ "-" ^ Num.to_string_sep sep n2 ^ " = " ^
            Num.to_string_sep sep n3 ^ "-" ^ Num.to_string_sep sep n4
          in
          VVstring r
      | GP_interv (Some (n1, n2, None)) ->
          let n2 = Num.sub n2 Num.one in
          let sep = transl conf "(thousand separator)" in
          let r =
            Num.to_string_sep sep n1 ^ "-" ^ Num.to_string_sep sep n2 ^
            " = ..."
          in
          VVstring r
      | GP_interv None -> VVstring "..."
      | _ -> VVstring "" ]
  | ["mother_sosa"] ->
      match (gp, get_env "all_gp" env) with
      [ (GP_person n ip _ | GP_same n _ ip, Vallgp all_gp) ->
          let n = Num.inc (Num.twice n) 1 in
          VVstring (parent_sosa conf base ip all_gp n mother)
      | _ -> VVstring "" ]
  | ["same" :: sl] ->
      match gp with
      [ GP_same _ n _ -> VVstring (eval_num conf n sl)
      | _ -> VVstring "" ]
  | ["sosa" :: sl] ->
      match gp with
      [ GP_person n _ _ | GP_same n _ _ -> VVstring (eval_num conf n sl)
      | _ -> VVstring "" ]
  | ["spouse" :: sl] ->
      match gp with
      [ GP_person _ ip (Some ifam) ->
          let ip = Gutil.spouse ip (coi base ifam) in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | sl ->
      match gp with
      [ GP_person _ ip _ | GP_same _ _ ip ->
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ] ]
and eval_anc_by_surnl_field_var conf base env ep
      (s, db, de, place, p, sosa_list, loc) =
  fun
  [ ["date_begin" :: sl] ->
      match db with
      [ Some d -> eval_date_field_var d sl
      | None -> VVstring "" ]
  | ["date_end" :: sl] ->
      match de with
      [ Some d -> eval_date_field_var d sl
      | None -> VVstring "" ]
  | ["nb_times"] -> VVstring (string_of_int (List.length sosa_list))
  | ["place"] -> VVstring (string_with_macros conf [] place)
  | ["sosa_access"] ->
      let (str, _) =
        List.fold_right
          (fun sosa (str, n) ->
             let str =
               str ^ ";s" ^ string_of_int n ^ "=" ^ Num.to_string sosa
             in
             (str, n + 1))
          sosa_list ("", 1)
      in
      let (p, _, _, _) = ep in
      VVstring (acces_n conf base "1" p ^ str)
  | sl ->
      let ep = make_ep conf base p.cle_index in
      eval_person_field_var conf base env ep loc sl ]
and eval_num conf n =
  fun
  [ ["hexa"] -> "0x" ^ Num.to_string_sep_base "" 16 n
  | ["octal"] -> "0o" ^ Num.to_string_sep_base "" 8 n
  | ["v"] -> Num.to_string n
  | [] -> Num.to_string_sep (transl conf "(thousand separator)") n
  | _ -> raise Not_found ]
and eval_person_field_var conf base env ((p, a, _, p_auth) as ep) loc =
  fun
  [ ["baptism_date" :: sl] ->
      match Adef.od_of_codate p.baptism with
      [ Some d when p_auth -> eval_date_field_var d sl
      | _ -> VVstring "" ]
  | ["birth_date" :: sl] ->
      match Adef.od_of_codate p.birth with
      [ Some d when p_auth -> eval_date_field_var d sl
      | _ -> VVstring "" ]
  | ["burial_date" :: sl] ->
      match p.burial with
      [ Buried cod when p_auth ->
          match Adef.od_of_codate cod with
          [ Some d -> eval_date_field_var d sl
          | None -> VVstring "" ]
      | _ -> VVstring "" ]
  | ["cremated_date" :: sl] ->
      match p.burial with
      [ Cremated cod when p_auth ->
          match Adef.od_of_codate cod with
          [ Some d -> eval_date_field_var d sl
          | None -> VVstring "" ]
      | _ -> VVstring "" ]
  | ["death_date" :: sl] ->
      match p.death with
      [ Death _ cd when p_auth -> eval_date_field_var (Adef.date_of_cdate cd) sl
      | _ -> VVstring "" ]
  | ["father" :: sl] ->
      match parents a with
      [ Some ifam ->
          let cpl = coi base ifam in
          let ep = make_ep conf base (father cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
          warning_use_has_parents_before_parent loc "father" (str_val "") ]
  | ["has_linked_page"; s] ->
      match get_env "nldb" env with
      [ Vnldb db ->
          let key =
            let fn = Name.lower (sou base p.first_name) in
            let sn = Name.lower (sou base p.surname) in
            (fn, sn, p.occ)
          in
          try
            do {
              List.iter
                (fun (pg, (_, il)) ->
                   match pg with
                   [ NotesLinks.PgMisc pg ->
                       if List.mem key il then
                         let (nenv, _) = Notes.read_notes base pg in
                         try let _ = List.assoc s nenv in raise Exit
                         with [ Not_found -> () ]
                       else ()
                   | _ -> () ])
                db;
              VVbool False
            }
          with
          [ Exit -> VVbool True ]
      | _ -> raise Not_found ]
  | ["linked_page"; s] ->
      match get_env "nldb" env with
      [ Vnldb db ->
          let key =
            let fn = Name.lower (sou base p.first_name) in
            let sn = Name.lower (sou base p.surname) in
            (fn, sn, p.occ)
          in
          let s =
            List.fold_left
              (fun str (pg, (_, il)) ->
                 match pg with
                 [ NotesLinks.PgMisc pg ->
                     if List.mem key il then
                       let (nenv, _) = Notes.read_notes base pg in
                       let v = try List.assoc s nenv with [ Not_found -> "" ] in
                       if v = "" then str
                       else
                         let v = Util.nth_field v (Util.index_of_sex p.sex) in
                         let str1 =
                           Printf.sprintf
                             "<a href=\"%sm=NOTES;f=%s\">%s</a>" (commd conf)
                             pg v
                         in
                         if str = "" then str1 else str ^ ", " ^ str1
                     else str
                | _ -> str ])
               "" db
          in
          VVstring s
      | _ -> raise Not_found ]
  | ["mother" :: sl] ->
      match parents a with
      [ Some ifam ->
          let cpl = coi base ifam in
          let ep = make_ep conf base (mother cpl) in
          eval_person_field_var conf base env ep loc sl
      | None ->
          warning_use_has_parents_before_parent loc "mother" (str_val "") ]
  | ["nobility_title" :: sl] ->
      match Util.main_title base p with
      [ Some t when p_auth ->
          let id = sou base t.t_ident in
          let pl = sou base t.t_place in
          eval_nobility_title_field_var (id, pl) sl
      | _ -> VVstring "" ]
  | ["self" :: sl] ->
      eval_person_field_var conf base env ep loc sl
  | ["spouse" :: sl] ->
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          let cpl = coi base fam.fam_index in
          let ip = Gutil.spouse p.cle_index cpl in
          let ep = make_ep conf base ip in
          eval_person_field_var conf base env ep loc sl
      | _ -> raise Not_found ]
  | ["var"] -> VVother ep
  | [s] ->
      try bool_val (eval_bool_person_field conf base env ep s) with
      [ Not_found ->
          try str_val (eval_str_person_field conf base env ep s) with
          [ Not_found -> obsolete_eval conf base env ep loc s ] ]
  | [] -> str_val (simple_person_text conf base p p_auth)
  | _ -> raise Not_found ]
and eval_date_field_var d =
  fun
  [ ["year"] ->
      match d with
      [ Dgreg dmy _ -> VVstring (string_of_int dmy.year)
      | _ -> VVstring "" ]
  | _ -> raise Not_found ]
and eval_nobility_title_field_var (id, pl) =
  fun
  [ ["ident_key"] -> VVstring (code_varenv id)
  | ["place_key"] -> VVstring (code_varenv pl)
  | [] -> VVstring (if pl = "" then id else id ^ " " ^ pl)
  | _ -> raise Not_found ]
and eval_bool_person_field conf base env (p, a, u, p_auth) =
  fun
  [ "access_by_key" ->
      Util.accessible_by_key conf base p (p_first_name base p)
        (p_surname base p)
  | "birthday" ->
      match (p_auth, Adef.od_of_codate p.birth) with
      [ (True, Some (Dgreg d _)) ->
          if d.prec = Sure && p.death = NotDead then
            d.day = conf.today.day && d.month = conf.today.month &&
            d.year < conf.today.year ||
            not (leap_year conf.today.year) && d.day = 29 && d.month = 2 &&
            conf.today.day = 1 && conf.today.month = 3
          else False
      | _ -> False ]
  | "computable_age" ->
      if p_auth then
        match (Adef.od_of_codate p.birth, p.death) with
        [ (Some (Dgreg d _), NotDead) ->
            not (d.day == 0 && d.month == 0 && d.prec <> Sure)
        | _ -> False ]
      else False
  | "computable_death_age" ->
      if p_auth then
        match Date.get_birth_death_date p with
        [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
           Some (Dgreg ({prec = Sure | About | Maybe} as d2) _), approx)
          when d1 <> d2 ->
            let a = time_gone_by d1 d2 in
            a.year > 0 ||
            a.year = 0 && (a.month > 0 || a.month = 0 && a.day > 0)
        | _ -> False ]
      else False
  | "has_aliases" -> p_auth && p.aliases <> []
  | "has_baptism_date" -> p_auth && p.baptism <> Adef.codate_None
  | "has_baptism_place" -> p_auth && sou base p.baptism_place <> ""
  | "has_birth_date" -> p_auth && p.birth <> Adef.codate_None
  | "has_birth_place" -> p_auth && sou base p.birth_place <> ""
  | "has_burial_date" ->
      if p_auth then
        match p.burial with
        [ Buried cod -> Adef.od_of_codate cod <> None
        | _ -> False ]
      else False
  | "has_burial_place" -> p_auth && sou base p.burial_place <> ""
  | "has_children" ->
      match get_env "fam" env with
      [ Vfam _ _ des _ -> Array.length des.children > 0
      | _ ->
          List.exists
            (fun ifam ->
             let des = doi base ifam in Array.length des.children > 0)
          (Array.to_list u.family) ]
  | "has_consanguinity" ->
      p_auth && consang a != Adef.fix (-1) && consang a != Adef.fix 0
  | "has_cremation_date" ->
      if p_auth then
        match p.burial with
        [ Cremated cod -> Adef.od_of_codate cod <> None
        | _ -> False ]
      else False
  | "has_cremation_place" -> p_auth && sou base p.burial_place <> ""
  | "has_death_date" ->
      match p.death with
      [ Death _ _ -> p_auth
      | _ -> False ]
  | "has_death_place" -> p_auth && sou base p.death_place <> ""
  | "has_families" -> Array.length u.family > 0
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_image" -> Util.has_image conf base p
  | "has_nephews_or_nieces" -> has_nephews_or_nieces conf base p
  | "has_nobility_titles" -> p_auth && p.titles <> []
  | "has_notes" -> p_auth && sou base p.notes <> ""
  | "has_occupation" -> p_auth && sou base p.occupation <> ""
  | "has_parents" -> parents a <> None
  | "has_public_name" -> p_auth && sou base p.public_name <> ""
  | "has_qualifiers" -> p_auth && p.qualifiers <> []
  | "has_relations" ->
      if p_auth && conf.use_restrict then
        let related =
          List.fold_left
            (fun l ip ->
               let rp = pget conf base ip in
               if is_hidden rp then l else [ip :: l])
          [] p.related
        in
        p.rparents <> [] || related <> []
      else p_auth && (p.rparents <> [] || p.related <> [])
  | "has_siblings" ->
      match parents a with
      [ Some ifam -> Array.length (doi base ifam).children > 1
      | None -> False ]
  | "has_sources" ->
      if conf.hide_names && not p_auth then False
      else if sou base p.psources <> "" then True
      else if
        p_auth &&
        (sou base p.birth_src <> "" || sou base p.baptism_src <> "" ||
         sou base p.death_src <> "" || sou base p.burial_src <> "") then
        True
      else
        List.exists
          (fun ifam ->
             let fam = foi base ifam in
             p_auth && sou base fam.marriage_src <> "" ||
             sou base fam.fsources <> "")
          (Array.to_list u.family)
  | "has_surnames_aliases" -> p.surnames_aliases <> []
  | "is_buried" ->
      match p.burial with
      [ Buried _ -> p_auth
      | _ -> False ]
  | "is_cremated" ->
      match p.burial with
      [ Cremated _ -> p_auth
      | _ -> False ]
  | "is_dead" ->
      match p.death with
      [ Death _ _ | DeadYoung | DeadDontKnowWhen -> p_auth
      | _ -> False ]
  | "is_descendant" ->
      match get_env "desc_mark" env with
      [ Vdmark r -> r.val.(Adef.int_of_iper p.cle_index)
      | _ -> raise Not_found ]
  | "is_female" -> p.sex = Female
  | "is_invisible" ->
      let conf = {(conf) with wizard = False; friend = False} in
      not (authorized_age conf base p)
  | "is_male" -> p.sex = Male
  | "is_private" -> p.access = Private
  | "is_public" -> p.access = Public
  | "is_restricted" -> is_hidden p
  | _ -> raise Not_found ]
and eval_str_person_field conf base env ((p, a, u, p_auth) as ep) =
  fun
  [ "access" -> acces conf base p
  | "age" ->
      match (p_auth, Adef.od_of_codate p.birth, p.death) with
      [ (True, Some (Dgreg d _), NotDead) ->
          let a = time_gone_by d conf.today in
          Date.string_of_age conf a
      | _ -> "" ]
  | "alias" ->
      match p.aliases with
      [ [nn :: _] when p_auth -> sou base nn
      | _ -> "" ]
  | "auto_image_file_name" ->
      match auto_image_file conf base p with
      [ Some s when p_auth -> s
      | _ -> "" ]
  | "birth_place" ->
      if p_auth then string_of_place conf base p.birth_place else ""
  | "baptism_place" ->
      if p_auth then string_of_place conf base p.baptism_place else ""
  | "burial_place" ->
      if p_auth then string_of_place conf base p.burial_place else ""
  | "child_name" ->
      let force_surname =
        match parents a with
        [ None -> False
        | Some ifam ->
            p_surname base (pget conf base (father (coi base ifam))) <>
              p_surname base p ]
      in
      if not p_auth && conf.hide_names then "x x"
      else if force_surname then person_text conf base p
      else person_text_no_surn_no_acc_chk conf base p
  | "consanguinity" ->
      if p_auth then
        string_of_decimal_num conf
          (round_2_dec (Adef.float_of_fix (consang a) *. 100.0)) ^ "%"
      else ""
  | "cremation_place" ->
      if p_auth then string_of_place conf base p.burial_place else ""
  | "dates" ->
      if p_auth then Date.short_dates_text conf base p else ""
  | "death_age" ->
      if p_auth then
        match Date.get_birth_death_date p with
        [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
           Some (Dgreg ({prec = Sure | About | Maybe} as d2) _), approx)
          when d1 <> d2 ->
            let a = time_gone_by d1 d2 in
            let s =
              if not approx && d1.prec = Sure && d2.prec = Sure then ""
              else transl_decline conf "possibly (date)" "" ^ " "
            in
            s ^ Date.string_of_age conf a
        | _ -> "" ]
      else ""
  | "death_place" ->
      if p_auth then string_of_place conf base p.death_place else ""
  | "desc_level" ->
      match get_env "desc_level_table" env with
      [ Vdesclevtab levt ->
          let levt = Lazy.force levt in
          string_of_int (levt.(Adef.int_of_iper p.cle_index))
      | _ -> raise Not_found ]
  | "set_infinite_desc_level" ->
      match get_env "desc_level_table" env with
      [ Vdesclevtab levt ->
          let levt = Lazy.force levt in
          do {
            levt.(Adef.int_of_iper p.cle_index) := infinite;
            ""
          }
      | _ -> raise Not_found ]
  | "died" -> string_of_died conf base env p p_auth
  | "fam_access" ->
      (* deprecated since 5.00: rather use "i=%family.index;;ip=%index;" *)
      match get_env "fam" env with
      [ Vfam fam _ _ _ ->
          Printf.sprintf "i=%d;ip=%d" (Adef.int_of_ifam fam.fam_index)
            (Adef.int_of_iper p.cle_index)
      | _ -> raise Not_found ]
  | "father_age_at_birth" -> string_of_parent_age conf base ep father
  | "first_name" ->
      if not p_auth && conf.hide_names then "x" else p_first_name base p
  | "first_name_key" ->
      if conf.hide_names && not p_auth then ""
      else code_varenv (Name.lower (p_first_name base p))
  | "first_name_key_val" ->
      if conf.hide_names && not p_auth then ""
      else Name.lower (p_first_name base p)
  | "image" -> if not p_auth then "" else sou base p.image
  | "image_html_url" -> string_of_image_url conf base env ep True
  | "image_size" -> string_of_image_size conf base env ep
  | "image_small_size" -> string_of_image_small_size conf base env ep
  | "image_url" -> string_of_image_url conf base env ep False
  | "ind_access" ->
      (* deprecated since 5.00: rather use "i=%index;" *)
      "i=" ^ string_of_int (Adef.int_of_iper p.cle_index)
  | "index" -> string_of_int (Adef.int_of_iper p.cle_index)
  | "mark_descendants" ->
      match get_env "desc_mark" env with
      [ Vdmark r ->
          let tab = Array.create base.data.persons.len False in
          let rec mark_descendants len p =
            let i = Adef.int_of_iper p.cle_index in
            if tab.(i) then ()
            else do {
              tab.(i) := True;
              let u = uget conf base p.cle_index in
              for i = 0 to Array.length u.family - 1 do {
                let des = doi base u.family.(i) in
                for i = 0 to Array.length des.children - 1 do {
                  mark_descendants (len + 1) (pget conf base des.children.(i))
                }
              }
            }
          in
          do {
            mark_descendants 0 p;
            r.val := tab;
            "";
          }
      | _ -> raise Not_found ]
  | "mother_age_at_birth" -> string_of_parent_age conf base ep mother
  | "misc_names" ->
      if p_auth then
        let list = Gutil.person_misc_names base p in
        let list =
          let first_name = p_first_name base p in
          let surname = p_surname base p in
          if first_name <> "?" && surname <> "?" then
            [Name.lower (first_name ^ " " ^ surname) :: list]
          else list
        in
        if list <> [] then
          "<ul>\n" ^
          List.fold_left (fun s n -> s ^ "<li>" ^ n ^ "\n") "" list ^
          "</ul>\n"
        else ""
      else ""
  | "nb_children" ->
      match get_env "fam" env with
      [ Vfam _ _ des _ -> string_of_int (Array.length des.children)
      | _ ->
          let n =
            List.fold_left
              (fun n ifam -> n + Array.length (doi base ifam).children) 0
              (Array.to_list u.family)
          in
          string_of_int n ]
  | "nb_families" -> string_of_int (Array.length u.family)
  | "notes" ->
      if p_auth then
        let env = [('i', fun () -> Util.default_image_name base p)] in
        let s = sou base p.notes in
        let s =
          let lines = Wiki.html_of_tlsw conf s in
          Wiki.syntax_links conf "NOTES" (Notes.file_path conf)
            (String.concat "\n" lines)
        in
        string_with_macros conf env s
      else ""
  | "occ" -> if p_auth then string_of_int p.occ else ""
  | "occupation" ->
      if p_auth then string_with_macros conf [] (sou base p.occupation)
      else ""
  | "on_baptism_date" ->
      match (p_auth, Adef.od_of_codate p.baptism) with
      [ (True, Some d) -> Date.string_of_ondate conf d
      | _ -> "" ]
  | "on_birth_date" ->
      match (p_auth, Adef.od_of_codate p.birth) with
      [ (True, Some d) -> Date.string_of_ondate conf d
      | _ -> "" ]
  | "on_burial_date" ->
      match p.burial with
      [ Buried cod ->
          match (p_auth, Adef.od_of_codate cod) with
          [ (True, Some d) -> Date.string_of_ondate conf d
          | _ -> "" ]
      | _ -> raise Not_found ]
  | "on_cremation_date" ->
      match p.burial with
      [ Cremated cod ->
          match (p_auth, Adef.od_of_codate cod) with
          [ (True, Some d) -> Date.string_of_ondate conf d
          | _ -> "" ]
      | _ -> raise Not_found ]
  | "on_death_date" ->
      match (p_auth, p.death) with
      [ (True, Death _ d) ->
          let d = Adef.date_of_cdate d in
          Date.string_of_ondate conf d
      | _ -> "" ]
  | "prev_fam_father" ->
      match get_env "prev_fam" env with
      [ Vfam fam (ifath, _, _) _ _ -> string_of_int (Adef.int_of_iper ifath)
      | _ -> raise Not_found ]
  | "prev_fam_index" ->
      match get_env "prev_fam" env with
      [ Vfam fam _ _ _ -> string_of_int (Adef.int_of_ifam fam.fam_index)
      | _ -> raise Not_found ]
  | "prev_fam_mother" ->
      match get_env "prev_fam" env with
      [ Vfam fam (_, imoth, _) _ _ -> string_of_int (Adef.int_of_iper imoth)
      | _ -> raise Not_found ]
  | "public_name" -> if not p_auth then "" else sou base p.public_name
  | "qualifier" ->
      match p.qualifiers with
      [ [nn :: _] when p_auth -> sou base nn
      | _ -> "" ]
  | "sex" -> string_of_int (index_of_sex p.sex)
  | "sosa_in_list" ->
      match get_env "all_gp" env with
      [ Vallgp all_gp ->
          match get_link all_gp p.cle_index with
          [ Some (GP_person s _ _) -> Num.to_string s
          | _ -> "" ]
      | _ -> raise Not_found ]
  | "sosa_link" ->
      match get_env "sosa" env with
      [ Vsosa x ->
          match x with
          [ Some (n, q) ->
              Printf.sprintf "m=RL;i1=%d;i2=%d;b1=1;b2=%s"
                (Adef.int_of_iper p.cle_index) (Adef.int_of_iper q.cle_index)
                (Num.to_string n)
          | None -> "" ]
      | _ -> raise Not_found ] 
  | "source" ->
      match get_env "src" env with
      [ Vstring s ->
          let env = [('i', fun () -> Util.default_image_name base p)] in
          string_with_macros conf env s
      | _ -> raise Not_found ]
  | "surname" ->
      if not p_auth && conf.hide_names then "x" else p_surname base p
  | "surname_begin" ->
      if not p_auth && conf.hide_names then ""
      else surname_begin base (p_surname base p)
  | "surname_end" ->
      if not p_auth && conf.hide_names then "x"
      else surname_end base (p_surname base p)
  | "surname_key" ->
      if conf.hide_names && not p_auth then ""
      else code_varenv (Name.lower (p_surname base p))
  | "surname_key_val" ->
      if conf.hide_names && not p_auth then ""
      else Name.lower (p_surname base p)
  | "title" -> person_title conf base p
  | _ -> raise Not_found ]
and eval_family_field_var conf base env
  ((fam, (ifath, imoth, _), _, m_auth) as fcd) loc
=
  fun
  [ ["father" :: sl] ->
      let ep = make_ep conf base ifath in
      eval_person_field_var conf base env ep loc sl
  | ["marriage_date" :: sl] ->
      match Adef.od_of_codate fam.marriage with
      [ Some d when m_auth -> eval_date_field_var d sl
      | _ -> VVstring "" ]
  | ["mother" :: sl] ->
      let ep = make_ep conf base imoth in
      eval_person_field_var conf base env ep loc sl
  | [s] -> str_val (eval_str_family_field conf base env fcd loc s)
  | _ -> raise Not_found ]
and eval_str_family_field conf base env (f, _, _, _) loc =
  fun
  [ "index" -> string_of_int (Adef.int_of_ifam f.fam_index)
  | _ -> raise Not_found ]
and simple_person_text conf base p p_auth =
  if p_auth then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t
    | None -> person_text conf base p ]
  else if conf.hide_names then "x x"
  else person_text conf base p
and string_of_died conf base env p p_auth =
  if p_auth then
    let is = index_of_sex p.sex in
    match p.death with
    [ Death dr _ ->
        match dr with
        [ Unspecified -> transl_nth conf "died" is
        | Murdered -> transl_nth conf "murdered" is
        | Killed -> transl_nth conf "killed (in action)" is
        | Executed -> transl_nth conf "executed (legally killed)" is
        | Disappeared -> transl_nth conf "disappeared" is ]
    | DeadYoung -> transl_nth conf "died young" is
    | DeadDontKnowWhen -> transl_nth conf "died" is
    | _ -> "" ]
  else ""
and string_of_image_url conf base env (p, _, _, p_auth) html =
  if p_auth then
    let v =
      image_and_size conf base p (limited_image_size max_im_wid max_im_wid)
    in
    match v with
    [ Some (True, fname, _) ->
        let s = Unix.stat fname in
        let b = acces conf base p in
        let k = default_image_name base p in
        Format.sprintf "%sm=IM%s;d=%d;%s;k=/%s" (commd conf)
          (if html then "H" else "")
          (int_of_float
             (mod_float s.Unix.st_mtime (float_of_int max_int)))
          b k
    | Some (False, link, _) -> link
    | None -> "" ]
  else ""
and string_of_parent_age conf base (p, a, _, p_auth) parent =
  match parents a with
  [ Some ifam ->
      let cpl = coi base ifam in
      let pp = pget conf base (parent cpl) in
      if p_auth && authorized_age conf base pp then
        match (Adef.od_of_codate pp.birth, Adef.od_of_codate p.birth) with
        [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
            Date.string_of_age conf (time_gone_by d1 d2)
        | _ -> "" ]
      else ""
  | None -> raise Not_found ]
and string_of_int_env var env =
  match get_env var env with
  [ Vint x -> string_of_int x
  | _ -> raise Not_found ]
and obsolete_eval conf base env (p, a, u, p_auth) loc =
  fun
  [ "married_to" ->
      let s =
        match get_env "fam" env with
        [ Vfam fam (_, _, ispouse) des m_auth ->
           let format = relation_txt conf p.sex fam in
           Printf.sprintf (fcapitale format)
             (fun _ ->
                if m_auth then string_of_marriage_text conf base fam else "")
        | _ -> raise Not_found ]
      in
      obsolete loc "4.08" "married_to" "" (str_val s)
  | _ -> raise Not_found ]
;

value eval_transl conf env upp s c =
  match c with
  [ "n" | "s" | "w" ->
      let n =
        match c with
        [ "n" ->
            (* replaced by %apply;nth([...],sex) *)
            match get_env "p" env with
            [ Vind p _ _ -> 1 - index_of_sex p.sex
            | _ -> 2 ]
        | "s" ->
            match get_env "child" env with
            [ Vind p _ _ -> index_of_sex p.sex
            | _ ->
                match get_env "p" env with
                [ Vind p _ _ -> index_of_sex p.sex
                | _ -> 2 ] ]
        | "w" ->
            match get_env "fam" env with
            [ Vfam fam _ _ _ -> if Array.length fam.witnesses = 1 then 0 else 1
            | _ -> 0 ]
        | _ -> assert False ]
      in
      let r = Gutil.nominative (Util.transl_nth conf s n) in
      if upp then capitale r else r
  | _ ->
      Templ.eval_transl conf upp s c ]
;

value print_foreach conf base print_ast eval_expr =
  let eval_int_expr env ep e =
    let s = eval_expr env ep e in
    try int_of_string s with [ Failure _ -> raise Not_found ]
  in
  let rec print_foreach env ini_ep loc s sl ell al =
    let rec loop ((_, a, _, _) as ep) efam =
      fun 
      [ [s] -> print_simple_foreach env ell al ini_ep ep efam loc s
      | ["ancestor" :: sl] ->
          let ip_ifamo =
            match get_env "ancestor" env with
            [ Vanc (GP_person _ ip ifamo) -> Some (ip, ifamo)
            | Vanc (GP_same _ _ ip) -> Some (ip, None)
            | _ -> None ]
          in
          match ip_ifamo with
          [ Some (ip, ifamo) ->
              let ((_, _, _, p_auth) as ep) = make_ep conf base ip in
              let efam =
                match ifamo with
                [ Some ifam ->
                    let cpl = coi base ifam in
                    let ifath = father cpl in
                    let imoth = mother cpl in
                    let ispouse = if ip = ifath then imoth else ifath in
                    let cpl = (ifath, imoth, ispouse) in
                    let m_auth =
                      p_auth && authorized_age conf base (poi base ispouse)
                    in
                    Vfam (foi base ifam) cpl (doi base ifam) m_auth
                | None -> efam ]
              in
              loop ep efam sl
          | _ -> raise Not_found ]
      | ["child" :: sl] ->
          match get_env "child" env with
          [ Vind p a u ->
              let auth =
                match get_env "auth" env with
                [ Vbool True -> authorized_age conf base p
                | _ -> False ]
              in
              let ep = (p, a, u, auth) in
              loop ep efam sl
          | _ -> raise Not_found ]
      | ["father" :: sl] ->
          match parents a with
          [ Some ifam ->
              let cpl = coi base ifam in
              let ((_, _, _, p_auth) as ep) = make_ep conf base (father cpl) in
              let imoth = mother cpl in
              let cpl = (father cpl, imoth, imoth) in
              let m_auth =
                p_auth && authorized_age conf base (poi base imoth)
              in
              let efam = Vfam (foi base ifam) cpl (doi base ifam) m_auth in
              loop ep efam sl
          | None ->
              warning_use_has_parents_before_parent loc "father" () ]
      | ["mother" :: sl] ->
          match parents a with
          [ Some ifam ->
              let cpl = coi base ifam in
              let ((_, _, _, p_auth) as ep) = make_ep conf base (mother cpl) in
              let ifath = father cpl in
              let cpl = (ifath, mother cpl, ifath) in
              let m_auth =
                p_auth && authorized_age conf base (poi base ifath)
              in
              let efam = Vfam (foi base ifam) cpl (doi base ifam) m_auth in
              loop ep efam sl
          | None ->
              warning_use_has_parents_before_parent loc "mother" () ]
      | ["self" :: sl] -> loop ep efam sl
      | ["spouse" :: sl] ->
          match efam with
          [ Vfam _ (_, _, ip) _ _ ->
              let ep = make_ep conf base ip in
              loop ep efam sl
          | _ -> raise Not_found ]
      | _ -> raise Not_found ]
    in
    let efam = get_env "fam" env in
    loop ini_ep efam [s :: sl]
  and print_simple_foreach env el al ini_ep ep efam loc =
    fun
    [ "alias" -> print_foreach_alias env al ep
    | "ancestor" -> print_foreach_ancestor env al ep
    | "ancestor_level" -> print_foreach_ancestor_level env el al ep
    | "ancestor_level2" -> print_foreach_ancestor_level2 env al ep
    | "ancestor_surname" -> print_foreach_anc_surn env el al loc ep
    | "ancestor_tree_line" -> print_foreach_ancestor_tree env el al ep
    | "cell" -> print_foreach_cell env el al ep
    | "child" -> print_foreach_child env al ep efam
    | "cousin_level" -> print_foreach_level "max_cous_level" env al ep
    | "descendant_level" -> print_foreach_descendant_level env al ep
    | "family" -> print_foreach_family env al ini_ep ep
    | "first_name_alias" -> print_foreach_first_name_alias env al ep
    | "nobility_title" -> print_foreach_nobility_title env al ep
    | "parent" -> print_foreach_parent env al ep
    | "qualifier" -> print_foreach_qualifier env al ep
    | "related" -> print_foreach_related env al ep
    | "relation" -> print_foreach_relation env al ep
    | "sorted_list_item" -> print_foreach_sorted_list_item env al ep
    | "source" -> print_foreach_source env al ep
    | "surname_alias" -> print_foreach_surname_alias env al ep
    | "witness" -> print_foreach_witness env al ep efam
    | "witness_relation" -> print_foreach_witness_relation env al ep
    | _ -> raise Not_found ]
  and print_foreach_alias env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      List.iter
        (fun a ->
           let env = [("alias", Vstring (sou base a)) :: env] in
           List.iter (print_ast env ep) al)
        p.aliases
    else ()
  and print_foreach_ancestor env al ((p, _, _, p_auth) as ep) =
    match get_env "gpl" env with
    [ Vgpl gpl ->
        List.iter
          (fun gp ->
             match gp with
             [ GP_missing _ _ -> ()
             | _ ->
                 let env = [("ancestor", Vanc gp) :: env] in
                 List.iter (print_ast env ep) al ])
          gpl
    | _ -> () ]
  and print_foreach_ancestor_level env el al ((p, _, _, _) as ep) =
    let max_level =
      match el with
      [ [[e]] -> eval_int_expr env ep e
      | [] ->
          match get_env "max_anc_level" env with
          [ Vint n -> n
          | _ -> 0 ]
      | _ -> raise Not_found ]
    in
    let mark = Array.create base.data.persons.len Num.zero in
    loop [GP_person Num.one p.cle_index None] 1 where rec loop gpl i =
      if i > max_level then ()
      else
        let env = [("gpl", Vgpl gpl); ("level", Vint i) :: env] in
        do {
          List.iter (print_ast env ep) al;
          let gpl = next_generation conf base mark gpl in
          loop gpl (succ i)
        }
  and print_foreach_ancestor_level2 env al ((p, _, _, _) as ep) =
    let max_lev = "max_anc_level" in
    let max_level =
      match get_env max_lev env with
      [ Vint n -> n
      | _ -> 0 ]
    in
    let mark = Array.create base.data.persons.len Num.zero in
    loop [GP_person Num.one p.cle_index None] 1 where rec loop gpl i =
      if i > max_level then ()
      else
        let env = [("gpl", Vgpl gpl); ("level", Vint i) :: env] in
        do {
          List.iter (print_ast env ep) al;
          for i = 0 to base.data.persons.len - 1 do { mark.(i) := Num.zero };
          let gpl = next_generation2 conf base mark gpl in
          loop gpl (succ i)
        }
  and print_foreach_anc_surn env el al loc ((p, _, _, _) as ep) =
    let max_level =
      match el with
      [ [[e]] -> eval_int_expr env ep e
      | [] ->
          match get_env "max_anc_level" env with
          [ Vint n -> n
          | _ -> 0 ]
      | _ -> raise Not_found ]
    in
    let list = build_surnames_list conf base max_level p in
    List.iter
      (fun (a, (((b, c, d), e), f)) ->
         let env = [("ancestor", Vanc_surn (a, b, c, d, e, f, loc)) :: env] in
         List.iter (print_ast env ep) al)
      list
  and print_foreach_ancestor_tree env el al ((p, _, _, _) as ep) =
    let (p, max_level) =
      match el with
      [ [[e1]; [e2]] ->
          let ip = eval_int_expr env ep e1 in
          let max_level = eval_int_expr env ep e2 in
          (pget conf base (Adef.iper_of_int ip), max_level)
      | [[e]] ->
          (p, eval_int_expr env ep e)
      | [] ->
          match get_env "max_anc_level" env with
          [ Vint n -> (p, n)
          | _ -> (p, 0) ]
      | _ -> raise Not_found ]
    in
    let gen = tree_generation_list conf base max_level p in
    loop True gen where rec loop first =
      fun
      [ [g :: gl] ->
          let env =
            [("celll", Vcelll g); ("first", Vbool first);
             ("last", Vbool (gl = [])) :: env]
          in
          do {
            List.iter (print_ast env ep) al;
            loop False gl
          }
      | [] -> () ]
  and print_foreach_cell env el al ((p, _, _, _) as ep) =
    let celll =
      match get_env "celll" env with
      [ Vcelll celll -> celll
      | _ -> raise Not_found ]
    in
    list_iter_first
      (fun first cell ->
         let env = [("cell", Vcell cell); ("first", Vbool first) :: env] in
         List.iter (print_ast env ep) al)
      celll
  and print_foreach_child env al ep =
    fun
    [ Vfam _ _ des _ ->
        let auth =
          List.for_all (fun ip -> authorized_age conf base (pget conf base ip))
            (Array.to_list des.children)
        in
        let env = [("auth", Vbool auth) :: env] in
        let n =
          let p =
            match get_env "p" env with
            [ Vind p _ _ -> p
            | _ -> assert False ]
          in
          let rec loop i =
            if i = Array.length des.children then -2
            else if des.children.(i) = p.cle_index then i
            else loop (i + 1)
          in
          loop 0
        in
        Array.iteri
          (fun i ip ->
             let p = pget conf base ip in
             let a = aget conf base ip in
             let u = uget conf base ip in
             let env = [("#loop", Vint 0) :: env] in
             let env = [("child", Vind p a u) :: env] in
             let env = [("child_cnt", Vint (i + 1)) :: env] in
             let env =
               if i = n - 1 && not (is_hidden p) then
                 [("pos", Vstring "prev") :: env]
               else if i = n then [("pos", Vstring "self") :: env]
               else if i = n + 1 && not (is_hidden p) then
                 [("pos", Vstring "next") :: env]
               else env
             in
             let ep =
               let p_auth = authorized_age conf base p in
               (p, a, u, p_auth)
             in
             List.iter (print_ast env ep) al)
          des.children
    | _ -> () ]
  and print_foreach_descendant_level env al ep =
    let max_level =
      match get_env "max_desc_level" env with
      [ Vint n -> n
      | _ -> 0 ]
    in
    loop 0 where rec loop i =
      if i > max_level then ()
      else
        let env = [("level", Vint i) :: env] in
        do {
          List.iter (print_ast env ep) al;
          loop (succ i)
        }
  and print_foreach_family env al ini_ep (p, _, u, _) =
    loop None 0 where rec loop prev i =
      if i = Array.length u.family then ()
      else
        let ifam = u.family.(i) in
        let fam = foi base ifam in
        let cpl = coi base ifam in
        let des = doi base ifam in
        let ifath = father cpl in
        let imoth = mother cpl in
        let ispouse = spouse p.cle_index cpl in
        let cpl = (ifath, imoth, ispouse) in
        let m_auth =
           authorized_age conf base (poi base ifath) &&
           authorized_age conf base (poi base imoth)
        in
        let vfam = Vfam fam cpl des m_auth in
        let env = [("#loop", Vint 0) :: env] in
        let env = [("fam", vfam) :: env] in
        let env = [("family_cnt", Vint (i + 1)) :: env] in
        let env =
          match prev with
          [ Some vfam -> [("prev_fam", vfam) :: env]
          | None -> env ]
        in
        do {
          List.iter (print_ast env ini_ep) al;
          loop (Some vfam) (i + 1);
        }
  and print_foreach_first_name_alias env al ((p, _, _, p_auth) as ep)
  =
    if p_auth then
      List.iter
        (fun s ->
           let env = [("first_name_alias", Vstring (sou base s)) :: env] in
           List.iter (print_ast env ep) al)
        p.first_names_aliases
    else ()
  and print_foreach_level max_lev env al ((p, _, _, _) as ep) =
    let max_level =
      match get_env max_lev env with
      [ Vint n -> n
      | _ -> 0 ]
    in
    loop 1 where rec loop i =
      if i > max_level then ()
      else
        let env = [("level", Vint i) :: env] in
        do {
          List.iter (print_ast env ep) al;
          loop (succ i)
        }
  and print_foreach_nobility_title env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      let titles = nobility_titles_list conf p in
      list_iter_first
        (fun first x ->
           let env = [("nobility_title", Vtitle p x) :: env] in
           let env = [("first", Vbool first) :: env] in
           List.iter (print_ast env ep) al)
        titles
    else ()
  and print_foreach_parent env al ((_, a, _, _) as ep) =
    match parents a with
    [ Some ifam ->
        let cpl = coi base ifam in
        Array.iter
          (fun iper ->
             let p = pget conf base iper in
             let a = aget conf base iper in
             let u = uget conf base iper in
             let env = [("parent", Vind p a u) :: env] in
             List.iter (print_ast env ep) al)
          (parent_array cpl)
    | None -> () ]
  and print_foreach_qualifier env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      list_iter_first
        (fun first nn ->
           let env = [("qualifier", Vstring (sou base nn)) :: env] in
           let env = [("first", Vbool first) :: env] in
           List.iter (print_ast env ep) al)
        p.qualifiers
    else ()
  and print_foreach_relation env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      List.iter
        (fun r ->
           let env = [("rel", Vrel r None) :: env] in
           List.iter (print_ast env ep) al)
        p.rparents
    else ()
  and print_foreach_related env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      let list =
        List.fold_left
          (fun list ic ->
             let c = pget conf base ic in
             loop c.rparents where rec loop =
               fun
               [ [r :: rl] ->
                   match r.r_fath with
                   [ Some ip when ip = p.cle_index -> [(c, r) :: list]
                   | _ ->
                       match r.r_moth with
                       [ Some ip when ip = p.cle_index -> [(c, r) :: list]
                       | _ -> loop rl ] ]
               | [] -> list ])
          [] p.related
      in
      let list =
        List.sort
          (fun (c1, _) (c2, _) ->
             let d1 =
               match Adef.od_of_codate c1.baptism with
               [ None -> Adef.od_of_codate c1.birth
               | x -> x ]
             in
             let d2 =
               match Adef.od_of_codate c2.baptism with
               [ None -> Adef.od_of_codate c2.birth
               | x -> x ]
             in
             match (d1, d2) with
             [ (Some d1, Some d2) ->
                 if strictly_before d1 d2 then -1 else 1
             | _ -> -1 ])
        (List.rev list)
      in
      List.iter
        (fun (c, r) ->
           let env = [("rel", Vrel r (Some c)) :: env] in
           List.iter (print_ast env ep) al)
        list
    else ()
  and print_foreach_sorted_list_item env al ep =
    let list =
      match get_env "list" env with
      [ Vslist l -> SortedList.elements l.val
      | _ -> [] ]
    in
    loop (Vslistlm []) list where rec loop prev_item =
      fun
      [ [_ :: sll] as gsll ->
           let item = Vslistlm gsll in
           let env = [("item", item); ("prev_item", prev_item) :: env] in
           do {
             List.iter (print_ast env ep) al;
             loop item sll
           }
      | [] -> () ]
  and print_foreach_source env al ((p, _, u, p_auth) as ep) =
    let rec insert_loop typ src =
      fun
      [ [(typ1, src1) :: srcl] ->
          if src = src1 then [(typ1 ^ ", " ^ typ, src1) :: srcl]
          else [(typ1, src1) :: insert_loop typ src srcl]
      | [] -> [(typ, src)] ]
    in
    let insert typ src srcl = insert_loop (nominative typ) src srcl in
    let srcl = [] in
    let srcl =
      if not conf.hide_names || p_auth then
        insert (transl_nth conf "person/persons" 0) p.psources srcl
      else srcl
    in
    let srcl =
      if p_auth then
        let srcl = insert (transl_nth conf "birth" 0) p.birth_src srcl in
        let srcl = insert (transl_nth conf "baptism" 0) p.baptism_src srcl in
        let srcl = insert (transl_nth conf "death" 0) p.death_src srcl in
        let srcl = insert (transl_nth conf "burial" 0) p.burial_src srcl in srcl
      else srcl
    in
    let (srcl, _) =
      Array.fold_left
        (fun (srcl, i) ifam ->
           let fam = foi base ifam in
           let lab =
             if Array.length u.family == 1 then "" else " " ^ string_of_int i
           in
           let srcl =
             if p_auth then
               let src_typ = transl_nth conf "marriage/marriages" 0 in
               insert (src_typ ^ lab) fam.marriage_src srcl
             else srcl
           in
           let src_typ = transl_nth conf "family/families" 0 in
           (insert (src_typ ^ lab) fam.fsources srcl, i + 1))
        (srcl, 1) u.family
    in
    let print_src (src_typ, src) =
      let s = sou base src in
      if s = "" then ()
      else
        let env = [("src_typ", Vstring src_typ); ("src", Vstring s) :: env] in
        List.iter (print_ast env ep) al
    in
    List.iter print_src srcl
  and print_foreach_surname_alias env al ((p, _, _, p_auth) as ep) =
    if p_auth then
      List.iter
        (fun s ->
           let env = [("surname_alias", Vstring (sou base s)) :: env] in
           List.iter (print_ast env ep) al)
        p.surnames_aliases
    else ()
  and print_foreach_witness env al ep =
    fun
    [ Vfam fam _ _ True ->
        list_iter_first
          (fun first ip ->
             let p = pget conf base ip in
             let a = aget conf base ip in
             let u = uget conf base ip in
             let env = [("witness", Vind p a u) :: env] in
             let env = [("first", Vbool first) :: env] in
             List.iter (print_ast env ep) al)
          (Array.to_list fam.witnesses)
    | _ -> () ]
  and print_foreach_witness_relation env al ((p, _, _, _) as ep) =
    let list =
      let list = ref [] in
      do {
        make_list p.related where rec make_list =
          fun
          [ [ic :: icl] ->
              do {
                let c = pget conf base ic in
                if c.sex = Male then
                  Array.iter
                    (fun ifam ->
                       let fam = foi base ifam in
                       if array_memq p.cle_index fam.witnesses then
                         list.val := [(ifam, fam) :: list.val]
                       else ())
                    (uget conf base ic).family
                else ();
                make_list icl
              }
          | [] -> () ];
        list.val
      }
    in
    let list =
      List.sort
        (fun (_, fam1) (_, fam2) ->
           match
             (Adef.od_of_codate fam1.marriage, Adef.od_of_codate fam2.marriage)
           with
           [ (Some d1, Some d2) ->
               if strictly_before d1 d2 then -1
               else if strictly_before d2 d1 then 1
               else 0
           | _ -> 0 ])
        list
    in
    List.iter
      (fun (ifam, fam) ->
         let cpl = coi base ifam in
         let des = doi base ifam in
         let ifath = father cpl in
         let imoth = mother cpl in
         let cpl = (ifath, imoth, imoth) in
         let m_auth =
           authorized_age conf base (poi base ifath) &&
           authorized_age conf base (poi base imoth)
         in
         if m_auth then
           let env = [("fam", Vfam fam cpl des True) :: env] in
           List.iter (print_ast env ep) al
         else ())
      list
  in
  print_foreach
;

value eval_predefined_apply conf env f vl =
  match (f, vl) with
  [ ("a_of_b", [s1; s2]) -> transl_a_of_b conf s1 s2
  | ("a_of_b_gr_eq_lev", [s1; s2]) -> transl_a_of_gr_eq_gen_lev conf s1 s2
  | ("add_in_sorted_list", sl) ->
      match get_env "list" env with
      [ Vslist l -> do { l.val := SortedList.add sl l.val; "" }
      | _ -> raise Not_found ]
  | ("hexa", [s]) -> Util.hexa_string s
  | ("initial", [s]) ->
      if String.length s = 0 then ""
      else String.sub s 0 (Util.index_of_next_char s 0)
  | ("lazy_print", [v]) ->
      match get_env "lazy_print" env with
      [ Vlazyp r -> do { r.val := Some v; "" }
      | _ -> raise Not_found ]
  | ("min", [s :: sl]) ->
      try
        let m =
          List.fold_right (fun s -> min (int_of_string s)) sl (int_of_string s)
        in
        string_of_int m
      with
      [ Failure _ -> raise Not_found ]
  | _ -> raise Not_found ]
;

value interp_templ templ_fname conf base p =
  let _ = do { template_file.val := templ_fname ^ ".txt"; } in
  let a = aget conf base p.cle_index in
  let u = uget conf base p.cle_index in
  let ep = (p, a, u, authorized_age conf base p) in
  let emal =
    match p_getint conf.env "v" with
    [ Some i -> i
    | None -> 120 ]
  in
  let env =
    let sosa_ref_l =
      let sosa_ref () = Util.find_sosa_ref conf base in
      Lazy.lazy_from_fun sosa_ref
    in
    let sosa () = Vsosa (find_sosa conf base p sosa_ref_l) in
    let desc_level_table_l =
      let dlt () = make_desc_level_table conf base (*infinite*)(limit_desc conf) p in
      Lazy.lazy_from_fun dlt
    in
    let mal () =  Vint (max_ancestor_level conf base p.cle_index emal + 1) in
    let mcl () =  Vint (max_cousin_level conf base p) in
    let mdl () =  Vint (max_descendant_level conf base desc_level_table_l) in
    let nldb () =
      let bdir = Util.base_path [] (conf.bname ^ ".gwb") in
      let fname = Filename.concat bdir "notes_links" in
      Vnldb (NotesLinks.read_db_from_file fname)
    in
    let all_gp () = Vallgp (get_all_generations conf base p) in
    [("p", Vind p a u);
     ("p_auth", Vbool (authorized_age conf base p));
     ("count", Vcnt (ref 0));
     ("list", Vslist (ref SortedList.empty));
     ("desc_mark", Vdmark (ref [| |]));
     ("lazy_print", Vlazyp (ref None));
     ("sosa",  Vlazy (Lazy.lazy_from_fun sosa));
     ("sosa_ref", Vsosa_ref sosa_ref_l);
     ("max_anc_level", Vlazy (Lazy.lazy_from_fun mal));
     ("max_cous_level", Vlazy (Lazy.lazy_from_fun mcl));
     ("max_desc_level", Vlazy (Lazy.lazy_from_fun mdl));
     ("desc_level_table", Vdesclevtab desc_level_table_l);
     ("nldb", Vlazy (Lazy.lazy_from_fun nldb));
     ("all_gp", Vlazy (Lazy.lazy_from_fun all_gp))]
  in
  Templ.interp conf base templ_fname (eval_var conf base) (eval_transl conf)
    (eval_predefined_apply conf) get_vother set_vother
    (print_foreach conf base) env ep
;

(* Main *)

value print conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match parents (aget conf base p.cle_index) with
        [ Some ifam -> sou base (foi base ifam).origin_file
        | None -> "" ]
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env) with
      [ Not_found -> None ]
  in
  match passwd with
  [ Some (src, passwd) when passwd <> conf.passwd ->
      Util.unauthorized conf src
  | _ ->
      interp_templ "perso" conf base p ]
;

value limit_by_tree conf =
  match p_getint conf.base_env "max_anc_tree" with
  [ Some x -> max 1 x
  | None -> 7 ]
;

value print_ancestors_dag conf base v p =
  let v = min (limit_by_tree conf) v in
  let set =
    loop Dag.Pset.empty v p.cle_index where rec loop set lev ip =
      let set = Dag.Pset.add ip set in
      if lev <= 1 then set
      else
        match parents (aget conf base ip) with
        [ Some ifam ->
            let cpl = coi base ifam in
            let set = loop set (lev - 1) (mother cpl) in
            loop set (lev - 1) (father cpl)
        | None -> set ]
  in
  let elem_txt p = Dag.Item p "" in
  let vbar_txt ip =
    let p = poi base ip in
    Printf.sprintf "%sm=A;t=T;v=%d;%s;dag=on" (commd conf) v
      (acces conf base p)
  in
  let page_title = Util.capitale (Util.transl conf "tree") in
  Dag.make_and_print_dag conf base elem_txt vbar_txt True set [] page_title ""
;

value print_ascend conf base p =
  match
    (p_getenv conf.env "t", p_getenv conf.env "dag", p_getint conf.env "v")
  with
  [ (Some "T", Some "on", Some v) -> print_ancestors_dag conf base v p
  | _ ->
      let templ =
        match p_getenv conf.env "t" with
        [ Some ("F" | "H" | "L") -> "anclist"
        | Some ("D" | "G" | "M" | "N") -> "ancsosa"
        | Some ("A" | "C" | "T") -> "anctree"
        | _ -> "ancmenu" ]
      in
      interp_templ templ conf base p ]
;
