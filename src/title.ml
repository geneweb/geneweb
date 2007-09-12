(* camlp5r ./pa_html.cmo *)
(* $Id: title.ml,v 5.30 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

type date_search = [ JustSelf | AddSpouse | AddChildren ];

value infinity = 10000;

value date_interval conf base t x =
  let d1 =
    ref {day = 0; month = 0; year = infinity; prec = Sure; delta = 0}
  in
  let d2 = ref {day = 0; month = 0; year = 0; prec = Sure; delta = 0} in
  let found = ref False in
  do {
    let rec loop t x =
      let set d =
        do {
          if CheckItem.strictly_before_dmy d d1.val then d1.val := d else ();
          if CheckItem.strictly_after_dmy d d2.val then d2.val := d else ();
          found.val := True;
        }
      in
      do {
        match Adef.od_of_codate (get_birth x) with
        [ Some (Dgreg d _) -> set d
        | _ -> () ];
        match Adef.od_of_codate (get_baptism x) with
        [ Some (Dgreg d _) -> set d
        | _ -> () ];
        match CheckItem.date_of_death (get_death x) with
        [ Some (Dgreg d _) -> set d
        | _ -> if get_death x = NotDead then set conf.today else () ];
        List.iter
          (fun t ->
             do {
               match Adef.od_of_codate t.t_date_start with
               [ Some (Dgreg d _) -> set d
               | _ -> () ];
               match Adef.od_of_codate t.t_date_end with
               [ Some (Dgreg d _) -> set d
               | _ -> () ];
             })
          (Util.nobtit conf base x);
        match t with
        [ JustSelf -> ()
        | _ ->
            let u = pget conf base (get_key_index x) in
            Array.iter
              (fun ifam ->
                 let fam = foi base ifam in
                 let md = get_marriage fam in
                 let conj = spouse (get_key_index x) fam in
                 do {
                   match Adef.od_of_codate md with
                   [ Some (Dgreg d _) -> set d
                   | _ -> () ];
                   loop JustSelf (pget conf base conj);
                   match t with
                   [ AddChildren ->
                       Array.iter (fun e -> loop JustSelf (pget conf base e))
                         (get_children fam)
                   | _ -> () ];
                 })
              (get_family u) ];
      }
    in
    loop t x;
    if found.val then Some (d1.val, d2.val) else None
  }
;

value compare_title_dates conf base (x1, t1) (x2, t2) =
  match
    ((get_birth x1, Adef.od_of_codate t1.t_date_start,
      Adef.od_of_codate t1.t_date_end, get_death x1),
     (get_birth x2, Adef.od_of_codate t2.t_date_start,
      Adef.od_of_codate t2.t_date_end, get_death x2))
  with
  [ ((_, Some (Dgreg d1 _), _, _), (_, Some (Dgreg d2 _), _, _)) ->
      if CheckItem.strictly_before_dmy d1 d2 then -1
      else if d1.year = d2.year then
        match
          (Adef.od_of_codate t1.t_date_end, Adef.od_of_codate t2.t_date_end)
        with
        [ (Some d1, Some d2) ->
            if not (CheckItem.strictly_after d1 d2) then -1 else 1
        | _ -> -1 ]
      else 1
  | ((_, _, Some (Dgreg _ _ as d1), _), (_, _, Some (Dgreg _ _ as d2), _)) ->
      if not (CheckItem.strictly_before d2 d1) then -1 else 1
  | ((_, _, _, Death _ d1), (_, Some d2, _, _))
    when not (CheckItem.strictly_before d2 (Adef.date_of_cdate d1)) ->
      -1
  | ((_, Some (Dgreg _ _ as d1), _, _), (_, _, _, Death _ d2))
    when not (CheckItem.strictly_before d1 (Adef.date_of_cdate d2)) ->
      1
  | _ ->
      match
        (date_interval conf base JustSelf x1,
         date_interval conf base JustSelf x2)
      with
      [ (Some (d11, d12), Some (d21, d22)) ->
          if not (CheckItem.strictly_before_dmy d21 d12) then -1
          else if not (CheckItem.strictly_before_dmy d11 d22) then 1
          else if CheckItem.strictly_after_dmy d21 d11 then -1
(*
          else if CheckItem.strictly_after_dmy d22 d12 then -1
*)
          else 1
      | _ ->
          match
            (date_interval conf base AddSpouse x1,
             date_interval conf base AddSpouse x2)
          with
          [ (Some (d11, d12), Some (d21, d22)) ->
              if not (CheckItem.strictly_before_dmy d21 d12) then -1
              else if not (CheckItem.strictly_before_dmy d11 d22) then 1
              else if not (CheckItem.strictly_before_dmy d22 d12) then -1
              else 1
          | _ ->
              match
                (date_interval conf base AddChildren x1,
                 date_interval conf base AddChildren x2)
              with
              [ (Some (d11, d12), Some (d21, d22)) ->
                  if not (CheckItem.strictly_before_dmy d21 d12) then -1
                  else if not (CheckItem.strictly_before_dmy d11 d22) then 1
                  else if not (CheckItem.strictly_before_dmy d22 d12) then -1
                  else 1
              | (Some _, None) -> -1
              | (None, Some _) -> 1
              | (None, None) -> -1 ] ] ] ]
;

value compare_title_order conf base (x1, t1) (x2, t2) =
  if t1.t_nth = 0 || t2.t_nth = 0 || t1.t_nth = t2.t_nth then
    compare_title_dates conf base (x1, t1) (x2, t2)
  else compare t1.t_nth t2.t_nth
;

value my_alphabetic n1 n2 =
(*
  compare (Name.abbrev (Name.lower n1)) (Name.abbrev (Name.lower n2))
*)
  compare (Name.lower n1) (Name.lower n2)
(**)
;

value string_list_uniq l =
  let l =
    List.fold_left
      (fun l e ->
         match l with
         [ [] -> [e]
         | [x :: _] -> if my_alphabetic e x = 0 then l else [e :: l] ])
      [] l
  in
  List.rev l
;

value string_cnt_list_uniq l =
  let l =
    List.fold_left
      (fun l (e, c) ->
         match l with
         [ [] -> [(e, c)]
         | [(x, d) :: l1] ->
             if my_alphabetic e x = 0 then [(x, c + d) :: l1]
             else [(e, c) :: l] ])
      [] l
  in
  List.rev l
;

value compare_places p1 p2 = compare (Name.lower p1) (Name.lower p2);

value compare_titles t1 t2 = my_alphabetic t1 t2;
value compare_titles2 (t1, _) (t2, _) = my_alphabetic t1 t2;

(*
value strip_abbrev_lower s = Name.strip (Name.abbrev (Name.lower s));
*)
value strip_abbrev_lower s = Name.lower s;
(**)

value select_title_place conf base title place = do {
  let list = ref [] in
  let clean_title = ref title in
  let clean_place = ref place in
  let all_names = ref [] in
  let title1 = strip_abbrev_lower title in
  let place1 = strip_abbrev_lower place in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let select x t =
    if absolute && sou base t.t_ident = title &&
       sou base t.t_place = place ||
       not absolute &&
       strip_abbrev_lower (sou base t.t_ident) = title1 &&
       strip_abbrev_lower (sou base t.t_place) = place1
    then do {
      let tn = sou base t.t_ident in
      clean_title.val := tn;
      clean_place.val := sou base t.t_place;
      list.val := [(x, t) :: list.val];
      if not (List.mem tn all_names.val) then
        all_names.val := [tn :: all_names.val]
      else ();
    }
    else ()
  in
  for i = 0 to nb_of_persons base - 1 do {
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter (select x) (nobtit conf base x)
  };
  (list.val, clean_title.val, clean_place.val, all_names.val)
};

value select_all_with_place conf base place =
  let list = ref [] in
  let clean_place = ref place in
  let place = strip_abbrev_lower place in
  let select x t =
    if strip_abbrev_lower (sou base t.t_place) = place then do {
      clean_place.val := sou base t.t_place; list.val := [(x, t) :: list.val]
    }
    else ()
  in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let x = pget conf base (Adef.iper_of_int i) in
      List.iter (select x) (nobtit conf base x)
    };
    (list.val, clean_place.val)
  }
;

value select_title conf base title =
  let set = ref StrSet.empty in
  let clean_name = ref title in
  let all_names = ref [] in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let title2 = strip_abbrev_lower title in
  let add_place t =
    let tn = sou base t.t_ident in
    if absolute && tn = title ||
       not absolute && strip_abbrev_lower tn = title2
    then do {
      let pn = sou base t.t_place in
      if not (StrSet.mem pn set.val) then do {
        clean_name.val := tn;
        set.val := StrSet.add pn set.val;
      }
      else ();
      if not (List.mem tn all_names.val) then
        all_names.val := [tn :: all_names.val]
      else ();
    }
    else ()
  in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let x = pget conf base (Adef.iper_of_int i) in
      List.iter add_place (nobtit conf base x)
    };
    (StrSet.elements set.val, clean_name.val, all_names.val)
  }
;

value select_place conf base place =
  let list = ref [] in
  let clean_name = ref place in
  let place2 = strip_abbrev_lower place in
  let add_title t =
    let pn = sou base t.t_place in
    if  strip_abbrev_lower pn = place2 then
      let tn = sou base t.t_ident in
      if not (List.mem tn list.val) then do {
        clean_name.val := pn;
        list.val := [tn :: list.val]
      }
      else ()
    else ()
  in
  do {
    for i = 0 to nb_of_persons base - 1 do {
      let x = pget conf base (Adef.iper_of_int i) in
      List.iter add_title (nobtit conf base x)
    };
    (list.val, clean_name.val)
  }
;

value select_all proj conf base =
  let s =
    loop 0 StrSet.empty where rec loop i s =
      if i = nb_of_persons base then s
      else
        let x = pget conf base (Adef.iper_of_int i) in
        let s =
          List.fold_left (fun s t -> StrSet.add (sou base (proj t)) s) s
            (nobtit conf base x)
        in
        loop (i + 1) s
  in
  StrSet.elements s
;

value select_all2 proj conf base = do {
  let ht = Hashtbl.create 1 in
  for i = 0 to nb_of_persons base - 1 do {
    let x = pget conf base (Adef.iper_of_int i) in
    List.iter
      (fun t ->
         let s = sou base (proj t) in
         let cnt =
           try Hashtbl.find ht s with
           [ Not_found -> do {
               let cnt = ref 0 in
               Hashtbl.add ht s cnt;
               cnt
             } ]
         in
         incr cnt)
      (nobtit conf base x);
  };
  Hashtbl.fold (fun s cnt list -> [(s, cnt.val) :: list]) ht []
};

value select_all_titles = select_all2 (fun t -> t.t_ident);
value select_all_places = select_all (fun t -> t.t_place);

value give_access_someone conf base (x, t) list =
  let t_date_start = Adef.od_of_codate t.t_date_start in
  let t_date_end = Adef.od_of_codate t.t_date_end in
  let has_dates =
    match (t_date_start, t_date_end) with
    [ (Some (Dgreg _ _), _) | (_, Some (Dgreg _ _)) -> True
    | _ -> False ]
  in
  do {
    if has_dates then Wserver.wprint "<em>" else ();
    match t_date_start with
    [ Some (Dgreg d _) -> Wserver.wprint "%d" d.year
    | _ -> () ];
    match t_date_end with
    [ Some (Dgreg d _) -> Wserver.wprint "-%d" d.year
    | _ -> () ];
    if has_dates then Wserver.wprint "</em>: " else ();
    if List.mem x list then Wserver.wprint "<em>"
    else Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base x);
    match (t.t_name, get_public_name x, get_qualifiers x) with
    [ (Tmain, pn, [nn :: _]) when sou base pn <> "" ->
        Wserver.wprint "%s <em>%s</em> %s" (sou base pn) (sou base nn)
          (p_surname base x)
    | (Tmain, pn, []) when sou base pn <> "" ->
        Wserver.wprint "%s %s" (sou base pn) (p_surname base x)
    | (Tname n, _, [nn :: _]) ->
        Wserver.wprint "%s <em>%s</em> %s" (sou base n) (sou base nn)
          (p_surname base x)
    | (Tname n, _, []) ->
        Wserver.wprint "%s %s" (sou base n) (p_surname base x)
    | _ -> Wserver.wprint "%s" (person_text conf base x) ];
    Wserver.wprint "\n";
    Wserver.wprint "%s" (Date.short_dates_text conf base x);
    if t.t_nth <> 0 then
      Wserver.wprint " (%s)"
        (if t.t_nth >= 100 then string_of_int t.t_nth
         else transl_nth conf "nth" t.t_nth)
    else ();
    if List.mem x list then Wserver.wprint "</em>"
    else Wserver.wprint "</a>";
  }
;

value give_access_title conf t p =
  do {
    Wserver.wprint "<a href=\"%sm=TT;sm=S;t=%s;p=%s\">" (commd conf)
      (code_varenv t) (code_varenv p);
    Wserver.wprint "%s" (capitale t);
    Wserver.wprint "</a>\n";
  }
;

value give_access_all_titles conf t absolute =
  stag "a" "href=\"%sm=TT;sm=S;t=%s%s\"" (commd conf) (code_varenv t)
    (if absolute then ";a=A" else "")
  begin
    Wserver.wprint "%s" (if absolute then t else capitale t);
  end
;

value give_access_all_places conf t =
  do {
    Wserver.wprint "<a href=\"%sm=TT;sm=S;p=%s\">" (commd conf)
      (code_varenv t);
    Wserver.wprint "... %s" t;
    Wserver.wprint "</a>\n";
  }
;

value print_title_place_list conf base t p t_equiv list =
  let absolute = p_getenv conf.env "a" = Some "A" in
  let title h =
    if h || absolute then do {
      Wserver.wprint "%s" t;
      if p <> "" then Wserver.wprint " %s" p else ()
    }
    else
      list_iter_first
        (fun first t -> do {
           if not first then Wserver.wprint ",\n" else ();
           Wserver.wprint "<a href=\"%sm=TT;sm=S;t=%s;a=A\">" (commd conf)
             (code_varenv t);
           Wserver.wprint "%s</a>" t;
           if p <> "" then do {
             Wserver.wprint "\n<a href=\"%sm=TT;sm=S;p=%s;a=A\">" (commd conf)
               (code_varenv p);
             Wserver.wprint "%s</a>" p;
           }
           else ()
         })
         t_equiv
  in
  do {
    header conf title;
    tag "ul" begin
      let _ =
        List.fold_left
          (fun list x ->
             do {
               stagn "li" begin
                 give_access_someone conf base x list;
               end;
               [fst x :: list]
             })
          [] list
      in
      ();
    end;
    let (list, _) =
      List.fold_left
        (fun (list, n) (p, _) ->
           let list =
             if List.mem_assq p list then list else [(p, n) :: list]
           in
           (list, n + 1))
        ([], 1) list
    in
    match List.rev list with
    [ [_; _ :: _] as list ->
        tag "p" begin
          Wserver.wprint "<a href=\"%sm=RLM" (commd conf);
          let _ =
            List.fold_left
              (fun i (p, n) ->
                 do {
                   Wserver.wprint ";i%d=%d;t%d=%d" i
                     (Adef.int_of_iper (get_key_index p)) i n;
                   i + 1
                 })
              1 list
          in
          Wserver.wprint ";lim=6\">%s</a>\n" (capitale (transl conf "tree"));
        end
    | _ -> () ];
    trailer conf;
  }
;

value print_all_with_place_list conf base p list =
  let title _ = Wserver.wprint "... %s\n" p in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    let _ =
      List.fold_left
        (fun list ((p, t) as x) ->
           do {
             html_li conf;
             give_access_someone conf base x [];
             Wserver.wprint ", %s\n" (sou base t.t_ident);
             Wserver.wprint "\n";
             [fst x :: list]
           })
        [] list
    in
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print_title_place conf base t p =
  let (l, t, p, t_equiv) = select_title_place conf base t p in
  let list = List.sort (compare_title_order conf base) l in
  print_title_place_list conf base t p t_equiv list
;

value print_all_with_place conf base p =
  let (l, p) = select_all_with_place conf base p in
  let list = List.sort (compare_title_dates conf base) l in
  print_all_with_place_list conf base p list
;

value print_places_list conf base t t_equiv list = do {
  let title h =
    if h || List.length t_equiv = 1 then Wserver.wprint "%s" t
    else
      list_iter_first
        (fun first t -> do {
           Wserver.wprint "%s" (if first then "" else ", ");
           give_access_all_titles conf t True;
         })
        t_equiv
  in
  let order s = capitale (Name.lower (surname_end base s)) in
  let list = List.sort (fun s1 s2 -> compare (order s1) (order s2)) list in
  let absolute = p_getenv conf.env "a" = Some "A" in
  let wprint_elem p =
    stag "a" "href=\"%sm=TT;sm=S;t=%s;p=%s%s\"" (commd conf) (code_varenv t)
      (code_varenv p) (if absolute then ";a=A" else "")
    begin
      if p = "" then Wserver.wprint "..."
      else Wserver.wprint "%s%s" (surname_end base p) (surname_begin base p);
    end
  in
  header conf title;
  wprint_in_columns conf order wprint_elem list;
  trailer conf;
};

value print_places conf base t =
  let (l, t, t_equiv) = select_title conf base t in
  let list = string_list_uniq (List.sort compare_places l) in
  match list with
  [ [p] -> print_title_place conf base t p
  | _ -> print_places_list conf base t t_equiv list ]
;

value print_titles conf base p =
  let (l, p) = select_place conf base p in
  let list = string_list_uniq (List.sort compare_titles l) in
  let title _ = Wserver.wprint "... %s" p in
  do {
    header conf title;
    tag "ul" begin
      List.iter (fun t -> stagn "li" begin give_access_title conf t p; end)
        list;
    end;
    if List.length list > 1 then
      stagn "a" "href=\"%sm=TT;sm=A;p=%s\"" (commd conf) (code_varenv p) begin
        Wserver.wprint "%s" (capitale (transl conf "the whole list"));
      end
    else ();
    trailer conf;
  }
;

value print_all_titles conf base = do {
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "all the titles"))
  in
  let list =
    let l = select_all_titles conf base in
    string_cnt_list_uniq (List.sort compare_titles2 l)
  in
  let order (s, _) = capitale (Name.lower s) in
  let wprint_elem (t, cnt) = do {
    give_access_all_titles conf t False;
    Wserver.wprint " (%d)" cnt;
  }
  in
  header conf title;
  wprint_in_columns conf order wprint_elem list;
  trailer conf;
};

value print_all_places conf base = do {
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "all the estates"))
  in
  let list =
    let l = select_all_places conf base in
    string_list_uniq (List.sort compare_places l)
  in
  header conf title;
  Wserver.wprint "<ul>\n";
  List.iter
    (fun t -> do { html_li conf; give_access_all_places conf t; () }) list;
  Wserver.wprint "</ul>\n";
  trailer conf;
};

value print conf base = do {
  Wserver.wrap_string.val := Util.xml_pretty_print;
  match
    (p_getenv conf.env "sm", p_getenv conf.env "t", p_getenv conf.env "p")
  with
  [ (Some "S", Some t, Some p) -> print_title_place conf base t p
  | (Some "S", Some t, None) -> print_places conf base t
  | (Some "S", None, Some p) -> print_titles conf base p
  | (Some "A", None, Some p) -> print_all_with_place conf base p
  | (_, Some "" | None, Some "" | None) -> print_all_titles conf base
  | (_, Some "" | None, Some "*") -> print_all_places conf base
  | (_, Some "" | None, Some p) -> print_titles conf base p
  | (_, Some t, Some "" | None) -> print_places conf base t
  | (_, Some t, Some p) -> print_title_place conf base t p ]
};
