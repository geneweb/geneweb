(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: title.ml,v 4.1 2001-04-22 03:31:16 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Gutil;
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
          if strictement_avant_dmy d d1.val then d1.val := d else ();
          if strictement_apres_dmy d d2.val then d2.val := d else ();
          found.val := True;
        }
      in
      do {
        match Adef.od_of_codate x.birth with
        [ Some (Dgreg d _) -> set d
        | _ -> () ];
        match date_of_death x.death with
        [ Some (Dgreg d _) -> set d
        | _ -> if x.death = NotDead then set conf.today else () ];
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
          x.titles;
        match t with
        [ JustSelf -> ()
        | _ ->
            let u = uoi base x.cle_index in
            Array.iter
              (fun ifam ->
                 let fam = foi base ifam in
                 let md = fam.marriage in
                 let conj = spouse x.cle_index (coi base ifam) in
                 do {
                   match Adef.od_of_codate md with
                   [ Some (Dgreg d _) -> set d
                   | _ -> () ];
                   loop JustSelf (poi base conj);
                   match t with
                   [ AddChildren ->
                       let des = doi base ifam in
                       Array.iter (fun e -> loop JustSelf (poi base e))
                         des.children
                   | _ -> () ];
                 })
              u.family ];
      }
    in
    loop t x;
    if found.val then Some (d1.val, d2.val) else None
  }
;

value compare_title_dates conf base (x1, t1) (x2, t2) =
  match
    ((x1.birth, Adef.od_of_codate t1.t_date_start,
      Adef.od_of_codate t1.t_date_end, x1.death),
     (x2.birth, Adef.od_of_codate t2.t_date_start,
      Adef.od_of_codate t2.t_date_end, x2.death))
  with
  [ ((_, Some (Dgreg d1 _), _, _), (_, Some (Dgreg d2 _), _, _)) ->
      if strictement_avant_dmy d1 d2 then True
      else if annee d1 == annee d2 then
        match
          (Adef.od_of_codate t1.t_date_end, Adef.od_of_codate t2.t_date_end)
        with
        [ (Some d1, Some d2) -> d1 avant d2
        | _ -> True ]
      else False
  | ((_, _, Some d1, _), (_, _, Some d2, _)) -> d2 apres d1
  | ((_, _, _, Death _ d1), (_, Some d2, _, _))
    when not (d2 strictement_avant Adef.date_of_cdate d1) ->
      True
  | ((_, Some d1, _, _), (_, _, _, Death _ d2))
    when not (d1 strictement_avant Adef.date_of_cdate d2) ->
      False
  | _ ->
      match
        (date_interval conf base JustSelf x1,
         date_interval conf base JustSelf x2)
      with
      [ (Some (d11, d12), Some (d21, d22)) ->
          if not (strictement_avant_dmy d21 d12) then True
          else if not (strictement_avant_dmy d11 d22) then False
          else if strictement_apres_dmy d21 d11 then True
          else strictement_apres_dmy d22 d12
      | _ ->
          match
            (date_interval conf base AddSpouse x1,
             date_interval conf base AddSpouse x2)
          with
          [ (Some (d11, d12), Some (d21, d22)) ->
              if not (strictement_avant_dmy d21 d12) then True
              else if not (strictement_avant_dmy d11 d22) then False
              else not (strictement_avant_dmy d22 d12)
          | _ ->
              match
                (date_interval conf base AddChildren x1,
                 date_interval conf base AddChildren x2)
              with
              [ (Some (d11, d12), Some (d21, d22)) ->
                  if not (strictement_avant_dmy d21 d12) then True
                  else if not (strictement_avant_dmy d11 d22) then False
                  else not (strictement_avant_dmy d22 d12)
              | (Some _, None) -> True
              | (None, Some _) -> False
              | (None, None) -> True ] ] ] ]
;

value compare_title_order conf base (x1, t1) (x2, t2) =
  if t1.t_nth == 0 || t2.t_nth == 0 || t1.t_nth = t2.t_nth then
    compare_title_dates conf base (x1, t1) (x2, t2)
  else t1.t_nth <= t2.t_nth
;

value my_alphabetique n1 n2 =
  compare (Name.abbrev (Name.lower n1)) (Name.abbrev (Name.lower n2))
;

value string_list_uniq l =
  let l =
    List.fold_left
      (fun l e ->
         match l with
         [ [] -> [e]
         | [x :: _] -> if my_alphabetique e x = 0 then l else [e :: l] ])
      [] l
  in
  List.rev l
;

value compare_places p1 p2 = compare (Name.lower p1) (Name.lower p2) <= 0;

value compare_titles t1 t2 = my_alphabetique t1 t2 <= 0;

value strip_abbrev_lower s = Name.strip (Name.abbrev (Name.lower s));

value select_title_place conf base title place =
  let list = ref [] in
  let clean_title = ref title in
  let clean_place = ref place in
  let title = strip_abbrev_lower title in
  let place = strip_abbrev_lower place in
  let select x t =
    if strip_abbrev_lower (sou base t.t_ident) = title &&
       strip_abbrev_lower (sou base t.t_place) = place then
       do {
      clean_title.val := sou base t.t_ident;
      clean_place.val := sou base t.t_place;
      list.val := [(x, t) :: list.val]
    }
    else ()
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let x = base.data.persons.get i in List.iter (select x) x.titles
    };
    (list.val, clean_title.val, clean_place.val)
  }
;

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
    for i = 0 to base.data.persons.len - 1 do {
      let x = base.data.persons.get i in List.iter (select x) x.titles
    };
    (list.val, clean_place.val)
  }
;

value select_title base title =
  let list = ref [] in
  let clean_name = ref title in
  let title = strip_abbrev_lower title in
  let add_place t =
    let tn = sou base t.t_ident in
    if strip_abbrev_lower tn = title then
      let pn = sou base t.t_place in
      if not (List.mem pn list.val) then do {
        clean_name.val := tn; list.val := [pn :: list.val]
      }
      else ()
    else ()
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let x = base.data.persons.get i in List.iter add_place x.titles
    };
    (list.val, clean_name.val)
  }
;

value select_place base place =
  let list = ref [] in
  let clean_name = ref place in
  let place = strip_abbrev_lower place in
  let add_title t =
    let pn = sou base t.t_place in
    if strip_abbrev_lower pn = place then
      let tn = sou base t.t_ident in
      if not (List.mem tn list.val) then do {
        clean_name.val := pn; list.val := [tn :: list.val]
      }
      else ()
    else ()
  in
  do {
    for i = 0 to base.data.persons.len - 1 do {
      let x = base.data.persons.get i in List.iter add_title x.titles
    };
    (list.val, clean_name.val)
  }
;

value select_all proj conf base =
  let module O = struct type t = string; value compare = compare; end in
  let module S = Set.Make O in
  let s =
    loop 0 S.empty where rec loop i s =
      if i = base.data.persons.len then s
      else
        let x = base.data.persons.get i in
        let s =
          List.fold_left (fun s t -> S.add (sou base (proj t)) s) s x.titles
        in
        loop (i + 1) s
  in
  S.elements s
;

value select_all_titles = select_all (fun t -> t.t_ident);
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
    [ Some (Dgreg d _) -> Wserver.wprint "%d" (annee d)
    | _ -> () ];
    match t_date_end with
    [ Some (Dgreg d _) -> Wserver.wprint "-%d" (annee d)
    | _ -> () ];
    if has_dates then Wserver.wprint "</em>: " else ();
    if List.memq x list then Wserver.wprint "<em>"
    else Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base x);
    match (t.t_name, x.public_name, x.qualifiers) with
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
    Date.afficher_dates_courtes conf base x;
    if t.t_nth <> 0 then
      Wserver.wprint " (%s)"
        (if t.t_nth >= 100 then string_of_int t.t_nth
         else transl_nth conf "nth" t.t_nth)
    else ();
    if List.memq x list then Wserver.wprint "</em>"
    else Wserver.wprint "</a>";
  }
;

value give_access_place conf base t p =
  do {
    Wserver.wprint "<a href=\"%sm=TT;sm=S;t=%s;p=%s\">" (commd conf)
      (code_varenv t) (code_varenv p);
    Wserver.wprint "... ";
    Wserver.wprint "%s" p;
    Wserver.wprint "</a>\n";
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

value give_access_all_titles conf t =
  do {
    Wserver.wprint "<a href=\"%sm=TT;sm=S;t=%s\">" (commd conf)
      (code_varenv t);
    Wserver.wprint "%s" (capitale t);
    Wserver.wprint "</a>\n";
  }
;

value give_access_all_places conf t =
  do {
    Wserver.wprint "<a href=\"%sm=TT;sm=S;p=%s\">" (commd conf)
      (code_varenv t);
    Wserver.wprint "... %s" t;
    Wserver.wprint "</a>\n";
  }
;

value print_title_place_list conf base t p list =
  let title h =
    if h then Wserver.wprint "%s %s\n" (capitale t) p
    else do {
      Wserver.wprint "<a href=\"%sm=TT;sm=S;t=%s\">\n" (commd conf)
        (code_varenv t);
      Wserver.wprint "%s</a>\n" (capitale t);
      Wserver.wprint "<a href=\"%sm=TT;sm=S;p=%s\">\n" (commd conf)
        (code_varenv p);
      Wserver.wprint "%s</a>\n" p;
    }
  in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    let _ =
      List.fold_left
        (fun list x ->
           do {
             html_li conf;
             give_access_someone conf base x list;
             Wserver.wprint "\n";
             [fst x :: list]
           })
        [] list
    in
    Wserver.wprint "</ul>\n";
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
        do {
          Wserver.wprint "<p>\n<a href=\"%sm=RLM" (commd conf);
          let _ =
            List.fold_left
              (fun i (p, n) ->
                 do {
                   Wserver.wprint ";i%d=%d;t%d=%d" i
                     (Adef.int_of_iper p.cle_index) i n;
                   i + 1
                 })
              1 list
          in
          Wserver.wprint ";lim=6\">%s</a>\n" (capitale (transl conf "tree"));
        }
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
  let (l, t, p) = select_title_place conf base t p in
  let list = Sort.list (compare_title_order conf base) l in
  print_title_place_list conf base t p list
;

value print_all_with_place conf base p =
  let (l, p) = select_all_with_place conf base p in
  let list = Sort.list (compare_title_dates conf base) l in
  print_all_with_place_list conf base p list
;

value print_places_list conf base t list =
  let title _ = Wserver.wprint "%s" (capitale t) in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    List.iter
      (fun p -> do { html_li conf; give_access_place conf base t p; () })
      list;
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print_places conf base t =
  let (l, t) = select_title base t in
  let list = string_list_uniq (Sort.list compare_places l) in
  match list with
  [ [p] -> print_title_place conf base t p
  | _ -> print_places_list conf base t list ]
;

value print_titles conf base p =
  let (l, p) = select_place base p in
  let list = string_list_uniq (Sort.list compare_titles l) in
  let title _ = Wserver.wprint "... %s" p in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    List.iter (fun t -> do { html_li conf; give_access_title conf t p; () })
      list;
    Wserver.wprint "</ul>\n";
    if List.length list > 1 then do {
      stag "a" "href=\"%sm=TT;sm=A;p=%s\"" (commd conf) (code_varenv p) begin
        Wserver.wprint "%s" (capitale (transl conf "the whole list"));
      end;
      Wserver.wprint "\n";
    }
    else ();
    trailer conf;
  }
;

value print_all_titles conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "all the titles"))
  in
  let list =
    let l = select_all_titles conf base in
    string_list_uniq (Sort.list compare_titles l)
  in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    List.iter
      (fun t -> do { html_li conf; give_access_all_titles conf t; () }) list;
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print_all_places conf base =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "all the estates"))
  in
  let list =
    let l = select_all_places conf base in
    string_list_uniq (Sort.list compare_places l)
  in
  do {
    header conf title;
    Wserver.wprint "<ul>\n";
    List.iter
      (fun t -> do { html_li conf; give_access_all_places conf t; () }) list;
    Wserver.wprint "</ul>\n";
    trailer conf;
  }
;

value print conf base =
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
;
