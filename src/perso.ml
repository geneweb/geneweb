(* camlp4r ./pa_html.cmo *)
(* $Id: perso.ml,v 3.62 2000-10-24 15:47:13 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value max_im_wid = 240;
value max_im_hei = 240;
value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value has_children base u =
  List.exists
    (fun ifam ->
       let des = doi base ifam in
       Array.length des.children > 0)
    (Array.to_list u.family)
;

value print_marriage_text conf base in_perso fam =
  let marriage = Adef.od_of_codate fam.marriage in
  let marriage_place = sou base fam.marriage_place in
  do if in_perso then
       match (marriage, marriage_place) with
       [ (None, "") -> ()
       | _ -> Wserver.wprint "<em>" ]
     else ();
     match marriage with
     [ Some d -> Wserver.wprint " %s" (Date.string_of_ondate conf d)
     | _ -> () ];
     match marriage_place with
     [ "" -> ()
     | s -> Wserver.wprint ", %s," s ];
     if in_perso then
       match (marriage, marriage_place) with
       [ (None, "") -> ()
       | _ -> Wserver.wprint "</em>" ]
     else ();
  return ()
;

value print_title conf base and_txt p (nth, name, title, places, dates) =
  do let href =
       "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
       code_varenv (sou base (List.hd places))
     in
     let (tit, est) = (sou base title, sou base (List.hd places)) in
     let s = tit ^ " " ^ est in
     wprint_geneweb_link conf href s;
     let rec loop places =
       do match places with
          [ [] -> ()
          | [_] -> Wserver.wprint "\n%s " and_txt
          | _ -> Wserver.wprint ",\n" ];
       return
       match places with
       [ [place :: places] ->
           let href =
             "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
             code_varenv (sou base place)
           in
           let est = sou base place in
           do wprint_geneweb_link conf href est;
           return loop places
       | _ -> () ]
     in
     loop (List.tl places);
  return
  let paren =
    match (nth, dates, name) with
    [ (n, _, _) when n > 0 -> True
    | (_, _, Tname _) -> True
    | (_, [(Some _, _) :: _], _) -> age_autorise conf base p
    | _ -> False ]
  in
  do if paren then Wserver.wprint "\n(" else (); return
  let first =
    if nth > 0 then
      do Wserver.wprint "%s"
           (if nth >= 100 then string_of_int nth
            else transl_nth conf "nth" nth);
      return False
    else True
  in
  let first =
    match name with
    [ Tname n ->
        do if not first then Wserver.wprint " ," else ();
           Wserver.wprint "%s" (sou base n);
        return False
    | _ -> first ]
  in
  do if age_autorise conf base p && dates <> [(None, None)] then
       let _ =
         List.fold_left
           (fun first (date_start, date_end) ->
              do if not first then Wserver.wprint ",\n" else ();
                 match date_start with
                 [ Some d -> Wserver.wprint "%s" (Date.string_of_date conf d)
                 | None -> () ];
                 match date_end with
                 [ Some (Dgreg d _) ->
                     do if d.month <> 0 then Wserver.wprint " - "
                        else Wserver.wprint "-";
                     return ()
                 | _ -> () ];
                 match date_end with
                 [ Some d -> Wserver.wprint "%s" (Date.string_of_date conf d)
                 | None -> () ];
              return False)
           first dates
       in
       ()
     else ();
     if paren then Wserver.wprint ")" else ();
  return ()
;

value name_equiv n1 n2 =
  n1 = n2 || n1 = Tmain && n2 = Tnone || n1 = Tnone && n2 = Tmain
;

value print_titles conf base cap and_txt p =
  let titles = p.titles in
  let titles =
    List.fold_right
      (fun t l ->
         let t_date_start = Adef.od_of_codate t.t_date_start in
         let t_date_end = Adef.od_of_codate t.t_date_end in
         match l with
         [ [(nth, name, title, place, dates) :: rl]
           when
             not conf.is_rtl &&
             nth = t.t_nth && name_equiv name t.t_name && title = t.t_ident &&
             place = t.t_place ->
             [(nth, name, title, place,
               [(t_date_start, t_date_end) :: dates]) ::
              rl]
         | _ ->
             [(t.t_nth, t.t_name, t.t_ident, t.t_place,
               [(t_date_start, t_date_end)]) ::
              l] ])
      titles []
  in
  let titles =
    List.fold_right
      (fun (t_nth, t_name, t_ident, t_place, t_dates) l ->
         match l with
         [ [(nth, name, title, places, dates) :: rl]
           when
             not conf.is_rtl &&
             nth = t_nth && name_equiv name t_name && title = t_ident &&
             dates = t_dates ->
             [(nth, name, title, [t_place :: places], dates) :: rl]
         | _ -> [(t_nth, t_name, t_ident, [t_place], t_dates) :: l] ])
      titles []
  in
  let _ =
    List.fold_left
      (fun first t ->
         do if not first then Wserver.wprint "," else ();
            Wserver.wprint "\n";
            print_title conf base and_txt p t;
         return False)
      True titles
  in
  ()
;

(* Version matching the Sosa number of the "ancestor" pages *)
type choice 'a 'b = [ Left of 'a | Right of 'b ];

value find_sosa_aux conf base a p =
  let tstab = Util.create_topological_sort conf base in
  let mark = Array.create base.data.persons.len False in
  let rec gene_find =
    fun
    [ [] -> Left []
    | [(z, ip) :: zil] ->
        if ip = a.cle_index then Right z
        else if mark.(Adef.int_of_iper ip) then gene_find zil
        else
          do mark.(Adef.int_of_iper ip) := True; return
          if tstab.(Adef.int_of_iper a.cle_index) <=
               tstab.(Adef.int_of_iper ip) then
            gene_find zil
          else
            let asc = aoi base ip in
            match asc.parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                let z = Num.twice z in
                match gene_find zil with
                [ Left zil ->
                    Left [(z, cpl.father); (Num.inc z 1, cpl.mother) :: zil]
                | Right z -> Right z ]
            | None -> gene_find zil ] ]
  in
  find [(Num.one, p.cle_index)] where rec find zil =
    match gene_find zil with
    [ Left [] -> None
    | Left zil -> find zil
    | Right z -> Some (z, p) ]
;
(* Male version
value find_sosa_aux conf base a p =
  let mark = Array.create base.data.persons.len False in
  let rec find z ip =
    if ip = a.cle_index then Some z
    else if mark.(Adef.int_of_iper ip) then None
    else
      do mark.(Adef.int_of_iper ip) := True; return
      let asc = aoi base ip in
      match asc.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let z = Num.twice z in
          match find z cpl.father with
          [ Some z -> Some z
          | None -> find (Num.inc z 1) cpl.mother ]
      | None -> None ]
  in
  find Num.one p.cle_index
;
*)

value find_sosa conf base a =
  match Util.find_person_in_env conf base "z" with
  [ Some p ->
      if a.cle_index = p.cle_index then Some (Num.one, p)
      else
        let u = uoi base a.cle_index in
	if has_children base u then find_sosa_aux conf base a p
	else None
  | None -> None ]
;

(* Old "classic" version;
   Temporarily kept to check the good behavior of the new version.
   When ok, this code will be deleted *)

module Classic =

struct

value has_parents base p =
  (aoi base p.cle_index).parents <> None
;

value has_grand_parents base p =
  let rec loop niveau a =
    if niveau = 2 then raise Exit
    else
      match a.parents with
      [ Some ifam ->
          let pere = (coi base ifam).father in
          let mere = (coi base ifam).mother in
          do loop (succ niveau) (aoi base pere);
             loop (succ niveau) (aoi base mere);
          return ()
      | _ -> () ]
  in
  try do loop 0 (aoi base p.cle_index); return False with [ Exit -> True ]
;

value has_grand_children base u =
  try
    do Array.iter
         (fun fi ->
            let el = (doi base fi).children in
            Array.iter
              (fun e ->
                 Array.iter
                   (fun fi ->
                      let eel = (doi base fi).children in
                      Array.iter (fun _ -> raise Exit) eel)
                   (uoi base e).family)
              el)
         u.family;
    return False
  with
  [ Exit -> True ]
;

value prev_sibling base p a =
  match a.parents with
  [ Some ifam ->
      let des = doi base ifam in
      loop 0 where rec loop i =
        if i == Array.length des.children then None
        else if des.children.(i) = p.cle_index then
          if i == 0 then None else Some (poi base des.children.(i-1))
        else loop (i + 1)
  | None -> None ]
;

value next_sibling base p a =
  match a.parents with
  [ Some ifam ->
      let des = doi base ifam in
      loop 0 where rec loop i =
        if i == Array.length des.children then None
        else if des.children.(i) = p.cle_index then
          if i == Array.length des.children - 1 then None
          else Some (poi base des.children.(i+1))
        else loop (i + 1)
  | None -> None ]
;

value print_dates conf base open_area p =
  let is = index_of_sex p.sex in
  do let birth_place = sou base p.birth_place in
     let anniv =
       match Adef.od_of_codate p.birth with
       [ Some (Dgreg d _) ->
           if d.prec = Sure && p.death = NotDead then
             d.day = conf.today.day && d.month = conf.today.month &&
             d.year < conf.today.year
           || not (leap_year conf.today.year) && d.day = 29 &&
             d.month = 2 && conf.today.day = 1 &&
             conf.today.month = 3
           else False
       | _ -> False ]
     in
     do match (Adef.od_of_codate p.birth, birth_place) with
        [ (None, "") -> ()
        | _ -> open_area () ];
        match Adef.od_of_codate p.birth with
        [ Some d ->
            do Wserver.wprint "%s " (capitale (transl_nth conf "born" is));
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if anniv then
                 Wserver.wprint " (%s)"
                   (transl conf "happy birthday to you!")
               else ();
               if birth_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if birth_place <> "" then
              Wserver.wprint "%s\n-&nbsp;"
                (capitale (transl_nth conf "born" is))
            else () ];
        if birth_place <> "" then Wserver.wprint "%s" birth_place else ();
        match (Adef.od_of_codate p.birth, birth_place) with
        [ (None, "") -> ()
        | _ -> Wserver.wprint "\n" ];
     return ();
     let baptism = Adef.od_of_codate p.baptism in
     let baptism_place = sou base p.baptism_place in
     do match (baptism, baptism_place) with
        [ (None, "") -> ()
        | _ -> open_area () ];
        match baptism with
        [ Some d ->
            do Wserver.wprint "%s "
                 (capitale (transl_nth conf "baptized" is));
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if baptism_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if baptism_place <> "" then
              Wserver.wprint "%s\n-&nbsp;"
                (capitale (transl_nth conf "baptized" is))
            else () ];
        if baptism_place <> "" then
          Wserver.wprint "%s" baptism_place
        else ();
        match (baptism, baptism_place) with
        [ (None, "") -> ()
        | _ -> Wserver.wprint "\n" ];
     return ();
     let death_place = sou base p.death_place in
     let something =
       match (p.death, death_place, p.burial) with
       [ (DontKnowIfDead | NotDead, "", _) -> False
       | (DeadDontKnowWhen, "", Buried _ | Cremated _) -> False
       | (DeadDontKnowWhen, _, _) ->
           death_place <> "" || not (Util.of_course_died conf p)
       | _ -> True ]
     in
     do if something then open_area () else ();
        match p.death with
        [ Death dr d ->
            let dr_w =
              match dr with
              [ Unspecified -> transl_nth conf "died" is
              | Murdered -> transl_nth conf "murdered" is
              | Killed -> transl_nth conf "killed (in action)" is
              | Executed -> transl_nth conf "executed (legally killed)" is
              | Disappeared -> transl_nth conf "disappeared" is ]
            in
            let d = Adef.date_of_cdate d in
            do Wserver.wprint "%s " (capitale dr_w);
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if death_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | DeadYoung ->
            do Wserver.wprint "%s"
                 (capitale (transl_nth conf "died young" is));
               if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
            return ()
        | DeadDontKnowWhen ->
            match (death_place, p.burial) with
            [ ("", Buried _ | Cremated _) -> ()
            | _ ->
                if death_place <> "" || not (of_course_died conf p) then
                  do Wserver.wprint "%s"
                       (capitale (transl_nth conf "died" is));
                     if death_place <> "" then Wserver.wprint "\n-&nbsp;"
                     else ();
                  return ()
                else () ]
        | DontKnowIfDead | NotDead -> () ];
        if death_place <> "" then
          do Wserver.wprint "%s" death_place;
          return ()
        else ();
        if something then Wserver.wprint "\n" else ();
     return ();
     match (Adef.od_of_codate p.birth, p.death) with
     [ (Some (Dgreg d _), NotDead) ->
         if d.day == 0 && d.month == 0 && d.prec <> Sure then ()
         else
           let a = temps_ecoule d conf.today in
           do open_area ();
              Wserver.wprint "%s: " (capitale (transl conf "age"));
              Date.print_age conf a;
              Wserver.wprint "\n";
          return ()
     | _ -> () ];
     let burial_date_place cod =
       let place = sou base p.burial_place in
       do match Adef.od_of_codate cod with
          [ Some d ->
              do Wserver.wprint " %s" (Date.string_of_ondate conf d);
                 if place <> "" then Wserver.wprint ",\n" else ();
              return ()
          | None ->
              if place <> "" then Wserver.wprint " -&nbsp;" else () ];
          if place <> "" then Wserver.wprint "%s" place else ();
       return ()
     in
     do match p.burial with
        [ Buried cod ->
            do open_area ();
               Wserver.wprint "%s" (capitale (transl_nth conf "buried" is));
               burial_date_place cod;
               Wserver.wprint "\n";
            return ()
        | Cremated cod ->
            do open_area ();
               Wserver.wprint "%s" (capitale (transl_nth conf "cremated" is));
               burial_date_place cod;
               Wserver.wprint "\n";
            return ()
        | UnknownBurial -> () ];
     return ();
     match Date.get_birth_death_date p with
     [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
        Some (Dgreg ({prec = Sure | About | Maybe} as d2) _),
        approx) when d1 <> d2 ->
         let a = temps_ecoule d1 d2 in
         if a.year < 0 || a.year = 0 && a.month = 0 then ()
         else
           do open_area ();
              Wserver.wprint "%s " (capitale (transl conf "age at death:"));
              if not approx && d1.prec = Sure && d2.prec = Sure then ()
              else Wserver.wprint "%s " (transl conf "possibly (date)");
              Date.print_age conf a;
              Wserver.wprint "\n";
           return ()
     | _ -> () ];
  return ()
;

value print_parents conf base p =
  match p.parents with
  [ Some ifam ->
      let ifath = (coi base ifam).father in
      let imoth = (coi base ifam).mother in
      let fath = poi base ifath in
      let moth = poi base imoth in
      do Wserver.wprint "<h3>%s</h3>\n<ul>\n"
           (std_color conf (capitale (nominative (transl conf "parents"))));
         html_li conf;
         Wserver.wprint "%s" (referenced_person_title_text conf base fath);
         Date.afficher_dates_courtes conf base fath;
         Wserver.wprint "\n";
         html_li conf;
         Wserver.wprint "%s" (referenced_person_title_text conf base moth);
         Date.afficher_dates_courtes conf base moth;
         Wserver.wprint "\n</ul>\n\n";
      return ()
  | _ -> () ]
;

value print_child conf base age_auth ip =
  let p = poi base ip in
  let a = aoi base ip in
  let force_surname =
    match a.parents with
    [ None -> False
    | Some ifam ->
        p_surname base (poi base (coi base ifam).father) <> p_surname base p ]
  in
  do Wserver.wprint "\n";
     html_li conf;
     if force_surname then afficher_personne_referencee conf base p
     else afficher_prenom_de_personne_referencee conf base p;
     if age_auth then Date.afficher_dates_courtes conf base p else ();
     Wserver.wprint "\n";
  return ()
;

value print_family conf base p a ifam =
  let fam = foi base ifam in
  let des = doi base ifam in
  let ispouse = spouse p.cle_index (coi base ifam) in
  let spouse = poi base ispouse in
  let children = des.children in
  let divorce = fam.divorce in
  let auth = age_autorise conf base p && age_autorise conf base spouse in
  do Wserver.wprint "\n";
     html_li conf;
     let format = relation_txt conf p.sex fam in
     Wserver.wprint (fcapitale format)
       (fun _ -> if auth then print_marriage_text conf base True fam else ());
     let txt =
       gen_referenced_person_title_text raw_access conf base (poi base ispouse)
     in
     Wserver.wprint " %s" txt;
     Date.afficher_dates_courtes conf base (poi base ispouse);
     if Array.length fam.witnesses > 0 then
       do Wserver.wprint "\n(";
          Wserver.wprint "%s"
            (nominative
              (transl_nth conf "witness/witnesses"
                 (if Array.length fam.witnesses = 1 then 0 else 1)));
          for i = 0 to Array.length fam.witnesses - 1 do
            let p = poi base fam.witnesses.(i) in
            do Wserver.wprint "%c " (if i = 0 then ':' else ',');
               Wserver.wprint "%s" (referenced_person_title_text conf base p);
               Date.afficher_dates_courtes conf base p;
            return ();
          done;
          Wserver.wprint ")";
       return ()
     else ();
     match divorce with
     [ Divorced d ->
         let d = Adef.od_of_codate d in
         do Wserver.wprint ",\n%s" (transl conf "divorced");
            match d with
            [ Some d when auth ->
                do Wserver.wprint " <em>";
                   Wserver.wprint "%s" (Date.string_of_ondate conf d);
                   Wserver.wprint "</em>";
                return ()
            | _ -> () ];
         return ()
     | Separated -> Wserver.wprint ",\n%s" (transl conf "separated")
     | _ -> () ];
     if age_autorise conf base p then
       match sou base fam.comment with
       [ "" -> ()
       | str ->
           do Wserver.wprint "\n(";
              copy_string_with_macros conf str;
              Wserver.wprint ")";
           return () ]
     else ();
     if Array.length children == 0 then ()
     else
       let age_auth =
         List.for_all (fun ip -> age_autorise conf base (poi base ip))
           (Array.to_list children)
       in
       do Wserver.wprint ", %s" (transl conf "having as children");
          Wserver.wprint "\n";
          tag "ul" begin
            Array.iter (print_child conf base age_auth) children;
          end;
       return ();
     Wserver.wprint "\n";
     if conf.wizard then
       match p_getenv conf.henv "opt" with
       [ Some "from" ->
           let n = sou base fam.origin_file in
           if n = "" then ()
           else do Wserver.wprint "<em>(%s)</em>" n; html_br conf; return ()
       | _ -> () ]
     else ();
  return ()
;

value print_families conf base p a u =
  match Array.to_list u.family with
  [ [] -> ()
  | faml ->
      do Wserver.wprint "<h3>%s</h3>\n<ul>"
           (std_color conf
              (capitale (transl_nth conf "marriage/marriages" 1) ^ " " ^
               transl conf "and" ^ " " ^ transl_nth conf "child/children" 1));
         List.iter (print_family conf base p a) faml;
         Wserver.wprint "</ul>\n";
      return () ]
;

value print_notes conf base p =
  match sou base p.notes with
  [ "" -> ()
  | notes ->
      do Wserver.wprint "<h3>%s</h3>\n"
           (std_color conf
              (capitale (nominative (transl_nth conf "note/notes" 1))));
         tag "ul" begin
           html_li conf;
           copy_string_with_macros conf notes;
           Wserver.wprint "\n";
         end;
      return () ]
;

value print_relation conf base r =
  do match (r.r_fath, r.r_moth) with
     [ (Some ip, None) ->
         let p = poi base ip in
         do html_li conf;
            Wserver.wprint "%s"
              (capitale (relation_type_text conf r.r_type 0));
            Wserver.wprint ": %s" (referenced_person_title_text conf base p);
            Date.afficher_dates_courtes conf base p;
            Wserver.wprint "\n";
         return ()
     | (None, Some ip) ->
         let p = poi base ip in
         do html_li conf;
            Wserver.wprint "%s"
              (capitale (relation_type_text conf r.r_type 1));
            Wserver.wprint ": %s" (referenced_person_title_text conf base p);
            Date.afficher_dates_courtes conf base p;
            Wserver.wprint "\n";
         return ()
     | (Some ip1, Some ip2) ->
         let p1 = poi base ip1 in
         let p2 = poi base ip2 in
         do html_li conf;
            Wserver.wprint "%s"
              (capitale (relation_type_text conf r.r_type 2));
            tag "ul" begin
              html_li conf;
              Wserver.wprint "%s" (referenced_person_title_text conf base p1);
              Date.afficher_dates_courtes conf base p1;
              Wserver.wprint "\n";
              html_li conf;
              Wserver.wprint "%s" (referenced_person_title_text conf base p2);
              Date.afficher_dates_courtes conf base p2;
              Wserver.wprint "\n";
            end;
         return ()
     | _ -> () ];
  return ()
;

value print_rchild conf base c r =
  do html_li conf;
     Wserver.wprint "%s"
       (capitale (rchild_type_text conf r.r_type (index_of_sex c.sex)));
     Wserver.wprint ": %s" (referenced_person_title_text conf base c);
     Date.afficher_dates_courtes conf base c;
     Wserver.wprint "\n";
  return ()
;

value print_witness_at_marriage conf base cpl =
  do html_li conf;
     Wserver.wprint
       (fcapitale (ftransl conf "witness at marriage of %s and %s"))
       (referenced_person_title_text conf base (poi base cpl.father))
       (referenced_person_title_text conf base (poi base cpl.mother));
     Wserver.wprint "\n";
  return ()
;

value print_related_not_as_witness conf base p ic =
  let c = poi base ic in
  List.iter
    (fun r ->
       do match r.r_fath with
          [ Some ip ->
              if ip = p.cle_index then print_rchild conf base c r else ()
          | None -> () ];
          match r.r_moth with
          [ Some ip ->
              if ip = p.cle_index then print_rchild conf base c r else ()
          | None -> () ];
       return ())
    c.rparents
;

value print_related_as_witness conf base p ic =
  let c = poi base ic in
  if c.sex = Male then
    List.iter
      (fun ifam ->
         let fam = foi base ifam in
         if array_memq p.cle_index fam.witnesses then
           print_witness_at_marriage conf base (coi base ifam)
         else ())
      (Array.to_list (uoi base ic).family)
  else ()
;

value print_relations conf base p =
  match (p.rparents, p.related) with
  [ ([], []) -> ()
  | (rl, cl) ->
      do Wserver.wprint "<h3>%s</h3>\n"
           (std_color conf
              (capitale (transl_nth conf "relation/relations" 1)));
         tag "ul" begin
           List.iter (print_relation conf base) rl;
           List.iter (print_related_not_as_witness conf base p) cl;
           List.iter (print_related_as_witness conf base p) cl;
         end;
      return () ]
;

value print_not_empty_src conf base new_parag first txt isrc =
  let src = sou base isrc in
  if src = "" then ()
  else
    do if first.val then
         do if new_parag then html_p conf else ();
            Wserver.wprint "<font size=-1><em>%s:</em></font>\n"
              (capitale (transl_nth conf "source/sources" 1));
         return ()
       else ();
       html_br conf;
       Wserver.wprint "-\n";
       first.val := False;
       Wserver.wprint "<font size=-1><em>%s: " (txt ());
       copy_string_with_macros conf src;
       Wserver.wprint "</em></font>\n";
    return ()
;

value print_sources conf base new_parag p =
  let u = uoi base p.cle_index in
  let first = ref True in
  let auth = age_autorise conf base p in
  do print_not_empty_src conf base new_parag first
       (fun () -> nominative (transl_nth conf "person/persons" 0))
       p.psources;
     if auth then
       do print_not_empty_src conf base new_parag first
            (fun () -> transl_nth conf "birth" 0)
             p.birth_src;
          print_not_empty_src conf base new_parag first
            (fun () -> transl_nth conf "baptism" 0)
            p.baptism_src;
          print_not_empty_src conf base new_parag first
            (fun () -> transl_nth conf "death" 0)
            p.death_src;
          print_not_empty_src conf base new_parag first
            (fun () -> transl_nth conf "burial" 0)
            p.burial_src;
       return ()
     else ();
     for i = 0 to Array.length u.family - 1 do
       let fam = foi base u.family.(i) in
       do if auth then
            print_not_empty_src conf base new_parag first
              (fun () ->
                 transl_nth conf "marriage/marriages" 0 ^
                 (if Array.length u.family == 1 then ""
                  else " " ^ string_of_int (i + 1)))
               fam.marriage_src
          else ();
          print_not_empty_src conf base new_parag first
            (fun () ->
               nominative (transl_nth conf "family/families" 0) ^
               (if Array.length u.family == 1 then ""
                else " " ^ string_of_int (i + 1)))
            fam.fsources;
       return ();
     done;
  return ()
;

value print_sosa conf base open_area a =
  fun
  [ Some (n, p) ->
      do open_area ();
         Wserver.wprint "<em>%s:\n" (capitale (transl conf "Sosa"));
         stag "a" "href=\"%sm=RL;i1=%d;i2=%d;b1=1;b2=%s\""
           (commd conf) (Adef.int_of_iper a.cle_index)
           (Adef.int_of_iper p.cle_index) (Num.to_string n)
         begin
           Num.print (fun x -> Wserver.wprint "%s" x)
             (transl conf "(thousand separator)") n;
         end;
         Wserver.wprint "</em><br>\n";
      return ()
  | None -> () ]
;

value print_compute_link conf base p mode text =
  Wserver.wprint "<a href=\"%s%s%s\"><b>%s</b></a>"
    (commd conf) (if mode <> "" then "m=" ^ mode ^ ";" else "")
    (acces conf base p) (std_color conf (capitale text))
;

value print_linked_first_name_and_surname conf base p =
  do wprint_geneweb_link conf
       ("m=P;v=" ^ code_varenv (Name.lower (p_first_name base p)))
       (p_first_name base p);
     Wserver.wprint " ";
     wprint_geneweb_link conf
       ("m=N;v=" ^ code_varenv (Name.lower (p_surname base p)))
       (p_surname base p);
  return ()
;

value print_sub_titles conf base p =
  let (open_area, close_area) =
    let opened = ref False in
    (fun () ->
       if not opened.val then
         do Wserver.wprint "\
<center>
<table border=%d cellspacing=0 cellpadding=0>
<tr><td>\n" conf.border;
            opened.val := True;
         return ()
       else (),
     fun () ->
       if opened.val then
         Wserver.wprint "</td></tr>\n</table>\n</center>\n<p>\n"
       else ())
  in
  do print_sosa conf base open_area p (find_sosa conf base p);
     match (p.public_name, p.qualifiers) with
     [ (n, [_ :: nnl]) when sou base n <> "" ->
         let n = sou base n in
         List.iter
           (fun nn ->
              do open_area ();
                 Wserver.wprint "%s <em>%s</em>" n (sou base nn);
                 html_br conf;
              return ())
           nnl
     | (_, [_ :: nnl]) ->
         let n = p_first_name base p in
         List.iter
           (fun nn ->
              do open_area ();
                 Wserver.wprint "%s <em>%s</em>" n (sou base nn);
                 html_br conf;
              return ())
           nnl
     | _ -> () ];
     List.iter
       (fun a ->
          do open_area ();
             Wserver.wprint "<em><strong>%s</strong></em>" (sou base a);
             html_br conf;
          return ())
       p.aliases;
     if p.titles <> [] && age_autorise conf base p then
       do open_area ();
          Wserver.wprint "<em>";
          print_titles conf base True (transl conf "and") p;
          Wserver.wprint "</em>\n";
          html_br conf;
       return ()
     else ();
     match (sou base p.public_name, p.qualifiers) with
     [ ("", []) -> ()
     | _ ->
         do open_area ();
            Wserver.wprint "<em>(";
            print_linked_first_name_and_surname conf base p;
            Wserver.wprint ")</em>\n";
            html_br conf;
         return () ];
     List.iter
       (fun n ->
          do open_area ();
             Wserver.wprint "<em>(%s %s)</em>\n"
               (p_first_name base p) (sou base n);
             html_br conf;
          return ())
       p.surnames_aliases;
     if age_autorise conf base p then
       List.iter
         (fun n ->
            do open_area ();
               Wserver.wprint "<em>(%s...)</em>\n" (sou base n);
               html_br conf;
            return ())
         p.first_names_aliases
     else ();
     close_area ();
  return ()
;

value print_occupation_dates conf base in_table p =
  let a = aoi base p.cle_index in
  let (open_area, close_area) =
    let opened = ref False in
    (fun () ->
       do if not opened.val then
            do if in_table then Wserver.wprint "<td>\n" else ();
               Wserver.wprint "<ul>\n";
	       opened.val := True;
	    return ()
          else ();
          html_li conf;
       return (),
     fun () ->
       if opened.val then
         do Wserver.wprint "</ul>\n";
	    if in_table then Wserver.wprint "</td>\n" else ();
	 return ()
       else ())
  in
  do match sou base p.occupation with
     [ "" -> ()
     | s ->
         if age_autorise conf base p then
           do open_area ();
              Wserver.wprint "%s\n" (capitale s);
           return ()
         else () ];
     if age_autorise conf base p then print_dates conf base open_area p
     else ();
     if age_autorise conf base p && a.consang != Adef.fix (-1) &&
        a.consang != Adef.fix 0 then
       do open_area ();
          Wserver.wprint "%s: " (capitale (transl conf "consanguinity"));
          print_decimal_num conf
            (round_2_dec (Adef.float_of_fix a.consang *. 100.0));
          Wserver.wprint "%%\n";
       return ()
     else ();
     close_area ();
  return ()
;

value print_photo_occupation_dates conf base p =
  let image_txt = transl_nth conf "image/images" 0 in
  match
    image_and_size conf base p (limited_image_size max_im_wid max_im_wid)
  with
  [ Some (fname, Some (Some (width, height))) ->
      tag "table" "border=%d width=\"90%%\"" conf.border begin
        tag "tr" begin
          let s = Unix.stat fname in
          let b = acces conf base p in
          let k = default_image_name base p in
          tag "td" begin
            Wserver.wprint "<a href=\"%sm=IM;%s;k=/%s\">" (commd conf) b k;
            Wserver.wprint "\
<img src=\"%sm=IM;d=%d;%s;k=/%s\" width=%d height=%d border=0 alt=\"(%s)\">"
              (commd conf)
              (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
              b k width height image_txt;
            Wserver.wprint "</a>";
          end;
          print_occupation_dates conf base True p;
        end;
      end
  | Some (link, None) ->
      tag "table" "border=%d width=\"95%%\"" conf.border begin
        tag "tr" begin
          tag "td" begin
            Wserver.wprint "<a href=\"%s\">" link;
            Wserver.wprint "<img src=\"%s\" border=0 height=%d alt=\"%s\">"
              link max_im_hei image_txt;
            Wserver.wprint "</a>\n";
          end;
          print_occupation_dates conf base True p;
        end;
      end
  | None | Some (_, Some None) ->
      print_occupation_dates conf base False p ]
;

value print_ancestors_descends_cousins conf base p a u =
  let (open_area, close_area) =
    let opened = ref False in
    (fun () ->
       if not opened.val then
         do Wserver.wprint "<td align=left><p><br>\n<ul>\n";
            opened.val := True;
         return ()
       else (),
     fun () ->
       if opened.val then Wserver.wprint "</ul>\n</td>\n" else ())
  in
  let print p mode txt =
    do open_area ();
       html_li conf;
       print_compute_link conf base p mode txt;
       Wserver.wprint "\n";
    return ()
  in
  do if has_parents base p then print p "A" (transl conf "ancestors")
     else ();
     if has_children base u then print p "D" (transl conf "descendants")
     else ();
     if has_grand_parents base p then
       print p "C" (transl conf "cousins (general term)")
     else if has_nephews_or_nieces base p then
       print p "C;v1=1;v2=2" (transl conf "nephews and nieces")
     else ();
     match prev_sibling base p a with
     [ Some p ->
         print p "" (transl_nth conf "previous sibling" (index_of_sex p.sex))
     | None -> () ];
     match next_sibling base p a with
     [ Some p ->
         print p "" (transl_nth conf "next sibling" (index_of_sex p.sex))
     | None -> () ];
     close_area ();
  return ()
;

value print_ok conf base p =
  let title h =
    match (sou base p.public_name, p.qualifiers) with
    [ (n, [nn :: _]) when n <> "" ->
        if h then Wserver.wprint "%s %s" n (sou base nn)
        else
          do Wserver.wprint "%s" n;
             Wserver.wprint " <em>";
             Wserver.wprint "%s" (sou base nn);
             Wserver.wprint "</em>";
          return ()
    | (n, []) when n <> "" ->
        Wserver.wprint "%s %s" n (p_surname base p)
    | (_, [nn :: _]) ->
        if h then
          Wserver.wprint "%s %s" (p_first_name base p)
            (sou base nn)
        else
          Wserver.wprint "%s <em>%s</em>" (p_first_name base p)
            (sou base nn)
    | (_, []) ->
        if h then
          Wserver.wprint "%s %s" (p_first_name base p) (p_surname base p)
        else print_linked_first_name_and_surname conf base p ]
  in
  let a = aoi base p.cle_index in
  let u = uoi base p.cle_index in
  do header_no_page_title conf title;
     let img f t =
       Wserver.wprint
         "<img src=\"%s/%s\" width=13 height=13 alt=\"(%s)\" align=left>\n"
         (image_prefix conf) f t
     in
     match p.sex with
     [ Male -> img "male.gif" (transl_nth conf "M/F" 0)
     | Female -> img "female.gif" (transl_nth conf "M/F" 1)
     | Neuter -> () ];
     Wserver.wprint "<h1 align=center><font color=%s>" conf.highlight;
     title False;
     Wserver.wprint "</font></h1>\n";
     print_sub_titles conf base p;
     print_link_to_welcome conf True;
     print_photo_occupation_dates conf base p;
     print_parents conf base a;
     print_families conf base p a u;
     if age_autorise conf base p then
       do print_relations conf base p;
          print_notes conf base p;
       return ()
     else ();
     print_sources conf base True p;
     if conf.cancel_links then ()
     else
       do tag "table" "border=%d width=\"90%%\"" conf.border begin
            tag "tr" begin
              tag "td" "align=center" begin
                print_compute_link conf base p "R"
                  (transl conf "relationship computing");
                Wserver.wprint "\n";
              end;
              print_ancestors_descends_cousins conf base p a u;
              if conf.wizard then
                do stag "td" "align=center" begin
                     print_compute_link conf base p "U" (transl conf "update");
                   end;
                   Wserver.wprint "\n";
                return ()
              else ();
            end;
          end;
       return ();
     match p_getenv conf.env "opt" with
     [ Some "misc" ->
         tag "ol" begin
           html_li conf;
           Wserver.wprint "%s\n"
             (Name.lower (p_first_name base p ^ " " ^ p_surname base p));
           List.iter
             (fun x ->
                do Wserver.wprint "\n";
                   html_li conf;
                   Wserver.wprint "%s\n" x;
                return ())
             (Gutil.person_misc_names base p);
         end
     | Some "image" ->
         do html_p conf; return
         Wserver.wprint "Default image name = \"%s\"\n"
           (default_image_name base p)
     | Some "tstab_val" ->
         let tstab = Util.create_topological_sort conf base in
         do html_p conf; return
         Wserver.wprint "Tstab val = %d\n"
           tstab.(Adef.int_of_iper p.cle_index)
     | _ -> () ];
     trailer conf;
  return ()
;

end;

(* End of old "classic" version *)

(* New version: interpretation of template file 'perso.txt' *)

value iter_first f al =
  let _ = List.fold_left
    (fun first a -> let () = f first a in False)
    True al
  in ()
;

value open_templ conf dir name =
  if dir = "" then
    let fname =
      List.fold_right Filename.concat [lang_dir.val; "etc"]
        (name ^ ".txt")
    in
    try Some (open_in fname) with [ Sys_error _ -> None ]
  else
    let dir = Filename.basename dir in
    let fname =
      List.fold_right Filename.concat [base_dir.val; "etc"; dir]
        (name ^ ".txt")
    in
    try Some (open_in fname) with [ Sys_error _ -> None ]
;

(* 1/ Parsing template *)

type ast =
  [ Atext of string
  | Avar of string and list string
  | Atransl of bool and string and char
  | Awid_hei of string
  | Aif of ast_expr and list ast and list ast
  | Aforeach of string and list string and list ast ]
and ast_expr =
  [ Eor of ast_expr and ast_expr
  | Eand of ast_expr and ast_expr
  | Enot of ast_expr
  | Evar of string and list string ]
;

type token = [ LPAREN | RPAREN | DOT | IDENT of string ];

value rec get_ident len =
  parser
  [ [: `('a'..'z' | 'A'..'Z' | '_' as c); strm :] ->
      get_ident (Buff.store len c) strm
  | [: :] -> Buff.get len ]
;

value get_variable =
  let rec var_kont =
    parser
    [ [: `'.'; s = get_ident 0; sl = var_kont :] -> [s :: sl]
    | [: `';' :] -> []
    | [: :] -> [] ]
  in
  parser
  [ [: `'%' :] -> ("%", [])
  | [: v = get_ident 0; vl = var_kont :] -> (v, vl) ]
;

value rec get_token =
  parser
  [ [: `(' ' | '\t' | '\n' | '\r'); strm :] -> get_token strm
  | [: `'(' :] -> LPAREN
  | [: `')' :] -> RPAREN
  | [: `'.' :] -> DOT
  | [: s = get_ident 0 :] -> IDENT s ]
;

value buff = ref (String.create 80);

value buff_store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;

value buff_mstore len s =
  add_rec len 0 where rec add_rec len i =
    if i == String.length s then len
    else add_rec (buff_store len s.[i]) (succ i)
;

value buff_get len = String.sub buff.val 0 len;

value lexicon_word =
  let upper = parser [ [: `'*' :] -> True | [: :] -> False ] in
  let rec text len =
    parser
    [ [: `']' :] -> Buff.get len
    | [: `c; strm:] -> text (Buff.store len c) strm ]
  in
  parser [: upp = upper; s = text 0; `n :] -> Atransl upp s n
;

value parse_templ conf base strm =
  let parse_bool_expr () =
    let rec parse_1 =
      parser
      [ [: e = parse_2;
           e =
             parser
             [ [: `IDENT "or"; strm :] -> Eor e (parse_1 strm)
             | [: :] -> e ] :] -> e ]
    and parse_2 =
      parser
      [ [: e = parse_3;
           e =
             parser
             [ [: `IDENT "and"; strm :] -> Eand e (parse_2 strm)
             | [: :] -> e ] :] -> e ]
    and parse_3 =
      parser
      [ [: `LPAREN; e = parse_1;
           _ = parser [ [: `RPAREN :] -> () | [: :] -> () ] :] -> e
      | [: `IDENT "not"; e = parse_3 :] -> Enot e
      | [: `IDENT id; idl = ident_list :] -> Evar id idl
      | [: :] -> Evar "bad_variable" [] ]
    and ident_list =
      parser
      [ [: `DOT; `IDENT id; idl = ident_list :] -> [id :: idl]
      | [: :] -> [] ]
    in
    let f _ = try Some (get_token strm) with [ Stream.Failure -> None ] in
    let r = parse_3 (Stream.from f) in
    do match strm with parser [ [: `';' :] -> () | [: :] -> () ]; return r
  in
  let rec parse_astl astl bol len end_list strm =
    match strm with parser
    [ [: `'%' :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        match get_variable strm with
        [ ("%", []) -> parse_astl [Atext "%" :: astl] False 0 end_list strm
        | (v, []) when List.mem v end_list -> (List.rev astl, v)
        | x ->
            let ast =
              match x with
              [ ("if", []) -> parse_if strm
              | ("foreach", []) -> parse_foreach strm
              | ("wid_hei", []) -> Awid_hei (get_ident 0 strm)
              | (v, vl) -> Avar v vl ]
            in
            parse_astl [ast :: astl] False 0 end_list strm ]
    | [: `'[' :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        let a = lexicon_word strm in
        parse_astl [a :: astl] False 0 end_list strm
    | [: `c :] ->
        let empty_c = c = ' ' || c = '\t' in
        let len = if empty_c && bol then len else buff_store len c in
        let bol = empty_c && bol || c = '\n' in
        parse_astl astl bol len end_list strm
    | [: :] ->
        let astl =
          if len = 0 then astl else [Atext (buff_get len) :: astl]
        in
        (List.rev astl, "") ]
  and parse_if strm =
    let e = parse_bool_expr () in
    let (al1, al2) =
      loop () where rec loop () =
        let (al1, tok) =
          parse_astl [] False 0 ["elseif"; "else"; "end"] strm
        in
        match tok with
        [ "elseif" ->
            let e2 = parse_bool_expr () in
            let (al2, al3) = loop () in
            (al1, [Aif e2 al2 al3])
        | "else" ->
            let (al2, _) = parse_astl [] False 0 ["end"] strm in
            (al1, al2)
        | _ -> (al1, []) ]
    in
    Aif e al1 al2
  and parse_foreach strm =
    let (v, vl) = get_variable strm in
    let (astl, _) = parse_astl [] False 0 ["end"] strm in
    Aforeach v vl astl
  in
  fst (parse_astl [] True 0 [] strm)
;

(* 2/ Evaluation of template *)

type env =
  [ Eind of person and ascend and union
  | Efam of family and couple and descend
  | Erel of relation
  | Ebool of bool
  | Estring of string
  | Esosa of Lazy.t (option (Num.t * person))
  | Eimage of Lazy.t (option (string * option (option (int * int))))
  | Enone ]
;

value get_env v env = try List.assoc v env with [ Not_found -> Enone ];

value eval_variable conf base env sl =
  let ep =
    match (get_env "p" env, get_env "p_auth" env) with
    [ (Eind p a u, Ebool p_auth) -> (p, a, u, p_auth)
    | _ -> assert False ]
  in
  let efam = get_env "fam" env in
  let make_ep ip =
    let p = poi base ip in
    let a = aoi base ip in
    let u = uoi base ip in
    let p_auth = age_autorise conf base p in
    (p, a, u, p_auth)
  in
  loop ep efam sl where rec loop (p, a, u, p_auth) efam =
    fun
    [ ["child" :: sl] ->
        match get_env "child" env with
        [ Eind p a u ->
            let auth =
              match get_env "auth" env with
              [ Ebool True -> age_autorise conf base p
              | _ -> False ]
            in
            let ep = (p, a, u, auth) in
            loop ep efam sl
        | _ -> None ]
    | ["father" :: sl] ->
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep cpl.father in
            let efam = Efam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> None ]
    | ["mother" :: sl] ->
        match a.parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            let ep = make_ep cpl.mother in
            let efam = Efam (foi base ifam) cpl (doi base ifam) in
            loop ep efam sl
        | None -> None ]
    | ["self" :: sl] -> loop (p, a, u, p_auth) efam sl
    | ["spouse" :: sl] ->
        match efam with
        [ Efam fam cpl _ ->
            let ip = spouse p.cle_index cpl in
            let ep = make_ep ip in
            loop ep efam sl
        | _ -> None ]
    | [] -> Some ((p, a, u, p_auth), efam, "")
    | [s] -> Some ((p, a, u, p_auth), efam, s)
    | _ -> None ]
;

value print_age conf base env p p_auth =
  if p_auth then
    match (Adef.od_of_codate p.birth, p.death) with
    [ (Some (Dgreg d _), NotDead) ->
        let a = temps_ecoule d conf.today in
        Date.print_age conf a
    | _ -> () ]
  else ()
;

value print_alias conf base env =
  match get_env "alias" env with
  [ Estring s -> Wserver.wprint "%s" s
  | _ -> () ]
;

value print_baptism_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.baptism_place) else ()
;

value print_birth_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.birth_place) else ()
;

value print_body_prop conf base env =
  let s =
    try " dir=" ^ Hashtbl.find conf.lexicon " !dir" with
    [ Not_found -> "" ]
  in
  let s = s ^ body_prop conf in
  Wserver.wprint "%s" s
;

value print_burial_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.burial_place) else ()
;

value print_comment conf base env p p_auth =
  fun
  [ Efam fam _ _ ->
      if p_auth then copy_string_with_macros conf (sou base fam.comment)
      else ()
  | _ -> () ]
;

value print_consanguinity conf base env a p_auth =
  if p_auth then
    do print_decimal_num conf
         (round_2_dec (Adef.float_of_fix a.consang *. 100.0));
       Wserver.wprint "%%";
    return ()
  else ()
;

value print_copyright conf base env =
  let env =
    [('s', commd conf);
     ('d',
      if conf.cancel_links then ""
      else " - <a href=\"" ^ conf.indep_command ^ "m=DOC\">DOC</a>")]
  in
  match open_etc_file "copyr" with
  [ Some ic -> copy_from_etc env conf.indep_command ic
  | None ->
      do html_p conf;
         Wserver.wprint "
<hr><font size=-1><em>(c) Copyright INRIA 2000 -
GeneWeb %s</em></font>" Version.txt;
         html_br conf;
      return () ]
;

value print_death_age conf base env p p_auth =
  if p_auth then
    match Date.get_birth_death_date p with
    [ (Some (Dgreg ({prec = Sure | About | Maybe} as d1) _),
       Some (Dgreg ({prec = Sure | About | Maybe} as d2) _),
       approx) when d1 <> d2 ->
        let a = temps_ecoule d1 d2 in
        do if not approx && d1.prec = Sure && d2.prec = Sure then ()
           else Wserver.wprint "%s " (transl conf "possibly (date)");
           Date.print_age conf a;
        return ()
    | _ -> () ]
  else ()
;

value print_death_place conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (sou base p.death_place) else ()
;

value print_died conf base env p p_auth =
  if p_auth then
    let is = index_of_sex p.sex in
    match p.death with
    [ Death dr _ ->
        let dr_w =
          match dr with
          [ Unspecified -> transl_nth conf "died" is
          | Murdered -> transl_nth conf "murdered" is
          | Killed -> transl_nth conf "killed (in action)" is
          | Executed -> transl_nth conf "executed (legally killed)" is
          | Disappeared -> transl_nth conf "disappeared" is ]
        in
        Wserver.wprint "%s" (capitale dr_w)
    | DeadYoung ->
        Wserver.wprint "%s" (capitale (transl_nth conf "died young" is))
    | DeadDontKnowWhen ->
        Wserver.wprint "%s" (capitale (transl_nth conf "died" is))
    | _ -> () ]
  else ()
;

value print_divorce_date conf base env p p_auth =
  fun
  [ Efam fam cpl _ ->
      match fam.divorce with
      [ Divorced d ->
          let d = Adef.od_of_codate d in
          let auth =
            let spouse = poi base (spouse p.cle_index cpl) in
            p_auth && age_autorise conf base spouse
          in
          match d with
          [ Some d when auth ->
              do Wserver.wprint " <em>";
                 Wserver.wprint "%s" (Date.string_of_ondate conf d);
                 Wserver.wprint "</em>";
              return ()
          | _ -> () ]
      | _ -> () ]
  | _ -> () ]
;

value print_first_name_alias conf base env =
  match get_env "first_name_alias" env with
  [ Estring s -> Wserver.wprint "%s" s
  | _ -> () ]
;  

value print_first_name_key conf base env p p_auth =
  Wserver.wprint "%s" (code_varenv (Name.lower (p_first_name base p)))
;

value print_image_size conf base env p p_auth =
  if p_auth then
    match get_env "image" env with
    [ Eimage x ->
        match Lazy.force x with
        [ Some (_, Some (Some (width, height))) ->
            Wserver.wprint " width=%d height=%d" width height
        | Some (link, None) ->
            Wserver.wprint " height=%d" max_im_hei
        | None | Some (_, Some None) -> () ]
    | _ -> () ]
  else ()
;

value print_image_url conf base env p p_auth =
  if p_auth then
    match get_env "image" env with
    [ Eimage x ->
        match Lazy.force x with
        [ Some (fname, Some (Some _)) ->
            let s = Unix.stat fname in
            let b = acces conf base p in
            let k = default_image_name base p in
            Wserver.wprint "%sm=IM;d=%d;%s;k=/%s" (commd conf)
              (int_of_float (mod_float s.Unix.st_mtime (float_of_int max_int)))
              b k
        | Some (link, None) ->
            Wserver.wprint "%s" link
        | None | Some (_, Some None) -> () ]
    | _ -> () ]
  else ()
;

value print_married_to conf base env p p_auth =
  fun
  [ Efam fam cpl des ->
      let ispouse = spouse p.cle_index cpl in
      let spouse = poi base ispouse in
      let auth = p_auth && age_autorise conf base spouse in
      let format = relation_txt conf p.sex fam in
      Wserver.wprint (fcapitale format)
        (fun _ ->
           if auth then print_marriage_text conf base True fam else ())
  | _ -> () ]
;

value print_nobility_titles conf base env p p_auth =
  if p_auth then print_titles conf base True (transl conf "and") p else ()
;

value print_notes conf base env p p_auth =
  if p_auth then copy_string_with_macros conf (sou base p.notes) else ()
;

value print_occupation conf base env p p_auth =
  if p_auth then Wserver.wprint "%s" (capitale (sou base p.occupation)) else ()
;

value print_on_baptism_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.baptism with
    [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | None -> () ]
  else ()
;

value print_on_birth_date conf base env p p_auth =
  if p_auth then
    match Adef.od_of_codate p.birth with
    [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | None -> () ]
  else ()
;

value print_on_burial_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Buried cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
        | None -> () ]
    | _ -> () ]
  else ()
;

value print_on_cremation_date conf base env p p_auth =
  if p_auth then
    match p.burial with
    [ Cremated cod ->
        match Adef.od_of_codate cod with
        [ Some d -> Wserver.wprint "%s" (Date.string_of_ondate conf d)
        | None -> () ]
    | _ -> () ]
  else ()
;

value print_on_death_date conf base env p p_auth =
  if p_auth then
    match p.death with
    [ Death _ d ->
        let d = Adef.date_of_cdate d in
        Wserver.wprint "%s" (Date.string_of_ondate conf d)
    | _ -> () ]
  else ()
;

value print_origin_file conf base env =
  if conf.wizard then
    match (p_getenv conf.henv "opt", get_env "fam" env) with
    [ (Some "from", Efam fam _ _) ->
        let n = sou base fam.origin_file in
        if n = "" then ()
        else do Wserver.wprint "<em>(%s)</em>" n; html_br conf; return ()
    | _ -> () ]
  else ()
;

value print_prefix_no_templ conf base env =
  let henv =
    List.fold_right
      (fun (k, v) henv -> if k = "templ" then henv else [(k, v) :: henv])
      conf.henv []
  in
  do conf.henv := henv;
      Wserver.wprint "%s" (commd conf);
  return ()
;

value print_public_name conf base env p p_auth =
  Wserver.wprint "%s" (sou base p.public_name)
;

value print_qualifier conf base env p p_auth =
  match (get_env "qualifier" env, p.qualifiers) with
  [ (Estring nn, _) -> Wserver.wprint "%s" nn
  | (_, [nn :: _]) -> Wserver.wprint "%s" (sou base nn)
  | _ -> () ]
;

value print_referer conf base env =
  Wserver.wprint "%s" (Wserver.extract_param "referer: " '\n' conf.request)
;

value print_related conf base env =
  match get_env "c" env with
  [ Eind c _ _ ->
      do Wserver.wprint "%s" (referenced_person_title_text conf base c);
         Date.afficher_dates_courtes conf base c;
      return ()
  | _ -> () ]
;

value print_related_type conf base env =
  match (get_env "c" env, get_env "rel" env) with
  [ (Eind c _ _, Erel r) ->
      Wserver.wprint "%s"
        (capitale (rchild_type_text conf r.r_type (index_of_sex c.sex)))
  | _ -> () ]
;

value print_relation_her conf base env =
  match get_env "rel" env with
  [ Erel {r_moth = Some ip} ->
      let p = poi base ip in
      do Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Date.afficher_dates_courtes conf base p;
      return ()
  | _ -> () ]
;

value print_relation_him conf base env =
  match get_env "rel" env with
  [ Erel {r_fath = Some ip} ->
      let p = poi base ip in
      do Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Date.afficher_dates_courtes conf base p;
      return ()
  | _ -> () ]
;

value print_relation_type conf base env =
  match get_env "rel" env with
  [ Erel r ->
      match (r.r_fath, r.r_moth) with
      [ (Some ip, None) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 0))
      | (None, Some ip) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 1))
      | (Some ip1, Some ip2) ->
           Wserver.wprint "%s" (capitale (relation_type_text conf r.r_type 2))
      | _ -> () ]
  | _ -> () ]
;

value print_sosa conf base env a a_auth =
  match get_env "sosa" env with
  [ Esosa x ->
      match Lazy.force x with
      [ Some (n, p) ->
          Num.print (fun x -> Wserver.wprint "%s" x)
            (transl conf "(thousand separator)") n
      | None -> () ]
  | _ -> () ]
;

value print_sosa_link conf base env a a_auth =
  match get_env "sosa" env with
  [ Esosa x ->
      match Lazy.force x with
      [ Some (n, p) ->
          Wserver.wprint "m=RL;i1=%d;i2=%d;b1=1;b2=%s"
            (Adef.int_of_iper a.cle_index) (Adef.int_of_iper p.cle_index)
            (Num.to_string n)
      | None -> () ]
  | _ -> () ]
;

value print_source conf base env =
  match get_env "src" env with
  [ Estring s -> copy_string_with_macros conf s
  | _ -> () ]
;

value print_source_type conf base env =
  match get_env "src_typ" env with
  [ Estring s -> Wserver.wprint "%s" s
  | _ -> () ]
;

value print_surname_alias conf base env =
  match get_env "surname_alias" env with
  [ Estring s -> Wserver.wprint "%s" s
  | _ -> () ]
;  

value print_surname_key conf base env p p_auth =
  Wserver.wprint "%s" (code_varenv (Name.lower (p_surname base p)))
;

value print_witness conf base env =
  match get_env "witness" env with
  [ Eind p _ _ ->
      do Wserver.wprint "%s" (referenced_person_title_text conf base p);
         Date.afficher_dates_courtes conf base p;
      return ()
  | _ -> () ]
;

value print_witness_relation conf base env =
  fun
  [ Efam _ cpl _ ->
      Wserver.wprint
        (fcapitale (ftransl conf "witness at marriage of %s and %s"))
        (referenced_person_title_text conf base (poi base cpl.father))
        (referenced_person_title_text conf base (poi base cpl.mother))
  | _ -> () ]
;

value print_simple_variable conf base env (p, a, u, p_auth) efam =
  fun
  [ "access" -> Wserver.wprint "%s" (acces conf base p)
  | "age" -> print_age conf base env p p_auth
  | "alias" -> print_alias conf base env
  | "baptism_place" -> print_baptism_place conf base env p p_auth
  | "base_header" -> include_hed_trl conf ".hed"
  | "base_trailer" -> include_hed_trl conf ".trl"
  | "birth_place" -> print_birth_place conf base env p p_auth
  | "body_prop" -> print_body_prop conf base env
  | "border" -> Wserver.wprint "%d" conf.border
  | "burial_place" -> print_burial_place conf base env p p_auth
  | "child_name" ->
      let force_surname =
        match a.parents with
        [ None -> False
        | Some ifam ->
            p_surname base (poi base (coi base ifam).father) <>
            p_surname base p ]
      in
      Wserver.wprint "%s"
        (if force_surname then person_text conf base p
         else person_text_without_surname conf base p)
  | "cremation_place" -> print_burial_place conf base env p p_auth
  | "comment" -> print_comment conf base env p p_auth efam
  | "consanguinity" -> print_consanguinity conf base env a p_auth
  | "copyright" -> print_copyright conf base env
  | "death_age" -> print_death_age conf base env p p_auth
  | "death_place" -> print_death_place conf base env p p_auth
  | "died" -> print_died conf base env p p_auth
  | "divorce_date" -> print_divorce_date conf base env p p_auth efam
  | "dates" -> if p_auth then Date.afficher_dates_courtes conf base p else ()
  | "fam_access" ->
      match efam with
      [ Efam fam _ _ ->
          Wserver.wprint "i=%d;ip=%d" (Adef.int_of_ifam fam.fam_index)
            (Adef.int_of_iper p.cle_index)
      | _ -> () ]
  | "first_name" -> Wserver.wprint "%s" (p_first_name base p)
  | "first_name_alias" -> print_first_name_alias conf base env
  | "first_name_key" -> print_first_name_key conf base env p p_auth
  | "highlight" -> Wserver.wprint "%s" conf.highlight
  | "image_prefix" -> Wserver.wprint "%s" (image_prefix conf)
  | "image_size" -> print_image_size conf base env p p_auth
  | "image_url" -> print_image_url conf base env p p_auth
  | "ind_access" -> Wserver.wprint "i=%d" (Adef.int_of_iper p.cle_index)
  | "married_to" -> print_married_to conf base env p p_auth efam
  | "nl" -> Wserver.wprint "\n"
  | "nobility_titles" -> print_nobility_titles conf base env p p_auth
  | "notes" -> print_notes conf base env p p_auth
  | "occupation" -> print_occupation conf base env p p_auth
  | "on_baptism_date" -> print_on_baptism_date conf base env p p_auth
  | "on_birth_date" -> print_on_birth_date conf base env p p_auth
  | "on_burial_date" -> print_on_burial_date conf base env p p_auth
  | "on_cremation_date" -> print_on_cremation_date conf base env p p_auth
  | "on_death_date" -> print_on_death_date conf base env p p_auth
  | "origin_file" -> print_origin_file conf base env
  | "prefix" -> Wserver.wprint "%s" (commd conf)
  | "prefix_no_templ" -> print_prefix_no_templ conf base env
  | "public_name" -> print_public_name conf base env p p_auth
  | "qualifier" -> print_qualifier conf base env p p_auth
  | "referer" -> print_referer conf base env
  | "related" -> print_related conf base env
  | "relation_her" -> print_relation_her conf base env
  | "relation_him" -> print_relation_him conf base env
  | "relation_type" -> print_relation_type conf base env
  | "related_type" -> print_related_type conf base env
  | "sosa" -> print_sosa conf base env p p_auth
  | "sosa_link" -> print_sosa_link conf base env p p_auth
  | "source_type" -> print_source_type conf base env
  | "source" -> print_source conf base env
  | "sp" -> Wserver.wprint " "
  | "surname" -> Wserver.wprint "%s" (p_surname base p)
  | "surname_alias" -> print_surname_alias conf base env
  | "surname_key" -> print_surname_key conf base env p p_auth
  | "title" -> Wserver.wprint "%s" (person_title conf base p)
  | "witness_relation" -> print_witness_relation conf base env efam
  | "witness" -> print_witness conf base env
  | v -> Wserver.wprint "%%%s;" v ]
;

value simple_person_text conf base p p_auth =
  if p_auth then
    match main_title base p with
    [ Some t -> titled_person_text conf base p t
    | None -> person_text conf base p ]
  else person_text conf base p
;

value print_simple_person_text conf base (p, _, _, p_auth) =
  Wserver.wprint "%s" (simple_person_text conf base p p_auth)
;

value print_variable conf base env sl =
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") -> print_simple_person_text conf base ep
  | Some (ep, efam, s) -> print_simple_variable conf base env ep efam s
  | None ->
      do iter_first
           (fun first s -> Wserver.wprint "%s%s" (if first then "" else ".") s)
           sl;
         Wserver.wprint "???";
      return () ]
;

value eval_simple_bool_variable conf base env (p, a, u, p_auth) efam =
  fun
  [ "are_divorced" ->
      match efam with
      [ Efam fam cpl _ ->
          match fam.divorce with
          [ Divorced d -> True
          | _ -> False ]
      | _ -> False ]
  | "are_separated" ->
      match efam with
      [ Efam fam cpl _ -> fam.divorce = Separated
      | _ -> False ]
  | "birthday" ->
      if p_auth then
        match Adef.od_of_codate p.birth with
        [ Some (Dgreg d _) ->
            if d.prec = Sure && p.death = NotDead then
              d.day = conf.today.day && d.month = conf.today.month &&
              d.year < conf.today.year
            || not (leap_year conf.today.year) && d.day = 29 &&
              d.month = 2 && conf.today.day = 1 &&
              conf.today.month = 3
            else False
        | _ -> False ]
      else False
  | "cancel_links" -> conf.cancel_links
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
           Some (Dgreg ({prec = Sure | About | Maybe} as d2) _),
           approx) when d1 <> d2 ->
            let a = temps_ecoule d1 d2 in
            not (a.year < 0 || a.year = 0 && a.month = 0)
        | _ -> False ]
      else False
  | "has_aliases" -> p.aliases <> []
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
  | "has_cremation_date" ->
      if p_auth then
        match p.burial with
        [ Cremated cod -> Adef.od_of_codate cod <> None
        | _ -> False ] 
      else False
  | "has_children" ->
      match efam with
      [ Efam _ _ des -> Array.length des.children > 0
      | _ ->
          List.exists
            (fun ifam ->
               let des = doi base ifam in
               Array.length des.children > 0)
            (Array.to_list u.family) ]
  | "has_comment" ->
      match efam with
      [ Efam fam _ _ -> p_auth && sou base fam.comment <> ""
      | _ -> False ]
  | "has_consanguinity" ->
      p_auth && a.consang != Adef.fix (-1) && a.consang != Adef.fix 0
  | "has_cremation_place" -> p_auth && sou base p.burial_place <> ""
  | "has_death_date" ->
      match p.death with
      [ Death _ _ -> p_auth
      | _ -> False ]
  | "has_death_place" -> p_auth && sou base p.death_place <> ""
  | "has_families" -> Array.length u.family > 0
  | "has_first_names_aliases" -> p.first_names_aliases <> []
  | "has_image" ->
      match get_env "image" env with
      [ Eimage x ->
          match Lazy.force x with
          [ Some (_, Some (Some _)) | Some (_, None) -> True
          | _ -> False ]
      | _ -> False ]
  | "has_nephews_or_nieces" -> has_nephews_or_nieces base p
  | "has_nobility_titles" -> p_auth && p.titles <> []
  | "has_notes" -> p_auth && sou base p.notes <> ""
  | "has_occupation" -> p_auth && sou base p.occupation <> ""
  | "has_parents" -> a.parents <> None
  | "has_public_name" -> sou base p.public_name <> ""
  | "has_qualifiers" -> p.qualifiers <> []
  | "has_referer" -> Wserver.extract_param "referer: " '\n' conf.request <> ""
  | "has_relation_her" ->
      match get_env "rel" env with
      [ Erel {r_moth = Some _} -> True
      | _ -> False ]
  | "has_relation_him" ->
      match get_env "rel" env with
      [ Erel {r_fath = Some _} -> True
      | _ -> False ]
  | "has_relations" -> p_auth && (p.rparents <> [] || p.related <> [])
  | "has_sosa" ->
      match get_env "sosa" env with
      [ Esosa x -> Lazy.force x <> None
      | _ -> False ]
  | "has_sources" ->
      if sou base p.psources <> "" then True
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
  | "has_witnesses" ->
      match efam with
      [ Efam fam _ _ -> Array.length fam.witnesses > 0
      | _ -> False ]
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
  | "is_female" -> p.sex = Female
  | "is_first" ->
       match get_env "first" env with
       [ Ebool x -> x
       | _ -> False ]
  | "is_male" -> p.sex = Male
  | "is_sibling_after" -> get_env "pos" env = Estring "next"
  | "is_sibling_before" -> get_env "pos" env = Estring "prev"
  | "is_self" -> get_env "pos" env = Estring "self"
  | "wizard" -> conf.wizard
  | v -> do Wserver.wprint "%%%s;" v; return True ]
;

value eval_bool_variable conf base env sl =
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") ->
      do iter_first
           (fun first s ->
              Wserver.wprint "%s%s" (if first then "" else ".") s)
           sl;
         Wserver.wprint "???";
      return False
  | Some (ep, efam, s) -> eval_simple_bool_variable conf base env ep efam s
  | None -> False ]
;

value split_at_coloncolon s =
  loop 0 where rec loop i =
    if i >= String.length s - 1 then None
    else
      match (s.[i], s.[i+1]) with
      [ (':', ':') ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 2) (String.length s - i - 2) in
          Some (s1, s2)
      | _ -> loop (i + 1) ]
;

value print_transl conf base env upp s c =
  let r =
    match c with
    [ '0'..'9' ->
        let n = Char.code c - Char.code '0' in
        match split_at_coloncolon s with
        [ None -> Util.transl_nth conf s n
        | Some (s1, s2) ->
            Util.transl_decline conf s1 (Util.transl_nth conf s2 n) ]
    | 'n' ->
        let n =
          match get_env "p" env with
          [ Eind p _ _ -> 1 - index_of_sex p.sex
          | _ -> 2 ]
        in
        Util.transl_nth conf s n
    | 's' ->
        let n =
          match get_env "child" env with
          [ Eind p _ _ -> index_of_sex p.sex
          | _ ->
              match get_env "p" env with
              [ Eind p _ _ -> index_of_sex p.sex
              | _ -> 2 ] ]
        in
        Util.transl_nth conf s n
    | 'w' ->
        let n =
          match get_env "fam" env with
          [ Efam fam _ _ -> if Array.length fam.witnesses = 1 then 0 else 1
          | _ -> 0 ]
        in
        Util.transl_nth conf s n
    | _ -> Util.transl conf s ^ String.make 1 c ]
  in
  Wserver.wprint "%s" (if upp then capitale r else r)
;

value print_wid_hei conf base env fname =
  match image_size (image_file_name fname) with
  [ Some (wid, hei) -> Wserver.wprint " width=%d height=%d" wid hei
  | None -> () ]
;

value eval_bool_value conf base env =
  eval where rec eval =
    fun
    [ Eor e1 e2 -> eval e1 || eval e2
    | Eand e1 e2 -> eval e1 && eval e2
    | Enot e -> not (eval e)
    | Evar s sl -> eval_bool_variable conf base env [s :: sl] ]
;

value make_ep base ip =
  let p = poi base ip in
  let a = aoi base ip in
  let u = uoi base ip in
  Eind p a u
;

value rec eval_ast conf base env =
  fun
  [ Atext s -> Wserver.wprint "%s" s
  | Atransl upp s n -> print_transl conf base env upp s n
  | Avar s sl -> print_variable conf base env [s :: sl]
  | Awid_hei s -> print_wid_hei conf base env s
  | Aif e alt ale -> eval_if conf base env e alt ale
  | Aforeach s sl al -> eval_foreach conf base env s sl al ]
and eval_if conf base env e alt ale =
  let al = if eval_bool_value conf base env e then alt else ale in
  List.iter (eval_ast conf base env) al
and eval_foreach conf base env s sl al =
  let (sl, s) =
    let sl = List.rev [s :: sl] in
    (List.rev (List.tl sl), List.hd sl)
  in
  match eval_variable conf base env sl with
  [ Some (ep, efam, "") -> eval_simple_foreach conf base env al ep efam s
  | Some (ep, efam, _) ->
      do Wserver.wprint "foreach ";
         List.iter (fun s -> Wserver.wprint "%s." s) sl;
         Wserver.wprint "%s???" s;
      return ()
  | None -> () ]
and eval_simple_foreach conf base env al ep efam =
  fun
  [ "alias" -> eval_foreach_alias conf base env al ep
  | "child" -> eval_foreach_child conf base env al efam
  | "family" -> eval_foreach_family conf base env al ep
  | "first_name_alias" -> eval_foreach_first_name_alias conf base env al ep
  | "qualifier" -> eval_foreach_qualifier conf base env al ep
  | "related" -> eval_foreach_related conf base env al ep
  | "relation" -> eval_foreach_relation conf base env al ep
  | "source" -> eval_foreach_source conf base env al ep
  | "surname_alias" -> eval_foreach_surname_alias conf base env al ep
  | "witness" -> eval_foreach_witness conf base env al efam
  | "witness_relation" -> eval_foreach_witness_relation conf base env al ep
  | s -> Wserver.wprint "foreach %s???" s ]
and eval_foreach_alias conf base env al (p, _, _, p_auth) =
  List.iter
    (fun a ->
       let env = [("alias", Estring (sou base a)) :: env] in
       List.iter (eval_ast conf base env) al)
    p.aliases
and eval_foreach_child conf base env al =
  fun
  [ Efam _ _ des ->
      let auth =
        List.for_all
          (fun ip -> age_autorise conf base (poi base ip))
          (Array.to_list des.children)
      in
      let env = [("auth", Ebool auth) :: env] in
      let n =
        let p =
          match get_env "p" env with
          [ Eind p _ _ -> p
          | _ -> assert False ]
        in
        loop 0 where rec loop i =
          if i = Array.length des.children then -2
          else if des.children.(i) = p.cle_index then i else loop (i + 1)
      in
      Array.iteri
        (fun i ip ->
           let p = poi base ip in
           let a = aoi base ip in
           let u = uoi base ip in
           let env = [("child", Eind p a u) :: env] in
           let env =
             if i = n - 1 then [("pos", Estring "prev") :: env]
             else if i = n then [("pos", Estring "self") :: env]
             else if i = n + 1 then [("pos", Estring "next") :: env]
             else env
           in
           List.iter (eval_ast conf base env) al)
        des.children
  | _ -> () ]
and eval_foreach_family conf base env al (p, _, u, _) =
  Array.iter
    (fun ifam ->
       let fam = foi base ifam in
       let cpl = coi base ifam in
       let des = doi base ifam in
       let env = [("fam", Efam fam cpl des) :: env] in
       List.iter (eval_ast conf base env) al)
    u.family
and eval_foreach_first_name_alias conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun s ->
         let env = [("first_name_alias", Estring (sou base s)) :: env] in
         List.iter (eval_ast conf base env) al)
      p.first_names_aliases
  else ()
and eval_foreach_qualifier conf base env al (p, _, _, _) =
  iter_first
    (fun first nn ->
       let env = [("qualifier", Estring (sou base nn)) :: env] in
       let env = [("first", Ebool first) :: env] in
       List.iter (eval_ast conf base env) al)
    p.qualifiers
and eval_foreach_relation conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun r ->
         let env = [("rel", Erel r) :: env] in
         List.iter (eval_ast conf base env) al)
      p.rparents
  else ()
and eval_foreach_related conf base env al (p, _, _, p_auth) =
  if p_auth then
    List.iter
      (fun ic ->
         let c = poi base ic in
         let a = aoi base ic in
         let u = uoi base ic in
         let env = [("c", Eind c a u) :: env] in
         List.iter
           (fun r ->
              do match r.r_fath with
                 [ Some ip ->
                     if ip = p.cle_index then
                       let env = [("rel", Erel r) :: env] in
                       List.iter (eval_ast conf base env) al
                     else ()
                 | None -> () ];
                 match r.r_moth with
                 [ Some ip ->
                     if ip = p.cle_index then
                       let env = [("rel", Erel r) :: env] in
                       List.iter (eval_ast conf base env) al
                     else ()
                 | None -> () ];
              return ())
           c.rparents)
      p.related
  else ()
and eval_foreach_source conf base env al (p, _, u, p_auth) =
  let print_src src_typ src =
    let s = sou base src in
    if s = "" then ()
    else
      let env =
        [("src_typ", Estring src_typ); ("src", Estring s) :: env]
      in
      List.iter (eval_ast conf base env) al
  in
  do print_src (transl_nth conf "person/persons" 0) p.psources;
     if p_auth then
       do print_src (transl_nth conf "birth" 0) p.birth_src;
          print_src (transl_nth conf "baptism" 0) p.baptism_src;
          print_src (transl_nth conf "death" 0) p.death_src;
          print_src (transl_nth conf "burial" 0) p.burial_src;
       return ()
     else ();
     for i = 0 to Array.length u.family - 1 do
       let fam = foi base u.family.(i) in
       let lab =
         if Array.length u.family == 1 then ""
         else " " ^ string_of_int (i + 1)
       in
       do if p_auth then
            let src_typ = transl_nth conf "marriage/marriages" 0 in
            print_src (src_typ ^ lab) fam.marriage_src
          else ();
          let src_typ = nominative (transl_nth conf "family/families" 0) in
          print_src (src_typ ^ lab) fam.fsources;
       return ();
     done;
  return ()
and eval_foreach_surname_alias conf base env al (p, _, _, _) =
  List.iter
    (fun s ->
       let env = [("surname_alias", Estring (sou base s)) :: env] in
       List.iter (eval_ast conf base env) al)
    p.surnames_aliases
and eval_foreach_witness conf base env al =
  fun
  [ Efam fam _ _ ->
      iter_first
        (fun first ip ->
           let p = poi base ip in
           let a = aoi base ip in
           let u = uoi base ip in
           let env = [("witness", Eind p a u) :: env] in
           let env = [("first", Ebool first) :: env] in
           List.iter (eval_ast conf base env) al)
        (Array.to_list fam.witnesses)
  | _ -> () ]
and eval_foreach_witness_relation conf base env al (p, _, _, _) =
  List.iter
    (fun ic ->
       let c = poi base ic in
       if c.sex = Male then
         Array.iter
           (fun ifam ->
              let fam = foi base ifam in
              if array_memq p.cle_index fam.witnesses then
                let cpl = coi base ifam in
                let des = doi base ifam in
                let env = [("fam", Efam fam cpl des) :: env] in
                List.iter (eval_ast conf base env) al
              else ())
           (uoi base ic).family
       else ())
    p.related
;

value strip_newlines_after_variables =
  loop where rec loop =
    fun
    [ [Atext s :: astl] ->
        let s =
          if s.[0] = '\n' then String.sub s 1 (String.length s - 1) else s
        in
        [Atext s :: loop astl]
    | [Aif s alt ale :: astl] -> [Aif s (loop alt) (loop ale) :: loop astl]
    | [Aforeach s sl al :: astl] -> [Aforeach s sl (loop al) :: loop astl]
    | [(Avar _ _ as ast) :: astl] -> [ast :: loop astl]
    | [(Atransl _ _ _ | Awid_hei _ as ast1); ast2 :: astl] ->
        [ast1; ast2 :: loop astl]
    | [ast] -> [ast]
    | [] -> [] ]
;

value copy_from_templ conf base ic p =
  let astl = parse_templ conf base (Stream.of_channel ic) in
  do close_in ic; return
  let a = aoi base p.cle_index in
  let u = uoi base p.cle_index in
  let env =
    let env = [] in
    let env =
      let v = lazy (find_sosa conf base p) in
      [("sosa", Esosa v) :: env]
    in
    let env =
      let v =
        lazy
          (image_and_size conf base p
             (limited_image_size max_im_wid max_im_wid))
      in
      [("image", Eimage v) :: env]
    in
    let env = [("p_auth", Ebool (age_autorise conf base p)) :: env] in
    let env = [("p", Eind p a u) :: env] in
    env
  in
  let astl = strip_newlines_after_variables astl in
  List.iter (eval_ast conf base env) astl
;

(* Main *)

value print_ok conf base p =
  let config_templ =
    try
      let s = List.assoc "template" conf.base_env in
      loop [] 0 0 where rec loop list i len =
        if i == String.length s then
          List.rev [Buff.get len :: list]
        else if s.[i] = ',' then
          loop [Buff.get len :: list] (i + 1) 0
        else
          loop list (i + 1) (Buff.store len s.[i])
    with
    [ Not_found -> ["*"] ]
  in
  let templ =
    match p_getenv conf.env "templ" with
    [ Some "off" -> None
    | Some x when List.mem "*" config_templ -> Some x
    | Some x when List.mem x config_templ -> Some x
    | Some _ | None ->
        match config_templ with
        [ [] | ["*"] -> Some ""
        | [x :: _] -> Some x ] ]
  in
  match templ with
  [ None -> Classic.print_ok conf base p
  | Some dir ->
      match open_templ conf dir "perso" with
      [ Some ic ->
          do html conf; copy_from_templ conf base ic p; return ()
      | None ->
          let title _ = Wserver.wprint "Error" in
          do Util.header conf title;
             tag "ul" begin
               html_li conf;
               Wserver.wprint "Cannot access file \"%s.txt\".\n" "perso";
             end;
             Util.gen_trailer True conf;
          return raise Exit ] ]
;

value print conf base p =
  let passwd =
    if conf.wizard || conf.friend then None
    else
      let src =
        match (aoi base p.cle_index).parents with
        [ Some ifam -> sou base (foi base ifam).origin_file
        | None -> "" ]
      in
      try Some (src, List.assoc ("passwd_" ^ src) conf.base_env)
      with [ Not_found -> None ]
  in
  match passwd with
  [ Some (src, passwd) when passwd <> conf.passwd -> Util.unauthorized conf src
  | _ -> print_ok conf base p ]
;

(* For external interface, using Classic *)

value print_sources = Classic.print_sources;
