(* camlp4r ./pa_html.cmo *)
(* $Id: perso.ml,v 3.20 2000-05-14 16:43:52 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

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

value has_children base u =
  List.exists
    (fun ifam ->
       let des = doi base ifam in
       Array.length des.children > 0)
    (Array.to_list u.family)
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

value
  print_title conf base cap and_txt p first (nth, name, title, places, dates) =
  do let href =
       "m=TT;sm=S;t=" ^ code_varenv (sou base title) ^ ";p=" ^
       code_varenv (sou base (List.hd places))
     in
     let tit = sou base title in
     let s =
       (if first && cap then capitale tit else tit) ^ " " ^
       sou base (List.hd places)
     in
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
           do wprint_geneweb_link conf href (sou base place);
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
            print_title conf base cap and_txt p first t;
         return False)
      True titles
  in
  ()
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
     [ (Some (Dgreg d1 _), Some (Dgreg d2 _), approx) when d1 <> d2 ->
         let a = temps_ecoule d1 d2 in
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

value print_family conf base p a ifam =
  let fam = foi base ifam in
  let des = doi base ifam in
  let ispouse = spouse p.cle_index (coi base ifam) in
  let spouse = poi base ispouse in
  let children = des.children in
  let divorce = fam.divorce in
  let is = index_of_sex p.sex in
  let auth = age_autorise conf base p && age_autorise conf base spouse in
  do Wserver.wprint "\n";
     html_li conf;
     let format =
       if fam.not_married then ftransl conf "relationship%t to"
       else ftransl_nth conf "married%t to" is
     in
     let txt =
       gen_referenced_person_title_text raw_access conf base (poi base ispouse)
     in
     Wserver.wprint (fcapitale (fdecline conf format txt))
       (fun _ -> if auth then print_marriage_text conf base True fam else ());
     Date.afficher_dates_courtes conf base (poi base ispouse);
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

value print_related conf base p ic =
  let c = poi base ic in
  do List.iter
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
       c.rparents;
     if c.sex = Male then
       List.iter
         (fun ifam ->
            let fam = foi base ifam in
            if array_memq p.cle_index fam.witnesses then
              print_witness_at_marriage conf base (coi base ifam)
            else ())
         (Array.to_list (uoi base ic).family)
     else ();
  return ()
;

value print_fwitnesses conf base p nfam n ifam =
  let fam = foi base ifam in
  if Array.length fam.witnesses <> 0 then
    Array.iter
      (fun ip ->
         let p = poi base ip in
         do html_li conf;
            Wserver.wprint "%s"
              (capitale (transl_nth conf "witness/witnesses" 0));
            if nfam > 1 then
              Wserver.wprint " (%s %d)"
                (transl_nth conf "marriage/marriages" 0) (n + 1)
            else ();
            Wserver.wprint ":\n%s" (referenced_person_title_text conf base p);
            Date.afficher_dates_courtes conf base p;
            Wserver.wprint "\n";
         return ())
      fam.witnesses
  else ()
;

value print_relations conf base p =
  let u = uoi base p.cle_index in
  let has_marriage_witnesses =
    List.exists
      (fun ifam -> (foi base ifam).witnesses <> [| |])
      (Array.to_list u.family)
  in
  match (p.rparents, p.related, has_marriage_witnesses) with
  [ ([], [], False) -> ()
  | (rl, cl, _) ->
      do Wserver.wprint "<h3>%s</h3>\n"
           (std_color conf (capitale (transl_nth conf "relation/relations" 1)));
         tag "ul" begin
           List.iter (print_relation conf base) rl;
           List.iter (print_related conf base p) cl;
           Array.iteri (print_fwitnesses conf base p (Array.length u.family))
             u.family;
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
  do print_not_empty_src conf base new_parag first
       (fun () -> nominative (transl_nth conf "person/persons" 0))
       p.psources;
     print_not_empty_src conf base new_parag first
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
     for i = 0 to Array.length u.family - 1 do
       let fam = foi base u.family.(i) in
       do print_not_empty_src conf base new_parag first
            (fun () ->
               transl_nth conf "marriage/marriages" 0 ^
               (if Array.length u.family == 1 then ""
                else " " ^ string_of_int (i + 1)))
            fam.marriage_src;
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

value print_sosa conf base open_area a =
  fun
  [ Some (n, p) ->
      do open_area ();
         Wserver.wprint "<em>Sosa:\n";
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
     match (p.public_name, p.nick_names) with
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
             Wserver.wprint "%s <em><strong>%s</strong></em>"
               (capitale (transl conf "alias")) (sou base a);
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
     match (sou base p.public_name, p.nick_names) with
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

value max_im_wid = 240;
value max_im_hei = 240;

value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

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
  let image_txt = capitale (transl_nth conf "image/images" 0) in
  match
    image_and_size conf base p (limited_image_size max_im_wid max_im_wid)
  with
  [ Some (fname, Some (width, height)) ->
      tag "table" "border=%d width=\"95%%\"" conf.border begin
        tag "tr" begin
          let s = Unix.stat fname in
          let b = acces conf base p in
          let k = default_image_name base p in
          tag "td" begin
            Wserver.wprint "<a href=\"%sm=IM;%s;k=/%s\">" (commd conf) b k;
            Wserver.wprint "\
<img src=\"%sm=IM;d=%d;%s;k=/%s\" width=%d height=%d border=0 alt=\"%s\">"
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
  | None ->
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
    match (sou base p.public_name, p.nick_names) with
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
  do header conf title;
     print_sub_titles conf base p;
     print_link_to_welcome conf True;
     print_photo_occupation_dates conf base p;
     print_parents conf base a;
     print_families conf base p a u;
     if age_autorise conf base p then
       do print_relations conf base p;
          print_notes conf base p;
          print_sources conf base True p;
       return ()
     else ();
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
