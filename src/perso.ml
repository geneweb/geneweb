(* camlp4r ./pa_html.cmo *)
(* $Id: perso.ml,v 2.50 1999-09-28 20:11:25 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

exception Ok;
value has_grand_parents base p =
  let rec loop niveau a =
    if niveau = 2 then raise Ok
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
  try do loop 0 (aoi base p.cle_index); return False with [ Ok -> True ]
;

value has_grand_children base p =
  try
    do Array.iter
         (fun fi ->
            let el = (foi base fi).children in
            Array.iter
              (fun e ->
                 Array.iter
                   (fun fi ->
                      let eel = (foi base fi).children in
                      Array.iter (fun _ -> raise Ok) eel)
                   (poi base e).family)
              el)
         p.family;
    return False
  with
  [ Ok -> True ]
;

value of_course_died conf p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> conf.today.year - d.year > 120
  | _ -> False ]
;

value prev_sibling base p a =
  match a.parents with
  [ Some ifam ->
      let fam = foi base ifam in
      loop 0 where rec loop i =
        if i == Array.length fam.children then None
        else if fam.children.(i) = p.cle_index then
          if i == 0 then None else Some (poi base fam.children.(i-1))
        else loop (i + 1)
  | None -> None ]
;

value next_sibling base p a =
  match a.parents with
  [ Some ifam ->
      let fam = foi base ifam in
      loop 0 where rec loop i =
        if i == Array.length fam.children then None
        else if fam.children.(i) = p.cle_index then
          if i == Array.length fam.children - 1 then None
          else Some (poi base fam.children.(i+1))
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
      do Wserver.wprint "%s" (transl_nth conf "nth" nth); return False
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

value print_dates conf base in_perso p =
  let cap s = if in_perso then capitale s else ", " ^ s in
  let is = index_of_sex p.sex in
  do let birth_place = sou base p.birth_place in
     do if in_perso then
          match (Adef.od_of_codate p.birth, birth_place) with
          [ (None, "") -> ()
          | _ -> Wserver.wprint "<em>\n" ]
        else ();
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
        match Adef.od_of_codate p.birth with
        [ Some d ->
            do Wserver.wprint "%s " (cap (transl_nth conf "born" is));
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if in_perso && anniv then
                 Wserver.wprint " (%s)"
                   (transl conf "happy birthday to you!")
               else ();
               if birth_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if birth_place <> "" then
              Wserver.wprint "%s\n-&nbsp;" (cap (transl_nth conf "born" is))
            else () ];
        if birth_place <> "" then Wserver.wprint "%s" birth_place else ();
        if in_perso then
          match (Adef.od_of_codate p.birth, birth_place) with
          [ (None, "") -> ()
          | _ -> do Wserver.wprint ".</em>"; html_br conf; return () ]
        else ();
     return ();
     let baptism = Adef.od_of_codate p.baptism in
     let baptism_place = sou base p.baptism_place in
     do if in_perso then
          match (baptism, baptism_place) with
          [ (None, "") -> ()
          | _ -> Wserver.wprint "<em>\n" ]
        else ();
        match baptism with
        [ Some d ->
            do Wserver.wprint "%s "
                 (cap (transl_nth conf "baptized" is));
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if baptism_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | None ->
            if baptism_place <> "" then
              Wserver.wprint "%s\n-&nbsp;"
                (cap (transl_nth conf "baptized" is))
            else () ];
        if baptism_place <> "" then
          Wserver.wprint "%s" baptism_place
        else ();
        if in_perso then
          match (baptism, baptism_place) with
          [ (None, "") -> ()
          | _ -> do Wserver.wprint ".</em>"; html_br conf; return () ]
        else ();
     return ();
     let death_place = sou base p.death_place in
     let something =
       match (p.death, death_place, p.burial) with
       [ (DontKnowIfDead | NotDead, "", _) -> False
       | (DeadDontKnowWhen, "", Buried _ | Cremated _) -> False
       | (DeadDontKnowWhen, _, _) ->
           death_place <> "" || not (of_course_died conf p)
       | _ -> True ]
     in
     do if something && in_perso then Wserver.wprint "<em>\n" else ();
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
            do Wserver.wprint "%s " (cap dr_w);
               Wserver.wprint "%s" (Date.string_of_ondate conf d);
               if death_place <> "" then Wserver.wprint ",\n" else ();
            return ()
        | DeadYoung ->
            do Wserver.wprint "%s" (cap (transl_nth conf "dead young" is));
               if death_place <> "" then Wserver.wprint "\n-&nbsp;" else ();
            return ()
        | DeadDontKnowWhen ->
            match (death_place, p.burial) with
            [ ("", Buried _ | Cremated _) -> ()
            | _ ->
                if death_place <> "" || not (of_course_died conf p) then
                  do Wserver.wprint "%s" (cap (transl_nth conf "died" is));
                     if death_place <> "" then Wserver.wprint "\n-&nbsp;"
                     else ();
                  return ()
                else () ]
        | DontKnowIfDead | NotDead -> () ];
        if death_place <> "" then
          do Wserver.wprint "%s" death_place;
          return ()
        else ();
        if something && in_perso then
          do Wserver.wprint ".</em>"; html_br conf; return ()
        else ();
     return ();
     if in_perso then
       match (Adef.od_of_codate p.birth, p.death) with
       [ (Some (Dgreg d _), NotDead) ->
           if d.day == 0 && d.month == 0 && d.prec <> Sure then ()
           else
             let a = temps_ecoule d conf.today in
             do Wserver.wprint "<em>%s: " (cap (transl conf "age"));
                Date.print_age conf a;
                Wserver.wprint ".</em>";
                html_br conf;
            return ()
       | _ -> () ]
     else ();
     let sure d = d.prec = Sure in
     match (Adef.od_of_codate p.birth, date_of_death p.death) with
     [ (Some (Dgreg d1 _), Some (Dgreg d2 _)) ->
         if sure d1 && sure d2 && d1 <> d2 then
           let a = temps_ecoule d1 d2 in
           do if in_perso then
                do Wserver.wprint "\n<em>";
                   Wserver.wprint "%s " (capitale (transl conf "death age:"));
                return ()
              else
                do Wserver.wprint "\n(";
                   Wserver.wprint "%s " (transl conf "death age:");
                return ();
              Date.print_age conf a;
              if in_perso then
                do Wserver.wprint ".</em>";
                   html_br conf;
                return ()
              else Wserver.wprint ")";
           return ()
         else ()
     | _ -> () ];
     let something =
       match p.burial with
       [ Buried _ | Cremated _ -> True
       | _ -> False ]
     in
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
     do if something && in_perso then Wserver.wprint "<em>\n" else ();
        match p.burial with
        [ Buried cod ->
            do Wserver.wprint "%s" (cap (transl_nth conf "buried" is));
               burial_date_place cod;
            return ()
        | Cremated cod ->
            do Wserver.wprint "%s"
                 (cap (transl_nth conf "cremated" is));
               burial_date_place cod;
            return ()
        | UnknownBurial -> () ];
        if something && in_perso then
          do Wserver.wprint ".</em>"; html_br conf; return ()
        else ();
     return ();
  return ()
;

value print_parents conf base p =
  match p.parents with
  [ Some ifam ->
      let ifath = (coi base ifam).father in
      let imoth = (coi base ifam).mother in
      let fath = poi base ifath in
      let moth = poi base imoth in
      do Wserver.wprint "<h3>%s</h3>\n\n<ul>\n"
           (capitale (nominative (transl conf "parents")));
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
  let ispouse = spouse p (coi base ifam) in
  let spouse = poi base ispouse in
  let children = fam.children in
  let divorce = fam.divorce in
  let is = index_of_sex p.sex in
  let auth = age_autorise conf base p && age_autorise conf base spouse in
  do Wserver.wprint "\n";
     html_li conf;
     let format =
       if fam.not_married && auth then ftransl conf "relationship%t to"
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

value print_families conf base p a =
  match Array.to_list p.family with
  [ [] -> ()
  | faml ->
      do Wserver.wprint "<h3>%s %s %s</h3>\n\n<ul>"
           (capitale (transl_nth conf "marriage/marriages" 1))
           (transl conf "and") (transl_nth conf "child/children" 1);
         List.iter (print_family conf base p a) faml;
         Wserver.wprint "</ul>\n";
      return () ]
;

value print_notes conf base p =
  match sou base p.notes with
  [ "" -> ()
  | notes ->
      if age_autorise conf base p then
        do Wserver.wprint "<h3>%s</h3>\n\n"
             (capitale (nominative (transl_nth conf "note/notes" 1)));
           tag "ul" begin
             html_li conf;
             copy_string_with_macros conf notes;
             Wserver.wprint "\n";
           end;
        return ()
      else () ]
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

value print_rchildren conf base p ic =
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
  let has_marriage_witnesses =
    List.exists
      (fun ifam -> (foi base ifam).witnesses <> [| |])
      (Array.to_list p.family)
  in
  match (p.rparents, p.related, has_marriage_witnesses) with
  [ ([], [], False) -> ()
  | (rl, cl, _) ->
      do Wserver.wprint "<h3>%s</h3>\n"
           (capitale (transl_nth conf "relation/relations" 1));
         tag "ul" begin
           List.iter (print_relation conf base) rl;
           List.iter (print_rchildren conf base p) cl;
           Array.iteri (print_fwitnesses conf base p (Array.length p.family))
             p.family;
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
     for i = 0 to Array.length p.family - 1 do
       let fam = foi base p.family.(i) in
       do print_not_empty_src conf base new_parag first
            (fun () ->
               transl_nth conf "marriage/marriages" 0 ^
               (if Array.length p.family == 1 then ""
                else " " ^ string_of_int (i + 1)))
            fam.marriage_src;
          print_not_empty_src conf base new_parag first
            (fun () ->
               nominative (transl_nth conf "family/families" 0) ^
               (if Array.length p.family == 1 then ""
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
    | Right z -> Some z ]
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

value find_sosa conf base a p =
  if a.cle_index = p.cle_index then Some Num.one
  else
    let has_children =
      List.exists
        (fun ifam ->
           let fam = foi base ifam in
           Array.length fam.children > 0)
        (Array.to_list a.family)
    in
    if has_children then find_sosa_aux conf base a p
    else None
;

value print_sosa conf base a p =
  match find_sosa conf base a p with
  [ Some n ->
      do Wserver.wprint "<em>Sosa:\n";
         stag "a" "href=\"%sm=RL;i1=%d;i2=%d;b1=1;b2=%s\""
           (commd conf) (Adef.int_of_iper a.cle_index)
           (Adef.int_of_iper p.cle_index) (Num.to_string n)
         begin
           Num.print (fun x -> Wserver.wprint "%s" x)
             (transl conf "(thousand separator)") n;
         end;
         Wserver.wprint "</em>";
         html_p conf;
      return ()
  | None -> () ]
;

value print_sosa_if_any conf base a =
  match Util.find_person_in_env conf base "z" with
  [ Some p -> print_sosa conf base a p 
  | None -> () ]
;

value print_ancestors_descends_cousins conf base p a =
  let st = ref 0 in
  let head () =
    match st.val with
    [ 0 -> do Wserver.wprint "\n<h4>"; st.val := 1; return ()
    | 1 -> Wserver.wprint " /\n"
    | 2 -> st.val := 1
    | _ -> () ]
  in
  let has_grand_parents = has_grand_parents base p in
  do if has_grand_parents then
       do head ();
          Wserver.wprint "<a href=\"%s%s;m=A\">%s</a>"
            (commd conf) (acces conf base p)
            (capitale (transl conf "ancestors"));
       return ()
     else ();
     if has_grand_children base p then
       do head ();
          Wserver.wprint "<a href=\"%s%s;m=D\">%s</a>"
            (commd conf) (acces conf base p)
            (capitale (transl conf "descendants"));
       return ()
     else ();
     if has_grand_parents then
       do head ();
          Wserver.wprint "<a href=\"%s%s;m=C\">%s</a>"
            (commd conf) (acces conf base p)
            (capitale (transl conf "cousins (general term)"));
       return ()
     else if has_nephews_or_nieces base p then
       do head ();
          Wserver.wprint "<a href=\"%s%s;m=C;v1=1;v2=2\">%s</a>"
            (commd conf) (acces conf base p)
            (capitale (transl conf "nephews and nieces"));
       return ()
     else ();
     match prev_sibling base p a with
     [ Some p ->
         do head ();
            stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
              Wserver.wprint "%s"
                (capitale
                   (transl_nth conf "previous sibling" (index_of_sex p.sex)));
            end;
         return ()
     | None -> () ];
     match next_sibling base p a with
     [ Some p ->
         do head ();
            stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
              Wserver.wprint "%s"
                (capitale
                   (transl_nth conf "next sibling" (index_of_sex p.sex)));
            end;
         return ()
     | None -> () ];
     if st.val != 0 then Wserver.wprint "</h4>" else ();
     Wserver.wprint "\n";
  return ()
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

value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value print conf base p =
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
  do header conf title;
     print_sosa_if_any conf base p;
     print_link_to_welcome conf True;
     if age_autorise conf base p then
       let image_txt = capitale (transl conf "image") in
       match sou base p.image with
       [ "" ->
           match auto_image_file conf base p with
           [ Some f ->
               let s = Unix.stat f in
               let b = Filename.basename f in
               let wid_hei =
                 match image_size f with
                 [ Some (wid, hei) ->
                     " width=" ^ string_of_int wid ^ " height=" ^
                     string_of_int hei
                 | None -> "" ]
               in
               do Wserver.wprint "<img src=\"%sm=IM;d=%d;v=/%s\"%s alt=\"%s\">"
                    (commd conf)
                    (int_of_float
                       (mod_float s.Unix.st_mtime (float_of_int max_int)))
                    (Util.code_varenv b)
                    wid_hei image_txt;
                  html_p conf;
               return ()
           | None -> () ]
       | s ->
           let http = "http://" in
           if String.length s > String.length http &&
              String.sub s 0 (String.length http) = http then
             do Wserver.wprint "<img src=\"%s\" alt=\"%s\">" s image_txt;
                html_p conf;
             return ()
           else if Filename.is_implicit s then
             let fname = Util.image_file_name conf.bname s in
             if Sys.file_exists fname then
               let wid_hei =
                 match image_size fname with
                 [ Some (wid, hei) ->
                     " width=" ^ string_of_int wid ^ " height=" ^
                     string_of_int hei
                 | None -> "" ]
               in
               do Wserver.wprint "<img src=\"%sm=IM;v=/%s\"%s alt=\"%s\">"
                    (commd conf) s wid_hei image_txt;
                  html_p conf;
               return ()
             else ()
           else () ]
     else ();
     match (p.public_name, p.nick_names) with
     [ (n, [_ :: nnl]) when sou base n <> "" ->
         let n = sou base n in
         List.iter
           (fun nn ->
              do Wserver.wprint "%s <em>%s</em>" n (sou base nn);
                 html_br conf;
              return ())
           nnl
     | (_, [_ :: nnl]) ->
         let n = p_first_name base p in
         List.iter
           (fun nn ->
              do Wserver.wprint "%s <em>%s</em>" n (sou base nn);
                 html_br conf;
              return ())
           nnl
     | _ -> () ];
     List.iter
       (fun a ->
          do Wserver.wprint "%s <em><strong>%s</strong></em>"
               (capitale (transl conf "alias")) (sou base a);
             html_br conf;
          return ())
       p.aliases;
     if p.titles <> [] && age_autorise conf base p then
       do Wserver.wprint "<em>";
          print_titles conf base True (transl conf "and") p;
          Wserver.wprint ".</em>\n";
          html_br conf;
       return ()
     else ();
     match (sou base p.public_name, p.nick_names) with
     [ ("", []) -> ()
     | _ ->
         do Wserver.wprint "<em>(";
            print_linked_first_name_and_surname conf base p;
            Wserver.wprint ")</em>\n";
            html_br conf;
         return () ];
     List.iter
       (fun n ->
          do Wserver.wprint "<em>(%s %s)</em>\n"
               (p_first_name base p) (sou base n);
             html_br conf;
          return ())
       p.surnames_aliases;
     if age_autorise conf base p then
       List.iter
         (fun n ->
            do Wserver.wprint "<em>(%s...)</em>\n" (sou base n);
               html_br conf;
            return ())
         p.first_names_aliases
     else ();
     match
       (sou base p.public_name, p.nick_names, p.aliases,
        List.length p.titles <> 0)
     with
     [ ("", [], _, _) | (_, _, [_ :: _], _) | (_, _, _, True) -> html_p conf
     | _ -> () ];
     match sou base p.occupation with
     [ "" -> ()
     | s ->
         if age_autorise conf base p then
           do stag "em" begin
                Wserver.wprint "%s" (capitale s);
              end;
              html_br conf;
           return ()
         else () ];
     if age_autorise conf base p then print_dates conf base True p else ();
     if age_autorise conf base p && a.consang != Adef.fix (-1) &&
        a.consang != Adef.fix 0 then
       do Wserver.wprint "<em>%s: " (capitale (transl conf "consanguinity"));
          print_decimal_num conf
            (round_2_dec (Adef.float_of_fix a.consang *. 100.0));
          Wserver.wprint "%%</em>";
          html_br conf;
       return ()
     else ();
     print_parents conf base a;
     print_families conf base p a;
     print_notes conf base p;
     print_relations conf base p;
     if conf.cancel_links then ()
     else
       do Wserver.wprint "\n<h4><a href=\"%s%s;m=R\">\n%s</a></h4>\n"
            (commd conf) (acces conf base p)
            (capitale (transl conf "relationship computing"));
          print_ancestors_descends_cousins conf base p a;
          if conf.wizard then
            Wserver.wprint "\n<h4><a href=\"%s%s;m=U\">\n%s</a></h4>\n"
              (commd conf) (acces conf base p)
              (capitale (transl conf "update"))
          else ();
       return ();
     if age_autorise conf base p then print_sources conf base True p else ();
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
