(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: relation.ml,v 2.29 1999-08-02 16:08:27 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

value print_with_relation text conf base p r is =
  fun
  [ Some ic ->
      let c = poi base ic in
      do html_li conf;
         Wserver.wprint "<input type=radio name=select value=%d>\n"
           (Adef.int_of_iper ic);
         Wserver.wprint "%s:\n" (text conf r.r_type is);
         Wserver.wprint "<a href=\"%sem=R;ei=%d;i=%d\">\n" (commd conf)
           (Adef.int_of_iper p.cle_index)
           (Adef.int_of_iper ic);
         afficher_personne_sans_titre conf base c;
         Wserver.wprint "</a>";
         afficher_titre conf base c;
         Wserver.wprint "\n";
      return ()
  | None -> () ]
;

value print_with_child_relation conf base p ip =
  let c = poi base ip in
  List.iter
    (fun r ->
       do match r.r_fath with
          [ Some ip1 when p.cle_index == ip1 ->
              print_with_relation rchild_type_text conf base p r
                (index_of_sex c.sex) (Some ip)
          | _ -> () ];
          match r.r_moth with
          [ Some ip1 when p.cle_index == ip1 ->
              print_with_relation rchild_type_text conf base p r
                (index_of_sex c.sex) (Some ip)
          | _ -> () ];
       return ())
    c.rparents
;

value print_menu conf base p =
  let title h =
    do Wserver.wprint "%s " (capitale (transl conf "link between"));
       if h then
         match sou base p.public_name with
         [ "" ->
             Wserver.wprint "%s %s" (p_first_name base p)
               (p_surname base p)
         | n -> Wserver.wprint "%s" n ]
       else Wserver.wprint "%s" (person_text conf base p);
       Wserver.wprint " %s..." (transl conf "and");
    return ()
  in
  let is = index_of_sex p.sex in
  do header conf title;
     tag "form" "method=get action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=em value=R>\n";
       wprint_hidden_person conf base "e" p;
       tag "ul" begin
         html_li conf;
         Wserver.wprint "<input type=hidden name=m value=NG>\n";
         Wserver.wprint "<input type=radio name=select value=input checked>\n";
         Wserver.wprint "<input name=n size=40 maxlength=200>\n";
         html_p conf;
         tag "ul" begin
           html_li conf;
           Wserver.wprint "<input type=radio name=t value=PN checked>\n";
           Wserver.wprint
             "<em>%s %s</em> %s <em>%s</em> %s <em>%s</em> %s <em>%s</em>\n"
             (transl_nth conf "first name/first names" 0)
             (transl_nth conf "surname/surnames" 0)
             (transl conf "or") (transl conf "public name")
             (transl conf "or") (nominative (transl conf "alias"))
             (transl conf "or") (transl_nth conf "surname/surnames" 0);
           html_li conf;
           Wserver.wprint "<input type=radio name=t value=P> <em>%s</em>\n"
             (transl_nth conf "first name/first names" 0);
           html_li conf;
           Wserver.wprint "<input type=radio name=t value=N> <em>%s</em>\n"
             (transl_nth conf "surname/surnames" 0);
         end;
         let auth = age_autorise conf base p in
         Array.iter
           (fun ifam ->
              let fam = foi base ifam in
              let cpl = coi base ifam in
              let c = spouse p cpl in
              let c = poi base c in
              if p_first_name base c <> "?" || p_surname base c <> "?" then
                do html_li conf;
                   Wserver.wprint "<input type=radio name=select value=%d>\n"
                     (Adef.int_of_iper c.cle_index);
                   if fam.not_married && auth && age_autorise conf base c then
                     ()
                   else
                     Wserver.wprint "%s\n"
                       (transl_nth conf "his wife/her husband" is);
                   Wserver.wprint "<a href=\"%sem=R;ei=%d;i=%d\">\n"
                     (commd conf)
                     (Adef.int_of_iper p.cle_index)
                     (Adef.int_of_iper c.cle_index);
                   afficher_personne_sans_titre conf base c;
                   Wserver.wprint "</a>";
                   afficher_titre conf base c;
                   Wserver.wprint "\n";
                return ()
              else ())
           p.family;
         List.iter
           (fun r ->
              do print_with_relation relation_type_text conf base p r 0
                   r.r_fath;
                 print_with_relation relation_type_text conf base p r 1
                   r.r_moth;
              return ())
           p.rparents;
         List.iter (print_with_child_relation conf base p) p.rchildren;
       end;
       html_p conf;
       Wserver.wprint "%s\n" (capitale (transl conf "long display"));
       Wserver.wprint "<input type=checkbox name=long value=on>\n";
       html_br conf;
       Wserver.wprint "%s\n"
         (capitale (transl conf "relationships by marriage"));
       Wserver.wprint "<input type=checkbox name=marr value=on>\n";
       html_br conf;
       Wserver.wprint "%s\n" (capitale (transl conf "include spouses"));
       Wserver.wprint "<input type=checkbox name=spouse value=on>\n";
       html_br conf;
       Wserver.wprint "%s\n" (capitale (transl conf "cancel GeneWeb links"));
       Wserver.wprint "<input type=checkbox name=cgl value=on>\n";
       html_p conf;
       Wserver.wprint "<input type=submit value=\"Ok\">\n";
     end;
     trailer conf;
  return ()
;

value parents_label conf =
  fun
  [ 1 -> transl conf "the parents"
  | 2 -> transl conf "grand-parents"
  | 3 -> transl conf "great-grand-parents"
  | n ->
      transl conf "ancestors (some)" ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
;

value parent_in_law_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "the father-in-law/the mother-in-law" is
;

value ancestor_label conf x sex =
  let is = index_of_sex sex in
  match x with
  [ 1 -> transl_nth conf "the father/the mother/a parent" is
  | 2 -> transl_nth conf "a grandfather/a grandmother/a grandparent" is
  | 3 ->
      transl_nth conf
        "a great-grandfather/a great-grandmother/a great-grandparent" is
  | n ->
      transl_nth conf "an ancestor" is ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
;

value child_in_law_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "a son-in-law/a daughter-in-law" is
;

value descendant_label conf x p =
  let is = index_of_sex p.sex in
  match x with
  [ 1 -> transl_nth conf "a son/a daughter/a child" is
  | 2 -> transl_nth conf "a grandson/a granddaughter/a grandchild" is
  | 3 ->
      transl_nth conf
        "a great-grandson/a great-granddaughter/a great-grandchild" is
  | n ->
      transl_nth conf "a descendant" is ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
;

value brother_label conf x sex =
  let is = index_of_sex sex in
  match x with
  [ 1 -> transl_nth conf "a brother/a sister/a sibling" is
  | 2 -> transl_nth conf "a cousin" is
  | 3 -> transl_nth conf "a 2nd cousin" is
  | 4 -> transl_nth conf "a 3rd cousin" is
  | n ->
      Printf.sprintf (ftransl_nth conf "a %s cousin" is)
        (transl_nth conf (transl_nth conf "*nth (cousin)*" is) (n - 1)) ]
;

value half_brother_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "a half-brother/a half-sister/a half-sibling" is
;

value brother_in_law_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "a brother-in-law/a sister-in-law" is
;

value nb_fields s =
  loop 1 0 where rec loop cnt i =
    if i == String.length s then cnt
    else if s.[i] == '/' then loop (cnt + 1) (i + 1)
    else loop cnt (i + 1)
;

value uncle_label conf x p side =
  let is = index_of_sex p.sex in
  match x with
  [ 1 ->
      let txt = transl conf "an uncle/an aunt" in
      let is = if nb_fields txt == 4 && side == Female then is + 2 else is in
      nth_field txt is
  | 2 ->
      let txt = transl conf "a great-uncle/a great-aunt" in
      let is = if nb_fields txt == 4 && side == Female then is + 2 else is in
      nth_field txt is
  | n ->
      transl_nth conf "an uncle/an aunt" is ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
;

value uncle_in_law_label conf sex side =
  let is = index_of_sex sex in
  let txt = transl conf "an uncle (aunt's husband)/an aunt (uncle's wife)" in
  let is = if nb_fields txt == 4 && side == Female then is + 2 else is in
  nth_field txt is
;

value nephew_label conf x p =
  let is = index_of_sex p.sex in
  match x with
  [ 1 -> transl_nth conf "a nephew/a niece" is
  | 2 -> transl_nth conf "a great-nephew/a great-niece" is
  | n ->
      transl_nth conf "a nephew/a niece" is ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
;

value nephew_in_law_label conf sex =
  let is = index_of_sex sex in
  transl_nth conf "a nephew (spouse's nephew)/a niece (spouse's niece)" is
;

value same_parents base p1 p2 =
  (aoi base p1.cle_index).parents = (aoi base p2.cle_index).parents
;

value ancestors_n base =
  loop [] where rec loop list n ip =
    if n == 0 then [(aoi base ip).parents :: list]
    else
      match (aoi base ip).parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let list = loop list (n - 1) cpl.father in
          let list = loop list (n - 1) cpl.mother in
          list
      | None -> list ]
;

value uncle_relation_side base p1 p2 x2 =
  let a_fam = (aoi base p1.cle_index).parents in
  match (aoi base p2.cle_index).parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath_side = ancestors_n base (x2 - 2) cpl.father in
      let moth_side = ancestors_n base (x2 - 2) cpl.mother in
      if List.mem a_fam fath_side then Male
      else if List.mem a_fam moth_side then Female
      else Neuter
  | None -> Neuter ]
;

value print_link conf base n p1 p2 pp1 pp2 x1 x2 =
  let (p1, pp1, x1, p2, pp2, x2) =
    if p1.sex <> Neuter then (p1, pp1, x1, p2, pp2, x2)
    else (p2, pp2, x2, p1, pp1, x1)
  in
  do afficher_personne_sans_titre conf base p1;
     afficher_titre conf base p1;
     Wserver.wprint " %s" (transl conf "is");
     if n > 1 then Wserver.wprint " %s" (transl conf "also") else ();
     Wserver.wprint "\n";
     let (s, sp1, sp2) =
       let ini_p1 = p1 and ini_p2 = p2 in
       let p1 = match pp1 with [ Some p1 -> p1 | _ -> p1 ] in
       let p2 = match pp2 with [ Some p2 -> p2 | _ -> p2 ] in
       let sp1 = pp1 <> None in
       let sp2 = pp2 <> None in
       if x1 == 0 then
         if sp2 && x2 == 1 then
           (parent_in_law_label conf ini_p1.sex, sp1, False)
         else (ancestor_label conf x2 p1.sex, sp1, sp2)
       else if x2 == 0 then
         if sp1 && x1 == 1 then
           (child_in_law_label conf ini_p1.sex, False, sp2)
         else (descendant_label conf x1 p1, sp1, sp2)
       else if x1 == x2 then
         if x1 == 1 && not (same_parents base p1 p2) then
           (half_brother_label conf p1.sex, sp1, sp2)
         else if x1 == 1 && (sp1 || sp2) && p1.sex <> Neuter then
           (brother_in_law_label conf ini_p1.sex, False, False)
         else (nominative (brother_label conf x2 p1.sex), sp1, sp2)
       else if x1 == 1 then
         let side =
           if x2 <= 3 then uncle_relation_side base p1 p2 x2 else Neuter
         in
         if x2 == 2 && sp1 then
           (uncle_in_law_label conf ini_p1.sex side, False, sp2)
         else (uncle_label conf (x2 - x1) p1 side, sp1, sp2)
       else if x2 == 1 then
         if x1 == 2 && x2 == 1 && sp2 then
           (nephew_in_law_label conf p1.sex, sp1, False)
         else (nephew_label conf (x1 - x2) p1, sp1, sp2)
       else if x1 < x2 then
         (nominative (brother_label conf x1 p1.sex) ^ " " ^
          transl_decline conf "of (same or greater generation level)"
            (ancestor_label conf (x2 - x1) Neuter),
          sp1, sp2)
       else
         (descendant_label conf (x1 - x2) p1 ^ " " ^
          transl_decline conf "of (same or greater generation level)"
            (brother_label conf x2 Male),
          sp1, sp2)
     in
     let s =
       if sp1 then
         transl_nth conf "the spouse" (index_of_sex p1.sex) ^ " " ^
         transl_decline conf "of (same or greater generation level)" s
       else s
     in
     let s =
       if sp2 then
         s ^ " " ^
         transl_decline conf "of (same or greater generation level)"
           (transl_nth conf "the spouse" (1 - index_of_sex p2.sex))
       else s
     in
     stag "strong" begin
       Wserver.wprint "%s" s;
     end;
     Wserver.wprint "\n";
     let s = gen_person_text_without_title raw_access conf base p2 in
     let s =
       if x1 < x2 then transl_decline conf "of" s
       else transl_decline conf "of (same or greater generation level)" s
     in
     Wserver.wprint "%s" s;
     afficher_titre conf base p2;
     Wserver.wprint ".\n";
  return ()
;

value wprint_num conf n =
  Num.print (fun x -> Wserver.wprint "%s" x)
    (transl conf "(thousand separator)") n
;

value string_of_big_int conf i =
  let sep = transl conf "(thousand separator)" in
  glop i where rec glop i =
    if i == 0 then ""
    else
      let s = glop (i / 1000) in
      if s = "" then string_of_int (i mod 1000)
      else s ^ sep ^ Printf.sprintf "%03d" (i mod 1000)
;

value print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (a, n) ->
          do html_li conf;
             Wserver.wprint "<em>%s %s" (string_of_big_int conf n)
               (transl_nth conf "branch/branches" (if n = 1 then 0 else 1));
             if not long then
               do Wserver.wprint ":\n%s " (transl conf "click");
                  let dp1 = match pp1 with [ Some p -> p | _ -> p1 ] in
                  let dp2 = match pp2 with [ Some p -> p | _ -> p2 ] in
                  Wserver.wprint
                    "<a href=\"%sm=RL;%s;l1=%d;%s;l2=%d;%s%s%s\">"
                    (commd conf) (acces conf base a) x1
                    (acces_n conf base "1" dp1) x2
                    (acces_n conf base "2" dp2)
                    (if pp1 = None then "" else ";" ^ acces_n conf base "3" p1)
                    (if pp2 = None then ""
                     else ";" ^ acces_n conf base "4" p2);
                  Wserver.wprint "%s</a>" (transl conf "here");
                  if n > 1 then
                    Wserver.wprint "%s"
                      (transl conf " to see the first branch")
                  else ();
               return ()
             else ();
             Wserver.wprint ".</em>\n";
          return ())
       list;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_solution_not_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list =
  do Wserver.wprint "<ul>";
     html_li conf;
     Wserver.wprint "%s\n" (capitale (transl conf "indeed,"));
     tag "ul" begin
       List.iter
         (fun (a, n) ->
            do html_li conf;
               afficher_personne_sans_titre conf base a;
               afficher_titre conf base a;
               Wserver.wprint "\n<em>(";
               Wserver.wprint "%d %s" n
                 (transl_nth conf "relationship link/relationship links"
                    (if n = 1 then 0 else 1));
               if not long then
                 do Wserver.wprint ":\n%s" (transl conf "click");
                    let dp1 = match pp1 with [ Some p -> p | _ -> p1 ] in
                    let dp2 = match pp2 with [ Some p -> p | _ -> p2 ] in
                    Wserver.wprint
                      " <a href=\"%sm=RL;%s;l1=%d;%s;l2=%d;%s%s%s\">"
                      (commd conf) (acces conf base a) x1
                      (acces_n conf base "1" dp1) x2
                      (acces_n conf base "2" dp2)
                      (if pp1 = None then ""
                       else ";" ^ acces_n conf base "3" p1)
                      (if pp2 = None then ""
                       else ";" ^ acces_n conf base "4" p2);
                    Wserver.wprint "%s</a>" (transl conf "here");
                    if n > 1 then
                      Wserver.wprint "%s"
                        (transl conf " to see the first relationship link")
                    else ();
                 return ()
               else ();
               Wserver.wprint "</em>).\n";
            return ())
         list;
     end;
     let is_are =
       match list with
       [ [_] -> transl conf "is"
       | _ -> transl conf "are" ]
     in
     Wserver.wprint "%s %s\n" is_are (transl conf "at the same time");
  return
  let lab x =
    match list with
    [ [(a, _)] -> ancestor_label conf x a.sex
    | _ -> parents_label conf x ]
  in
  do tag "ul" begin
       html_li conf;
       Wserver.wprint "%s " (lab x1);
       let s =
         transl_decline conf "of"
           (gen_person_text_without_title raw_access conf base p1)
       in
       let s =
         if pp1 = None then s
         else
           transl_decline conf "of (same or greater generation level)"
             (transl_nth conf "the spouse" (1 - index_of_sex p1.sex)) ^ " " ^ s
       in
       Wserver.wprint "%s" s;
       afficher_titre conf base p1;
       Wserver.wprint "\n";
       html_li conf;
       Wserver.wprint "%s " (lab x2);
       let s =
         transl_decline conf "of"
           (gen_person_text_without_title raw_access conf base p2)
       in
       let s =
         if pp2 = None then s
         else
           transl_decline conf "of (same or greater generation level)"
             (transl_nth conf "the spouse" (1 - index_of_sex p2.sex)) ^ " " ^ s
       in
       Wserver.wprint "%s" s;
       afficher_titre conf base p2;
       Wserver.wprint "\n";
     end;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_solution conf base long n p1 p2 (pp1, pp2, (x1, x2, list)) =
  do print_link conf base n p2 p1 pp2 pp1 x2 x1;
     if x1 == 0 || x2 == 0 then
       print_solution_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list
     else print_solution_not_ancestor conf base long p1 p2 pp1 pp2 x1 x2 list;
     Wserver.wprint "\n";
  return ()
;

value print_propose_upto conf base p1 p2 rl =
  match rl with
  [ [(None, None, (x1, x2, _)) :: _] when x1 == 0 || x2 == 0 ->
      let maxlen =
        List.fold_right
          (fun (_, _, (x1, x2, _)) maxlen -> max maxlen (max x1 x2))
          rl 0
      in
      let (p, a) = if x1 == 0 then (p2, p1) else (p1, p2) in
      do html_p conf;
         Wserver.wprint "<font size=-1><em>%s %s</em>\n"
           (capitale (transl conf "ancestors"))
           (transl_decline conf "of" "");
         afficher_personne_titre conf base p;
         Wserver.wprint " <em>%s</em>\n" (transl conf "up to");
         afficher_personne_titre conf base a;
         Wserver.wprint ":\n<em>%s\n" (transl conf "click");
         Wserver.wprint "<a href=\"%sm=A;t=D;%s;%s;l=%d\">"
           (commd conf) (acces conf base p)
           (acces_n conf base "1" a) maxlen;
         Wserver.wprint "%s</a>." (transl conf "here");
         Wserver.wprint "</em></font>\n";
      return ()
  | _ -> () ]
;

value compute_simple_relationship conf base tstab p1 p2 =
  let tab = Consang.make_relationship_table base tstab in
  let (relationship, ancestors) =
    Consang.relationship_and_links base tab True p1.cle_index p2.cle_index
  in
  if ancestors = [] then None
  else
    let total =
      List.fold_left
        (fun n i ->
           let u = tab.Consang.info.(i) in
           List.fold_left
             (fun n (_, n1) ->
                List.fold_left
                  (fun n (_, n2) -> Num.add n (Num.of_int (n1 * n2)))
                  n u.Consang.lens1)
             n u.Consang.lens2)
        Num.zero ancestors
    in
    let rl =
      List.fold_left
        (fun rl i ->
           let u = tab.Consang.info.(i) in
           let p = base.data.persons.get i in
           List.fold_left
             (fun rl (len1, n1) ->
                List.fold_left
                  (fun rl (len2, n2) ->
                     [(len1, len2, (p, n1 * n2)) :: rl])
                  rl u.Consang.lens2)
             rl u.Consang.lens1)
        [] ancestors
    in
    let rl =
      Sort.list
        (fun (len11, len12, _) (len21, len22, _) ->
           if len11 + len12 > len21 + len22 then True
           else if len11 + len12 < len21 + len22 then False
           else len11 > len21)
        rl
    in
    let rl =
      List.fold_left
        (fun l (len1, len2, sol) ->
           match l with
           [ [(l1, l2, sols) :: l] when len1 == l1 && len2 == l2 ->
               [(l1, l2, [sol :: sols]) :: l]
           | _ -> [(len1, len2, [sol]) :: l] ])
        [] rl                 
    in
    Some (rl, total, relationship)
;

value known_spouses_list base p excl_p =
  List.fold_left
    (fun spl ifam ->
       let sp = poi base (spouse p (coi base ifam)) in
       if sou base sp.first_name <> "?" && sou base sp.surname <> "?"
       && sp.cle_index <> excl_p.cle_index then
         [sp :: spl]
       else spl)
    [] (Array.to_list p.family)
;

value merge_relations rl1 rl2 =
  Sort.merge
    (fun (_, _, (l11, l12, _)) (_, _, (l21, l22, _)) ->
       if l11 + l12 < l21 + l22 then True
       else if l11 + l12 > l21 + l22 then False
       else l11 < l21)
    rl1 rl2
;

value combine_relationship conf base tstab pl1 pl2 f_sp1 f_sp2 =
  List.fold_right
    (fun p1 sl ->
       List.fold_right
         (fun p2 sl ->
            let sol = compute_simple_relationship conf base tstab p1 p2 in
            match sol with
            [ Some (rl, total, _) ->
                let s = List.map (fun r -> (f_sp1 p1, f_sp2 p2, r)) rl in
                [(s, total) :: sl]
            | None -> sl ])
       pl2 sl)
    pl1
;

value sp p = Some p;
value no_sp p = None;

value compute_relationship conf base by_marr p1 p2 =
  if p1.cle_index == p2.cle_index then None
  else
    let _ = base.data.ascends.array () in
    let _ = base.data.couples.array () in
    let tstab = Util.create_topological_sort conf base in
    let sol = compute_simple_relationship conf base tstab p1 p2 in
    let sol_by_marr =
      if by_marr then
        let spl1 = known_spouses_list base p1 p2 in
        let spl2 = known_spouses_list base p2 p1 in
        let sl = [] in
        let sl =
          match sol with
          [ Some ([(_, 0, _) :: _], _, _) -> sl
          | _ -> combine_relationship conf base tstab [p1] spl2 no_sp sp sl ]
        in
        let sl =
          match sol with
          [ Some ([(0, _, _) :: _], _, _) -> sl
          | _ -> combine_relationship conf base tstab spl1 [p2] sp no_sp sl ]
        in
        match (sol, sl) with
        [ (Some ([(x1, x2, _) :: _], _, _), _) when x1 == 0 || x2 == 0 -> sl
        | (_, [([(_, _, (x1, x2, _)) :: _], _) :: _])
          when x1 == 0 || x2 == 0 ->
            sl
        | _ -> combine_relationship conf base tstab spl1 spl2 sp sp sl ]
      else []
    in
    let (all_sol, rel) =
      match sol with
      [ Some (rl, total, rel) ->
          let s = List.map (fun r -> (None, None, r)) rl in
          ([(s, total) :: sol_by_marr], rel)
      | None -> (sol_by_marr, 0.0) ]
    in
    let (sl, total) =
      List.fold_right
        (fun (rl1, total1) (rl, total) ->
           (merge_relations rl1 rl, Num.add total1 total))
        all_sol ([], Num.zero)
     in
     if sl = [] then None else Some (sl, total, rel)
;

open RelationLink;

value print_one_path conf base found a p1 p2 pp1 pp2 l1 l2 =
  let ip = a.cle_index in
  let sp1 = match pp1 with [ Some _ -> Some p1 | _ -> None ] in
  let sp2 = match pp2 with [ Some _ -> Some p2 | _ -> None ] in
  let p1 = match pp1 with [ Some p1 -> p1 | _ -> p1 ] in
  let p2 = match pp2 with [ Some p2 -> p2 | _ -> p2 ] in
  let ip1 = p1.cle_index in
  let ip2 = p2.cle_index in
  let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
  let b1 = find_first_branch base dist ip l1 ip1 Neuter in
  let b2 = find_first_branch base dist ip l2 ip2 Neuter in
  match (b1, b2) with
  [ (Some b1, Some b2) ->
      let info =
        {ip = ip; sp = a.sex; ip1 = ip1; ip2 = ip2; b1 = b1; b2 = b2;
         c1 = 1; c2 = 1; pb1 = None; pb2 = None; nb1 = None; nb2 = None;
         sp1 = sp1; sp2 = sp2}
      in
      if List.mem (b1, b2) found.val then ()
      else
        do tag "center" begin
             tag "table" "border=1" begin
               tag "tr" begin
                 tag "td" begin
                   RelationLink.print_relation_path conf base info;
                 end;
               end;
             end;
           end;
           html_p conf;
           found.val := [(b1, b2) :: found.val];
        return ()
  | _ -> () ]
;

value print_path conf base i p1 p2 (pp1, pp2, (l1, l2, list)) =
  let found = ref [] in
  do List.iter
       (fun (a, n) -> print_one_path conf base found a p1 p2 pp1 pp2 l1 l2)
       list;
     Wserver.wprint "\n";
  return ()
;

value print_main_relationship conf base long p1 p2 rel =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "relationship")) in
  do header conf title;
     match p_getenv conf.env "spouse" with
     [ Some "on" -> conf.senv := conf.senv @ [("spouse", "on")]
     | _ -> () ];
     match p_getenv conf.env "cgl" with
     [ Some "on" -> conf.senv := conf.senv @ [("cgl", "on")]
     | _ -> () ];
     match rel with
     [ None ->
         if p1.cle_index == p2.cle_index then
           Wserver.wprint "%s\n"
             (capitale (transl conf "it is the same person!"))
         else
           Wserver.wprint "%s\n"
             (capitale
                (cftransl conf "no known relationship link between %s and %s"
                   [gen_referenced_person_title_text raw_access conf base p1;
                    gen_referenced_person_title_text raw_access conf base p2]))
     | Some (rl, total, relationship) ->
         let a1 = aoi base p1.cle_index in
         let a2 = aoi base p2.cle_index in
         do let _ =
              List.fold_left
                (fun i sol ->
                   do print_solution conf base long i p1 p2 sol;
                      if long then print_path conf base i p1 p2 sol else ();
                   return succ i)
                1 rl
            in
            ();
            Wserver.wprint "\n";
            html_p conf;
            Wserver.wprint "%s: <em>" (capitale (transl conf "total"));
            wprint_num conf total;
            Wserver.wprint "</em> %s\n"
              (transl_nth conf "relationship link/relationship links"
                 (if Num.eq total Num.one then 0 else 1));
            if
              age_autorise conf base p1 && age_autorise conf base p2 &&
              a1.consang != Adef.fix (-1) && a2.consang != Adef.fix (-1)
            then
              do html_p conf;
                 Wserver.wprint "<em>%s: "
                   (capitale (transl conf "relationship"));
                 print_decimal_num conf
                   (round_2_dec
                      (Adef.float_of_fix
                         (Adef.fix_of_float relationship) *. 100.0));
                 Wserver.wprint "%%</em>";
                 html_p conf;
              return ()
            else ();
            print_propose_upto conf base p1 p2 rl;
         return () ];
     trailer conf;
  return ()
;

value print_base_loop conf base =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "error")) in
  do header conf title;
     Wserver.wprint "%s\n" (capitale (transl conf "probable loop in base"));
     trailer conf;
  return ()
;

value print conf base p =
  fun
  [ Some p1 ->
      let long =
        match p_getenv conf.env "long" with
        [ Some "on" -> True
        | _ -> False ]
      in
      let by_marr =
        match p_getenv conf.env "marr" with
        [ Some "on" -> True
        | _ -> False ]
      in
      match
        try Some (compute_relationship conf base by_marr p1 p) with
        [ Consang.TopologicalSortError -> None ]
      with
      [ Some rel -> print_main_relationship conf base long p1 p rel
      | None -> print_base_loop conf base ]
  | None -> print_menu conf base p ]
;
