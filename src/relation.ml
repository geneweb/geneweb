(* camlp4r ./pa_lock.cmo ./pa_html.cmo *)
(* $Id: relation.ml,v 2.21 1999-07-22 22:14:01 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value round_2_dec x = floor (x *. 100.0 +. 0.5) /. 100.0;

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
       Wserver.wprint "<input type=hidden name=ei value=%d>\n"
         (Adef.int_of_iper p.cle_index);
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
                   if fam.not_married && auth && age_autorise conf base c then ()
                   else
                     Wserver.wprint "%s\n"
                       (transl_nth conf "his wife/her husband" is);
                   Wserver.wprint "<a href=\"%sem=R;ei=%d;i=%d\">\n" (commd conf)
                     (Adef.int_of_iper p.cle_index)
                     (Adef.int_of_iper c.cle_index);
                   afficher_personne_sans_titre conf base c;
                   Wserver.wprint "</a>";
                   afficher_titre conf base c;
                   Wserver.wprint "\n";
                return ()
              else ())
           p.family;
       end;
       html_p conf;
       Wserver.wprint "%s\n" (capitale (transl conf "long display"));
       Wserver.wprint "<input type=checkbox name=long value=on>\n";
       html_br conf;
       Wserver.wprint "%s\n" (capitale (transl conf "cancel GeneWeb links"));
       Wserver.wprint "<input type=checkbox name=cgl value=on>\n";
       html_br conf;
       Wserver.wprint "%s:\n" (capitale (transl_nth conf "branch/branches" 1));
       Wserver.wprint "%s\n" (transl conf "include spouses");
       Wserver.wprint "<input type=checkbox name=opt value=spouse>\n";
       html_br conf;
       Wserver.wprint "%s:\n" (capitale (transl_nth conf "branch/branches" 1));
       Wserver.wprint "%s\n" (transl conf "cancel GeneWeb links");
       Wserver.wprint "<input type=checkbox name=cglb value=on>\n";
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

value uncle_label conf x p =
  let is = index_of_sex p.sex in
  match x with
  [ 1 -> transl_nth conf "an uncle/an aunt" is
  | 2 -> transl_nth conf "a great-uncle/a great-aunt" is
  | n ->
      transl_nth conf "an uncle/an aunt" is ^ " " ^
        Printf.sprintf (ftransl conf "of the %s generation")
          (transl_nth conf "nth (generation)" n) ]
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

value same_parents base p1 p2 =
  (aoi base p1.cle_index).parents = (aoi base p2.cle_index).parents
;

value print_link conf base n p1 p2 x1 x2 =
  let (p1, x1, p2, x2) =
    if p1.sex <> Neuter then (p1, x1, p2, x2) else (p2, x2, p1, x1)
  in
  do afficher_personne_sans_titre conf base p1;
     afficher_titre conf base p1;
     Wserver.wprint " %s" (transl conf "is");
     if n > 1 then Wserver.wprint " %s" (transl conf "also") else ();
     Wserver.wprint "\n";
     stag "strong" begin
       if x1 == 0 then Wserver.wprint "%s" (ancestor_label conf x2 p1.sex)
       else if x2 == 0 then Wserver.wprint "%s" (descendant_label conf x1 p1)
       else if x1 == x2 then
         if x1 == 1 && not (same_parents base p1 p2) then
           Wserver.wprint "%s" (half_brother_label conf p1.sex)
         else
           Wserver.wprint "%s" (nominative (brother_label conf x2 p1.sex))
       else if x1 == 1 || x2 == 1 then
         if x1 == 1 then Wserver.wprint "%s" (uncle_label conf (x2 - x1) p1)
         else Wserver.wprint "%s" (nephew_label conf (x1 - x2) p1)
       else if x1 < x2 then
         do Wserver.wprint "%s" (nominative (brother_label conf x1 p1.sex));
            Wserver.wprint " %s"
              (transl_decline conf
               "of (same or greater generation level)"
               (ancestor_label conf (x2 - x1) Neuter));
         return ()
       else
         do Wserver.wprint "%s" (descendant_label conf (x1 - x2) p1);
            Wserver.wprint " %s"
              (transl_decline conf "of (same or greater generation level)"
               (brother_label conf x2 Male));
         return ();
     end;
     Wserver.wprint "\n";
     let s = gen_person_text_without_title raw_access conf base p2 in
     Wserver.wprint "%s"
       (if x1 < x2 then transl_decline conf "of" s
        else transl_decline conf "of (same or greater generation level)" s);
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

value print_solution_ancestor conf long p1 p2 x1 x2 list =
  do Wserver.wprint "<ul>\n";
     List.iter
       (fun (a, n) ->
          do html_li conf;
             Wserver.wprint "<em>%s %s" (string_of_big_int conf n)
               (transl_nth conf "branch/branches" (if n = 1 then 0 else 1));
             if conf.cancel_links || long then ()
             else
               do Wserver.wprint ":\n%s " (transl conf "click");
                  Wserver.wprint
                    "<a href=\"%sm=RL;i=%d;l1=%d;i1=%d;l2=%d;i2=%d\">%s</a>"
                    (commd conf) (Adef.int_of_iper a.cle_index) x1
                    (Adef.int_of_iper p1.cle_index) x2
                    (Adef.int_of_iper p2.cle_index)
                    (transl conf "here");
                  if n > 1 then
                    Wserver.wprint "%s"
                      (transl conf " to see the first branch")
                  else ();
               return ();
             Wserver.wprint ".</em>\n";
          return ())
       list;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_solution_not_ancestor conf base long p1 p2 x1 x2 list =
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
               if conf.cancel_links || long then ()
               else
                 do Wserver.wprint ":\n%s" (transl conf "click");
                    Wserver.wprint
                      " <a href=\"%sm=RL;i=%d;l1=%d;i1=%d;l2=%d;i2=%d\">"
                      (commd conf) (Adef.int_of_iper a.cle_index) x1
                      (Adef.int_of_iper p1.cle_index) x2
                      (Adef.int_of_iper p2.cle_index);
                    Wserver.wprint "%s</a>" (transl conf "here");
                    if n > 1 then
                      Wserver.wprint "%s"
                        (transl conf " to see the first relationship link")
                    else ();
                 return ();
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
       Wserver.wprint "%s %s\n" (lab x1)
         (transl_decline conf "of"
            (gen_person_text_without_title raw_access conf base p1));
       afficher_titre conf base p1;
       Wserver.wprint "\n";
       html_li conf;
       Wserver.wprint "%s %s\n" (lab x2)
         (transl_decline conf "of"
            (gen_person_text_without_title raw_access conf base p2));
       afficher_titre conf base p2;
       Wserver.wprint "\n";
     end;
     Wserver.wprint "</ul>\n";
  return ()
;

value print_solution conf base long n p1 p2 (x1, x2, list) =
  do print_link conf base n p2 p1 x2 x1; return
  if x1 == 0 || x2 == 0 then print_solution_ancestor conf long p1 p2 x1 x2 list
  else print_solution_not_ancestor conf base long p1 p2 x1 x2 list
;

value print_propose_upto conf base p1 p2 rl =
  match rl with
  [ [(x1, x2, _) :: _] when x1 == 0 || x2 == 0 ->
      let maxlen =
        List.fold_right (fun (x1, x2, _) maxlen -> max maxlen (max x1 x2))
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
         Wserver.wprint "<a href=\"%sm=A;t=D;i=%d;v=%d;l=%d\">"
           (commd conf) (Adef.int_of_iper p.cle_index)
           (Adef.int_of_iper a.cle_index) maxlen;
         Wserver.wprint "%s</a>." (transl conf "here");
         Wserver.wprint "</em></font>\n";
      return ()
  | _ -> () ]
;

value compute_relationship conf base p1 p2 =
  if p1.cle_index == p2.cle_index then None
  else
    let _ = base.data.ascends.array () in
    let _ = base.data.couples.array () in
    let tstab = Util.create_topological_sort conf base in
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

open RelationLink;

value print_one_path conf base found a p1 p2 l1 l2 =
  let ip = a.cle_index in
  let ip1 = p1.cle_index in
  let ip2 = p2.cle_index in
  let dist = make_dist_tab conf base ip (max l1 l2 + 1) in
  let b1 = find_first_branch base dist ip l1 ip1 Neuter in
  let b2 = find_first_branch base dist ip l2 ip2 Neuter in
  match (b1, b2) with
  [ (Some b1, Some b2) ->
      let info =
        {ip = ip; sp = a.sex; ip1 = ip1; ip2 = ip2; b1 = b1; b2 = b2;
         c1 = 1; c2 = 1; pb1 = None; pb2 = None; nb1 = None; nb2 = None}
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

value print_path conf base i p1 p2 (l1, l2, list) =
  let found = ref [] in
  List.iter (fun (a, n) -> print_one_path conf base found a p1 p2 l1 l2) list
;

value print_main_relationship conf base long p1 p2 rel =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "relationship")) in
  do header conf title;
     match p_getenv conf.env "opt" with
     [ Some "spouse" -> conf.senv := conf.senv @ [("opt", "spouse")]
     | _ -> () ];
     match p_getenv conf.env "cglb" with
     [ Some "on" -> conf.senv := conf.senv @ [("cgl", "on")]
     | _ -> () ];
     match rel with
     [ None ->
         if p1.cle_index == p2.cle_index then
           Wserver.wprint "%s\n"
             (capitale (transl conf "it is the same person!"))
         else
           do Wserver.wprint
                (fcapitale
                   (ftransl conf
                      "no known relationship link between %t and %t"))
                (fun _ ->
                   Wserver.wprint "%s"
                     (referenced_person_title_text conf base p1))
                (fun _ ->
                   Wserver.wprint "%s"
                     (referenced_person_title_text conf base p2));
              Wserver.wprint "\n";
           return ()
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
(*
            if long then ()
            else
              let href =
                List.fold_right
                  (fun (x, v) s ->
                     let sep = if s = "" then "" else ";" in
                     x ^ "=" ^ code_varenv v ^ sep ^ s)
                  (conf.env @ [("long", "on")]) ""
              in
              Wserver.wprint "<a href=\"%s%s\">%s</a>\n" (commd conf)
                href (capitale (transl conf "long display"));
*)
            if conf.cancel_links then ()
            else print_propose_upto conf base p1 p2 rl;
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
      match
        try Some (compute_relationship conf base p1 p) with
        [ Consang.TopologicalSortError -> None ]
      with
      [ Some rel -> print_main_relationship conf base long p1 p rel
      | None -> print_base_loop conf base ]
  | None -> print_menu conf base p ]
;
