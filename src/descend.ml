(* camlp4r ./pa_html.cmo *)
(* $Id: descend.ml,v 2.15 1999-07-19 09:56:17 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

value limit_desc = 12;

value infini = 10000;

value make_level_table base niveau_max p =
  let mark = Array.create (base.data.persons.len) False in
  let levt = Array.create (base.data.persons.len) infini in
  let rec fill p lev =
    if niveau_max == infini && mark.(Adef.int_of_iper p.cle_index) then ()
    else
      do mark.(Adef.int_of_iper p.cle_index) := True; return
      if lev <= niveau_max then
        do if lev < levt.(Adef.int_of_iper p.cle_index) then
             levt.(Adef.int_of_iper p.cle_index) := lev
           else ();
        return
        Array.iter
          (fun ifam ->
             let pl = (foi base ifam).children in
             Array.iter (fun p -> fill (poi base p) (succ lev)) pl)
          p.family
      else ()
  in
  do fill p 0; return levt
;

value level_max base p =
  let levt = make_level_table base infini p in
  let x = ref 0 in
  do for i = 0 to Array.length levt - 1 do
       let lev = levt.(i) in
       if lev != infini && x.val < lev then x.val := lev else ();
     done;
  return x.val
;

value text_to conf =
  fun
  [ 0 -> transl conf "specify" ^ " " ^ transl conf "generation"
  | 1 -> transl conf "to the children"
  | 2 -> transl conf "to the grandchildren"
  | 3 -> transl conf "to the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value text_level conf =
  fun
  [ 0 -> transl conf "specify" ^ " " ^ transl conf "generation"
  | 1 -> transl conf "the children"
  | 2 -> transl conf "the grandchildren"
  | 3 -> transl conf "the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value print_choice conf base p niveau_effectif =
  tag "form" "method=get action=\"%s\"" conf.command begin
    List.iter
      (fun (k, v) ->
         Wserver.wprint "\n<input type=hidden name=%s value=%s>" k v)
      conf.henv;
    Wserver.wprint "<input type=hidden name=m value=D>\n";
    if conf.wizard && conf.friend && sou base p.surname <> "?"
    && sou base p.first_name <> "?" then
      do Wserver.wprint "<input type=hidden name=n value=\"%s\">\n"
           (sou base p.surname);
         Wserver.wprint "<input type=hidden name=p value=\"%s\">\n"
           (sou base p.first_name);
         if p.occ > 0 then
           Wserver.wprint "<input type=hidden name=oc value=\"%d\">\n" p.occ
         else ();
      return ()
    else
      Wserver.wprint "<input type=hidden name=i value=%d>\n"
        (Adef.int_of_iper p.cle_index);
    tag "select" "name=v" begin
      let rec boucle i =
        if i > niveau_effectif then ()
        else
          do Wserver.wprint "  <option value=%d%s> %s\n" i
               (if i == 0 then " selected" else "")
               (capitale (text_to conf i));
          return boucle (succ i)
      in
      boucle 0;
    end;
    tag "ul" begin
      html_li conf;
      Wserver.wprint "<input type=radio name=t value=L checked> %s\n"
        (capitale (transl conf "list"));
      html_li conf;
      Wserver.wprint "<input type=radio name=t value=N> %s\n"
        (capitale (transl conf "families with encoding"));
      html_li conf;
      Wserver.wprint "<input type=radio name=t value=G> -> %s\n"
        (capitale (transl conf "index of the descendants"));
      html_li conf;
      Wserver.wprint "<input type=radio name=t value=C> -> %s\n"
        (capitale (transl conf "index of the spouses (non descendants)"));
      html_li conf;
      Wserver.wprint "<input type=radio name=t value=S> %s\n"
        (capitale (transl conf "only the generation selected"));
    end;
    html_p conf;
    tag "ul" begin
      html_li conf;
      Wserver.wprint "%s\n" (capitale (transl conf "cancel GeneWeb links"));
      Wserver.wprint "<input type=checkbox name=cgl value=on><br>\n";
    end;
    html_p conf;      
    Wserver.wprint "<input type=submit value=\"Ok\">";
    html_br conf;
  end
;

value afficher_menu_descendants conf base p =
  let niveau_effectif = min limit_desc (level_max base p) in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text conf base p))
  in
  do header conf title;
     print_choice conf base p niveau_effectif;
     trailer conf;
  return ()
;

value s_appelle_comme_son_pere base ip =
  let a = aoi base ip in
  match a.parents with
  [ Some ifam ->
      (poi base ip).surname = (poi base (coi base ifam).father).surname
  | _ -> False ]
;

value afficher_marie conf base first fam p spouse =
  let is = index_of_sex p.sex in
  let auth = age_autorise conf base p && age_autorise conf base spouse in
  do let format =
       if fam.not_married && auth then ftransl conf "relationship%t to"
       else ftransl_nth conf "married%t to" is
     in
     Wserver.wprint (fcapitale format)
       (fun _ ->
          if auth then Perso.print_marriage_text conf base True fam else ());
     stag "strong" begin
       afficher_personne_referencee conf base spouse;
     end;
     Date.afficher_dates conf base spouse;
     if age_autorise conf base p && age_autorise conf base spouse then
       match fam.divorce with
       [ NotDivorced -> ()
       | Divorced cod ->
           do Wserver.wprint ",\n";
              stag "em" begin
                Wserver.wprint "%s" (transl conf "divorced");
                match Adef.od_of_codate cod with
                [ Some d -> Wserver.wprint " %s" (Date.string_of_ondate conf d)
                | None -> () ];
              end;
           return () ]
     else ();
  return ()
;

value print_child conf base levt boucle niveau_max niveau compte ix =
  let x = poi base ix in
  do html_li conf;
     stag "strong" begin
       if s_appelle_comme_son_pere base ix then
         afficher_prenom_de_personne_referencee conf base x
       else afficher_personne_referencee conf base x;
     end;
     Date.afficher_dates conf base x;
     if levt.(Adef.int_of_iper x.cle_index) < niveau then
       Wserver.wprint "<em>, %s</em>"
         (transl conf "see further")
     else if
       levt.(Adef.int_of_iper x.cle_index) > niveau then
       Wserver.wprint "<em>, %s</em>"
         (transl conf "see above")
     else incr compte;
     Wserver.wprint ".";
  return
  if levt.(Adef.int_of_iper x.cle_index) == niveau then
    do levt.(Adef.int_of_iper x.cle_index) := infini;
       if Array.length x.family <> 0 then html_br conf
       else Wserver.wprint "\n";
       if niveau == niveau_max then
         let _ =
           List.fold_left
             (fun first ifam ->
                let fam = foi base ifam in
                let c = spouse x (coi base ifam) in
                let c = poi base c in
                do if connais base c then
                     do afficher_marie conf base first fam x
                          c;
                        Wserver.wprint ".";
                        html_br conf;
                     return ()
                   else ();
                return False)
             True (Array.to_list x.family)
         in
         ()
       else ();
    return boucle (succ niveau) x
  else Wserver.wprint "\n"
;

value afficher_descendants_jusqu_a conf base niveau_max p =
  let niveau_max = min limit_desc niveau_max in
  let levt = make_level_table base niveau_max p in
  let compte = ref 0 in
  let rec boucle niveau p =
    if niveau <= niveau_max then
      let ifaml = Array.to_list p.family in
      let _ =
        List.fold_left
          (fun first ifam ->
             let fam = foi base ifam in
             let cpl = coi base ifam in
             let conj = spouse p cpl in
             let enfants = fam.children in
             let conj = poi base conj in
             do if connais base conj || List.length ifaml > 1 then
                  do afficher_marie conf base first fam p conj;
                     if Array.length enfants <> 0 then
                       Wserver.wprint ", <em>%s</em>"
                         (transl conf "having as children")
                     else Wserver.wprint ".";
                     html_br conf;
                  return ()
                else ();
                if Array.length enfants <> 0 then
                  tag "ul" begin
                    Array.iter
                      (print_child conf base levt boucle niveau_max niveau
                         compte)
                      enfants;
                  end
                else ();
             return False)
          True ifaml
      in
      ()
    else ()
  in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text conf base p))
  in
  do header conf title;
(**)
     if niveau_max > 6 then enter_nobr () else ();
(**)
     Wserver.wprint "%s." (capitale (text_to conf niveau_max));
     html_p conf;
     stag "strong" begin
       afficher_personne_referencee conf base p;
     end;
     Date.afficher_dates conf base p;
     Wserver.wprint ".";
     html_br conf;
     boucle 1 p;
     html_p conf;
     Wserver.wprint "%s: %d %s" (capitale (transl conf "total")) compte.val
       (transl_nth conf "person/persons" 1);
     if niveau_max > 1 then
       Wserver.wprint " (%s)" (transl conf "spouses not included")
     else ();
     Wserver.wprint ".\n";
(**)
     if niveau_max > 6 then exit_nobr () else ();
(**)
     trailer conf;
  return ()
;

value afficher_descendants_niveau conf base niveau_max ancetre =
  let niveau_max = min limit_desc niveau_max in
  let levt = make_level_table base niveau_max ancetre in
  let rec get_level niveau p list =
    List.fold_left
      (fun list ifam ->
         let fam = foi base ifam in
         let enfants = fam.children in
         List.fold_left
           (fun list x ->
              let x = poi base x in
              if niveau == niveau_max then
                if sou base x.first_name = "x" ||
                   levt.(Adef.int_of_iper x.cle_index) != niveau then
                  list
                else [x :: list]
              else if niveau < niveau_max then get_level (succ niveau) x list
              else list)
           list (Array.to_list enfants))
      list (Array.to_list p.family)
  in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text_no_html conf base ancetre))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text conf base ancetre))
  in
  let len = ref 0 in
  let liste = get_level 1 ancetre [] in
  let liste =
    Sort.list
      (fun p1 p2 ->
         let c = alphabetique (sou base p1.surname) (sou base p2.surname) in
         if c == 0 then
           let c =
             alphabetique (sou base p1.first_name) (sou base p2.first_name)
           in
           if c == 0 then p1.occ > p2.occ else c > 0
         else c > 0)
      liste
  in
  let liste =
    List.fold_left
      (fun pl p ->
         match pl with
         [ [(p1, n) :: pl] when p.cle_index == p1.cle_index ->
              [(p1, succ n) :: pl]
         | _ -> do incr len; return [(p, 1) :: pl] ])
      [] liste
  in
  do header conf title;
     Wserver.wprint "%s" (capitale (text_level conf niveau_max));
     if len.val > 1 then
       Wserver.wprint " (%d %s)" len.val (transl_nth conf "person/persons" 1)
     else ();
     Wserver.wprint ".\n";
     html_p conf;
     print_alphab_list conf
       (fun (p, _) ->
          String.sub (sou base p.surname) (initiale (sou base p.surname)) 1)
       (fun (p, c) ->
          do afficher_personne_titre_referencee conf base p;
             Date.afficher_dates_courtes conf base p;
             if c > 1 then Wserver.wprint " <em>(%d)</em>" c else ();
             Wserver.wprint "\n";
          return ())
       liste;
     trailer conf;
  return ()
;

(* Avec numerotation *)

value mark_descendants base marks max_lev =
  loop 0 where rec loop lev p =
    if lev <= max_lev then
      do marks.(Adef.int_of_iper p.cle_index) := True; return
      Array.iter
        (fun ifam ->
           let el = (foi base ifam).children in
           Array.iter (fun e -> loop (succ lev) (poi base e)) el)
        p.family
    else ()
;

value label_descendants base marks paths max_lev =
  loop [] 0 where rec loop path lev p =
    if lev < max_lev then
      let _ =
        List.fold_left
          (fun cnt ifam ->
             let fam = foi base ifam in
             let c = spouse p (coi base ifam) in
             let el = fam.children in
             List.fold_left
               (fun cnt e ->
                  do if p.sex == Male ||
                        not marks.(Adef.int_of_iper c) then
                       let path = [Char.chr (Char.code 'A' + cnt) :: path] in
                       do paths.(Adef.int_of_iper e) := path;
                          loop path (succ lev) (poi base e);
                       return ()
                     else ();
                  return succ cnt)
               cnt (Array.to_list el))
          0 (Array.to_list p.family)
      in
      ()
    else ()
;

value close_lev = 2;

value close_to_end base marks max_lev lev p =
  if lev + close_lev >= max_lev then True
  else
    let rec short dlev p =
      List.for_all
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse p (coi base ifam) in
           let el = fam.children in
           if p.sex == Male || not marks.(Adef.int_of_iper c) then
             if dlev == close_lev then Array.length el = 0
             else
               List.for_all (fun e -> short (succ dlev) (poi base e))
                 (Array.to_list el)
           else True)
        (Array.to_list p.family)
    in
    short 1 p
;

value labelled base marks max_lev lev p =
  let a = aoi base p.cle_index in
  Array.length p.family <> 0 &&
  (match a.parents with
   [ Some ifam ->
       let cpl = coi base ifam in
       List.exists
         (fun ifam ->
            let el = (foi base ifam).children in
            List.exists
              (fun e ->
                 let e = poi base e in
                 Array.length e.family <> 0 &&
                 not (close_to_end base marks max_lev lev e))
              (Array.to_list el))
         (Array.to_list (poi base cpl.father).family)
   | _ -> False ])
;

value label_of_path paths p =
  loop paths.(Adef.int_of_iper p.cle_index) where rec loop =
    fun
    [ [] -> ""
    | [c :: cl] -> loop cl ^ String.make 1 c ]
;

value print_child conf base p1 p2 e =
  do stag "strong" begin
       if p1.sex == Male && e.surname == p1.surname ||
          p2.sex == Male && e.surname == p2.surname then
         afficher_prenom_de_personne_referencee conf base e
       else afficher_personne_referencee conf base e;
     end;
     Date.afficher_dates_courtes conf base e;
  return ()
;

value print_repeat_child conf base p1 p2 e =
  stag "em" begin
    if p1.sex == Male && e.surname == p1.surname ||
       p2.sex == Male && e.surname == p2.surname then
      afficher_prenom_de_personne conf base e
    else afficher_personne conf base e;
  end
;

value afficher_date_mariage conf base p c dmar =
  if age_autorise conf base p && age_autorise conf base c then
    match dmar with
    [ Some d -> Wserver.wprint "<font size=-2>%d</font>" (annee d)
    | None -> () ]
  else ()
;

value afficher_spouse conf base marks paths p c dmar =
  do Wserver.wprint "\n&amp;";
     afficher_date_mariage conf base p c dmar;
     Wserver.wprint " ";
     stag "strong" begin
       afficher_personne_referencee conf base c;
     end;
     if marks.(Adef.int_of_iper c.cle_index) then
       Wserver.wprint " (<tt><b>%s</b></tt>)" (label_of_path paths c)
     else Date.afficher_dates_courtes conf base c;
  return ()
;

value total = ref 0;

value print_family_locally conf base marks paths max_lev lev p1 c1 e =
  loop lev e where rec loop lev p =
    if lev < max_lev then
      let _ =
        List.fold_left
          (fun (cnt, first, need_br) ifam ->
             let fam = foi base ifam in
             let dmar = Adef.od_of_codate fam.marriage in
             let c = spouse p (coi base ifam) in
             let el = fam.children in
             let c = poi base c in
             do if need_br then html_br conf else ();
                if not first then print_repeat_child conf base p1 c1 e
                else ();
                afficher_spouse conf base marks paths p c dmar;
                Wserver.wprint "\n";
             return
             let print_children =
               p.sex == Male ||
               not marks.(Adef.int_of_iper c.cle_index)
             in
             do if print_children then
                  Wserver.wprint "<ol start=%d>\n" (succ cnt)
                else ();
             return
             let cnt =
               List.fold_left
                 (fun cnt e ->
                    let e = poi base e in
                    do if print_children then
                         do Wserver.wprint "<li type=A> ";
                            print_child conf base p c e;
                            Wserver.wprint "\n";
                            incr total;
                            if succ lev == max_lev then
                              let _ =
                                List.fold_left
                                  (fun first ifam ->
                                     let fam = foi base ifam in
                                     let dm =
                                       Adef.od_of_codate fam.marriage
                                     in
                                     let c1 = spouse e (coi base ifam) in
                                     let el = fam.children in
                                     let c1 = poi base c1 in
                                     do if not first then
                                          do html_br conf;
                                             print_repeat_child conf base p c
                                               e;
                                          return ()
                                        else ();
                                        afficher_spouse conf base marks
                                          paths e c1 dm;
                                        if Array.length el <> 0 then
                                          Wserver.wprint "....."
                                        else ();
                                        Wserver.wprint "\n";
                                     return False)
                                  True (Array.to_list e.family)
                              in
                              ()
                            else loop (succ lev) e;
                         return ()
                       else ();
                    return succ cnt)
                 cnt (Array.to_list el)
             in
             do if print_children then Wserver.wprint "</ol>\n" else (); return
             (cnt, False, not print_children))
          (0, True, False) (Array.to_list p.family)
      in
      ()
    else ()
;

value last_label = ref "";

value print_family conf base marks paths max_lev lev p =
  do if lev <> 0 then
       do Wserver.wprint "<tt><b>%s</b></tt>." (label_of_path paths p);
          html_br conf;
       return ()
     else ();
     do let lab = label_of_path paths p in
        if lab < last_label.val then failwith "print_family"
        else last_label.val := lab;
     return ();
  return
  let _ =
    List.fold_left
      (fun cnt ifam ->
         let fam = foi base ifam in
         let dmar = Adef.od_of_codate fam.marriage in
         let c = spouse p (coi base ifam) in
         let el = fam.children in
         let c = poi base c in
         do stag "strong" begin
              afficher_personne_referencee conf base p;
            end;
            afficher_spouse conf base marks paths p c dmar;
            Wserver.wprint "<ol start=%d>\n" (succ cnt);
         return
         let cnt =
           List.fold_left
             (fun cnt e ->
                let e = poi base e in
                do if p.sex == Male ||
                      not marks.(Adef.int_of_iper c.cle_index) then
                     do Wserver.wprint "<li type=A>";
                        print_child conf base p c e;
                        incr total;
                        Wserver.wprint "\n";
                        if labelled base marks max_lev lev e then
                          Wserver.wprint " => <tt><b>%s</b></tt>\n"
                            (label_of_path paths e)
                        else if succ lev == max_lev then
                          Array.iter
                            (fun ifam ->
                               let fam = foi base ifam in
                               let dm =
                                 Adef.od_of_codate fam.marriage
                               in
                               let c = spouse e (coi base ifam) in
                               let el = fam.children in
                               let c = poi base c in
                               do afficher_spouse conf base marks paths e c
                                    dm;
                                  if Array.length el <> 0 then
                                    Wserver.wprint "....."
                                  else ();
                                  Wserver.wprint "\n";
                               return ())
                            e.family
                        else
                          print_family_locally conf base marks paths max_lev
                            (succ lev) p c e;
                     return ()
                   else ();
                return succ cnt)
             cnt (Array.to_list el)
         in
         do Wserver.wprint "</ol>\n"; return cnt)
      0 (Array.to_list p.family)
  in
  ()
;

value print_families conf base marks paths max_lev =
  loop 0 where rec loop lev p =
    if lev < max_lev then
      do print_family conf base marks paths max_lev lev p; return
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse p (coi base ifam) in
           let el = fam.children in
           let c = poi base c in
           if p.sex == Male ||
              not marks.(Adef.int_of_iper c.cle_index) then
             Array.iter
               (fun e ->
                  let e = poi base e in
                  if labelled base marks max_lev lev e then loop (succ lev) e
                  else ())
               el
           else ())
        p.family
    else ()
;

value afficher_descendants_numerotation conf base niveau_max ancetre =
  let niveau_max = min limit_desc niveau_max in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "descendants"))
        (transl_decline conf "of (same or greater generation level)"
        (person_text_no_html conf base ancetre))
    else
      wprint_geneweb_link conf
        ("m=D;i=" ^ string_of_int (Adef.int_of_iper ancetre.cle_index) ^
         ";v=" ^ string_of_int niveau_max ^ ";t=G")
        (capitale (transl conf "descendants") ^ " " ^
         transl_decline conf "of (same or greater generation level)"
           (person_text conf base ancetre))
  in
  let marks = Array.create (base.data.persons.len) False in
  let paths = Array.create (base.data.persons.len) [] in
  do header conf title;
     total.val := 0;
     Date.afficher_dates_courtes conf base ancetre;
     let p = ancetre in
     if age_autorise conf base p then
       match (Adef.od_of_codate p.birth, p.death) with
       [ (Some _, _) | (_, Death _ _) -> html_br conf
       | _ -> () ]
     else ();
     Wserver.wprint "%s." (capitale (text_to conf niveau_max));
     html_p conf;
     mark_descendants base marks niveau_max ancetre;
     label_descendants base marks paths niveau_max ancetre;
     print_families conf base marks paths niveau_max ancetre;
     if total.val > 1 then
       do html_p conf;
          Wserver.wprint "%s: %d %s" (capitale (transl conf "total"))
            total.val (transl_nth conf "person/persons" 1);
          if niveau_max > 1 then
            Wserver.wprint " (%s)" (transl conf "spouses not included")
          else ();
          Wserver.wprint ".\n";
       return ()
     else ();
     trailer conf;
  return ()
;

value print_ref conf base paths p =
  if paths.(Adef.int_of_iper p.cle_index) <> [] then
    Wserver.wprint " => <tt><b>%s</b></tt>" (label_of_path paths p)
  else
    Array.iter
      (fun ifam ->
         let c = spouse p (coi base ifam) in
         if paths.(Adef.int_of_iper c) <> [] then
           let c = poi base c in
           Wserver.wprint " => %s %s <tt><b>%s</b></tt>"
             (sou base c.first_name)
             (sou base c.surname)
             (label_of_path paths c)
         else ())
      p.family
;

value print_elem conf base paths precision (n, pll) =
  do html_li conf;
     match pll with
     [ [[p]] ->
         do Wserver.wprint "<strong>%s " (surname_end n);
            wprint_geneweb_link conf
              ("i=" ^ string_of_int (Adef.int_of_iper p.cle_index))
              (sou base p.first_name);
            Wserver.wprint "%s</strong>" (surname_begin n);
            Date.afficher_dates_courtes conf base p;
            print_ref conf base paths p;
            Wserver.wprint "\n";
         return ()
     | _ ->
         do Wserver.wprint "<strong>%s%s</strong>\n"
              (surname_end n) (surname_begin n);
            tag "ul" begin
              List.iter
                (fun pl ->
                   let several =
                     match pl with
                     [ [_] -> False
                     | _ -> True ]
                   in
                   List.iter
                     (fun p ->
                        do html_li conf;
                           stag "strong" begin
                             wprint_geneweb_link conf
                               ("i=" ^
                                string_of_int (Adef.int_of_iper p.cle_index))
                               (sou base p.first_name);
                           end;
                           if several && precision then
                             do Wserver.wprint " <em>";
                                preciser_homonyme conf base p;
                                Wserver.wprint "</em>";
                             return ()
                           else ();
                           Date.afficher_dates_courtes conf base p;
                           print_ref conf base paths p;
                           Wserver.wprint "\n";
                        return ())
                     pl)
                pll;
            end;
         return () ];
  return ()
;

value trier_et_afficher conf base paths precision liste =
  let liste = List.map (poi base) liste in
  let liste =
    Sort.list
      (fun p1 p2 ->
         let c = alphabetique (sou base p1.surname) (sou base p2.surname) in
         if c == 0 then
           let c =
             alphabetique (sou base p1.first_name) (sou base p2.first_name)
           in
           c < 0
         else c > 0)
      liste
  in
  let liste =
    List.fold_left
      (fun npll p ->
         match npll with
         [ [(n, pl) :: npll] when n == sou base p.surname ->
             [(n, [p :: pl]) :: npll]
         | _ -> [(sou base p.surname, [p]) :: npll] ])
      [] liste
  in
  let liste =
    List.map
      (fun (n, pl) ->
         let pll =
           List.fold_left
             (fun pll p ->
                match pll with
                [ [([p1 :: _] as pl) :: pll]
                  when p1.first_name == p.first_name ->
                    [[p :: pl] :: pll]
                | _ -> [[p] :: pll] ])
             [] pl
         in
         (n, pll))
      liste
  in
  if liste <> [] then
    tag "ul" begin
      List.iter (print_elem conf base paths precision) liste;
    end
  else ()
;

value afficher_index_descendants conf base niveau_max ancetre =
  let niveau_max = min limit_desc niveau_max in
  let title h =
    let txt = capitale (transl conf "index of the descendants") in
    if not h then
      wprint_geneweb_link conf
        ("m=D;i=" ^ string_of_int (Adef.int_of_iper ancetre.cle_index) ^
         ";v=" ^ string_of_int niveau_max ^ ";t=C")
        txt
    else Wserver.wprint "%s" txt
  in
  do header conf title; return
  let marks = Array.create (base.data.persons.len) False in
  let paths = Array.create (base.data.persons.len) [] in
  do mark_descendants base marks niveau_max ancetre;
     label_descendants base marks paths niveau_max ancetre;
  return
  let liste = ref [] in
  do for i = 0 to base.data.persons.len - 1 do
       if paths.(i) <> [] then
         let p = base.data.persons.get i in
         if sou base p.first_name <> "?" && sou base p.surname <> "?" &&
            sou base p.first_name <> "x" then
           liste.val := [p.cle_index :: liste.val]
         else ()
       else ();
     done;
     trier_et_afficher conf base paths True liste.val;
     trailer conf;
  return ()
;

value afficher_index_spouses conf base niveau_max ancetre =
  let niveau_max = min limit_desc niveau_max in
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl conf "index of the spouses (non descendants)"))
  in
  do header conf title; return
  let marks = Array.create (base.data.persons.len) False in
  let paths = Array.create (base.data.persons.len) [] in
  do mark_descendants base marks niveau_max ancetre;
     label_descendants base marks paths niveau_max ancetre;
  return
  let liste = ref [] in
  do for i = 0 to base.data.persons.len - 1 do
       if paths.(i) <> [] then
         let p = base.data.persons.get i in
         if sou base p.first_name <> "?" && sou base p.surname <> "?" &&
            sou base p.first_name <> "x" then
           Array.iter
             (fun ifam ->
                let c = spouse p (coi base ifam) in
                if paths.(Adef.int_of_iper c) = [] then
                  let c = poi base c in
                  if sou base c.first_name <> "?" &&
                     sou base c.surname <> "?" &&
                     sou base p.first_name <> "x" &&
                     not (List.memq c.cle_index liste.val) then
                    liste.val := [c.cle_index :: liste.val]
                  else ()
                else ())
             p.family
         else ()
       else ();
     done;
     trier_et_afficher conf base paths False liste.val;
     trailer conf;
  return ()
;

value print_someone conf base p =
  do afficher_personne_titre_referencee conf base p;
     Date.afficher_dates_courtes conf base p;
     Wserver.wprint "\n";
  return ()
;

value children_of base ip =
  List.fold_right
    (fun ifam children -> Array.to_list (foi base ifam).children @ children)
    (Array.to_list (poi base ip).family) []
;

value rec print_table_person conf base max_lev ip =
  do Wserver.wprint "\n";
     tag "table" "border" begin
       Wserver.wprint "<tr>\n";
       tag "td" "valign=top" begin
         print_someone conf base (poi base ip);
       end;
       if max_lev > 0 then
         match children_of base ip with
         [ [] -> ()
         | ipl ->
             do Wserver.wprint "\n<td>";
                List.iter (print_table_person conf base (max_lev - 1)) ipl;
                Wserver.wprint "</td>\n";
             return () ]
       else ();
     end;
  return ()
;

value afficher_descendants_table conf base max_lev a =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "descendants")) in
  let max_lev = min limit_desc max_lev in
  do header conf title;
     print_table_person conf base max_lev a.cle_index;
     trailer conf;
  return ()
;

value print conf base p =
  match (p_getenv conf.env "t", p_getint conf.env "v") with
  [ (Some "L", Some v) -> afficher_descendants_jusqu_a conf base v p
  | (Some "S", Some v) -> afficher_descendants_niveau conf base v p
  | (Some "T", Some v) -> afficher_descendants_table conf base v p
  | (Some "N", Some v) -> afficher_descendants_numerotation conf base v p
  | (Some "G", Some v) -> afficher_index_descendants conf base v p
  | (Some "C", Some v) -> afficher_index_spouses conf base v p
  | _ -> afficher_menu_descendants conf base p ]
;
