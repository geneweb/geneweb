(* camlp4r ./pa_html.cmo *)
(* $Id: descend.ml,v 3.30 2001-01-31 17:43:30 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;
open Gutil;
open Util;
open Dag2html;

value limit_desc conf =
  match p_getint conf.base_env "max_desc_level" with
  [ Some x -> max 1 x
  | None -> 12 ]
;

value limit_by_tree conf =
  match p_getint conf.base_env "max_desc_tree" with
  [ Some x -> max 1 x
  | None -> 4 ]
;

value infini = 10000;

value make_level_table base niveau_max p =
  let mark = Array.create (base.data.persons.len) False in
  let levt = Array.create (base.data.persons.len) infini in
  let rec fill ip u lev =
    if niveau_max == infini && mark.(Adef.int_of_iper ip) then ()
    else
      do mark.(Adef.int_of_iper ip) := True; return
      if lev <= niveau_max then
        do if lev < levt.(Adef.int_of_iper ip) then
             levt.(Adef.int_of_iper ip) := lev
           else ();
        return
        Array.iter
          (fun ifam ->
             let ipl = (doi base ifam).children in
             Array.iter (fun ip -> fill ip (uoi base ip) (succ lev)) ipl)
          u.family
      else ()
  in
  do fill p.cle_index (uoi base p.cle_index) 0; return levt
;

value level_max base p =
(*
  let _ = base.data.unions.array () in
  let _ = base.data.descends.array () in
*)
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
  [ 0 ->
      transl conf "specify" ^ " " ^ transl_nth conf "generation/generations" 0
  | 1 -> transl conf "to the children"
  | 2 -> transl conf "to the grandchildren"
  | 3 -> transl conf "to the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value text_level conf =
  fun
  [ 0 ->
      transl conf "specify" ^ " " ^ transl_nth conf "generation/generations" 0
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
         Wserver.wprint "<input type=hidden name=%s value=\"%s\">\n" k
           (quote_escaped (decode_varenv v)))
      conf.henv;
    Wserver.wprint "<input type=hidden name=m value=D>\n";
    wprint_hidden_person conf base "" p;
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
    Wserver.wprint "<input type=submit value=\"Ok\">\n";
    html_p conf;
    tag "table" "border=%d width=\"90%%\"" conf.border begin
      tag "tr" begin
        tag "td" begin
          Wserver.wprint "<input type=radio name=t value=L checked> %s<br>\n"
            (capitale (transl_nth conf "list/list (ancestors)" 0));
          Wserver.wprint "<input type=radio name=t value=M> %s\n"
            (capitale (transl_nth conf "male line/female line" 0));
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=F> %s\n"
            (capitale (transl_nth conf "male line/female line" 1));
          Wserver.wprint "<br>\n";
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=T> %s\n"
            (capitale (transl conf "tree"));
          if niveau_effectif <= limit_by_tree conf then ()
          else
            Wserver.wprint "(%s %d %s)\n" (transl conf "maximum")
              (limit_by_tree conf)
              (transl_nth conf "generation/generations" 1);
          Wserver.wprint "<br>\n";
          Wserver.wprint
            "- %s <input type=checkbox name=image value=on><br>\n"
            (capitale (transl_nth conf "image/images" 1));
        end;
        tag "td" begin
          Wserver.wprint "<input type=radio name=t value=S> %s<br>\n"
            (capitale (transl conf "only the generation selected"));
          Wserver.wprint "<input type=radio name=t value=N> %s<br>\n"
            (capitale (transl conf "families with encoding"));
          Wserver.wprint "<input type=radio name=t value=G> - %s<br>\n"
            (capitale (transl conf "index of the descendants"));
          Wserver.wprint "<input type=radio name=t value=C> - %s<br>\n"
            (capitale (transl conf "index of the spouses (non descendants)"));
          Wserver.wprint "<input type=radio name=t value=A> d'Aboville<br>\n";
        end;
      end;
      tag "tr" begin
        tag "td" "colspan=2 align=center" begin
          Wserver.wprint "<br>\n%s\n"
            (capitale (transl conf "cancel GeneWeb links"));
          Wserver.wprint "<input type=checkbox name=cgl value=on><br>\n";
        end;
      end;
    end;
    html_p conf;      
  end
;

value descendants_title conf base p h =
  let txt_fun = if h then gen_person_text_no_html else gen_person_text in
  let s =
    transl_decline2 conf "%1 of (same or greater generation level) %2"
       (transl conf "descendants") (txt_fun raw_access conf base p)
  in
  Wserver.wprint "%s" (capitale s)
;

value afficher_menu_descendants conf base p =
  let niveau_effectif = min (limit_desc conf) (level_max base p) in
  do header conf (descendants_title conf base p);
     tag "center" begin
       print_choice conf base p niveau_effectif;
     end;
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
  let auth = age_autorise conf base p && age_autorise conf base spouse in
  do Wserver.wprint (fcapitale (relation_txt conf p.sex fam))
       (fun _ ->
          if auth then Perso.print_marriage_text conf base False fam else ());
     Wserver.wprint "\n";
     stag "strong" begin
       Wserver.wprint "%s"
         (reference conf base spouse (person_text conf base spouse));
     end;
     if auth then Date.print_dates conf base spouse else ();
     if auth then
       match fam.divorce with
       [ NotDivorced -> ()
       | Separated -> Wserver.wprint ",\n%s" (transl conf "separated")
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

value print_child
  conf base levt boucle niveau_max niveau compte auth always_surname x
=
  let ix = x.cle_index in
  let ux = uoi base ix in
  do html_li conf;
     stag "strong" begin
       if not always_surname && s_appelle_comme_son_pere base ix then
         afficher_prenom_de_personne_referencee conf base x
       else afficher_personne_referencee conf base x;
     end;
     if auth then Date.print_dates conf base x else ();
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
       if Array.length ux.family <> 0 then html_br conf
       else Wserver.wprint "\n";
       if niveau == niveau_max then
         list_iter_first
           (fun first ifam ->
              let fam = foi base ifam in
              let c = spouse x.cle_index (coi base ifam) in
              let c = poi base c in
              do if connais base c then
                   do afficher_marie conf base first fam x c;
                      Wserver.wprint ".";
                      html_br conf;
                   return ()
                 else ();
              return ())
           (Array.to_list ux.family)
       else ();
    return boucle (succ niveau) x ux
  else Wserver.wprint "\n"
;

value afficher_descendants_jusqu_a conf base niveau_max p line =
  let niveau_max = min (limit_desc conf) niveau_max in
  let levt = make_level_table base niveau_max p in
  let compte = ref 0 in
  let always_surname =
    try List.assoc "always_surname" conf.base_env = "yes" with
    [ Not_found -> False ]
  in
  let rec boucle niveau p u =
    if niveau <= niveau_max then
      let ifaml = Array.to_list u.family in
      list_iter_first
        (fun first ifam ->
           let fam = foi base ifam in
           let cpl = coi base ifam in
           let des = doi base ifam in
           let conj = spouse p.cle_index cpl in
           let conj = poi base conj in
           let children =
             let list = Array.to_list des.children in
             List.fold_right
               (fun ip pl ->
                 let p = poi base ip in
                 if line = Neuter
                 || line = Male && p.sex <> Female
                 || line = Female && p.sex <> Male
                 then [p :: pl]
                 else pl)
               list []
           in
           do if connais base conj || List.length ifaml > 1 then
                do afficher_marie conf base first fam p conj;
                   if children <> [] then
                     Wserver.wprint ", <em>%s</em>"
                       (transl conf "having as children")
                   else Wserver.wprint ".";
                   html_br conf;
                return ()
              else ();
              if children <> [] then
                let age_auth =
                  List.for_all (fun p -> age_autorise conf base p) children
                in
                tag "ul" begin
                  List.iter
                    (print_child conf base levt boucle niveau_max niveau
                       compte age_auth always_surname)
                    children;
                end
              else ();
           return ())
        ifaml
    else ()
  in
  do header conf (descendants_title conf base p);
     print_link_to_welcome conf True;
(**)
     if niveau_max > 6 then enter_nobr () else ();
(**)
     Wserver.wprint "%s.<br>\n" (capitale (text_to conf niveau_max));
     if line = Neuter then ()
     else
       Wserver.wprint "%s.<br>\n"
         (capitale
            (transl_nth conf "male line/female line"
               (if line = Male then 0 else 1)));
     html_p conf;
     stag "strong" begin
       afficher_personne_referencee conf base p;
     end;
     if age_autorise conf base p then Date.print_dates conf base p
     else ();
     Wserver.wprint ".";
     html_br conf;
     boucle 1 p (uoi base p.cle_index);
     if compte.val > 1 then
       do html_p conf;
          Wserver.wprint "%s: %d %s" (capitale (transl conf "total"))
            compte.val (transl_nth conf "person/persons" 1);
          if niveau_max > 1 then
            Wserver.wprint " (%s)" (transl conf "spouses not included")
          else ();
          Wserver.wprint ".\n";
       return ()
     else ();
(**)
     if niveau_max > 6 then exit_nobr () else ();
(**)
     trailer conf;
  return ()
;

value afficher_descendants_niveau conf base niveau_max ancetre =
  let niveau_max = min (limit_desc conf) niveau_max in
  let levt = make_level_table base niveau_max ancetre in
  let rec get_level niveau u list =
    List.fold_left
      (fun list ifam ->
         let des = doi base ifam in
         let enfants = des.children in
         List.fold_left
           (fun list ix ->
              let x = poi base ix in
              if niveau == niveau_max then
                if p_first_name base x = "x" ||
                   levt.(Adef.int_of_iper x.cle_index) != niveau then
                  list
                else [x :: list]
              else if niveau < niveau_max then
                get_level (succ niveau) (uoi base ix) list
              else list)
           list (Array.to_list enfants))
      list (Array.to_list u.family)
  in
  let len = ref 0 in
  let liste = get_level 1 (uoi base ancetre.cle_index) [] in
  let liste =
    Sort.list
      (fun p1 p2 ->
         let c = alphabetique (p_surname base p1) (p_surname base p2) in
         if c == 0 then
           let c =
             alphabetique (p_first_name base p1) (p_first_name base p2)
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
  do header conf (descendants_title conf base ancetre);
     Wserver.wprint "%s" (capitale (text_level conf niveau_max));
     if len.val > 1 then
       Wserver.wprint " (%d %s)" len.val (transl_nth conf "person/persons" 1)
     else ();
     Wserver.wprint ".\n";
     html_p conf;
     print_alphab_list conf
       (fun (p, _) ->
          String.sub (p_surname base p) (initiale (p_surname base p)) 1)
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

value mark_descendants base marks max_lev p =
  loop 0 p.cle_index (uoi base p.cle_index) where rec loop lev ip u =
    if lev <= max_lev then
      do marks.(Adef.int_of_iper ip) := True; return
      Array.iter
        (fun ifam ->
           let el = (doi base ifam).children in
           Array.iter (fun e -> loop (succ lev) e (uoi base e)) el)
        u.family
    else ()
;

value label_descendants base marks paths max_lev =
  loop [] 0 where rec loop path lev p =
    if lev < max_lev then
      let u = uoi base p.cle_index in
      let _ =
        List.fold_left
          (fun cnt ifam ->
             let des = doi base ifam in
             let c = spouse p.cle_index (coi base ifam) in
             let el = des.children in
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
          0 (Array.to_list u.family)
      in
      ()
    else ()
;

value close_lev = 2;

value close_to_end base marks max_lev lev p =
  if lev + close_lev >= max_lev then True
  else
    let rec short dlev p =
      let u = uoi base p.cle_index in
      List.for_all
        (fun ifam ->
           let des = doi base ifam in
           let c = spouse p.cle_index (coi base ifam) in
           let el = des.children in
           if p.sex == Male || not marks.(Adef.int_of_iper c) then
             if dlev == close_lev then Array.length el = 0
             else
               List.for_all (fun e -> short (succ dlev) (poi base e))
                 (Array.to_list el)
           else True)
        (Array.to_list u.family)
    in
    short 1 p
;

value labelled base marks max_lev lev ip =
  let a = aoi base ip in
  let u = uoi base ip in
  Array.length u.family <> 0 &&
  (match a.parents with
   [ Some ifam ->
       let cpl = coi base ifam in
       List.exists
         (fun ifam ->
            let el = (doi base ifam).children in
            List.exists
              (fun ie ->
                 let e = poi base ie in
                 let u = uoi base ie in
                 Array.length u.family <> 0 &&
                 not (close_to_end base marks max_lev lev e))
              (Array.to_list el))
         (Array.to_list (uoi base cpl.father).family)
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

value afficher_date_mariage conf base fam p c =
  Wserver.wprint "%s" (Date.short_marriage_date_text conf base fam p c)
;

value afficher_spouse conf base marks paths fam p c =
  do Wserver.wprint "\n&amp;";
     afficher_date_mariage conf base fam p c;
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
             let des = doi base ifam in
             let c = spouse p.cle_index (coi base ifam) in
             let el = des.children in
             let c = poi base c in
             do if need_br then html_br conf else ();
                if not first then print_repeat_child conf base p1 c1 p
                else ();
                afficher_spouse conf base marks paths fam p c;
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
                 (fun cnt ie ->
                    let e = poi base ie in
                    do if print_children then
                         do Wserver.wprint "<li type=A> ";
                            print_child conf base p c e;
                            Wserver.wprint "\n";
                            incr total;
                            if succ lev == max_lev then
                              list_iter_first
                                (fun first ifam ->
                                   let fam = foi base ifam in
                                   let des = doi base ifam in
                                   let c1 = spouse ie (coi base ifam) in
                                   let el = des.children in
                                   let c1 = poi base c1 in
                                   do if not first then
                                        do html_br conf;
                                           print_repeat_child conf base p c
                                             e;
                                        return ()
                                      else ();
                                      afficher_spouse conf base marks
                                        paths fam e c1;
                                      if Array.length el <> 0 then
                                        Wserver.wprint "....."
                                      else ();
                                      Wserver.wprint "\n";
                                   return ())
                                (Array.to_list (uoi base ie).family)
                            else loop (succ lev) e;
                         return ()
                       else ();
                    return succ cnt)
                 cnt (Array.to_list el)
             in
             do if print_children then Wserver.wprint "</ol>\n" else (); return
             (cnt, False, not print_children))
          (0, True, False) (Array.to_list (uoi base p.cle_index).family)
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
         let des = doi base ifam in
         let c = spouse p.cle_index (coi base ifam) in
         let el = des.children in
         let c = poi base c in
         do stag "strong" begin
              afficher_personne_referencee conf base p;
            end;
            afficher_spouse conf base marks paths fam p c;
            Wserver.wprint "<ol start=%d>\n" (succ cnt);
         return
         let cnt =
           List.fold_left
             (fun cnt ie ->
                let e = poi base ie in
                do if p.sex == Male ||
                      not marks.(Adef.int_of_iper c.cle_index) then
                     do Wserver.wprint "<li type=A>";
                        print_child conf base p c e;
                        incr total;
                        Wserver.wprint "\n";
                        if labelled base marks max_lev lev ie then
                          Wserver.wprint " => <tt><b>%s</b></tt>\n"
                            (label_of_path paths e)
                        else if succ lev == max_lev then
                          Array.iter
                            (fun ifam ->
                               let fam = foi base ifam in
                               let des = doi base ifam in
                               let c = spouse ie (coi base ifam) in
                               let el = des.children in
                               let c = poi base c in
                               do afficher_spouse conf base marks paths fam e
                                    c;
                                  if Array.length el <> 0 then
                                    Wserver.wprint "....."
                                  else ();
                                  Wserver.wprint "\n";
                               return ())
                            (uoi base ie).family
                        else
                          print_family_locally conf base marks paths max_lev
                            (succ lev) p c e;
                     return ()
                   else ();
                return succ cnt)
             cnt (Array.to_list el)
         in
         do Wserver.wprint "</ol>\n"; return cnt)
      0 (Array.to_list (uoi base p.cle_index).family)
  in
  ()
;

value print_families conf base marks paths max_lev =
  loop 0 where rec loop lev p =
    if lev < max_lev then
      do print_family conf base marks paths max_lev lev p; return
      Array.iter
        (fun ifam ->
           let des = doi base ifam in
           let c = spouse p.cle_index (coi base ifam) in
           let el = des.children in
           let c = poi base c in
           if p.sex == Male ||
              not marks.(Adef.int_of_iper c.cle_index) then
             Array.iter
               (fun ie ->
                  let e = poi base ie in
                  if labelled base marks max_lev lev ie then loop (succ lev) e
                  else ())
               el
           else ())
        (uoi base p.cle_index).family
    else ()
;

value afficher_descendants_numerotation conf base niveau_max ancetre =
  let niveau_max = min (limit_desc conf) niveau_max in
  let title h =
    if h then descendants_title conf base ancetre h
    else
      wprint_geneweb_link conf
        ("m=D;i=" ^ string_of_int (Adef.int_of_iper ancetre.cle_index) ^
         ";v=" ^ string_of_int niveau_max ^ ";t=G")
        (capitale
           (transl_decline2 conf "%1 of (same or greater generation level) %2"
              (transl conf "descendants")
              (person_text conf base ancetre)))
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
         let c = spouse p.cle_index (coi base ifam) in
         if paths.(Adef.int_of_iper c) <> [] then
           let c = poi base c in
           Wserver.wprint " => %s %s <tt><b>%s</b></tt>"
             (p_first_name base c)
             (p_surname base c)
             (label_of_path paths c)
         else ())
      (uoi base p.cle_index).family
;

value print_elem conf base paths precision (n, pll) =
  do html_li conf;
     match pll with
     [ [[p]] ->
         do Wserver.wprint "<strong>%s " (surname_end n);
            wprint_geneweb_link conf
              ("i=" ^ string_of_int (Adef.int_of_iper p.cle_index))
              (p_first_name base p);
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
                               (p_first_name base p);
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
         let c = alphabetique (p_surname base p1) (p_surname base p2) in
         if c == 0 then
           let c =
             alphabetique (p_first_name base p1) (p_first_name base p2)
           in
           c < 0
         else c > 0)
      liste
  in
  let liste =
    List.fold_left
      (fun npll p ->
         match npll with
         [ [(n, pl) :: npll] when n == p_surname base p ->
             [(n, [p :: pl]) :: npll]
         | _ -> [(p_surname base p, [p]) :: npll] ])
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
  let niveau_max = min (limit_desc conf) niveau_max in
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
         if p_first_name base p <> "?" && p_surname base p <> "?" &&
            p_first_name base p <> "x" then
           liste.val := [p.cle_index :: liste.val]
         else ()
       else ();
     done;
     trier_et_afficher conf base paths True liste.val;
     trailer conf;
  return ()
;

value afficher_index_spouses conf base niveau_max ancetre =
  let niveau_max = min (limit_desc conf) niveau_max in
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
         let u = base.data.unions.get i in
         if p_first_name base p <> "?" && p_surname base p <> "?" &&
            p_first_name base p <> "x" then
           Array.iter
             (fun ifam ->
                let c = spouse p.cle_index (coi base ifam) in
                if paths.(Adef.int_of_iper c) = [] then
                  let c = poi base c in
                  if p_first_name base c <> "?" &&
                     p_surname base c <> "?" &&
                     p_first_name base p <> "x" &&
                     not (List.memq c.cle_index liste.val) then
                    liste.val := [c.cle_index :: liste.val]
                  else ()
                else ())
             u.family
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
    (fun ifam children -> Array.to_list (doi base ifam).children @ children)
    (Array.to_list (uoi base ip).family) []
;

value rec print_table_person conf base max_lev ip =
  do Wserver.wprint "\n";
     tag "table" "border=1" begin
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
  let max_lev = min (limit_desc conf) max_lev in
  do header conf title;
     print_table_person conf base max_lev a.cle_index;
     trailer conf;
  return ()
;

value make_tree_hts conf base gv p =
  let gv = min (limit_by_tree conf) gv in
  let rec nb_column n v u =
    if v == 0 then n + 1
    else
      if Array.length u.family = 0 then n + 1
      else
        List.fold_left (fun n ifam -> fam_nb_column n v (doi base ifam))
          n (Array.to_list u.family)
  and fam_nb_column n v des =
    if Array.length des.children = 0 then n + 1
    else
      List.fold_left (fun n iper -> nb_column n (v - 1) (uoi base iper)) n
        (Array.to_list des.children)
  in
  let vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then []
      else [(1, LeftA, TDstring "&nbsp;") :: tdl]
    in
    let td =
      match po with
      [ Some (_, u, _) ->
          let ncol = nb_column 0 (v - 1) u in
          (2 * ncol - 1, CenterA, TDstring "|")
      | None -> (1, LeftA, TDstring "&nbsp;") ]
    in
    [td :: tdl]
  in
  let children_vertical_bars v gen =
    let tdl = List.fold_left (vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let spouses_vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then []
      else [(1, LeftA, TDstring "&nbsp;") :: tdl]
    in
    match po with
    [ Some (p, u, _) when Array.length u.family > 0 ->
        fst (List.fold_left
          (fun (tdl, first) ifam ->
             let tdl =
               if first then tdl
               else [(1, LeftA, TDstring "&nbsp;") :: tdl]
             in
             let des = doi base ifam in
             let td =
               if Array.length des.children = 0 then
                 (1, LeftA, TDstring "&nbsp;")
               else
                 let ncol = fam_nb_column 0 (v - 1) des in
                 (2 * ncol - 1, CenterA, TDstring "|")
             in
             ([td :: tdl], False))
          (tdl, True) (Array.to_list u.family))
    | _ -> [(1, LeftA, TDstring "&nbsp;") :: tdl] ]
  in
  let spouses_vertical_bar v gen =
    let tdl = List.fold_left (spouses_vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let horizontal_bar_txt v tdl po =
    let tdl =
      if tdl = [] then []
      else [(1, LeftA, TDstring "&nbsp;") :: tdl]
    in
    match po with
    [ Some (p, u, _) when Array.length u.family > 0 ->
        fst (List.fold_left
          (fun (tdl, first) ifam ->
             let tdl =
               if first then tdl
               else [(1, LeftA, TDstring "&nbsp;") :: tdl]
             in
             let des = doi base ifam in
             let tdl =
               if Array.length des.children = 0 then
                 [(1, LeftA, TDstring "&nbsp;") :: tdl]
               else if Array.length des.children = 1 then
                 let u = uoi base des.children.(0) in
                 let ncol = nb_column 0 (v - 1) u in
                 [(2 * ncol - 1, CenterA, TDstring "|") ::
                  tdl]
               else
                 loop tdl 0 where rec loop tdl i =
                   if i = Array.length des.children then tdl
                   else
                     let iper = des.children.(i) in
                     let u = uoi base iper in
                     let tdl =
                       if i > 0 then
                         let align = CenterA in
                         [(1, align, TDhr align) :: tdl]
                       else tdl
                     in
                     let ncol = nb_column 0 (v - 1) u in
                     let align =
                       if i == 0 then RightA
                       else if i == Array.length des.children - 1
                         then LeftA
                       else CenterA
                     in
                     let td = (2 * ncol - 1, align, TDhr align) in
                     loop [td :: tdl] (i + 1)
             in
             (tdl, False))
          (tdl, True) (Array.to_list u.family))
    | _ -> [(1, LeftA, TDstring "&nbsp;") :: tdl] ]
  in
  let horizontal_bars v gen =
    let tdl = List.fold_left (horizontal_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let person_txt v tdl po =
    let tdl =
      if tdl = [] then []
      else [(1, LeftA, TDstring "&nbsp;") :: tdl]
    in
    let td =
      match po with
      [ Some (p, u, auth) ->
          let ncol = nb_column 0 (v - 1) u in
          let txt =
            if v = 1 then person_text_without_surname conf base p
            else person_title_text conf base p
          in
          let txt = reference conf base p txt in
          let txt =
            if auth then txt ^ Date.short_dates_text conf base p else txt
          in
          (2 * ncol - 1, CenterA,
           TDstring (txt ^ Dag.image_txt conf base p))
       | None -> (1, LeftA, TDstring "&nbsp;") ]
    in
    [td :: tdl]
  in
  let spouses_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDstring "&nbsp;") :: tdl]
    in
    match po with
    [ Some (p, u, auth) when Array.length u.family > 0 ->
        loop tdl 0 where rec loop tdl i =
          if i = Array.length u.family then tdl
          else
            let ifam = u.family.(i) in
            let tdl =
              if i > 0 then [(1, LeftA, TDstring "...") :: tdl] else tdl
            in
            let td =
              let fam = foi base ifam in
              let des = doi base ifam in
              let ncol = fam_nb_column 0 (v - 1) des in
              let s =
                let sp = poi base (spouse p.cle_index (coi base ifam)) in
                let txt = person_title_text conf base sp in
                let txt = reference conf base sp txt in
                let txt =
                  if auth then txt ^ Date.short_dates_text conf base sp
                  else txt
                in
                "&amp;" ^
                (if auth then Date.short_marriage_date_text conf base fam p sp
                 else "") ^
                "&nbsp;" ^ txt ^ Dag.image_txt conf base sp
              in
              (2 * ncol - 1, CenterA, TDstring s)
            in
            loop [td :: tdl] (i + 1)
    | _ -> [(1, LeftA, TDstring "&nbsp;") :: tdl] ]
  in
  let next_gen gen =
    List.fold_right
      (fun po gen ->
         match po with
         [ Some (p, u, auth) ->
             if Array.length u.family = 0 then [None :: gen]
             else
               List.fold_right
                 (fun ifam gen ->
                    let des = doi base ifam in
                    if Array.length des.children = 0 then [None :: gen]
                    else
                      let age_auth =
                        List.for_all
                          (fun ip -> age_autorise conf base (poi base ip))
                          (Array.to_list des.children)
                      in
                      List.fold_right
                        (fun iper gen ->
                           let g = (poi base iper, uoi base iper, age_auth) in
                           [Some g :: gen])
                        (Array.to_list des.children) gen)
                 (Array.to_list u.family) gen
         | None -> [None :: gen] ])
      gen []
  in
  let hts =
    let tdal =
      loop [] [] [Some (p, uoi base p.cle_index, True)] (gv + 1)
      where rec loop tdal prev_gen gen v =
        let tdal =
          if prev_gen <> [] then
            [children_vertical_bars v gen;
             horizontal_bars v prev_gen;
             spouses_vertical_bar (v + 1) prev_gen :: tdal]
          else tdal
        in
        let tdal =
          let tdl = List.fold_left (person_txt v) [] gen in
          [Array.of_list (List.rev tdl) :: tdal]
        in
        if v > 1 then
          let tdl = List.fold_left (spouses_txt v) [] gen in
          let tdal = [Array.of_list (List.rev tdl) :: tdal] in
          loop tdal gen (next_gen gen) (v - 1)
        else tdal
    in
    Array.of_list (List.rev tdal)
  in
  hts
;

value print_tree conf base gv p =
  let hts = make_tree_hts conf base gv p in
  if p_getenv conf.env "slices" = Some "on" then
    Dag.print_slices_menu conf base (Some hts)
  else
    let title _ =
      Wserver.wprint "%s: %s" (capitale (transl conf "tree"))
        (person_text_no_html conf base p)
    in
    do header_no_page_title conf title;
       Dag.print_html_table conf hts;
       trailer conf;
    return ()
;

value print_aboville conf base max_level p =
  let max_level = min (limit_desc conf) max_level in
  do Util.header conf (descendants_title conf base p);
     print_link_to_welcome conf True;
     Wserver.wprint "%s.<br><p>" (capitale (text_to conf max_level));
     loop_ind 0 "" p where rec loop_ind lev lab p =
       do Wserver.wprint "<tt>%s</tt>\n" lab;
          Wserver.wprint "%s%s\n"
            (referenced_person_title_text conf base p)
            (Date.short_dates_text conf base p);
       return
       let u = uoi base p.cle_index in
       do if lev < max_level then
            for i = 0 to Array.length u.family - 1 do
              let cpl = coi base u.family.(i) in
              let spouse = poi base (Gutil.spouse p.cle_index cpl) in
              let mdate =
                if age_autorise conf base p && age_autorise conf base spouse
                then
                  let fam = foi base u.family.(i) in
                  match Adef.od_of_codate fam.marriage with
                  [ Some (Dgreg d _) ->
                      "<font size=-2><em>" ^ Date.year_text d ^ "</em></font>"
                  | _ -> "" ]
                else ""
              in
              Wserver.wprint "&amp;%s %s%s\n" mdate
                (referenced_person_title_text conf base spouse)
                (Date.short_dates_text conf base spouse);
            done
          else ();
          Wserver.wprint "<br>\n";
       return
       if lev < max_level then
         loop_fam 1 0 where rec loop_fam cnt_chil i =
           if i == Array.length u.family then ()
           else
             let des = doi base u.family.(i) in
             loop_chil cnt_chil 0 where rec loop_chil cnt_chil j =
               if j == Array.length des.children then
                 loop_fam cnt_chil (i + 1)
               else
                 do loop_ind (lev + 1) (lab ^ string_of_int cnt_chil ^ ".")
                      (poi base des.children.(j));
                 return loop_chil (cnt_chil + 1) (j + 1)
       else ();
     Util.trailer conf;
  return ()
;

value print conf base p =
  match (p_getenv conf.env "t", p_getint conf.env "v") with
  [ (Some "A", Some v) -> print_aboville conf base v p
  | (Some "L", Some v) -> afficher_descendants_jusqu_a conf base v p Neuter
  | (Some "M", Some v) -> afficher_descendants_jusqu_a conf base v p Male
  | (Some "F", Some v) -> afficher_descendants_jusqu_a conf base v p Female
  | (Some "S", Some v) -> afficher_descendants_niveau conf base v p
  | (Some "H", Some v) -> afficher_descendants_table conf base v p
  | (Some "N", Some v) -> afficher_descendants_numerotation conf base v p
  | (Some "G", Some v) -> afficher_index_descendants conf base v p
  | (Some "C", Some v) -> afficher_index_spouses conf base v p
  | (Some "T", Some v) -> print_tree conf base v p
  | _ -> afficher_menu_descendants conf base p ]
;
