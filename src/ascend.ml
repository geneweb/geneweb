(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: ascend.ml,v 3.0 1999-10-29 10:30:57 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;
open Gutil;
open Util;

value limit_by_list conf =
  match p_getint conf.base_env "max_anc_level" with
  [ Some x -> max 1 x
  | None -> 8 ]
;
value limit_by_tree = 5;

value niveau_max_ascendance base ip =
  let _ = base.data.ascends.array () in
  let _ = base.data.couples.array () in
  let x = ref 0 in
  let mark = Array.create base.data.persons.len False in
  do let rec loop niveau ip =
       if mark.(Adef.int_of_iper ip) then ()
       else
         do mark.(Adef.int_of_iper ip) := True;
            x.val := max x.val niveau;
         return
         match (aoi base ip).parents with
         [ Some ifam ->
             let cpl = coi base ifam in
             do loop (succ niveau) cpl.father;
                loop (succ niveau) cpl.mother;
             return ()
         | _ -> () ]
     in
     loop 0 ip;
  return x.val
;

value text_to conf =
  fun
  [ 1 -> transl conf "specify" ^ " " ^ transl conf "generation"
  | 2 -> transl conf "to the parents"
  | 3 -> transl conf "to the grandparents"
  | 4 -> transl conf "to the great-grandparents"
  | i ->
      Printf.sprintf (ftransl conf "to the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value text_level conf =
  fun
  [ 1 -> transl conf "specify" ^ " " ^ transl conf "generation"
  | 2 -> transl conf "the parents"
  | 3 -> transl conf "the grandparents"
  | 4 -> transl conf "the great-grandparents"
  | i ->
      Printf.sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value print_choice conf base p niveau_effectif =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Srcfile.hidden_env conf;
    Wserver.wprint "\n";
    Wserver.wprint "<input type=hidden name=m value=A>\n";
    wprint_hidden_person conf base "" p;
    tag "center" begin
      tag "select" "name=v" begin
        let rec boucle i =
          if i > niveau_effectif + 1 then ()
          else
            do Wserver.wprint "  <option value=%d%s> %s\n" i
                 (if i == 0 then " selected" else "")
                 (capitale (text_to conf i));
            return boucle (succ i)
        in
        boucle 1;
      end;
      Wserver.wprint "<input type=submit value=\"Ok\">\n";
    end;
    html_br conf;
    tag "table" "border=%d width=\"90%%\"" conf.border begin
      tag "tr" begin
        tag "td" "valign=top" begin
          Wserver.wprint "<input type=radio name=t value=N checked> %s<br>\n"
            (capitale (transl conf "short display"));
          Wserver.wprint "<input type=radio name=t value=G> %s<br>\n"
            (capitale (transl conf "long display"));
          Wserver.wprint
            "- %s <input type=checkbox name=siblings value=on checked><br>\n"
            (capitale (transl conf "siblings"));
          Wserver.wprint
            "- %s <input type=checkbox name=notes value=on checked><br>\n"
            (capitale (nominative (transl_nth conf "note/notes" 1)));
          Wserver.wprint "<input type=radio name=t value=T> %s\n"
            (capitale (transl conf "tree"));
          let limit =
            if browser_doesnt_have_tables conf then 3 else limit_by_tree
          in
          if niveau_effectif <= limit then ()
          else
            do Wserver.wprint "(";
               Wserver.wprint (ftransl conf "max %d generations") limit;
               Wserver.wprint ")\n";
            return ();
          Wserver.wprint "<br>\n";
          Wserver.wprint "<input type=radio name=t value=L> %s\n"
            (capitale (transl conf "list"));
          if niveau_effectif <= limit_by_list conf then ()
          else
            do Wserver.wprint "(";
               Wserver.wprint (ftransl conf "max %d generations")
                 (limit_by_list conf);
               Wserver.wprint ")\n";
            return ();
        end;
        tag "td valign=top" begin
          Wserver.wprint "<input type=radio name=t value=S> %s<br>\n"
            (capitale (transl conf "only the generation selected"));
          Wserver.wprint "<input type=radio name=t value=M> %s<br>\n"
            (capitale (transl conf "missing ancestors"));
          Wserver.wprint "- %s\n"
            (capitale (transl conf "alphabetic order"));
          Wserver.wprint "<input type=checkbox name=al value=on><br>\n";
          Wserver.wprint "- %s\n"
            (capitale (transl conf "include missing spouses"));
          Wserver.wprint "<input type=checkbox name=ms value=on><br>\n";
          Wserver.wprint "- %s\n" (capitale (transl conf "after"));
          Wserver.wprint "<input name=after size=5 maxlength=5>\n";
          Wserver.wprint "%s\n" (capitale (transl conf "before"));
          Wserver.wprint "<input name=before size=5 maxlength=5><br>\n";
        end;
      end;
      tag "tr" begin
        tag "td" "colspan=2 align=center" begin
          Wserver.wprint "%s\n"
            (capitale (transl conf "cancel GeneWeb links"));
          Wserver.wprint "<input type=checkbox name=cgl value=on>\n";
        end;
      end;
    end;
  end
;

value rec list_remove_assoc x =
  fun
  [ [] -> []
  | [((a, b) as pair) :: l] ->
      if a = x then l else [pair :: list_remove_assoc x l] ]
;

value afficher_menu_ascendants conf base p =
  let niveau_effectif = niveau_max_ascendance base p.cle_index in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  do header conf title;
     tag "center" begin
       print_choice conf base p niveau_effectif;
       html_p conf;
       Wserver.wprint
         (fcapitale (ftransl conf "navigation with %t as Sosa reference"))
         (fun _ ->
            do conf.henv := list_remove_assoc "iz" conf.henv;
               stag "a" "href=\"%siz=%d\"" (commd conf)
                 (Adef.int_of_iper p.cle_index)
               begin
                 afficher_personne_sans_titre conf base p;
               end;
               afficher_titre conf base p;
            return ());
       Wserver.wprint ".\n";
     end;
     Wserver.wprint "<br>\n";
     trailer conf;
  return ()
;

value afficher_ancetre conf base x p =
  do afficher_personne_referencee conf base p;
     Date.afficher_dates_courtes conf base p;
  return ()
;

value afficher_ascendants_jusqu_a conf base niveau_max p =
  let niveau_max = min (limit_by_list conf)  niveau_max in
  let rec boucle niveau ip =
    if niveau < niveau_max then
      let x = aoi base ip in
      match x.parents with
      [ Some ifam ->
          let cpl = coi base ifam in
          let pere = poi base cpl.father in
          let mere = poi base cpl.mother in
          let know_fath = connais base pere in
          let know_moth = connais base mere in
          if know_fath || know_moth then
            tag "ul" begin
              if know_fath then
                do Wserver.wprint "<li type=square> ";
                   afficher_ancetre conf base p pere;
                   Wserver.wprint "\n";
                return boucle (succ niveau) cpl.father
              else ();
              if know_moth then
                do Wserver.wprint "<li type=circle> ";
                   afficher_ancetre conf base p mere;
                   Wserver.wprint "\n";
                return boucle (succ niveau) cpl.mother
              else ();
            end
          else ()
      | None -> () ]
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  do header conf title;
     tag "nobr" begin
       Wserver.wprint "%s.\n" (capitale (text_to conf niveau_max));
       boucle 1 p.cle_index;
     end;
     trailer conf;
  return ()
;

(* Print ancestors with numbers.
   The mark table holds the number of the ancestor after it has been
   printed or Num.zero if it has not yet been printed.
   At each generation, count and print a list of generation_person *)

type generation_person =
  [ GP_person of Num.t and iper and option ifam
  | GP_same of Num.t and Num.t and iper
  | GP_interv of option (Num.t * Num.t * option (Num.t * Num.t))
  | GP_missing of Num.t and iper ]
;

value next_generation base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
         match gp with
         [ GP_person n ip _ ->
             let n_fath = Num.twice n in
             let n_moth = Num.inc n_fath 1 in
             let a = aoi base ip in
             match a.parents with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 [GP_person n_fath cpl.father (Some ifam);
                  GP_person n_moth cpl.mother (Some ifam) ::
                  gpl]
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
             if Num.eq m Num.zero then do mark.(i) := n; return [gp :: gpl]
             else [GP_same n m ip :: gpl]
         | _ -> [gp :: gpl] ])
      [] gpl
  in
  List.rev gpl
;

value wpr s = Wserver.wprint "%s" s;

value print_generation_person conf base cnt gp =
  match gp with
  [ GP_person n ip _ ->
      let p = poi base ip in
      do html_li conf;
         Num.print wpr (transl conf "(thousand separator)") n;
         Wserver.wprint " -\n";
         afficher_personne_titre_referencee conf base p;
         Date.afficher_dates_courtes conf base p;
         Wserver.wprint "\n";
         incr cnt;
      return ()
  | GP_same n1 n2 ip ->
      let p = poi base ip in
      do html_li conf;
         Num.print wpr (transl conf "(thousand separator)") n1;
         Wserver.wprint " =&gt; ";
         let s =
           let b = ref "" in
           do Num.print (fun s -> b.val := b.val ^ s)
                (transl conf "(thousand separator)") n2;
           return b.val
         in
         Wserver.wprint "%s" (reference conf base p s);
         Wserver.wprint "\n\n";
      return ()
  | GP_interv None ->
      do html_li conf;
         Wserver.wprint "...\n\n";
      return ()
  | GP_interv (Some (n1, n2, x)) ->
      do html_li conf;
         Num.print wpr (transl conf "(thousand separator)") n1;
         Wserver.wprint "-";
         Num.print wpr (transl conf "(thousand separator)")
           (Num.sub n2 Num.one);
         Wserver.wprint " = ";
         match x with
         [ Some (m1, m2) ->
             do Num.print wpr (transl conf "(thousand separator)") m1;
                Wserver.wprint "-";
                Num.print wpr (transl conf "(thousand separator)")
                  (Num.sub m2 Num.one);
             return ()
         | None -> Wserver.wprint "..." ];
         Wserver.wprint "\n\n";
      return ()
  | _ -> () ]
;

value will_print =
  fun
  [ GP_person _ _ _ -> True
  | GP_same _ _ _ -> True
  | _ -> False ]
;

value afficher_ascendants_numerotation conf base niveau_max p =
  let mark = Array.create (base.data.persons.len) Num.zero in
  let cnt = ref 0 in
  let rec generation niveau gpl =
    if niveau <= niveau_max then
      do html_li conf;
         Wserver.wprint "%s %d\n" (capitale (transl conf "generation")) niveau;
         tag "ul" begin
           List.iter (print_generation_person conf base cnt) gpl;
         end;
      return
      let gpl = next_generation base mark gpl in
      if List.exists will_print gpl then generation (niveau + 1) gpl else ()
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  do header conf title;
     Wserver.wprint "%s.\n" (capitale (text_to conf niveau_max));
     tag "ul" begin
       mark.(Adef.int_of_iper p.cle_index) := Num.one;
       generation 1 [GP_person Num.one p.cle_index None];
     end;
     html_p conf;
     Wserver.wprint "%s: %d %s" (capitale (transl conf "total")) cnt.val
       (transl_nth conf "person/persons" 1);
     trailer conf;
  return ()
;

value print_link_long conf n =
  stag "strong" begin
    stag "a" "href=\"#%s\"" (Num.to_string n) begin
      Num.print wpr (transl conf "(thousand separator)") n;
    end;
  end
;

value print_person_long_info conf base auth link p =
  do List.iter
       (fun a ->
          Wserver.wprint ", %s <em>%s</em>" (transl conf "alias")
            (sou base a))
       p.aliases;
     if link = None && List.length p.titles > 0 &&
       (p.access <> Private || conf.friend || conf.wizard)
     then
       do Wserver.wprint ", <em>";
          Perso.print_titles conf base False (transl conf "and") p;
          Wserver.wprint "</em>";
       return ()
     else ();
     if auth then Date.print_dates conf base p else ();
     let occu = sou base p.occupation in
     if auth && link = None && occu <> "" then
       Wserver.wprint ", %s" occu
     else ();
     match link with
     [ Some n ->
         do Wserver.wprint ".\n %s " (capitale (transl conf "see"));
            print_link_long conf n;
         return ()
     | None -> () ];
  return ()
;

value title_reference conf base t =
  let ident = sou base t.t_ident in
  let place = sou base t.t_place in
  let s = if place = "" then ident else ident ^ " " ^ place in
  "<em>" ^
  geneweb_link conf
    ("m=TT;sm=S;t=" ^ code_varenv ident ^ ";p=" ^ code_varenv place)
    s ^
  "</em>"
;

value strong_referenced_person_title_text conf base p =
  if p.access <> Private || conf.friend || conf.wizard then
    match main_title base p with
    [ Some t ->
        "<strong>" ^ reference conf base p (titled_person_text conf base p t) ^
        "</strong>,\n" ^ title_reference conf base t
    | None ->
        "<strong>" ^ reference conf base p (person_text conf base p) ^
        "</strong>" ]
  else
    "<strong>" ^ reference conf base p (person_text conf base p) ^ "</strong>"
;

value get_link all_gp ip =
  loop all_gp where rec loop =
    fun
    [ [GP_person n ip0 _ :: gpl] ->
        if ip = ip0 then Some n else loop gpl
    | [gp :: gpl] -> loop gpl
    | [] -> None ]
;

value print_persons_parents conf base all_gp p =
  let a = aoi base p.cle_index in
  match a.parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let print ip =
        let p = poi base ip in        
        let link = get_link all_gp ip in
        do Wserver.wprint "%s"
              (strong_referenced_person_title_text conf base p);
           Wserver.wprint "%s" (Date.short_dates_text conf base p);
           match link with
           [ Some n ->
               do Wserver.wprint "\n(";
                  Wserver.wprint "%s " (transl conf "see");
                  print_link_long conf n;
                  Wserver.wprint ")";
               return ()
           | None -> () ];
        return ()
      in
      do Wserver.wprint ", %s"
           (transl_nth conf "son/daughter/child" (index_of_sex p.sex));
         Wserver.wprint " %s\n"
           (transl_decline conf "of (same or greater generation level)" "");
         print cpl.father;
         Wserver.wprint "\n%s\n" (transl conf "and");
         print cpl.mother;
      return ()
  | None -> () ]
;

value print_marriage_long conf base all_gp auth marr_nb p ifam =
  let fam = foi base ifam in
  let ispouse = spouse p (coi base ifam) in
  let spouse = poi base ispouse in
  let divorce = fam.divorce in
  let is = index_of_sex p.sex in
  let auth = auth && age_autorise conf base spouse in
  do let format =
       if fam.not_married && auth then ftransl conf "relationship%t to"
       else ftransl_nth conf "married%t to" is
     in
     Wserver.wprint (fcapitale format)
       (fun _ ->
          do match marr_nb with
             [ Some n -> Wserver.wprint " (%d)" n
             | None -> () ];
             if auth then Perso.print_marriage_text conf base False fam
             else ();
          return ());
     Wserver.wprint "\n";
     stag "strong" begin
       Wserver.wprint "%s"
         (reference conf base spouse (person_text conf base spouse));
     end;
     print_person_long_info conf base auth (get_link all_gp ispouse) spouse;
     print_persons_parents conf base all_gp spouse;
     Wserver.wprint ".\n";
     match divorce with
     [ Divorced d ->
         let d = Adef.od_of_codate d in
         do Wserver.wprint "\n%s" (capitale (transl conf "divorced"));
            match d with
            [ Some d when auth ->
                Wserver.wprint " %s" (Date.string_of_ondate conf d)
            | _ -> () ];
            Wserver.wprint ".\n";
         return ()
     | _ -> () ];
  return ()
;

value person_has_notes base p =
  sou base p.notes <> "" || sou base p.psources <> "" ||
  sou base p.birth_src <> "" || sou base p.baptism_src <> "" ||
  sou base p.death_src <> "" || sou base p.burial_src <> "" ||
  List.exists
    (fun ifam ->
       let fam = foi base ifam in
       sou base fam.marriage_src <> "" || sou base fam.marriage_src <> "")
    (Array.to_list p.family)
;
 
value has_notes conf base =
  List.exists
    (fun
     [ GP_person _ ip _ ->
         let p = poi base ip in
         age_autorise conf base p && person_has_notes base p
     | _ -> False ])
;

value print_other_marriages conf base ws all_gp auth ifamo p =
  if ws && (ifamo = None || Array.length p.family >= 2) then
    do for i = 0 to Array.length p.family - 1 do
         if ifamo = None || ifamo <> Some p.family.(i) then
           let marr_nb = if ifamo = None then None else Some (i + 1) in
           print_marriage_long conf base all_gp auth marr_nb p p.family.(i)
         else ();
       done;
    return ()
  else ()
;

value print_family_long conf base ws all_gp ifam nth =
  let fam = foi base ifam in
  let cpl = coi base ifam in
  let auth =
    age_autorise conf base (poi base cpl.father) &&
    age_autorise conf base (poi base cpl.mother)
  in
  do Wserver.wprint "<p>\n... ";
     Wserver.wprint "%s" (transl conf "having as children");
     match nth with
     [ Some (n, i) ->
         do Wserver.wprint " ";
            print_link_long conf n;
            Wserver.wprint "-(X%d)" i;
         return ()
     | None -> () ];
     Wserver.wprint ":\n<p>\n";
     let auth =
       List.for_all
         (fun ip -> age_autorise conf base (poi base ip))
         (Array.to_list fam.children)
     in
     tag "ol" "type=a" begin
       for i = 0 to Array.length fam.children - 1 do
         let ipc = fam.children.(i) in
         if ws || get_link all_gp ipc <> None then
           let pc = poi base ipc in
           let n = get_link all_gp ipc in
           do Wserver.wprint "<li>\n";
              stag "strong" begin
                if pc.surname = (poi base cpl.father).surname then
                  afficher_prenom_de_personne_referencee conf base
                    pc
                else
                  afficher_personne_referencee conf base pc;
              end;
              print_person_long_info conf base auth n pc;
              Wserver.wprint ".\n";
              match n with
              [ Some _ -> ()
              | None ->
                  for i = 0 to Array.length pc.family - 1 do
                    print_marriage_long conf base all_gp auth None
                      pc (pc.family.(i));
                  done ];
              if i <> Array.length fam.children - 1 then
                Wserver.wprint "<br>&nbsp;\n"
              else ();
           return ()
         else ();
       done;
     end;
  return ()
;

value print_other_families conf base all_gp excl_ifam moth_nb =
  let print ip n =
    let p = poi base ip in
    for i = 0 to Array.length p.family - 1 do
      let ifam = p.family.(i) in
      if ifam <> excl_ifam && Array.length (foi base ifam).children <> 0 then
        print_family_long conf base True all_gp ifam (Some (n, i + 1))
      else ();
    done
  in
  let cpl = coi base excl_ifam in
  do print cpl.father (Num.sub moth_nb (Num.one));
     print cpl.mother moth_nb;
  return ()
;

value print_generation_person_long conf base ws wn all_gp last_gen gp =
  match gp with
  [ GP_person n ip ifamo ->
      let p = poi base ip in
      do Wserver.wprint "<p>\n";
         match ifamo with
         [ Some ifam ->
             if not (Num.even n) then
               let fam = foi base ifam in
               let cpl = coi base ifam in
               let auth =
                 age_autorise conf base (poi base cpl.father) &&
                 age_autorise conf base (poi base cpl.mother)
               in
               do Wserver.wprint "... ";
                  let format =
                    if fam.not_married && auth then
                      ftransl conf "relationship%t to"
                    else ftransl_nth conf "married%t to" 0
                  in
                  Wserver.wprint format
                    (fun _ ->
                       if auth then
                         Perso.print_marriage_text conf base False fam
                       else ());
                  Wserver.wprint "...\n<p>\n";
               return ()
             else ()
         | _ -> () ];
         stag "strong" begin
           stag "a" "name=\"%s\"" (Num.to_string n) begin
             Num.print wpr (transl conf "(thousand separator)") n;
           end;
         end;
         Wserver.wprint ":\n";
         stag "strong" begin
           afficher_personne_referencee conf base p;
         end;
         print_person_long_info conf base (age_autorise conf base p) None p;
         Wserver.wprint ".\n";
         match (aoi base ip).parents with
         [ Some ifam ->
             let cpl = coi base ifam in
             let prec =
               if last_gen then
                 (get_link all_gp cpl.father, get_link all_gp cpl.mother)
               else
                 let n1 = Num.twice n in
                 (Some n1, Some (Num.inc n1 1))
             in
             match prec with
             [ (Some n1, Some n2) ->
                 do Wserver.wprint "%s: " (capitale (transl conf "parents"));
                    print_link_long conf n1;
                    Wserver.wprint " %s " (transl conf "and");
                    print_link_long conf n2;
                    Wserver.wprint ".\n";
                 return ()
             | _ -> () ]
         | None -> () ];
         if wn && age_autorise conf base p && person_has_notes base p then
           do Wserver.wprint "[%s "
                (capitale (transl_nth conf "note/notes" 0));
              stag "strong" begin
                stag "a" "href=\"#notes-%s\"" (Num.to_string n) begin
                  Num.print wpr (transl conf "(thousand separator)") n;
                end;
              end;
              Wserver.wprint "].\n";
           return ()
         else ();
         print_other_marriages conf base ws all_gp (age_autorise conf base p)
           ifamo p;
         match ifamo with
         [ Some ifam ->
             if not (Num.even n) then
               do print_family_long conf base ws all_gp ifam None;
                  if ws then print_other_families conf base all_gp ifam n
                  else ();
               return ()
             else ()
         | _ -> () ];
      return ()
  | GP_same n1 n2 ip ->
      let p = poi base ip in
      do Wserver.wprint "<p>\n";
         stag "strong" begin
           stag "a" "name=\"%s\"" (Num.to_string n1) begin
             Num.print wpr (transl conf "(thousand separator)") n1;
           end;
         end;
         Wserver.wprint ": %s " (transl conf "see");
         print_link_long conf n2;
         Wserver.wprint ".\n\n";
      return ()
  | _ -> () ]
;

value print_notes conf base =
  fun
  [ GP_person n ip _ ->
      let p = poi base ip in
      if age_autorise conf base p && person_has_notes base p then
        let notes = sou base p.notes in
        do Wserver.wprint "<dt>\n";
           stag "strong" begin
             stag "a" "name=\"notes-%s\"" (Num.to_string n) begin
               stag "a" "href=#%s" (Num.to_string n) begin
                 Num.print wpr (transl conf "(thousand separator)") n;
               end;
             end;
           end;
           Wserver.wprint ": \n<dd>\n";
           copy_string_with_macros conf notes;
           Perso.print_sources conf base (notes <> "") p;
           Wserver.wprint "<p>\n";
        return ()
      else ()
  | _ -> () ]
;

value afficher_ascendants_numerotation_long conf base niveau_max ws wn p =
  let mark = Array.create (base.data.persons.len) Num.zero in
  let rec get_generations niveau gpll gpl =
    let gpll = [gpl :: gpll] in
    if niveau < niveau_max then
      let next_gpl = next_generation base mark gpl in
      if List.exists will_print next_gpl then
        get_generations (niveau + 1) gpll next_gpl
      else gpll
    else gpll
  in
  let rec generation niveau all_gp =
    fun
    [ [gpl :: gpll] ->
        do tag "h3" begin
             Wserver.wprint "<em>%s %d</em>\n"
               (capitale (transl conf "generation")) niveau;
           end;
           List.iter
             (print_generation_person_long conf base ws wn all_gp (gpll = []))
             gpl;
           Wserver.wprint "<p>";
           html_br conf;
        return
        generation (niveau + 1) all_gp gpll
    | [] -> () ]
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  do header conf title;
     Wserver.wprint "%s.\n" (capitale (text_to conf niveau_max));
     mark.(Adef.int_of_iper p.cle_index) := Num.one;
     let gpll = get_generations 1 [] [GP_person Num.one p.cle_index None] in
     let gpll = List.rev gpll in
     let all_gp = List.flatten gpll in
     do generation 1 all_gp gpll;
        if wn && has_notes conf base all_gp then
          do Wserver.wprint "<p><hr><p>\n";
             Wserver.wprint "<h3>%s</h3>\n"
               (capitale (nominative (transl_nth conf "note/notes" 1)));
             tag "dl" begin
               List.iter (print_notes conf base) all_gp;
             end;
          return ()
        else ();
     return ();
     trailer conf;
  return ()
;

value print_ancestors_same_time_descendants conf base p a =
  let maxlen =
    match p_getint conf.env "l" with
    [ Some len -> len
    | None -> -1 ]
  in
  let predic =
    let tab = Array.create base.data.persons.len False in
    let rec mark_descendants len p =
      let i = Adef.int_of_iper p.cle_index in
      if maxlen > 0 && len > maxlen then ()
      else if tab.(i) then ()
      else
        do tab.(i) := True; return
        for i = 0 to Array.length p.family - 1 do
          let fam = foi base p.family.(i) in
          for i = 0 to Array.length fam.children - 1 do
            mark_descendants (len + 1) (poi base fam.children.(i));
          done;
        done
    in
    do mark_descendants 0 a; return
    fun ip -> tab.(Adef.int_of_iper ip)
  in
  let will_print =
    fun
    [ GP_person _ ip _ -> predic ip
    | GP_same _ _ _ -> False
    | _ -> False ]
  in
  let mark = Array.create (base.data.persons.len) Num.zero in
  let cnt = ref 0 in
  let rec generation niveau gpl =
    if List.exists will_print gpl then
      do html_li conf;
         Wserver.wprint "%s %d\n" (capitale (transl conf "generation")) niveau;
         tag "ul" begin
           List.iter
              (fun gp ->
                 if will_print gp then print_generation_person conf base cnt gp
                 else ())
               gpl;
         end;
      return
      let gpl = next_generation base mark gpl in
      generation (niveau + 1) gpl
    else ()
  in
  let title h =
    if h then
      Wserver.wprint "%s... %s..."
        (capitale (transl conf "ancestors"))
        (transl conf "up to")
    else
      Wserver.wprint "%s %s %s %s" (capitale (transl conf "ancestors"))
        (transl_decline conf "of" (gen_person_text raw_access conf base p))
        (transl conf "up to") (person_text conf base a)
  in
  do header conf title;
     conf.senv := [];
     tag "ul" begin
       mark.(Adef.int_of_iper p.cle_index) := Num.one;
       generation 1 [GP_person Num.one p.cle_index None];
     end;
     trailer conf;
  return ()
;

value afficher_ascendants_niveau conf base niveau_max p =
  let mark = Array.create (base.data.persons.len) Num.zero in
  let cnt = ref 0 in
  let rec generation niveau gpl =
    do for i = 0 to base.data.persons.len - 1 do
         mark.(i) := Num.zero;
       done;
    return
    if niveau < niveau_max then
      let gpl =
        List.map
          (fun gp ->
             match gp with
             [ GP_same n m ip ->
                 GP_interv (Some (n, Num.inc n 1, Some (m, Num.inc m 1)))
             | _ -> gp ])
          gpl
      in
      let gpl = next_generation base mark gpl in
      let gpl =
        List.fold_right
          (fun gp gpl ->
             match (gp, gpl) with
             [ (GP_interv (Some (n1, n2, x)),
                [GP_interv (Some (n3, n4, y)) :: gpl1]) ->
                 if Num.eq n2 n3 then
                   let z =
                     match (x, y) with
                     [ (Some (m1, m2), Some (m3, m4)) ->
                         if Num.eq m2 m3 then Some (m1, m4)
                         else None
                     | _ -> None ]
                   in
                   [GP_interv (Some (n1, n4, z)) :: gpl1]
                 else [GP_interv None :: gpl1]
             | (GP_interv _, [GP_interv _ :: gpl]) -> [GP_interv None :: gpl]
             | (GP_missing _ _, gpl) -> gpl
             | _ -> [gp :: gpl] ])
          gpl []
      in
      generation (niveau + 1) gpl
    else
      do html_li conf;
         Wserver.wprint "%s\n" (capitale (text_level conf niveau_max));
         tag "ul" begin
           List.iter (print_generation_person conf base cnt) gpl;
         end;
      return ()
  in
  let title h =
    if h then
      Wserver.wprint "%s %d\n" (capitale (transl conf "generation"))
        niveau_max
    else
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_decline conf "of" (gen_person_text raw_access conf base p))
  in
  do header conf title;
     tag "ul" begin
       mark.(Adef.int_of_iper p.cle_index) := Num.one;
       generation 1 [GP_person Num.one p.cle_index None];
     end;
     trailer conf;
  return ()
;

value print_generation_missing_persons conf base title sp_incl gp =
  let print_title () =        
    match title.val with
    [ Some level ->
       do html_li conf;
          Wserver.wprint "%s %d\n" (capitale (transl conf "generation")) level;
          Wserver.wprint "<ul>\n";
          title.val := None;
       return ()
    | _ -> () ]
  in
  match gp with
  [ GP_person n ip _ ->
      let p = poi base ip in
      if sp_incl &&
      p_first_name base p = "?" && p_surname base p = "?" then
        do print_title ();
           html_li conf;
           Num.print wpr (transl conf "(thousand separator)") n;
           Wserver.wprint " -\n";
           if Array.length p.family > 0 then
             let cpl = coi base p.family.(0) in
             let (parent_name_index, conj) =
               match p.sex with
               [ Male -> (0, cpl.mother)
               | _ -> (1, cpl.father) ]
             in
             do wprint_geneweb_link conf (acces conf base p)
                  (capitale
                     (transl_nth conf "husband/wife" parent_name_index));
                Wserver.wprint " %s\n" (transl_decline conf "of" "");
                afficher_personne_titre conf base (poi base conj);
                Date.afficher_dates_courtes conf base (poi base conj);
             return ()
           else
             do afficher_personne_titre_referencee conf base p;
                Date.afficher_dates_courtes conf base p;
             return ();
           Wserver.wprint "\n";
        return ()
      else ()
  | GP_missing n ip ->
      let p = poi base ip in
      if p_first_name base p = "?" && p_surname base p = "?" then ()
      else
        let n1 = Num.twice n in
        let n2 = Num.inc n1 1 in
        do print_title ();
           html_li conf;
           Num.print wpr (transl conf "(thousand separator)") n1;
           Wserver.wprint "-";
           Wserver.wprint "%d" (Num.modl n2 10);
           Wserver.wprint " -\n";
           if sp_incl then
             Wserver.wprint "%s %s" (capitale (transl conf "parents"))
               (transl_decline conf "of" "")
           else ();
           afficher_personne_titre_referencee conf base p;
           Date.afficher_dates_courtes conf base p;
           Wserver.wprint "\n";
        return ()
  | _ -> () ]
;

value one_year base p =
  match Adef.od_of_codate p.birth with
  [ Some (Dgreg d _) -> Some d.year
  | _ ->
      match Adef.od_of_codate p.baptism with
      [ Some (Dgreg d _) -> Some (annee d)
      | _ ->
          match date_of_death p.death with
          [ Some (Dgreg d _) -> Some (annee d)
          | _ ->
              match p.burial with
              [ Buried cod ->
                  match Adef.od_of_codate cod with
                  [ Some (Dgreg d _) -> Some (annee d)
                  | _ -> None ]
              | Cremated cod ->
                  match Adef.od_of_codate cod with
                  [ Some (Dgreg d _) -> Some (annee d)
                  | _ -> None ]
              | UnknownBurial -> None ] ] ] ]
;

value one_year_gp base =
  fun
  [ GP_person _ ip _ -> one_year base (poi base ip)
  | GP_same _ _ ip -> one_year base (poi base ip)
  | GP_interv _ -> None
  | GP_missing _ ip -> one_year base (poi base ip) ]
;

value print_missing_ancestors conf base v spouses_included p =
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  let mark = Array.create (base.data.persons.len) Num.zero in
  let rec generation niveau gpl =
    if niveau > v + 1 then ()
    else if gpl <> [] then
      let title = ref (Some niveau) in
      let gpl_to_print =
        List.fold_left
          (fun gpl gp ->
             match (after, before) with
             [ (Some a1, Some a2) ->
                 match one_year_gp base gp with
                 [ Some a ->
                    if a >= a1 && a <= a2 then [gp :: gpl]
                    else gpl
                 | None -> gpl ]
             | (Some a1, None) ->
                 match one_year_gp base gp with
                 [ Some a -> if a >= a1 then [gp :: gpl] else gpl
                 | None -> gpl ]
             | (None, Some a2) ->
                 match one_year_gp base gp with
                 [ Some a -> if a <= a2 then [gp :: gpl] else gpl
                 | None -> gpl ]
             | (None, None) -> [gp :: gpl] ])
          [] gpl
      in
      let gpl_to_print = List.rev gpl_to_print in
      do List.iter
           (print_generation_missing_persons conf base title spouses_included)
           gpl_to_print;
         if title.val = None then Wserver.wprint "</ul>\n" else ();
      return
      let gpl = next_generation base mark gpl in
      generation (niveau + 1) gpl
    else ()
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  do header conf title;
     Wserver.wprint "%s" (capitale (text_to conf v));
     match after with
     [ Some a -> Wserver.wprint " %s %d" (transl conf "after") a
     | None -> () ];
     match before with
     [ Some a -> Wserver.wprint " %s %d" (transl conf "before") a
     | None -> () ];
     Wserver.wprint ".\n";
     if not spouses_included then
       do html_br conf; html_br conf; return
       Wserver.wprint "%s %s...\n" (capitale (transl conf "parents"))
         (transl_decline conf "of" "")
     else ();  
     mark.(Adef.int_of_iper p.cle_index) := Num.one;
     tag "ul" begin
       generation 1 [GP_person Num.one p.cle_index None];
     end;
     trailer conf;
  return ()
;

type missing_type =
  [ A_person
  | A_surname_of_husband_of of string
  | A_surname_of_wife_of of string
  | A_husband_of
  | A_wife_of
  | A_parents_of ]
;

value add_missing conf base spouses_included list =
  fun
  [ GP_person n ip _ ->
      let p = poi base ip in
      if spouses_included && p_first_name base p = "?"
      && p_surname base p = "?" then
        if Array.length p.family > 0 then
          let cpl = coi base p.family.(0) in
          let (a, p) =
            match p.sex with
            [ Male -> (A_husband_of, poi base cpl.mother)
            | _ -> (A_wife_of, poi base cpl.father) ]
          in
          [(a, p) :: list]
        else [(A_person, p) :: list]
      else list
  | GP_missing n ip ->
      let p = poi base ip in
      if spouses_included
      && (p_surname base p = "?" || p_surname base p = "N..." )then
        if p_first_name base p = "?" then list
        else
          if Array.length p.family > 0 then
            let n = person_text_without_surname conf base p in
            let cpl = coi base p.family.(0) in
            let (a, p) =
              match p.sex with
              [ Male -> (A_surname_of_husband_of n, poi base cpl.mother)
              | _ -> (A_surname_of_wife_of n, poi base cpl.father) ]
            in
            if p_surname base p = "?" then list else [(a, p) :: list]
          else [(A_parents_of, p) :: list]
      else if p_surname base p = "?" || p_surname base p = "?" then
        list
      else [(A_parents_of, p) :: list]
  | _ -> list ]
;

value val_of_mt =
  fun
  [ A_person -> 0
  | A_surname_of_husband_of _ -> 1
  | A_surname_of_wife_of _ -> 2
  | A_husband_of -> 3
  | A_wife_of -> 4
  | A_parents_of -> 5 ]
;

value compare base (mt1, p1) (mt2, p2) =
  let c = alphabetique (p_surname base p1) (p_surname base p2) in
  if c == 0 then
    let c =
      alphabetique (p_first_name base p1) (p_first_name base p2)
    in
    if c == 0 then
      if p1 == p2 then val_of_mt mt1 < val_of_mt mt2
      else
        match (Adef.od_of_codate p1.birth, Adef.od_of_codate p2.birth) with
        [ (Some d1, Some d2) -> d1 strictement_avant d2
        | _ -> p1.occ < p2.occ ]
    else c < 0
  else c > 0
;

value print_missing_type conf =
  fun
  [ A_person -> ()
  | A_surname_of_husband_of x ->
      Wserver.wprint "%s %s %s"
        (transl_nth conf "surname/surnames" 0)
        (transl_decline conf "of" (transl_nth conf "his wife/her husband" 1)) x
  | A_surname_of_wife_of x ->
      Wserver.wprint "%s %s %s"
        (transl_nth conf "surname/surnames" 0)
        (transl_decline conf "of" (transl_nth conf "his wife/her husband" 0)) x
  | A_husband_of ->
      Wserver.wprint "%s" (transl_nth conf "husband/wife" 0) 
  | A_wife_of ->
      Wserver.wprint "%s" (transl_nth conf "husband/wife" 1)
  | A_parents_of ->
      Wserver.wprint "%s" (transl conf "parents") ]
;

value print_spouses conf base p =
  Array.iter
    (fun ifam ->
       let fam = foi base ifam in
       let cpl = coi base ifam in
       let sp = poi base (spouse p cpl) in
       if p_first_name base sp = "?" && p_surname base sp = "?" then ()
       else
         do Wserver.wprint "\n&amp;%s\n"
              (Date.short_marriage_date_text conf base fam p sp);
            afficher_personne_titre conf base sp;
            Date.afficher_dates_courtes conf base sp;
         return ())
    p.family
;

value print_someone_missing conf base begin_surname spouses_incl (mt, mtl, p) =
  do let href= "i=" ^ string_of_int (Adef.int_of_iper p.cle_index) in
     wprint_geneweb_link conf href (person_text_without_surname conf base p);
     Wserver.wprint "%s" begin_surname;
     afficher_titre conf base p;
     Date.afficher_dates_courtes conf base p;
     if spouses_incl then
       do Wserver.wprint "\n=&gt; ";
          print_missing_type conf mt;
          List.iter
            (fun mt ->
               do Wserver.wprint ", ";
                  print_missing_type conf mt;
               return ())
            mtl;
       return ()
     else
       print_spouses conf base p;
  return ()
;

value print_alphabetic_missing conf base spouses_included (surname, list) =
  do Wserver.wprint "%s " (surname_end surname);
     match list with
     [ [e] ->
         print_someone_missing conf base (surname_begin surname)
            spouses_included e
     | _ ->
        do Wserver.wprint "%s\n" (surname_begin surname);
           tag "ul" begin
             List.iter
               (fun e ->
                  do html_li conf;
                     print_someone_missing conf base "" spouses_included e;
                     Wserver.wprint "\n";
                  return ())
               list;
           end;
        return () ];
  return ()
;

value print_missing_ancestors_alphabetically conf base v spouses_included p =
  let mark = Array.create (base.data.persons.len) Num.zero in
  let rec generation list niveau gpl =
    if niveau > v then list
    else if gpl <> [] then
      let list =
        List.fold_left (add_missing conf base spouses_included) list gpl
      in
      let gpl = next_generation base mark gpl in
      generation list (niveau + 1) gpl
    else list
  in
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
      (transl_decline conf "of" (txt_fun raw_access conf base p))
  in
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  do header conf title;
     let list = generation [] 1 [GP_person Num.one p.cle_index None] in
     let list =
       List.fold_left
         (fun npl (n, p) ->
            match (after, before) with
            [ (Some a1, Some a2) ->
                match one_year base p with
                [ Some a ->
                   if a >= a1 && a <= a2 then [(n, p) :: npl]
                   else npl
                | None -> npl ]
            | (Some a1, None) ->
                match one_year base p with
                [ Some a -> if a >= a1 then [(n, p) :: npl] else npl
                | None -> npl ]
            | (None, Some a2) ->
                match one_year base p with
                [ Some a -> if a <= a2 then [(n, p) :: npl] else npl
                | None -> npl ]
            | (None, None) -> [(n, p) :: npl] ])
         [] list
     in
     let list = Sort.list (compare base) list in
     let list =
       List.fold_left
         (fun nell ((_, p) as elm) ->
            match nell with
            [ [(n, el) :: nell] when n = p_surname base p ->
                [(n, [elm :: el]) :: nell]
            | _ -> [(p_surname base p, [elm]) :: nell] ])
         [] list
     in
     let list =
       List.map
         (fun (n, el) ->
            let ell =
              List.fold_left
                (fun ell ((a, p) as e) ->
                   match ell with
                   [ [(a1, al, p1) :: el] when p1 == p ->
                       [(a, [a1:: al], p) :: el]
                   | _ -> [(a, [], p) :: ell] ])
                [] el
            in
            (n, ell))
         list
     in
     let initials =
       List.fold_left
         (fun l (n, _) ->
            let i = n.[initiale n] in
            match l with
            [ [] -> [i]
            | [x :: l'] -> if x = i then l else [i :: l] ])
         [] list
     in
     let print_initials =
       List.length initials > 3 && List.length list > 100
     in
     do if print_initials then
          do html_p conf;
             List.iter
                (fun i ->
                   do stag "a" "href=\"#%c\"" i begin
                        Wserver.wprint "%c" i;
                      end;
                      Wserver.wprint "\n";
                   return ())
                (List.rev initials);
             html_p conf;
          return ()
        else ();
        Wserver.wprint "%s" (capitale (text_to conf v));
        match after with
        [ Some a -> Wserver.wprint " %s %d" (transl conf "after") a
        | None -> () ];
        match before with
        [ Some a -> Wserver.wprint " %s %d" (transl conf "before") a
        | None -> () ];
        Wserver.wprint ".\n";
        if not spouses_included then
          do html_br conf; html_br conf; return
          Wserver.wprint "%s %s...\n" (capitale (transl conf "parents"))
            (transl_decline conf "of" "")
        else ();  
        tag "ul" begin
          let _ = List.fold_left
            (fun prev_i ((n, _) as e) ->
               let i = n.[initiale n] in
               do if print_initials then
                    match prev_i with
                    [ Some pi ->
                        if i <> pi then
                          do Wserver.wprint "</ul>\n";
                             html_li conf;
                             Wserver.wprint "<a name=\"%c\">%c</a>\n" i i;
                             Wserver.wprint "<ul>\n";
                          return ()
                        else ()
                    | None ->
                        do html_li conf;
                           Wserver.wprint "<a name=\"%c\">%c</a>\n" i i;
                           Wserver.wprint "<ul>\n";
                        return () ]
                  else ();
                  html_li conf;
                  print_alphabetic_missing conf base spouses_included e;
                  Wserver.wprint "\n";
               return Some i)
            None list
          in ();
          if print_initials then Wserver.wprint "</ul>\n" else ();
        end;
     return ();
     trailer conf;
  return ()
;

value tree_reference gv conf base p s =
  if conf.cancel_links then s
  else
    "<a href=\"" ^ commd conf ^ "m=A;t=T;v=" ^ string_of_int gv ^ ";" ^
    acces conf base p ^ "\">" ^ s ^ "</a>"
;

value someone_text conf base reference p =
  reference conf base p (person_title_text conf base p) ^
  Date.short_dates_text conf base p
;

value print_centered_on sz pos s =
  let len = pre_text_size s in
  let sh =
    if len / 2 < pos then
      if pos + len / 2 < sz then pos - len / 2
      else sz - 1 - len
    else 1
  in
  Wserver.wprint "%s%s\n" (String.make sh ' ') s
;

value print_tree_with_pre conf base v p =
  let sz = 79 in
  let v = min 3 v in
  let print_parents sz pf pm ifath imoth =
    let fath = poi base ifath in
    let moth = poi base imoth in
    do print_centered_on sz pm
         (someone_text conf base (tree_reference v) moth);
       print_centered_on sz pf
         (someone_text conf base (tree_reference v) fath);
    return ()
  in
  let str2 = String.make (sz / 2) in
  let str4 = String.make (sz / 4) in
  tag "pre" begin
     match (aoi base p.cle_index).parents with
     [ Some ifam ->
         let cpl = coi base ifam in
          do if v >= 3 then
              do match (aoi base cpl.mother).parents with
                 [ Some ifam ->
                     let cpl = coi base ifam in
                     print_parents sz (5 * sz / 8) (7 * sz / 8)
                       cpl.father cpl.mother
                 | None -> () ];
                 match (aoi base cpl.father).parents with
                 [ Some ifam ->
                     let cpl = coi base ifam in
                     print_parents sz (sz / 8) (3 * sz / 8)
                       cpl.father cpl.mother
                 | None -> () ];
                 print_pre_center sz 
                   ("|" ^ str4 ' ' ^ "|" ^ str4 ' ' ^ "|" ^ str4 ' ' ^ "|");
                 print_pre_center sz
                   (str4 '_' ^ " " ^ str4 ' ' ^ " " ^ str4 '_');
                 print_pre_center sz ("|" ^ str2 ' ' ^ "|");
              return ()
            else ();
            print_parents sz (sz / 4) (3 * sz / 4) cpl.father cpl.mother;
         return ()
     | None -> () ];
     print_pre_center sz ("|" ^ str2 ' ' ^ "|");
     print_pre_center sz (str2 '_');
     print_pre_center sz "|";
     print_pre_center sz (someone_text conf base reference p);
  end
;

value print_tree_with_table conf base gv p =
  let gv = min limit_by_tree gv in
  let next_gen pol =
    List.fold_right
      (fun po list ->
         match po with
         [ Some p ->
             match (aoi base p.cle_index).parents with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 let fath =
                   let p = poi base cpl.father in
                   if connais base p then Some p else None
                 in
                 let moth =
                   let p = poi base cpl.mother in
                   if connais base p then Some p else None
                 in
                 [fath; moth :: list]
             | _ -> [None; None :: list] ]
         | None -> [None; None :: list] ])
      pol []
  in
  let gen =
    loop (gv - 1) [Some p] [] where rec loop i gen list =
      if i == 0 then [gen :: list]
      else loop (i - 1) (next_gen gen) [gen :: list]
  in
  let down_reference p s =
    if conf.cancel_links then s
(*
    else if Array.length p.family > 0 then
      "<a href=\"" ^ commd conf ^ "m=D;t=T;v=1;k=" ^ string_of_int v ^ ";" ^
      acces conf base p ^ "\">" ^ s ^ "</a>"
*)
    else reference conf base p s
  in
  let colspan = fun [ 1 -> "" | n -> " colspan=" ^ string_of_int n ] in
  let print_ancestor gen n first po =
    do if not first then Wserver.wprint "<td>&nbsp;&nbsp;</td>\n"
       else ();
       stag "td" "align=center%s" (colspan n) begin
         let txt =
           match po with
           [ Some p ->
               let txt = person_title_text conf base p in
               let txt =
                 if List.length gen = 1 then down_reference p txt
                 else tree_reference gv conf base p txt
               in
               txt ^ Date.short_dates_text conf base p
           | _ -> "&nbsp;" ]
         in
         Wserver.wprint "%s" txt;
       end;
       Wserver.wprint "\n";
    return ()
  in
  let print_vertical_bars n cs =
    for i = 1 to n do
      stag "td" "align=center%s" (colspan cs) begin Wserver.wprint "|"; end;
      Wserver.wprint "\n";
      if i < n then Wserver.wprint "<td>&nbsp;</td>\n" else ();
    done
  in
  let print_horizontal_line n cs =
    for i = 1 to n do
      stag "td" "align=right%s" (colspan cs) begin
        Wserver.wprint "<hr noshade size=1 width=\"50%%\" align=right>";
      end;
      Wserver.wprint "\n";
      Wserver.wprint "<td><hr noshade size=1></td>\n";
      stag "td" "align=left%s" (colspan cs) begin
        Wserver.wprint "<hr noshade size=1 width=\"50%%\" align=left>";
      end;
      Wserver.wprint "\n";
      if i < n then Wserver.wprint "<td>&nbsp;</td>\n" else ();
    done
  in
  tag "table" "border=%d cellspacing=0 cellpadding=0 width=\"100%%\""
    conf.border
  begin
    let _ =
      List.fold_left
        (fun cs gen ->
           do if cs > 1 then
                let n = List.length gen in
                do tag "tr" begin print_vertical_bars (2 * n) (cs - 1); end;
                   tag "tr" begin print_horizontal_line n (cs - 1); end;
                   tag "tr" begin print_vertical_bars n (2 * cs - 1); end;
                return ()
              else ();
              tag "tr" begin
                let _ =
                  List.fold_left
                    (fun first po ->
                       do print_ancestor gen (2 * cs - 1) first po;
                       return False)
                    True gen
                in ();
              end;
           return 2 * cs)
        1 gen
    in
    ();
  end
;

value print_tree conf base v p =
  let title _ =
    Wserver.wprint "%s: %s" (capitale (transl conf "tree"))
      (person_text_no_html conf base p)
  in
  do header_no_page_title conf title;
     if browser_doesnt_have_tables conf then print_tree_with_pre conf base v p
     else print_tree_with_table conf base v p;
     trailer conf;
  return ()
;

value print conf base p =
  match (p_getenv conf.env "t", p_getint conf.env "v") with
  [ (Some "L", Some v) -> afficher_ascendants_jusqu_a conf base v p
  | (Some "N", Some v) -> afficher_ascendants_numerotation conf base v p
  | (Some "G", Some v) ->
      let ws =
        match p_getenv conf.env "siblings" with
        [ Some "on" -> True
        | _ -> False ]
      in
      let wn =
        match p_getenv conf.env "notes" with
        [ Some "on" -> True
        | _ -> False ]
      in
      afficher_ascendants_numerotation_long conf base v ws wn p
  | (Some "S", Some v) -> afficher_ascendants_niveau conf base v p
  | (Some "M", Some v) ->
      let al =
        match p_getenv conf.env "al" with
        [ Some "on" -> True
        | _ -> False ]
      in
      let si =
        match p_getenv conf.env "ms" with
        [ Some "on" -> True
        | _ -> False ]
      in
      if al then print_missing_ancestors_alphabetically conf base v si p
      else print_missing_ancestors conf base v si p
  | (Some "T", Some v) -> print_tree conf base v p
  | (Some "D", x) ->
      match (find_person_in_env conf base "1", x) with
      [ (Some anc, _) ->
          print_ancestors_same_time_descendants conf base p anc
      | (_, Some v) -> (* compatibility *)
          print_ancestors_same_time_descendants conf base p
            (base.data.persons.get v)
      | _ -> afficher_menu_ascendants conf base p ]
  | _ -> afficher_menu_ascendants conf base p ]
;
