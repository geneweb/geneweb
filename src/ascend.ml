(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: ascend.ml,v 1.3 1998-11-12 10:59:34 ddr Exp $ *)

open Config;
open Def;
open Gutil;
open Util;

value limit_by_list = 8;

value niveau_max_ascendance base ip =
  let x = ref 0 in
  let mark = Array.create base.persons.len False in
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
    if conf.wizard && conf.friend then
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
        if i > niveau_effectif + 1 then ()
        else
          do Wserver.wprint "  <option value=%d%s> %s\n" i
               (if i == 0 then " selected" else "")
               (capitale (text_to conf i));
          return boucle (succ i)
      in
      boucle 1;
    end;
    tag "ul" begin
      Wserver.wprint "<li> <input type=radio name=t value=N checked> %s\n"
        (capitale (transl conf "Sosa numbers"));
      Wserver.wprint "<li> <input type=radio name=t value=L> %s%t\n"
        (capitale (transl conf "list"))
        (fun oc ->
          if niveau_effectif <= limit_by_list then ()
          else
            do Printf.fprintf oc " (";
               Printf.fprintf oc (ftransl conf "max %d generations")
                 limit_by_list;
               Printf.fprintf oc ")";
            return ());
      Wserver.wprint "<li> <input type=radio name=t value=S> %s\n"
        (capitale (transl conf "only the generation selected"));
   end;
   Wserver.wprint "<p>\n";
   tag "ul" begin
      Wserver.wprint "<li> <input type=radio name=t value=M> %s\n"
        (capitale (transl conf "missing ancestors"));
      Wserver.wprint "<li> <input type=radio name=t value=A> %s (%s)\n"
        (capitale (transl conf "missing ancestors"))
        (transl conf "alphabetic order");
      Wserver.wprint "<br>\n";
      Wserver.wprint "%s\n" (capitale (transl conf "after"));
      Wserver.wprint "<input name=after size=5 maxlength=5>\n";
      Wserver.wprint "%s\n" (capitale (transl conf "before"));
      Wserver.wprint "<input name=before size=5 maxlength=5>\n";
    end;
    Wserver.wprint "<p>\n";
    Wserver.wprint "<input type=submit value=\"Ok\"><br>\n";
  end
;

value afficher_menu_ascendants conf base p =
  let niveau_effectif = niveau_max_ascendance base p.cle_index in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text conf base p))
  in
  do header conf title;
     print_choice conf base p niveau_effectif;
     trailer conf;
  return ()
;

value afficher_ancetre conf base x p =
  do afficher_personne_referencee conf base p;
     Date.afficher_dates_courtes conf base p;
  return ()
;

value afficher_ascendants_jusqu_a conf base niveau_max p =
  let niveau_max = min limit_by_list niveau_max in
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
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text conf base p))
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
  [ GP_person of Num.t and iper
  | GP_same of Num.t and Num.t and iper
  | GP_missing of Num.t and iper ]
;

value next_generation base mark gpl =
  let gpl =
    List.fold_right
      (fun gp gpl ->
         match gp with
         [ GP_person n ip ->
             let n_fath = Num.twice n in
             let n_moth = Num.inc n_fath 1 in
             let a = aoi base ip in
             match a.parents with
             [ Some ifam ->
                 let cpl = coi base ifam in
                 [GP_person n_fath cpl.father; GP_person n_moth cpl.mother ::
                  gpl]
             | None -> [GP_missing n ip :: gpl] ]
         | _ -> gpl ])
      gpl []
  in
  let gpl =
    List.fold_left
      (fun gpl gp ->
         match gp with
         [ GP_person n ip ->
             let i = Adef.int_of_iper ip in
             let m = mark.(i) in
             if Num.eq m Num.zero then do mark.(i) := n; return [gp :: gpl]
             else [GP_same n m ip :: gpl]
         | _ -> [gp :: gpl] ])
      [] gpl
  in
  List.rev gpl
;

value print_generation_person conf base gp =
  match gp with
  [ GP_person n ip ->
      let p = poi base ip in
      do Wserver.wprint "<li> ";
         Num.print (transl conf "(thousand separator)") n;
         Wserver.wprint " -\n";
         afficher_personne_titre_referencee conf base p;
         Date.afficher_dates_courtes conf base p;
         Wserver.wprint "\n";
      return ()
  | GP_same n1 n2 ip ->
      let p = poi base ip in
      do Wserver.wprint "<li> ";
         Num.print (transl conf "(thousand separator)") n1;
         Wserver.wprint " =&gt; ";
         stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
           Num.print (transl conf "(thousand separator)") n2;
         end;
         Wserver.wprint "\n\n";
      return ()
  | _ -> () ]
;

value will_print =
  fun
  [ GP_person _ _ -> True
  | GP_same _ _ _ -> True
  | _ -> False ]
;

value afficher_ascendants_numerotation conf base niveau_max p =
  let mark = Array.create (base.persons.len) Num.zero in
  let rec generation niveau gpl =
    if niveau <= niveau_max then
      do Wserver.wprint "<li>%s %s\n"
           (transl_nth conf "nth (generation)" niveau)
           (transl conf "generation");
         tag "ul" begin
           List.iter (print_generation_person conf base) gpl;
         end;
      return
      let gpl = next_generation base mark gpl in
      if List.exists will_print gpl then generation (niveau + 1) gpl else ()
    else ()
  in
  let title h =
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text conf base p))
  in
  do header conf title;
     Wserver.wprint "%s.\n" (capitale (text_to conf niveau_max));
     tag "ul" begin
       mark.(Adef.int_of_iper p.cle_index) := Num.one;
       generation 1 [GP_person Num.one p.cle_index];
     end;
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
    let tab = Array.create base.persons.len False in
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
    [ GP_person _ ip -> predic ip
    | GP_same _ _ _ -> False
    | _ -> False ]
  in
  let mark = Array.create (base.persons.len) Num.zero in
  let rec generation niveau gpl =
    if List.exists will_print gpl then
      do Wserver.wprint "<li>%s %s\n"
           (transl_nth conf "nth (generation)" niveau)
           (transl conf "generation");
         tag "ul" begin
           List.iter
              (fun gp ->
                 if will_print gp then print_generation_person conf base gp
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
        (transl_concat conf "of" (person_text conf base p))
        (transl conf "up to") (person_text conf base a)
  in
  do header conf title;
     conf.senv := "";
     tag "nobr" begin
       tag "ul" begin
         mark.(Adef.int_of_iper p.cle_index) := Num.one;
         generation 1 [GP_person Num.one p.cle_index];
       end;
     end;
     trailer conf;
  return ()
;

value afficher_ascendants_niveau conf base niveau_max p =
  let mark = Array.create (base.persons.len) Num.zero in
  let rec generation niveau gpl =
    do for i = 0 to base.persons.len - 1 do
         mark.(i) := Num.zero;
       done;
    return
    if niveau < niveau_max then
      generation (niveau + 1) (next_generation base mark gpl)
    else
      do Wserver.wprint "<li>%s\n" (capitale (text_level conf niveau_max));
         tag "ul" begin
           List.iter (print_generation_person conf base) gpl;
         end;
      return ()
  in
  let title h =
    if h then
      Wserver.wprint "%s %s\n"
        (transl_nth conf "nth (generation)" niveau_max)
        (transl conf "generation")
    else
      Wserver.wprint "%s %s" (capitale (transl conf "ancestors"))
        (transl_concat conf "of" (person_text conf base p))
  in
  do header conf title;
     tag "nobr" begin
       tag "ul" begin
         mark.(Adef.int_of_iper p.cle_index) := Num.one;
         generation 1 [GP_person Num.one p.cle_index];
       end;
     end;
     trailer conf;
  return ()
;

value print_generation_missing_persons conf base title sp_incl gp =
  let print_title () =        
    match title.val with
    [ Some level ->
       do Wserver.wprint "<li>%s %s\n"
            (transl_nth conf "nth (generation)" level)
            (transl conf "generation");
          Wserver.wprint "<ul>\n";
          title.val := None;
       return ()
    | _ -> () ]
  in
  match gp with
  [ GP_person n ip ->
      let p = poi base ip in
      if sp_incl &&
      sou base p.first_name = "?" && sou base p.surname = "?" then
        do print_title ();
           Wserver.wprint "<li> ";
           Num.print (transl conf "(thousand separator)") n;
           Wserver.wprint " -\n";
           if Array.length p.family > 0 then
             let cpl = coi base p.family.(0) in
             let (parent_name_index, conj) =
               match p.sexe with
               [ Masculin -> (0, cpl.mother)
               | _ -> (1, cpl.father) ]
             in
             do stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
                  Wserver.wprint "%s"
                    (capitale
                       (transl_nth conf "husband/wife" parent_name_index));
                end;
                Wserver.wprint " %s\n" (transl_nth conf "of" 0);
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
      if sou base p.first_name = "?" && sou base p.surname = "?" then ()
      else
        let n1 = Num.twice n in
        let n2 = Num.inc n1 1 in
        do print_title ();
           Wserver.wprint "<li> ";
           Num.print (transl conf "(thousand separator)") n1;
           Wserver.wprint "-";
           Wserver.wprint "%d" (Num.modl n2 10);
           Wserver.wprint " -\n";
           if sp_incl then
             Wserver.wprint "%s %s " (capitale (transl conf "parents"))
               (transl_nth conf "of" 0)
           else ();
           afficher_personne_titre_referencee conf base p;
           Date.afficher_dates_courtes conf base p;
           Wserver.wprint "\n";
        return ()
  | _ -> () ]
;

value one_year base p =
  match Adef.od_of_codate p.birth with
  [ Some d -> Some (annee d)
  | None ->
      match Adef.od_of_codate p.baptism with
      [ Some d -> Some (annee d)
      | None ->
          match p.death with
          [ Death _ cd -> Some (annee (Adef.date_of_cdate cd))
          | _ ->
              match p.burial with
              [ Buried cod ->
                  match Adef.od_of_codate cod with
                  [ Some d -> Some (annee d)
                  | None -> None ]
              | Cremated cod ->
                  match Adef.od_of_codate cod with
                  [ Some d -> Some (annee d)
                  | None -> None ]
              | UnknownBurial -> None ] ] ] ]
;

value one_year_gp base =
  fun
  [ GP_person _ ip -> one_year base (poi base ip)
  | GP_same _ _ ip -> one_year base (poi base ip)
  | GP_missing _ ip -> one_year base (poi base ip) ]
;

value print_missing_ancestors conf base v spouses_included p =
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  let mark = Array.create (base.persons.len) Num.zero in
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
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
        (transl_concat conf "of" (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
        (transl_concat conf "of" (person_text conf base p))
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
       Wserver.wprint "<br>\n%s %s:\n" (capitale (transl conf "parents"))
         (transl_nth conf "of" 0)
     else ();  
     mark.(Adef.int_of_iper p.cle_index) := Num.one;
     tag "ul" begin
       generation 1 [GP_person Num.one p.cle_index];
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
  [ GP_person n ip ->
      let p = poi base ip in
      if spouses_included && sou base p.first_name = "?"
      && sou base p.surname = "?" then
        if Array.length p.family > 0 then
          let cpl = coi base p.family.(0) in
          let (a, p) =
            match p.sexe with
            [ Masculin -> (A_husband_of, poi base cpl.mother)
            | _ -> (A_wife_of, poi base cpl.father) ]
          in
          [(a, p) :: list]
        else [(A_person, p) :: list]
      else list
  | GP_missing n ip ->
      let p = poi base ip in
      if spouses_included
      && (sou base p.surname = "?" || sou base p.surname = "N..." )then
        if sou base p.first_name = "?" then list
        else
          if Array.length p.family > 0 then
            let n = person_text_without_surname conf base p in
            let cpl = coi base p.family.(0) in
            let (a, p) =
              match p.sexe with
              [ Masculin -> (A_surname_of_husband_of n, poi base cpl.mother)
              | _ -> (A_surname_of_wife_of n, poi base cpl.father) ]
            in
            if sou base p.surname = "?" then list else [(a, p) :: list]
          else [(A_parents_of, p) :: list]
      else if sou base p.surname = "?" || sou base p.surname = "?" then
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
  let c = alphabetique (sou base p1.surname) (sou base p2.surname) in
  if c == 0 then
    let c =
      alphabetique (sou base p1.first_name) (sou base p2.first_name)
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
      Wserver.wprint "%s %s %s %s"
        (transl_nth conf "surname/surnames" 0)
        (transl_nth conf "of" 0)
        (transl_nth conf "his wife/her husband" 1) x
  | A_surname_of_wife_of x ->
      Wserver.wprint "%s %s %s %s"
        (transl_nth conf "surname/surnames" 0)
        (transl_nth conf "of" 0)
        (transl_nth conf "his wife/her husband" 0) x
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
       let sp = poi base (conjoint p cpl) in
       if sou base sp.first_name = "?" && sou base sp.surname = "?" then ()
       else
         do (*Wserver.wprint "<br>\n&nbsp;&nbsp;&nbsp;&nbsp;";*)
            Wserver.wprint "\n&amp;";
            match Adef.od_of_codate fam.marriage with
            [ Some d -> stag "font" "size=-2" begin Date.display_year d; end
            | None -> () ];
            Wserver.wprint "\n";
            afficher_personne_titre conf base sp;
            Date.afficher_dates_courtes conf base sp;
         return ())
    p.family
;

value print_someone_missing conf base begin_surname spouses_incl (mt, mtl, p) =
  do stag "a" "href=\"%si=%d\"" (commd conf) (Adef.int_of_iper p.cle_index)
     begin
       Wserver.wprint "%s" (person_text_without_surname conf base p);
     end;
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
                  do Wserver.wprint "<li> ";
                     print_someone_missing conf base "" spouses_included e;
                     Wserver.wprint "\n";
                  return ())
               list;
           end;
        return () ];
  return ()
;

value print_missing_ancestors_alphabetically conf base v spouses_included p =
  let mark = Array.create (base.persons.len) Num.zero in
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
    if h then
      Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
        (transl_concat conf "of" (person_text_no_html conf base p))
    else
      Wserver.wprint "%s %s" (capitale (transl conf "missing ancestors"))
        (transl_concat conf "of" (person_text conf base p))
  in
  let after = p_getint conf.env "after" in
  let before = p_getint conf.env "before" in
  do header conf title;
     let list = generation [] 1 [GP_person Num.one p.cle_index] in
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
            [ [(n, el) :: nell] when n == sou base p.surname ->
                [(n, [elm :: el]) :: nell]
            | _ -> [(sou base p.surname, [elm]) :: nell] ])
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
          do Wserver.wprint "<p>\n";
             List.iter
                (fun i ->
                   do stag "a" "href=\"#%c\"" i begin
                        Wserver.wprint "%c" i;
                      end;
                      Wserver.wprint "\n";
                   return ())
                (List.rev initials);
             Wserver.wprint "<p>\n";
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
          Wserver.wprint "<br>\n%s %s:\n" (capitale (transl conf "parents"))
            (transl_nth conf "of" 0)
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
                             Wserver.wprint "<li> <a name=\"%c\">%c</a>\n" i i;
                             Wserver.wprint "<ul>\n";
                          return ()
                        else ()
                    | None ->
                        do Wserver.wprint "<li> <a name=\"%c\">%c</a>\n" i i;
                           Wserver.wprint "<ul>\n";
                        return () ]
                  else ();
                  Wserver.wprint "<li> ";
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

value print conf base p =
  match (p_getenv conf.env "t", p_getint conf.env "v") with
  [ (Some "L", Some v) -> afficher_ascendants_jusqu_a conf base v p
  | (Some "N", Some v) -> afficher_ascendants_numerotation conf base v p
  | (Some "S", Some v) -> afficher_ascendants_niveau conf base v p
  | (Some "M", Some v) ->
      let si =
        match p_getenv conf.env "ms" with
        [ Some "on" -> True
        | _ -> False ]
      in
      print_missing_ancestors conf base v si p
  | (Some "A", Some v) ->
      let si =
        match p_getenv conf.env "ms" with
        [ Some "on" -> True
        | _ -> False ]
      in
      print_missing_ancestors_alphabetically conf base v si p
  | (Some "D", Some v) ->
      print_ancestors_same_time_descendants conf base p (base.persons.get v)
  | _ -> afficher_menu_ascendants conf base p ]
;

value incorrect_request conf =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "incorrect request"))
  in
  do header conf title; trailer conf; return ()
;
