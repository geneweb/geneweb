(* camlp4r ./def.syn.cmo *)
(* $Id: some.ml,v 2.9 1999-07-15 17:13:22 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Gutil;
open Config;
open Util;

value first_name_not_found conf x =
  let title _ =
    Wserver.wprint "%s: \"%s\""
      (capitale (transl conf "first name not found")) x
  in
  do header conf title; trailer conf; return ()
;

value surname_not_found conf x =
  let title _ =
    Wserver.wprint "%s: \"%s\"" (capitale (transl conf "surname not found"))
      x
  in
  do header conf title; trailer conf; return ()
;

value persons_of_fsname base find proj x =
  let istrl = base.func.strings_of_fsname x in
  let l =
    let x = Name.crush_lower x in
    List.fold_right
      (fun istr l ->
         let str = sou base istr in
         if Name.crush_lower str = x
         || List.mem x (List.map Name.crush_lower (surnames_pieces str)) then
           let iperl = find istr in
           let iperl =
             List.fold_left
               (fun iperl iper ->
                  if proj (poi base iper) = istr then [iper :: iperl]
                  else iperl)
               [] iperl
           in
           if iperl = [] then l else [(str, istr, iperl) :: l]
         else l)
      istrl []
  in
  let (l, name_inj) =
    let (l1, name_inj) =
      let x = Name.lower x in
      (List.fold_right
         (fun (str, istr, iperl) l ->
            if x = Name.lower str then [(str, istr, iperl) :: l] else l)
         l [],
       Name.lower)
    in
    let (l1, name_inj) =
      if l1 = [] then
        let x = Name.strip_lower x in
        (List.fold_right
           (fun (str, istr, iperl) l ->
              if x = Name.strip_lower str then [(str, istr, iperl) :: l]
              else l)
            l [],
         Name.strip_lower)
      else (l1, name_inj)
    in
    if l1 = [] then (l, Name.crush_lower) else (l1, name_inj)
  in
  (l, name_inj)
;

value print_elem conf base is_surname (p, xl) =
  match xl with
  [ [x] ->
      do Wserver.wprint "<a href=\"%s%s\">" (commd conf) (acces conf base x);
         if is_surname then
           Wserver.wprint "%s%s" (surname_end p) (surname_begin p)
         else Wserver.wprint "%s" p;
         Wserver.wprint "</a>\n";
         Date.afficher_dates_courtes conf base x;
      return ()
  | _ ->
      let _ =
        List.fold_left
          (fun first x ->
             do if not first then html_li conf else ();
                Wserver.wprint "<a href=\"%s%s\">" (commd conf)
                  (acces conf base x);
                if is_surname then
                  Wserver.wprint "%s%s" (surname_end p) (surname_begin p)
                else Wserver.wprint "%s" p;
                Wserver.wprint "</a>";
                Date.afficher_dates_courtes conf base x;
                Wserver.wprint " <em>";
                preciser_homonyme conf base x;
                Wserver.wprint "</em>\n";
             return False)
          True xl
      in
      () ]
;

value first_name_print_list conf base xl liste =
  let liste =
    let l =
      Sort.list
        (fun x1 x2 ->
           match alphabetique (sou base x1.surname) (sou base x2.surname) with
           [ 0 ->
               match
                 (Adef.od_of_codate x1.birth, Adef.od_of_codate x2.birth)
               with
               [ (Some d1, Some d2) -> d1 strictement_apres d2
               | (Some d1, _) -> False
               | _ -> True ]
           | n -> n > 0 ])
        liste
    in
    List.fold_left
      (fun l x ->
         let px = sou base x.surname in
         match l with
         [ [(p, l1) :: l] when alphabetique px p == 0 -> [(p, [x :: l1]) :: l]
         | _ -> [(px, [x]) :: l] ])
      [] l
  in
  let title _ =
    do Wserver.wprint "%s" (List.hd xl);
       List.iter (fun x -> Wserver.wprint ", %s" x) (List.tl xl);
    return ()
  in
  do header conf title;
     print_link_to_welcome conf True;
     print_alphab_list conf (fun (p, _) -> String.sub p (initiale p) 1)
       (print_elem conf base True) liste;
     trailer conf;
  return ()
;

value select_first_name conf base n list =
  let title _ =
    Wserver.wprint "%s \"%s\" : %s"
      (capitale (transl_nth conf "first name/first names" 0)) n
      (transl conf "specify")
  in
  do header conf title;
     Wserver.wprint "<ul>";
     List.iter
       (fun (sstr, (strl, _)) ->
          do Wserver.wprint "\n";
             html_li conf;
             Wserver.wprint "<a href=\"%sm=P;v=%s\">"
               (commd conf) (code_varenv sstr);
             Wserver.wprint "%s" (List.hd strl);
             List.iter (fun s -> Wserver.wprint ", %s" s)
               (List.tl strl);
             Wserver.wprint "</a>\n";
          return ())
       list;
     Wserver.wprint "</ul>\n";
     trailer conf;
  return ()
;

value rec merge_insert ((sstr, (strl, iperl)) as x) =
  fun
  [ [((sstr1, (strl1, iperl1)) as y) :: l] ->
      if sstr < sstr1 then [x; y :: l]
      else if sstr > sstr1 then [y :: merge_insert x l]
      else [(sstr, (strl @ strl1, iperl @ iperl1)) :: l]
  | [] -> [x] ]
;

value first_name_print conf base x =
  let (list, _) =
    persons_of_fsname base base.func.persons_of_first_name.find
      (fun x -> x.first_name) x
  in
  let list =
    List.map (fun (str, istr, iperl) -> (Name.strip_lower str, ([str], iperl)))
      list
  in
  let list = List.fold_right merge_insert list [] in
  match list with
  [ [] -> first_name_not_found conf x
  | [(_, (strl, iperl))] ->
      first_name_print_list conf base strl (List.map (poi base) iperl)
  | _ -> select_first_name conf base x list ]
;

value she_has_children_with_her_name base wife husband children =
  let wife_surname = Name.strip_lower (sou base wife.surname) in
  if Name.strip_lower (sou base husband.surname) = wife_surname then
    False
  else
    List.exists
      (fun c ->
         Name.strip_lower (sou base (poi base c).surname) = wife_surname)
      (Array.to_list children)
;

value afficher_date_mariage conf base p c dmar =
  if age_autorise conf base p && age_autorise conf base c then
    match dmar with
    [ Some d ->
        do Wserver.wprint "<font size=-2>";
           Date.display_year d;
           Wserver.wprint "</font>";
        return ()
    | None -> () ]
  else ()
;

value max_lev = 3;

value rec print_branch conf base lev name p =
  do if lev == 0 then html_br conf else html_li conf;
     Wserver.wprint "<strong>";
     if sou base p.surname = name then
       afficher_prenom_de_personne_referencee conf base p
     else afficher_personne_referencee conf base p;
     Wserver.wprint "</strong>";
     Date.afficher_dates_courtes conf base p;
     Wserver.wprint "\n";
  return
  if Array.length p.family == 0 then ()
  else
    let _ = List.fold_left
      (fun (first, need_br) ifam ->
         let fam = foi base ifam in
         let dmar = Adef.od_of_codate fam.marriage in
         let c = spouse p (coi base ifam) in
         let el = fam.children in
         let c = poi base c in
         do if need_br then html_br conf else ();
            if not first then
              do Wserver.wprint "<em>";
                 if sou base p.surname = name then
                   afficher_prenom_de_personne conf base p
                 else afficher_personne conf base p;
                 Wserver.wprint "</em>";
                 Date.afficher_dates_courtes conf base p;
                 Wserver.wprint "\n";
              return ()
            else ();
            Wserver.wprint "  &amp;";
            afficher_date_mariage conf base p c dmar;
            Wserver.wprint " <strong>";
            afficher_personne_referencee conf base c;
            Wserver.wprint "</strong>";
            Date.afficher_dates_courtes conf base c;
            Wserver.wprint "\n";
         return
         let down =
           p.sex = Male &&
           (Name.crush_lower (sou base p.surname) = Name.crush_lower name
            || lev == 0) &&
           Array.length el <> 0 ||
           p.sex = Female && she_has_children_with_her_name base p c el
         in
         if down then
           do Wserver.wprint "<ul>\n";
              List.iter
                (fun e -> print_branch conf base (succ lev) name (poi base e))
                (Array.to_list el);
              Wserver.wprint "</ul>\n";
           return (False, not down)
         else (False, not down))
      (True, False) (Array.to_list p.family)
    in ()
;

value rec print_by_branch x conf base (ipl, homonymes) =
  let l = List.map (poi base) ipl in
  let ancestors =
    Sort.list
      (fun p1 p2 ->
         alphabetique (sou base p1.first_name) (sou base p2.first_name) <= 0)
      l
  in
  let len = List.length ancestors in
  if len == 0 then surname_not_found conf x
  else
    let x =
      match homonymes with
      [ [x :: _] -> x
      | _ -> x ]
    in
    let title h =
      let access x =
        if h || List.length homonymes = 1 then x
        else
          geneweb_link conf ("m=N;v=" ^ code_varenv (Name.lower x))
            x
      in
      do Wserver.wprint "%s" (access (List.hd homonymes));
         List.iter (fun x -> Wserver.wprint ", %s" (access x))
           (List.tl homonymes);
      return ()
    in
    do header conf title;
       print_link_to_welcome conf True;
       Wserver.wprint "<font size=-1><em>\n";
       Wserver.wprint "%s " (capitale (transl conf "click"));
       Wserver.wprint "<a href=\"%sm=N;o=i;v=%s\">%s</a>\n" (commd conf)
         (code_varenv x) (transl conf "here");
       Wserver.wprint "%s"
         (transl conf "for the first names by alphabetic order");
       Wserver.wprint ".</em></font>\n";
       html_p conf;
       Wserver.wprint "<nobr>\n";
       if len > 1 then
         do Wserver.wprint "%s: %d"
             (capitale (transl conf "number of branches")) len;
            html_p conf;
            Wserver.wprint "<ol>\n";
         return ()
       else ();
       let _ = List.fold_left
         (fun n p ->
            do if len > 1 then html_li conf else ();
               print_branch conf base 0 x p;
            return n + 1)
         1 ancestors
       in ();
       if len > 1 then Wserver.wprint "</ol>\n" else ();
       Wserver.wprint "</nobr>\n";
       trailer conf;
    return ()
;

value print_family_alphabetic x conf base liste =
  let liste =
    let l =
      Sort.list
        (fun x1 x2 ->
           match
             alphabetique (sou base x1.first_name) (sou base x2.first_name)
           with
           [ 0 -> x1.occ > x2.occ
           | n -> n > 0 ])
        liste
    in
    List.fold_left
      (fun l x ->
         let px = sou base x.first_name in
         match l with
         [ [(p, l1) :: l] when alphabetique px p == 0 -> [(p, [x :: l1]) :: l]
         | _ -> [(px, [x]) :: l] ])
      [] l
  in
  match liste with
  [ [] -> surname_not_found conf x
  | _ ->
      let title _ = Wserver.wprint "%s" x in
      do header conf title;
         print_link_to_welcome conf True;
         print_alphab_list conf (fun (p, _) -> String.sub p (initiale p) 1)
           (print_elem conf base False) liste;
         trailer conf;
      return () ]
;

value has_at_least_2_children_with_surname base fam surname =
  loop 0 0 where rec loop cnt i =
    if i == Array.length fam.children then False
    else
      let p = poi base fam.children.(i) in
      if p.surname == surname then
        if cnt == 1 then True
        else loop (cnt + 1) (i + 1)
      else loop cnt (i + 1)
;

value select_ancestors base name_inj ipl =
  let str_inj s = name_inj (sou base s) in
  List.fold_left
    (fun ipl ip ->
       let p = poi base ip in
       let a = aoi base ip in
       match a.parents with
       [ Some ifam ->
           let cpl = coi base ifam in
           let fath = poi base cpl.father in
           let moth = poi base cpl.mother in
           let s = str_inj p.surname in
           if str_inj fath.surname <> s && str_inj moth.surname <> s
           && not (List.memq ip ipl) then
             if List.memq cpl.father ipl then ipl
             else if
               has_at_least_2_children_with_surname base (foi base ifam)
                 p.surname
             then [cpl.father :: ipl]
             else [ip :: ipl]
           else ipl
       | _ -> [ip :: ipl] ])
    [] ipl
;

value surname_print conf base x =
  let (l, name_inj) =
    persons_of_fsname base base.func.persons_of_surname.find
      (fun x -> x.surname) x
  in
  let (iperl, strl) =
    List.fold_right
      (fun (str, istr, iperl1) (iperl, strl)  ->
         (iperl1 @ iperl, [str :: strl]))
      l ([], [])
  in
  match p_getenv conf.env "o" with
  [ Some "i" ->
      let liste =
        List.fold_right (fun ip ipl -> [poi base ip :: ipl]) iperl []
      in
      print_family_alphabetic x conf base liste
  | _ ->
      let iperl = select_ancestors base name_inj iperl in
      print_by_branch x conf base (iperl, strl) ]
;
