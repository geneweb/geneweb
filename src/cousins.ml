(* camlp4r ./pa_html.cmo *)
(* $Id: cousins.ml,v 1.1 1998-12-12 17:26:26 ddr Exp $ *)

open Def;
open Gutil;
open Util;
open Config;

value max_lev = 5;
value max_cnt = 2000;

(* Utilities *)

value niveau_max_ascendance base ip =
  let x = ref 0 in
  let mark = Array.create base.persons.len False in
  do let rec loop niveau ip =
       if mark.(Adef.int_of_iper ip) then ()
       else
         do mark.(Adef.int_of_iper ip) := True;
            x.val := max x.val niveau;
         return
         if x.val = max_lev then ()
         else 
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

value rec ancestor_of_sosa base ip sosa =
  if Num.eq sosa Num.one then Some ip
  else
    match ancestor_of_sosa base ip (Num.half sosa) with
    [ Some ip ->
        match (aoi base ip).parents with
        [ Some ifam ->
            let cpl = coi base ifam in
            Some (if Num.even sosa then cpl.father else cpl.mother)
        | None -> None ]
    | None -> None ]
;

value brother_label conf x =
  match x with
  [ 1 -> transl conf "specify" ^ " " ^ transl conf "generation"
  | 2 -> transl conf "cousins"
  | 3 -> transl conf "2nd cousins"
  | 4 -> transl conf "3rd cousins"
  | n ->
      Printf.sprintf (ftransl conf "%s cousins")
        (transl_nth conf (transl_nth conf "*nth (cousin)*" 0) (n - 1)) ]
;

value rec except x =
  fun
  [ [] -> []
  | [y :: l] -> if x = y then l else [y :: except x l] ]
;

value merge_lists l1 l2 =
  let l =
    rev_merge (List.rev l1) l2 where rec rev_merge r =
      fun
      [ [x :: l] -> rev_merge (if List.mem x r then r else [x :: r]) l
      | [] -> r ]
  in
  List.rev l
;

value children_of base p =
  List.fold_right
    (fun ifam list ->
       let fam = foi base ifam in
       Array.to_list fam.children @ list)
    (Array.to_list p.family) []
;

value siblings_by base iparent ip =
  let list = children_of base (poi base iparent) in
  except ip list
;

value siblings base ip =
  match (aoi base ip).parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      merge_lists (siblings_by base cpl.father ip)
        (siblings_by base cpl.mother ip)
  | None -> [] ]
;

value rec has_desc_lev base lev p =
  if lev <= 1 then True
  else
    List.exists
      (fun ifam ->
         let fam = foi base ifam in
         List.exists (fun ip -> has_desc_lev base (lev - 1) (poi base ip))
           (Array.to_list fam.children))
      (Array.to_list p.family)
;

(*
value select_having_desc base lev list =
  List.fold_right
    (fun ip list ->
       if has_desc_lev base lev (poi base ip) then [ip :: list] else list)
    list []
;
*)

(* Algorithms *)

value print_choice conf base p niveau_effectif =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Srcfile.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=C>\n";
    Wserver.wprint "<input type=hidden name=i value=%d>\n"
      (Adef.int_of_iper p.cle_index);
    tag "select" "name=v" begin
      let rec boucle i =
        if i > niveau_effectif then ()
        else
          do Wserver.wprint "  <option value=%d%s> %s\n" (i - 1)
               (if i == 0 then " selected" else "")
               (capitale (brother_label conf i));
          return boucle (succ i)
      in
      boucle 2;
    end;
    Wserver.wprint "<p>\n";
    Wserver.wprint "<input type=submit value=\"Ok\"><br>\n";
  end
;

value cnt = ref 0;

value rec print_descend_upto conf base lev children =
  if lev > 0 && cnt.val < max_cnt then
    do if lev <= 2 then Wserver.wprint "<ul>\n" else ();
       List.iter
         (fun ip ->
            let p = poi base ip in
            if cnt.val < max_cnt && has_desc_lev base lev p then
              do if lev <= 2 then
                   do Wserver.wprint "<li>\n";
                      if lev = 1 then
                        do afficher_personne_titre_referencee conf base p;
                           Date.afficher_dates_courtes conf base p;
                           incr cnt;
                        return ()
                      else
                        do Wserver.wprint "%s %s "
                             (capitale (transl_nth conf "child/children" 1))
                             (transl_nth conf "of" 0);
                           afficher_personne_titre conf base p;
                           Wserver.wprint ":";
                        return ();
                      Wserver.wprint "\n";
                   return ()
                 else ();
                 print_descend_upto conf base (lev - 1) (children_of base p);
              return ()
            else ())
        children;
      if lev <= 2 then Wserver.wprint "</ul>\n" else ();
    return ()
  else ()
;

value print_cousins_side_of conf base p lev =
  let sib = siblings base p.cle_index in
  if List.exists (fun ip -> has_desc_lev base lev (poi base ip)) sib then
    do Wserver.wprint "<li>\n";
       Wserver.wprint "%s " (capitale (transl conf "of the side of"));
       afficher_personne_titre conf base p;
       Wserver.wprint ":\n";
       print_descend_upto conf base lev sib;
    return True
  else False
;

value print_cousins_lev conf base p lev =
  let first_sosa =
    loop (Num.one) lev where rec loop sosa lev =
      if lev <= 1 then sosa
      else loop (Num.twice sosa) (lev - 1)
  in
  let last_sosa = Num.twice first_sosa in
  tag "ul" begin
    let some =
      loop first_sosa False where rec loop sosa some =
        if cnt.val < max_cnt && Num.gt last_sosa sosa then
          let some =
            match ancestor_of_sosa base p.cle_index sosa with
            [ Some ia ->
                print_cousins_side_of conf base (poi base ia) lev || some
            | _ -> some ]
          in
          loop (Num.inc sosa 1) some
        else some
    in
    if some then ()
    else Wserver.wprint "<li>%s\n" (capitale (transl conf "no match"));
  end
;

(* HTML main *)

value print_cousins conf base p lev =
  let title h =
    do Wserver.wprint "%s " (capitale (brother_label conf lev));
       let txt =
         if h then person_text_no_html conf base p
         else person_text conf base p
       in
       Wserver.wprint "%s" (transl_concat conf "of" txt);
    return ()
  in
  do header conf title;
     cnt.val := 0;
     print_cousins_lev conf base p lev;
     if cnt.val >= max_cnt then Wserver.wprint "etc...\n"
     else if cnt.val > 1 then
       Wserver.wprint "%s: %d %s.\n" (capitale (transl conf "total"))
         cnt.val (transl_nth conf "person/persons" 1)
     else ();
     trailer conf;
  return ()
;

value print_menu conf base p effective_level =
  let title h =
    do Wserver.wprint "%s " (capitale (transl conf "cousins"));
       let txt =
         if h then person_text_no_html conf base p
         else person_text conf base p
       in
       Wserver.wprint "%s" (transl_concat conf "of" txt);
    return ()
  in
  do header conf title;
     print_choice conf base p effective_level;
     trailer conf;
  return ()
;

value print conf base p =
  match p_getint conf.env "v" with
  [ Some lev -> print_cousins conf base p (min (max 1 lev) max_lev + 1)
  | _ ->
      let effective_level = niveau_max_ascendance base p.cle_index + 1 in
      if effective_level == 2 then print_cousins conf base p 2
      else print_menu conf base p effective_level ]
;
