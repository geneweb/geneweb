(* camlp4r ./pa_html.cmo *)
(* $Id: cousins.ml,v 3.12 2001-01-06 09:55:53 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value max_lev = 5;
value max_cnt = 2000;

(* Utilities *)

value niveau_max_ascendance base ip =
  let x = ref 0 in
  let mark = Array.create base.data.persons.len False in
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

value brother_label conf x =
  match x with
  [ 1 -> transl conf "siblings"
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

value children_of base u =
  List.fold_right
    (fun ifam list ->
       let des = doi base ifam in
       Array.to_list des.children @ list)
    (Array.to_list u.family) []
;

value siblings_by base iparent ip =
  let list = children_of base (uoi base iparent) in
  except ip list
;

value merge_siblings l1 l2 =
  let l =
    rev_merge (List.rev l1) l2 where rec rev_merge r =
      fun
      [ [((v, _) as x) :: l] ->
          rev_merge (if List.mem_assoc v r then r else [x :: r]) l
      | [] -> r ]
  in
  List.rev l
;

value siblings base p =
  let ip = p.cle_index in
  match (aoi base ip).parents with
  [ Some ifam ->
      let cpl = coi base ifam in
      let fath_sib =
        List.map (fun ip -> (ip, (cpl.father, Male)))
          (siblings_by base cpl.father ip)
      in
      let moth_sib =
        List.map (fun ip -> (ip, (cpl.mother, Female)))
          (siblings_by base cpl.mother ip)
      in
      merge_siblings fath_sib moth_sib
  | None -> [] ]
;

value rec has_desc_lev base lev u =
  if lev <= 1 then True
  else
    List.exists
      (fun ifam ->
         let des = doi base ifam in
         List.exists (fun ip -> has_desc_lev base (lev - 1) (uoi base ip))
           (Array.to_list des.children))
      (Array.to_list u.family)
;

value br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1
;

(* Algorithms *)

value print_choice conf base p niveau_effectif =
  tag "form" "method=get action=\"%s\"" conf.command begin
    Util.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=C>\n";
    wprint_hidden_person conf base "" p;
    tag "select" "name=v1" begin
      let rec boucle i =
        if i > niveau_effectif then ()
        else
          do Wserver.wprint "  <option value=%d%s> %s\n" i
               (if i == 2 then " selected" else "")
               (capitale (brother_label conf i));
          return boucle (succ i)
      in
      boucle 1;
    end;
    Wserver.wprint "<input type=submit value=\"Ok\">\n";
  end
;

value cnt = ref 0;

value give_access conf base ia_asex p1 b1 p2 b2 =
  do stag "a" "href=\"%sm=RL;%s;b1=%s;%s;b2=%s;spouse=on\"" (commd conf)
       (acces_n conf base "1" p1)
       (Num.to_string (Util.sosa_of_branch [ia_asex :: b1]))
       (acces_n conf base "2" p2)
       (Num.to_string (Util.sosa_of_branch [ia_asex :: b2]))
     begin
       afficher_personne_sans_titre conf base p2;
     end;
     afficher_titre conf base p2;
     Date.afficher_dates_courtes conf base p2;
  return ()
;

value rec print_descend_upto conf base max_cnt ini_p ini_br lev children =
  if lev > 0 && cnt.val < max_cnt then
    do if lev <= 2 then Wserver.wprint "<ul>\n" else ();
       List.iter
         (fun (ip, ia_asex, rev_br) ->
            let p = poi base ip in
            let u = uoi base ip in
            let br = List.rev [(ip, p.sex) :: rev_br] in
            let is_valid_rel = br_inter_is_empty ini_br br in
            if is_valid_rel && cnt.val < max_cnt && has_desc_lev base lev u
            then
              do if lev <= 2 then
                   do html_li conf;
                      if lev = 1 then
                        do give_access conf base ia_asex ini_p ini_br p br;
                           incr cnt;
                        return ()
                      else
                        let s =
                          transl_decline2 conf
                            "%1 of (same or greater generation level) %2"
                            (transl_nth conf "child/children" 1)
                            (person_title_text conf base p)
                        in
                        do Wserver.wprint "%s" (capitale s);
                           Wserver.wprint ":";
                        return ();
                      Wserver.wprint "\n";
                   return ()
                 else ();
                 let children =
                   List.map
                     (fun ip ->
                        (ip, ia_asex, [(p.cle_index, p.sex) :: rev_br]))
                   (children_of base u)
                 in
                 print_descend_upto conf base max_cnt ini_p ini_br (lev - 1)
                   children;
              return ()
            else ())
        children;
      if lev <= 2 then Wserver.wprint "</ul>\n" else ();
    return ()
  else ()
;

value sibling_has_desc_lev base lev (ip, _) =
  has_desc_lev base lev (uoi base ip)
;

value print_cousins_side_of conf base max_cnt a ini_p ini_br lev1 lev2 =
  let sib = siblings base a in
  if List.exists (sibling_has_desc_lev base lev2) sib then
    do if lev1 > 1 then
         do html_li conf;
            Wserver.wprint "%s:\n"
              (capitale
                 (cftransl conf "on %s's side"
                    [gen_person_title_text raw_access conf base a]));
         return ()
       else ();
       let sib = List.map (fun (ip, ia_asex) -> (ip, ia_asex, [])) sib in
       print_descend_upto conf base max_cnt ini_p ini_br lev2 sib;
    return True
  else False
;

value print_cousins_lev conf base max_cnt p lev1 lev2 =
  let first_sosa =
    loop Num.one lev1 where rec loop sosa lev =
      if lev <= 1 then sosa
      else loop (Num.twice sosa) (lev - 1)
  in
  let last_sosa = Num.twice first_sosa in
  do if lev1 > 1 then Wserver.wprint "<ul>\n" else ();
     let some =
       loop first_sosa False where rec loop sosa some =
         if cnt.val < max_cnt && Num.gt last_sosa sosa then
           let some =
             match Util.branch_of_sosa base p.cle_index sosa with
             [ Some ([(ia, _) :: _] as br) ->
                 print_cousins_side_of conf base max_cnt (poi base ia) p br
                   lev1 lev2
                 || some
             | _ -> some ]
           in
           loop (Num.inc sosa 1) some
         else some
     in
     if some then ()
     else
       do html_li conf; return
       Wserver.wprint "%s\n" (capitale (transl conf "no match"));
     if lev1 > 1 then Wserver.wprint "</ul>\n" else ();
  return ()
;

(* HTML main *)

value print_cousins conf base p lev1 lev2 =
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    if lev1 == lev2 then
      let s =
        transl_decline2 conf "%1 of (same or greater generation level) %2"
          (brother_label conf lev1) (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale s)
    else if lev1 == 2 && lev2 == 1 then
      let s =
        transl_decline2 conf "%1 of %2" (transl conf "uncles and aunts")
          (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale s)
    else if lev1 == 1 && lev2 == 2 then
      let s =
        transl_decline2 conf "%1 of (same or greater generation level) %2"
          (transl conf "nephews and nieces") (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale s)
    else
      Wserver.wprint "%s %d / %s %d"
        (capitale (transl conf "ancestors")) lev1
        (capitale (transl conf "descendants")) lev2
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env) with
    [ Not_found | Failure _ -> max_cnt ]
  in
  do header conf title;
     cnt.val := 0;
     print_cousins_lev conf base max_cnt p lev1 lev2;
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
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    let s =
      transl_decline2 conf "%1 of (same or greater generation level) %2"
        (transl conf "cousins (general term)")
        (txt_fun raw_access conf base p)
    in
    Wserver.wprint "%s" (capitale s)
  in
  do header conf title;
     tag "ul" begin
       html_li conf;
       print_choice conf base p effective_level;
       html_li conf;
       Wserver.wprint "<a href=\"%s%s;m=C;v1=2;v2=1\">%s</a>\n"
         (commd conf) (acces conf base p)
         (capitale (transl conf "uncles and aunts"));
       if has_nephews_or_nieces base p then
         do html_li conf;
            Wserver.wprint "<a href=\"%s%s;m=C;v1=1;v2=2\">%s</a>\n"
              (commd conf) (acces conf base p)
              (capitale (transl conf "nephews and nieces"));
         return ()
       else ();
     end;
     match p.death with
     [ NotDead | DontKnowIfDead when conf.wizard || conf.friend ->
         do html_p conf;
            tag "ul" begin
              html_li conf;
              Wserver.wprint "<a href=\"%s%s;m=C;t=AN"
                (commd conf) (acces conf base p);
(*
              Wserver.wprint ";em=R;%s" (acces_n conf base "e" p);
*)
              Wserver.wprint "\">%s</a>\n"
                (capitale (transl conf "birthdays"));
            end;
         return ()
     | _ -> () ];
     trailer conf;
  return ()
;

value sosa_of_persons base =
  loop 1 where rec loop n =
    fun
    [ [] -> n
    | [ip :: list] ->
        loop (if (poi base ip).sex = Male then 2 * n else 2 * n + 1) list ]
;

value print_anniv conf base p level =
  let module S =
    Map.Make (struct type t = iper; value compare = compare; end)
  in
  let s_mem x m = try let _ = S.find x m in True with [ Not_found -> False ] in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = S.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = (uoi base ip).family in
        let down_br = [ip :: down_br] in
        loop set 0 where rec loop set i =
          if i = Array.length u then set
          else
            let chil = (doi base u.(i)).children in
            let set =
              loop set 0 where rec loop set i =
                if i = Array.length chil then set
                else
                  let set =
                    insert_desc set up_sosa down_br (n - 1) chil.(i)
                  in
                  loop set (i + 1)
            in
            loop set (i + 1)
  in
  let set =
    let module P =
      Pqueue.Make
        (struct
           type t = (iper * int * int);
           value leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2;
         end)
    in
    let a = P.add (p.cle_index, 0, 1) P.empty in
    loop S.empty a where rec loop set a =
      if P.is_empty a then set
      else
        let ((ip, n, up_sosa), a) = P.take a in
        let set = insert_desc set up_sosa [] (n + 3) ip in
        if n >= level then set
        else
          let a =
            match (aoi base ip).parents with
            [ Some ifam ->
                let cpl = coi base ifam in
                let n = n + 1 in
                let up_sosa = 2 * up_sosa in
                let a = P.add (cpl.father, n, up_sosa) a in
                P.add (cpl.mother, n, up_sosa + 1) a
            | None -> a ]
          in
          loop set a
  in
  let set =
    S.fold
      (fun ip (up_sosa, down_br) set ->
         let u = (uoi base ip).family in
         let set = S.add ip (up_sosa, down_br, None) set in
         if Array.length u = 0 then set
         else
           loop set 0 where rec loop set i =
             if i = Array.length u then set
             else
               let cpl = coi base u.(i) in
               let c = spouse ip cpl in
               loop (S.add c (up_sosa, down_br, Some ip) set) (i + 1))
      set S.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    "<a href=\"" ^ commd conf ^ "m=RL;" ^ acces_n conf base "1" p ^
    ";b1=" ^ string_of_int up_sosa ^ ";" ^
    acces_n conf base "2"
      (match spouse with [ Some ip -> poi base ip | _ -> c ]) ^
    ";b2=" ^ string_of_int (sosa_of_persons base down_br) ^
    (match spouse with [ Some _ -> ";" ^ acces_n conf base "4" c | _ -> "" ]) ^
    ";spouse=on\">" ^
    person_title_text conf base c ^ "</a>"
  in
  let f_scan =
    let list = ref (S.fold (fun ip b list -> [(ip, b) :: list]) set []) in
    fun () ->
      match list.val with
      [ [(x, b) :: l] -> do list.val := l; return (poi base x, txt_of b)
      | [] -> raise Not_found ]
  in
  let mode () =
    do Wserver.wprint "<input type=hidden name=m value=C>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=t value=AN>\n";
    return ()
  in
  match p_getint conf.env "v" with
  [ Some i -> Birthday.gen_print conf base i f_scan False
  | _ -> Birthday.gen_print_menu_birth conf base f_scan mode ]
;

value print conf base p =
  match (p_getint conf.env "v1", p_getenv conf.env "t") with
  [ (Some lev1, _) ->
      let lev1 = min (max 1 lev1) 10 in
      let lev2 =
        match p_getint conf.env "v2" with
        [ Some lev2 -> min (max 1 lev2) 10
        | None -> lev1 ]
      in
      print_cousins conf base p lev1 lev2
  | (_, Some "AN") when conf.wizard || conf.friend ->
      print_anniv conf base p max_lev
  | _ ->
      let effective_level = niveau_max_ascendance base p.cle_index + 1 in
      if effective_level == 2 then print_cousins conf base p 2 2
      else print_menu conf base p effective_level ]
;
