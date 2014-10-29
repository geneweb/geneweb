(* camlp5r ./pa_html.cmo *)
(* $Id: cousins.ml,v 5.17 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Gutil;
open Gwdb;
open Hutil;
open Util;

value default_max_cnt = 2000;

value brother_label conf x =
  match x with
  [ 1 -> transl conf "siblings"
  | 2 -> transl conf "cousins"
  | 3 -> transl conf "2nd cousins"
  | 4 -> transl conf "3rd cousins"
  | n ->
      Printf.sprintf (ftransl conf "%s cousins")
        (transl_nth conf "nth (cousin)" (n - 1)) ]
;

value rec except x =
  fun
  [ [] -> []
  | [y :: l] -> if x = y then l else [y :: except x l] ]
;

value children_of base u =
  List.fold_right
    (fun ifam list ->
       let des = foi base ifam in
       Array.to_list (get_children des) @ list)
    (Array.to_list (get_family u)) []
;

value siblings_by conf base iparent ip =
  let list = children_of base (pget conf base iparent) in
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

value siblings conf base ip =
  match get_parents (pget conf base ip) with
  [ Some ifam ->
      let cpl = foi base ifam in
      let fath_sib =
        List.map (fun ip -> (ip, (get_father cpl, Male)))
          (siblings_by conf base (get_father cpl) ip)
      in
      let moth_sib =
        List.map (fun ip -> (ip, (get_mother cpl, Female)))
          (siblings_by conf base (get_mother cpl) ip)
      in
      merge_siblings fath_sib moth_sib
  | None -> [] ]
;

value rec has_desc_lev conf base lev u =
  if lev <= 1 then True
  else
    List.exists
      (fun ifam ->
         let des = foi base ifam in
         List.exists
           (fun ip -> has_desc_lev conf base (lev - 1) (pget conf base ip))
           (Array.to_list (get_children des)))
      (Array.to_list (get_family u))
;

value br_inter_is_empty b1 b2 =
  List.for_all (fun (ip, _) -> not (List.mem_assoc ip b2)) b1
;

(* Algorithms *)

value cnt = ref 0;

value give_access conf base ia_asex p1 b1 p2 b2 =
  let reference _ _ p s =
    if is_hidden p then s
    else
    "<a href=\"" ^ commd conf ^ "m=RL;" ^ acces_n conf base "1" p1 ^ ";b1=" ^
      Num.to_string (Util.sosa_of_branch [ia_asex :: b1]) ^ ";" ^
      acces_n conf base "2" p2 ^ ";b2=" ^
      Num.to_string (Util.sosa_of_branch [ia_asex :: b2]) ^ ";spouse=" ^
      (if p_getenv conf.env "spouse" = Some "on" then "on" else "") ^
      ";image=" ^ (if p_getenv conf.env "image" = Some "on" then "on" else "") ^
      ";bd=" ^ (match p_getenv conf.env "bd" with [Some x -> x | None -> "0"]) ^
      "\">" ^ s ^ "</a>"
  in
  let reference_sp p3 _ _ p s =
    if is_hidden p then s
    else
    "<a href=\"" ^ commd conf ^ "m=RL;" ^ acces_n conf base "1" p1 ^ ";b1=" ^
      Num.to_string (Util.sosa_of_branch [ia_asex :: b1]) ^ ";" ^
      acces_n conf base "2" p2 ^ ";b2=" ^
      Num.to_string (Util.sosa_of_branch [ia_asex :: b2]) ^ ";" ^
      acces_n conf base "4" p3 ^ ";spouse=" ^
      (if p_getenv conf.env "spouse" = Some "on" then "on" else "") ^
      ";image=" ^ (if p_getenv conf.env "image" = Some "on" then "on" else "") ^
      ";bd=" ^ (match p_getenv conf.env "bd" with [Some x -> x | None -> "0"]) ^
      "\">" ^ s ^ "</a>"
  in
  let print_nospouse _ =
    do {
      Perso.print_sosa conf base p2 True;
      Wserver.wprint "%s%s"
        (gen_person_title_text reference std_access conf base p2)
        (Date.short_dates_text conf base p2)
    }
  in
  let print_spouse sp first =
    do {
      if first then do {
        Perso.print_sosa conf base p2 True;
        Wserver.wprint "%s"
          (gen_person_title_text reference std_access conf base p2)
      }
      else do {
        Wserver.wprint "<br%s>%s" conf.xhs (person_title_text conf base p2)
      };
      Wserver.wprint "%s &amp; " (Date.short_dates_text conf base p2);
      Perso.print_sosa conf base sp True;
      Wserver.wprint "%s%s"
        (gen_person_title_text (reference_sp sp) std_access conf base sp)
        (Date.short_dates_text conf base sp)
    }
  in
  if match p_getenv conf.env "spouse" with
     [ Some "on" -> False
     | _ -> True ]
  then
    print_nospouse ()
  else
    let u = Array.to_list (get_family p2) in
    match u with
    [ [] -> print_nospouse ()
    | _ ->
        let _ =
          List.fold_left
            (fun a ifam ->
               let cpl = foi base ifam in
               let sp =
                 if get_sex p2 = Female then pget conf base (get_father cpl)
                 else pget conf base (get_mother cpl)
               in
               let _ = print_spouse sp a in
               False)
            True u
        in
        () ]
;

value rec print_descend_upto conf base max_cnt ini_p ini_br lev children =
  if lev > 0 && cnt.val < max_cnt then do {
    if lev <= 2 then Wserver.wprint "<ul>\n" else ();
    List.iter
      (fun (ip, ia_asex, rev_br) ->
         let p = pget conf base ip in
         let br = List.rev [(ip, get_sex p) :: rev_br] in
         let is_valid_rel = br_inter_is_empty ini_br br in
         if is_valid_rel && cnt.val < max_cnt && has_desc_lev conf base lev p
         then do {
           if lev <= 2 then do {
             Wserver.wprint "<li>\n";
             if lev = 1 then do {
               give_access conf base ia_asex ini_p ini_br p br; incr cnt
             }
             else do {
               let s =
                 transl_a_of_gr_eq_gen_lev conf
                   (transl_nth conf "child/children" 1)
                   (person_title_text conf base p)
               in
               Wserver.wprint "%s" (capitale (Util.translate_eval s));
               Wserver.wprint ":"
             }
           }
           else ();
           let children =
             List.map
               (fun ip ->
                  (ip, ia_asex, [(get_key_index p, get_sex p) :: rev_br]))
               (children_of base p)
           in
           print_descend_upto conf base max_cnt ini_p ini_br (lev - 1)
             children;
           if lev <= 2 then Wserver.wprint "</li>\n" else ()
         }
         else ())
      children;
    if lev <= 2 then Wserver.wprint "</ul>\n" else ()
  }
  else ()
;

value sibling_has_desc_lev conf base lev (ip, _) =
  has_desc_lev conf base lev (pget conf base ip)
;

value print_cousins_side_of conf base max_cnt a ini_p ini_br lev1 lev2 tips =
  let sib = siblings conf base (get_key_index a) in
  if List.exists (sibling_has_desc_lev conf base lev2) sib then do {
    if tips then Util.print_tips_relationship conf
    else ();
    if lev1 > 1 then do {
      Wserver.wprint "<li>\n";
      Wserver.wprint "%s:\n"
        (capitale
           (cftransl conf "on %s's side"
              [gen_person_title_text no_reference raw_access conf base a]))
    }
    else ();
    let sib = List.map (fun (ip, ia_asex) -> (ip, ia_asex, [])) sib in
    print_descend_upto conf base max_cnt ini_p ini_br lev2 sib;
    if lev1 > 1 then Wserver.wprint "</li>\n" else ();
    True
  }
  else False
;

value print_cousins_lev conf base max_cnt p lev1 lev2 =
  let first_sosa =
    loop Num.one lev1 where rec loop sosa lev =
      if lev <= 1 then sosa else loop (Num.twice sosa) (lev - 1)
  in
  let last_sosa = Num.twice first_sosa in
  do {
    if lev1 > 1 then Wserver.wprint "<ul>\n" else ();
    let some =
      loop first_sosa False True where rec loop sosa some print_tips =
        if cnt.val < max_cnt && Num.gt last_sosa sosa then
          let some =
            match Util.branch_of_sosa conf base (get_key_index p) sosa with
            [ Some ([(ia, _) :: _] as br) ->
                print_cousins_side_of conf base max_cnt (pget conf base ia) p
                  br lev1 lev2 print_tips ||
                some
            | _ -> some ]
          in
          loop (Num.inc sosa 1) some False
        else some
    in
    if some then ()
    else
      Wserver.wprint "%s.\n" (capitale (transl conf "no match"));
    if lev1 > 1 then Wserver.wprint "</ul>\n" else ()
  }
;

(* HTML main *)

value print_cousins conf base p lev1 lev2 =
  let title h =
    let txt_fun = if h then gen_person_text_no_html else gen_person_text in
    if lev1 = lev2 then
      let s =
        transl_a_of_gr_eq_gen_lev conf
          (brother_label conf lev1) (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale (Util.translate_eval s))
    else if lev1 = 2 && lev2 = 1 then
      let s =
        transl_a_of_b conf (transl conf "uncles and aunts")
          (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale (Util.translate_eval s))
    else if lev1 = 1 && lev2 = 2 then
      let s =
        transl_a_of_gr_eq_gen_lev conf
          (transl conf "nephews and nieces") (txt_fun raw_access conf base p)
      in
      Wserver.wprint "%s" (capitale (Util.translate_eval s))
    else
      Wserver.wprint "%s %d / %s %d" (capitale (transl conf "ancestors")) lev1
        (capitale (transl conf "descendants")) lev2
  in
  let max_cnt =
    try int_of_string (List.assoc "max_cousins" conf.base_env) with
    [ Not_found | Failure _ -> default_max_cnt ]
  in
  do {
    header conf title;
    print_link_to_welcome conf True;
    cnt.val := 0;
    (* Construction de la table des sosa de la base *)
    let () = Perso.build_sosa_ht conf base in
    print_cousins_lev conf base max_cnt p lev1 lev2;
    tag "p" begin
      if cnt.val >= max_cnt then Wserver.wprint "etc...\n"
      else if cnt.val > 1 then
        Wserver.wprint "%s: %d %s.\n" (capitale (transl conf "total")) cnt.val
          (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1))
      else ();
    end;
    trailer conf
  }
;

value sosa_of_persons conf base =
  loop 1 where rec loop n =
    fun
    [ [] -> n
    | [ip :: list] ->
        loop (if get_sex (pget conf base ip) = Male then 2 * n else 2 * n + 1)
          list ]
;

value print_anniv conf base p dead_people level =
  let module S = Map.Make (struct type t = iper; value compare = compare; end)
  in
  let s_mem x m =
    try
      let _ = S.find x m in
      True
    with
    [ Not_found -> False ]
  in
  let rec insert_desc set up_sosa down_br n ip =
    if s_mem ip set then set
    else
      let set = S.add ip (up_sosa, down_br) set in
      if n = 0 then set
      else
        let u = get_family (pget conf base ip) in
        let down_br = [ip :: down_br] in
        let rec loop set i =
          if i = Array.length u then set
          else
            let chil = get_children (foi base u.(i)) in
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
        loop set 0
  in
  let set =
    let module P =
      Pqueue.Make
        (struct
           type t = (iper * int * int);
           value leq (_, lev1, _) (_, lev2, _) = lev1 <= lev2;
         end)
    in
    let a = P.add (get_key_index p, 0, 1) P.empty in
    let rec loop set a =
      if P.is_empty a then set
      else
        let ((ip, n, up_sosa), a) = P.take a in
        let set = insert_desc set up_sosa [] (n + 3) ip in
        if n >= level then set
        else
          let a =
            match get_parents (pget conf base ip) with
            [ Some ifam ->
                let cpl = foi base ifam in
                let n = n + 1 in
                let up_sosa = 2 * up_sosa in
                let a = P.add (get_father cpl, n, up_sosa) a in
                P.add (get_mother cpl, n, up_sosa + 1) a
            | None -> a ]
          in
          loop set a
    in
    loop S.empty a
  in
  let set =
    S.fold
      (fun ip (up_sosa, down_br) set ->
         let u = get_family (pget conf base ip) in
         let set = S.add ip (up_sosa, down_br, None) set in
         if Array.length u = 0 then set
         else
           let rec loop set i =
             if i = Array.length u then set
             else
               let cpl = foi base u.(i) in
               let c = spouse ip cpl in
               loop (S.add c (up_sosa, down_br, Some ip) set) (i + 1)
           in
           loop set 0)
      set S.empty
  in
  let txt_of (up_sosa, down_br, spouse) conf base c =
    "<a href=\"" ^ commd conf ^ "m=RL;" ^ acces_n conf base "1" p ^ ";b1=" ^
      string_of_int up_sosa ^ ";" ^
      acces_n conf base "2"
        (match spouse with
         [ Some ip -> pget conf base ip
         | _ -> c ]) ^
      ";b2=" ^ string_of_int (sosa_of_persons conf base down_br) ^
      (match spouse with
       [ Some _ -> ";" ^ acces_n conf base "4" c
       | _ -> "" ]) ^
      ";spouse=on\">" ^ person_title_text conf base c ^ "</a>"
  in
  let f_scan =
    let list = ref (S.fold (fun ip b list -> [(ip, b) :: list]) set []) in
    fun () ->
      match list.val with
      [ [(x, b) :: l] -> do { list.val := l; (pget conf base x, txt_of b) }
      | [] -> raise Not_found ]
  in
  let mode () =
    do {
      xtag "input" "type=\"hidden\" name=\"m\" value=\"C\"";
      xtag "input" "type=\"hidden\" name=\"i\" value=\"%d\""
        (Adef.int_of_iper (get_key_index p));
      xtag "input" "type=\"hidden\" name=\"t\" value=\"%s\""
        (if dead_people then "AD" else "AN")
    }
  in
  match p_getint conf.env "v" with
  [ Some i -> Birthday.gen_print conf base i f_scan dead_people
  | _ ->
      if dead_people then
        Birthday.gen_print_menu_dead conf base f_scan mode
      else
        Birthday.gen_print_menu_birth conf base f_scan mode ]
;

value cousmenu_print = Perso.interp_templ "cousmenu";

value print conf base p =
  let max_lev =
    try int_of_string (List.assoc "max_cousins_level" conf.base_env) with
    [ Not_found | Failure _ -> Perso.default_max_cousin_lev ]
  in
  match (p_getint conf.env "v1", p_getenv conf.env "t") with
  [ (Some lev1, _) ->
      let lev1 = min (max 1 lev1) 10 in
      let lev2 =
        match p_getint conf.env "v2" with
        [ Some lev2 -> min (max 1 lev2) 10
        | None -> lev1 ]
      in
      print_cousins conf base p lev1 lev2
  | (_, Some (("AN" | "AD") as t)) when conf.wizard || conf.friend ->
      print_anniv conf base p (t = "AD") max_lev
  | _ ->
      cousmenu_print conf base p ]
;
