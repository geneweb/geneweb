(* camlp5r ./pa_html.cmo *)
(* $Id: descend.ml,v 5.27 2007-09-12 09:58:44 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config;
open Def;
open Dag2html;
open Gutil;
open Gwdb;
open Hutil;
open Mutil;
open Util;

value limit_by_tree conf =
  match p_getint conf.base_env "max_desc_tree" with
  [ Some x -> max 1 x
  | None -> 4 ]
;

value text_to conf =
  fun
  [ 0 ->
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
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
      transl_decline conf "specify"
        (transl_nth conf "generation/generations" 0)
  | 1 -> transl conf "the children"
  | 2 -> transl conf "the grandchildren"
  | 3 -> transl conf "the great-grandchildren"
  | i ->
      Printf.sprintf (ftransl conf "the %s generation")
        (transl_nth conf "nth (generation)" i) ]
;

value descendants_title conf base p h =
  let txt_fun = if h then gen_person_text_no_html else gen_person_text in
  let s =
    translate_eval
      (transl_a_of_gr_eq_gen_lev conf
         (transl conf "descendants") (txt_fun raw_access conf base p))
  in
  Wserver.wprint "%s" (capitale s)
;

value display_descendants_level conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let (levt, _) = Perso.make_desc_level_table conf base max_level ancestor in
  let mark = Array.make (Array.length levt) False in
  let rec get_level level u list =
    List.fold_left
      (fun list ifam ->
         let des = foi base ifam in
         let enfants = get_children des in
         List.fold_left
           (fun list ix ->
              let x = pget conf base ix in
              if mark.(Adef.int_of_iper ix) then list
              else
                let _ = mark.(Adef.int_of_iper ix) := True in
                if levt.(Adef.int_of_iper ix) > max_level then list
                else if level = max_level then
                  if p_first_name base x = "x" ||
                     levt.(Adef.int_of_iper ix) != level
                  then list
                  else [x :: list]
                else if level < max_level then
                  get_level (succ level) (pget conf base ix) list
                else list)
           list (Array.to_list enfants))
      list (Array.to_list (get_family u))
  in
  let len = ref 0 in
  let list = get_level 1 (pget conf base (get_key_index ancestor)) [] in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           let c =
             alphabetic (p_first_name base p2) (p_first_name base p1)
           in
           if c = 0 then compare (get_occ p2) (get_occ p1) else c
         else c)
      list
  in
  let list =
    List.fold_left
      (fun pl p ->
         match pl with
         [ [(p1, n) :: pl] when get_key_index p = get_key_index p1 ->
             [(p1, succ n) :: pl]
         | _ -> do { incr len; [(p, 1) :: pl] } ])
      [] list
  in
  do {
    header conf (descendants_title conf base ancestor);
    Wserver.wprint "%s" (capitale (text_level conf max_level));
    if len.val > 1 then
      Wserver.wprint " (%d %s)" len.val
        (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1))
    else ();
    Wserver.wprint ".\n";
    html_p conf;
    print_alphab_list conf
      (fun (p, _) ->
         if is_hidden p then "?" else
         String.sub (p_surname base p) (initial (p_surname base p)) 1)
      (fun (p, c) ->
         do {
           Wserver.wprint "\n%s" (referenced_person_title_text conf base p);
           Wserver.wprint "%s" (Date.short_dates_text conf base p);
           if not (is_hidden p) && c > 1 then
             Wserver.wprint " <em>(%d)</em>" c
           else ();
           Wserver.wprint "\n"
         })
      list;
    trailer conf
  }
;

(* With number *)

value mark_descendants conf base marks max_lev ip =
  loop 0 ip (pget conf base ip) where rec loop lev ip u =
    if lev <= max_lev then do {
      marks.(Adef.int_of_iper ip) := True;
      Array.iter
        (fun ifam ->
           let el = get_children (foi base ifam) in
           Array.iter (fun e -> loop (succ lev) e (pget conf base e)) el)
        (get_family u)
    }
    else ()
;

value label_descendants conf base marks paths max_lev =
  loop [] 0 where rec loop path lev p =
    if lev < max_lev then
      let u = p in
      let _ =
        List.fold_left
          (fun cnt ifam ->
             let fam = foi base ifam in
             let c = spouse (get_key_index p) fam in
             let el = get_children fam in
             List.fold_left
               (fun cnt e ->
                  do {
                    if get_sex p = Male || not marks.(Adef.int_of_iper c)
                    then do {
                      let path = [Char.chr (Char.code 'A' + cnt) :: path] in
                      paths.(Adef.int_of_iper e) := path;
                      loop path (succ lev) (pget conf base e)
                    }
                    else ();
                    succ cnt
                  })
               cnt (Array.to_list el))
          0 (Array.to_list (get_family u))
      in
      ()
    else ()
;

value close_lev = 2;

value close_to_end conf base marks max_lev lev p =
  if lev + close_lev >= max_lev then True
  else
    let rec short dlev p =
      let u = p in
      List.for_all
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           if get_sex p = Male || not marks.(Adef.int_of_iper c) then
             if dlev = close_lev then Array.length el = 0
             else
               List.for_all (fun e -> short (succ dlev) (pget conf base e))
                 (Array.to_list el)
           else True)
        (Array.to_list (get_family u))
    in
    short 1 p
;

value labelled conf base marks max_lev lev ip =
  let a = pget conf base ip in
  let u = a in
  Array.length (get_family u) <> 0 &&
  (match get_parents a with
   [ Some ifam ->
       let fam = foi base ifam in
       List.exists
         (fun ifam ->
            let el = get_children fam in
            List.exists
              (fun ie ->
                 let e = pget conf base ie in
                 let u = e in
                 Array.length (get_family u) <> 0 &&
                 not (close_to_end conf base marks max_lev lev e))
              (Array.to_list el))
         (Array.to_list (get_family (pget conf base (get_father fam))))
   | _ -> False ])
;

value label_of_path paths p =
  loop paths.(Adef.int_of_iper (get_key_index p)) where rec loop =
    fun
    [ [] -> ""
    | [c :: cl] -> loop cl ^ String.make 1 c ]
;

value print_child conf base p1 p2 e = do {
  stag "strong" begin
    if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
       get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
    then
      Wserver.wprint "%s"
        (referenced_person_text_without_surname conf base e)
    else Wserver.wprint "\n%s" (referenced_person_text conf base e);
  end;
  Wserver.wprint "%s" (Date.short_dates_text conf base e)
};

value print_repeat_child conf base p1 p2 e =
  stag "em" begin
    if get_sex p1 = Male && eq_istr (get_surname e) (get_surname p1) ||
       get_sex p2 = Male && eq_istr (get_surname e) (get_surname p2)
    then
      Wserver.wprint "%s" (person_text_without_surname conf base e)
    else Wserver.wprint "%s" (person_text conf base e);
  end
;

value display_spouse conf base marks paths fam p c =
  do {
    Wserver.wprint "\n&amp;";
    Wserver.wprint "%s" (Date.short_marriage_date_text conf base fam p c);
    Wserver.wprint " ";
    stag "strong" begin
      Wserver.wprint "\n%s" (referenced_person_text conf base c);
    end;
    if marks.(Adef.int_of_iper (get_key_index c)) then
      Wserver.wprint " (<tt><b>%s</b></tt>)" (label_of_path paths c)
    else Wserver.wprint "%s" (Date.short_dates_text conf base c)
  }
;

value total = ref 0;

value print_family_locally conf base marks paths max_lev lev p1 c1 e =
  loop lev e where rec loop lev p =
    if lev < max_lev then
      let _ =
        List.fold_left
          (fun (cnt, first, need_br) ifam ->
             let fam = foi base ifam in
             let c = spouse (get_key_index p) fam in
             let el = get_children fam in
             let c = pget conf base c in
             do {
               if need_br then html_br conf else ();
               if not first then print_repeat_child conf base p1 c1 p else ();
               display_spouse conf base marks paths fam p c;
               Wserver.wprint "\n";
               let print_children =
                 get_sex p = Male ||
                 not marks.(Adef.int_of_iper (get_key_index c))
               in
               if print_children then
                 Wserver.wprint "<ol start=\"%d\">\n" (succ cnt)
               else ();
               let cnt =
                 List.fold_left
                   (fun cnt ie ->
                      let e = pget conf base ie in
                      do {
                        if print_children then do {
                          Wserver.wprint "<li type=\"A\"> ";
                          print_child conf base p c e;
                          Wserver.wprint "\n";
                          incr total;
                          if succ lev = max_lev then
                            list_iter_first
                              (fun first ifam ->
                                 let fam = foi base ifam in
                                 let c1 = spouse ie fam in
                                 let el = get_children fam in
                                 let c1 = pget conf base c1 in
                                 do {
                                   if not first then do {
                                     html_br conf;
                                     print_repeat_child conf base p c e
                                   }
                                   else ();
                                   display_spouse conf base marks paths fam e
                                     c1;
                                   if Array.length el <> 0 then
                                     Wserver.wprint "....."
                                   else ();
                                   Wserver.wprint "\n"
                                 })
                              (Array.to_list (get_family (pget conf base ie)))
                          else loop (succ lev) e
                        }
                        else ();
                        succ cnt
                      })
                   cnt (Array.to_list el)
               in
               if print_children then Wserver.wprint "</ol>\n" else ();
               (cnt, False, not print_children)
             })
          (0, True, False) (Array.to_list (get_family p))
      in
      ()
    else ()
;

value last_label = ref "";

value print_family conf base marks paths max_lev lev p =
  do {
    if lev <> 0 then do {
      Wserver.wprint "<tt><b>%s</b></tt>." (label_of_path paths p);
      html_br conf
    }
    else ();
    let lab = label_of_path paths p in
    if lab < last_label.val then failwith "print_family"
    else last_label.val := lab;
    let _ =
      List.fold_left
        (fun cnt ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           let c = pget conf base c in
           do {
             stag "strong" begin
               Wserver.wprint "\n%s" (referenced_person_text conf base p);
             end;
             display_spouse conf base marks paths fam p c;
             Wserver.wprint "<ol start=\"%d\">\n" (succ cnt);
             let cnt =
               List.fold_left
                 (fun cnt ie ->
                    let e = pget conf base ie in
                    do {
                      if get_sex p = Male ||
                         not marks.(Adef.int_of_iper (get_key_index c))
                      then do {
                        Wserver.wprint "<li type=\"A\">";
                        print_child conf base p c e;
                        incr total;
                        Wserver.wprint "\n";
                        if labelled conf base marks max_lev lev ie then
                          Wserver.wprint " => <tt><b>%s</b></tt>\n"
                            (label_of_path paths e)
                        else if succ lev = max_lev then
                          Array.iter
                            (fun ifam ->
                               let fam = foi base ifam in
                               let c = spouse ie fam in
                               let el = get_children fam in
                               let c = pget conf base c in
                               do {
                                 display_spouse conf base marks paths fam e
                                   c;
                                 if Array.length el <> 0 then
                                   Wserver.wprint "....."
                                 else ();
                                 Wserver.wprint "\n"
                               })
                            (get_family (pget conf base ie))
                        else
                          print_family_locally conf base marks paths max_lev
                            (succ lev) p c e
                      }
                      else ();
                      succ cnt
                    })
                 cnt (Array.to_list el)
             in
             Wserver.wprint "</ol>\n";
             cnt
           })
        0 (Array.to_list (get_family p))
    in
    ()
  }
;

value print_families conf base marks paths max_lev =
  loop 0 where rec loop lev p =
    if lev < max_lev then do {
      print_family conf base marks paths max_lev lev p;
      Array.iter
        (fun ifam ->
           let fam = foi base ifam in
           let c = spouse (get_key_index p) fam in
           let el = get_children fam in
           let c = pget conf base c in
           if get_sex p = Male ||
              not marks.(Adef.int_of_iper (get_key_index c))
           then
             Array.iter
               (fun ie ->
                  let e = pget conf base ie in
                  if labelled conf base marks max_lev lev ie then
                    loop (succ lev) e
                  else ())
               el
           else ())
        (get_family p)
    }
    else ()
;

value display_descendants_with_numbers conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    if h then descendants_title conf base ancestor h
    else
      wprint_geneweb_link conf
        ("m=D;i=" ^
           string_of_int (Adef.int_of_iper (get_key_index ancestor)) ^
           ";v=" ^ string_of_int max_level ^ ";t=G")
        (capitale
           (transl_a_of_gr_eq_gen_lev conf
              (transl conf "descendants") (person_text conf base ancestor)))
  in
  let marks = Array.create (nb_of_persons base) False in
  let paths = Array.create (nb_of_persons base) [] in
  do {
    header conf title;
    total.val := 0;
    Wserver.wprint "%s" (Date.short_dates_text conf base ancestor);
    let p = ancestor in
    if authorized_age conf base p then
      match (Adef.od_of_codate (get_birth p), get_death p) with
      [ (Some _, _) | (_, Death _ _) -> html_br conf
      | _ -> () ]
    else ();
    Wserver.wprint "%s." (capitale (text_to conf max_level));
    html_p conf;
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    print_families conf base marks paths max_level ancestor;
    if total.val > 1 then do {
      html_p conf;
      Wserver.wprint "%s: %d %s" (capitale (transl conf "total")) total.val
        (Util.translate_eval ("@(c)" ^ transl_nth conf "person/persons" 1));
      if max_level > 1 then
        Wserver.wprint " (%s)" (transl conf "spouses not included")
      else ();
      Wserver.wprint ".\n"
    }
    else ();
    trailer conf
  }
;

value print_ref conf base paths p =
  if paths.(Adef.int_of_iper (get_key_index p)) <> [] then
    Wserver.wprint " => <tt><b>%s</b></tt>" (label_of_path paths p)
  else
    Array.iter
      (fun ifam ->
         let c = spouse (get_key_index p) (foi base ifam) in
         if paths.(Adef.int_of_iper c) <> [] then
           let c = pget conf base c in
           Wserver.wprint " => %s %s <tt><b>%s</b></tt>" (p_first_name base c)
             (p_surname base c) (label_of_path paths c)
         else ())
      (get_family p)
;

value print_elem conf base paths precision (n, pll) =
  do {
    html_li conf;
    match List.rev pll with
    [ [[p]] ->
        do {
          Wserver.wprint "<strong>%s %s %s</strong>" (surname_end base n)
            (reference conf base p (person_text_without_surname conf base p))
            (surname_begin base n);
          Wserver.wprint "%s" (Date.short_dates_text conf base p);
          print_ref conf base paths p;
          Wserver.wprint "\n"
        }
    | pll ->
        do {
          Wserver.wprint "<strong>%s%s</strong>\n" (surname_end base n)
            (surname_begin base n);
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
                      do {
                        html_li conf;
                        stag "strong" begin
                          wprint_geneweb_link conf (acces conf base p)
                            (p_first_name base p);
                        end;
                        if several && precision then do {
                          Wserver.wprint " <em>";
                          specify_homonymous conf base p;
                          Wserver.wprint "</em>"
                        }
                        else ();
                        Wserver.wprint "%s"
                          (Date.short_dates_text conf base p);
                        print_ref conf base paths p;
                        Wserver.wprint "\n"
                      })
                   pl)
              pll;
          end
        } ]
  }
;

value sort_and_display conf base paths precision list =
  let list = List.map (pget conf base) list in
  let list =
    List.sort
      (fun p1 p2 ->
         let c = alphabetic (p_surname base p2) (p_surname base p1) in
         if c = 0 then
           alphabetic (p_first_name base p2) (p_first_name base p1)
         else c)
      list
  in
  let list =
    List.fold_left
      (fun npll p ->
         match npll with
         [ [(n, pl) :: npll] when n = p_surname base p ->
             [(n, [p :: pl]) :: npll]
         | _ -> [(p_surname base p, [p]) :: npll] ])
      [] list
  in
  let list =
    List.map
      (fun (n, pl) ->
         let pll =
           List.fold_left
             (fun pll p ->
                match pll with
                [ [([p1 :: _] as pl) :: pll]
                  when eq_istr (get_first_name p1) (get_first_name p) ->
                    [[p :: pl] :: pll]
                | _ -> [[p] :: pll] ])
             [] pl
         in
         (n, pll))
      list
  in
  if list <> [] then
    tag "ul" begin List.iter (print_elem conf base paths precision) list; end
  else ()
;

value display_descendant_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title h =
    let txt = capitale (transl conf "index of the descendants") in
    if not h then
      wprint_geneweb_link conf
        ("m=D;i=" ^
           string_of_int (Adef.int_of_iper (get_key_index ancestor)) ^
           ";v=" ^ string_of_int max_level ^ ";t=C")
        txt
    else Wserver.wprint "%s" txt
  in
  do {
    header conf title;
    let marks = Array.create (nb_of_persons base) False in
    let paths = Array.create (nb_of_persons base) [] in
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    let list = ref [] in
    for i = 0 to nb_of_persons base - 1 do {
      if paths.(i) <> [] then
        let p = pget conf base (Adef.iper_of_int i) in
        if p_first_name base p <> "?" && p_surname base p <> "?" &&
           p_first_name base p <> "x"
        && (not (is_hide_names conf p) || fast_auth_age conf p) then
          list.val := [get_key_index p :: list.val]
        else ()
      else ()
    };
    sort_and_display conf base paths True list.val;
    trailer conf
  }
;

value display_spouse_index conf base max_level ancestor =
  let max_level = min (Perso.limit_desc conf) max_level in
  let title _ =
    Wserver.wprint "%s"
      (capitale (transl conf "index of the spouses (non descendants)"))
  in
  do {
    header conf title;
    let marks = Array.create (nb_of_persons base) False in
    let paths = Array.create (nb_of_persons base) [] in
    mark_descendants conf base marks max_level (get_key_index ancestor);
    label_descendants conf base marks paths max_level ancestor;
    let list = ref [] in
    for i = 0 to nb_of_persons base - 1 do {
      if paths.(i) <> [] then
        let p = pget conf base (Adef.iper_of_int i) in
        let u = p in
        if p_first_name base p <> "?" && p_surname base p <> "?" &&
           p_first_name base p <> "x" then
          Array.iter
            (fun ifam ->
               let c = spouse (get_key_index p) (foi base ifam) in
               if paths.(Adef.int_of_iper c) = [] then
                 let c = pget conf base c in
                 if p_first_name base c <> "?" && p_surname base c <> "?" &&
                    p_first_name base p <> "x" &&
                    (not (is_hide_names conf c) || fast_auth_age conf c) &&
                    not (List.mem (get_key_index c) list.val)
                 then
                   list.val := [get_key_index c :: list.val]
                 else ()
               else ())
            (get_family u)
        else ()
      else ()
    };
    sort_and_display conf base paths False list.val;
    trailer conf
  }
;

value print_someone conf base p =
  do {
    Wserver.wprint "\n%s" (referenced_person_title_text conf base p);
    Wserver.wprint "%s" (Date.short_dates_text conf base p);
    Wserver.wprint "\n"
  }
;

value children_of conf base ip =
  List.fold_right
    (fun ifam children ->
       Array.to_list (get_children (foi base ifam)) @ children)
    (Array.to_list (get_family (pget conf base ip))) []
;

value rec print_table_person conf base max_lev ip =
  do {
    Wserver.wprint "\n";
    tag "table" "border=\"1\"" begin
      Wserver.wprint "<tr align=\"%s\">\n" conf.left;
      tag "td" "valign=\"top\"" begin
        print_someone conf base (pget conf base ip);
      end;
      if max_lev > 0 then
        match children_of conf base ip with
        [ [] -> ()
        | ipl ->
            do {
              Wserver.wprint "\n<td>";
              List.iter (print_table_person conf base (max_lev - 1)) ipl;
              Wserver.wprint "</td>\n"
            } ]
      else ();
    end
  }
;

value display_descendant_with_table conf base max_lev a =
  let title _ = Wserver.wprint "%s" (capitale (transl conf "descendants")) in
  let max_lev = min (Perso.limit_desc conf) max_lev in
  do {
    header conf title;
    print_table_person conf base max_lev (get_key_index a);
    trailer conf
  }
;

value make_tree_hts conf base gv p =
  let bd =
    match Util.p_getint conf.env "bd" with
    [ Some x -> x
    | None -> 0 ]
  in
  let td_prop =
    match Util.p_getenv conf.env "td" with
    [ Some x -> " " ^ x
    | _ ->
        match Util.p_getenv conf.env "color" with
        [ None | Some "" -> ""
        | Some x -> " class=\"" ^ x ^ "\"" ] ]
  in
  let rec nb_column n v u =
    if v = 0 then n + (max 1 (Array.length (get_family u)))
    else if Array.length (get_family u) = 0 then n + 1
    else
      List.fold_left (fun n ifam -> fam_nb_column n v (foi base ifam)) n
        (Array.to_list (get_family u))
  and fam_nb_column n v des =
    if Array.length (get_children des) = 0 then n + 1
    else
      List.fold_left
        (fun n iper -> nb_column n (v - 1) (pget conf base iper)) n
        (Array.to_list (get_children des))
  in
  let vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    let td =
      match po with
      [ Some (p, _) ->
          let ncol = nb_column 0 (v - 1) p in
          (2 * ncol - 1, CenterA, TDbar None)
      | None -> (1, LeftA, TDnothing) ]
    in
    [td :: tdl]
  in
  let children_vertical_bars v gen =
    let tdl = List.fold_left (vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let spouses_vertical_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, _) when Array.length (get_family p) > 0 ->
        fst
          (List.fold_left
             (fun (tdl, first) ifam ->
                let tdl =
                  if first then tdl
                  else [(1, LeftA, TDnothing) :: tdl]
                in
                let des = foi base ifam in
                let td =
                  if Array.length (get_children des) = 0 then
                    (1, LeftA, TDnothing)
                  else
                    let ncol = fam_nb_column 0 (v - 1) des in
                    (2 * ncol - 1, CenterA, TDbar None)
                in
                ([td :: tdl], False))
             (tdl, True) (Array.to_list (get_family p)))
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let spouses_vertical_bar v gen =
    let tdl = List.fold_left (spouses_vertical_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let horizontal_bar_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, _) when Array.length (get_family p) > 0 ->
        fst
          (List.fold_left
             (fun (tdl, first) ifam ->
                let tdl =
                  if first then tdl
                  else [(1, LeftA, TDnothing) :: tdl]
                in
                let des = foi base ifam in
                let tdl =
                  if Array.length (get_children des) = 0 then
                    [(1, LeftA, TDnothing) :: tdl]
                  else if Array.length (get_children des) = 1 then
                    let u = pget conf base (get_children des).(0) in
                    let ncol = nb_column 0 (v - 1) u in
                    [(2 * ncol - 1, CenterA, TDbar None) :: tdl]
                  else
                    let rec loop tdl i =
                      if i = Array.length (get_children des) then tdl
                      else
                        let iper = (get_children des).(i) in
                        let u = pget conf base iper in
                        let tdl =
                          if i > 0 then
                            let align = CenterA in
                            [(1, align, TDhr align) :: tdl]
                          else tdl
                        in
                        let ncol = nb_column 0 (v - 1) u in
                        let align =
                          if i = 0 then RightA
                          else if
                            i = Array.length (get_children des) - 1
                          then
                            LeftA
                          else CenterA
                        in
                        let td = (2 * ncol - 1, align, TDhr align) in
                        loop [td :: tdl] (i + 1)
                    in
                    loop tdl 0
                in
                (tdl, False))
             (tdl, True) (Array.to_list (get_family p)))
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let horizontal_bars v gen =
    let tdl = List.fold_left (horizontal_bar_txt v) [] gen in
    Array.of_list (List.rev tdl)
  in
  let person_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    let td =
      match po with
      [ Some (p, auth) ->
          let ncol = 
            if v > 1 then nb_column 0 (v - 1) p
            else Array.length (get_family p)
          in
          let txt = person_title_text conf base p in
          let txt = reference conf base p txt in
          let txt =
            if auth then txt ^ Date.short_dates_text conf base p else txt
          in
          let txt =
            if bd > 0 || td_prop <> "" then
              Printf.sprintf
                "<table border=\"%d\"><tr><td align=\"center\"%s>%s</td>\
                 </tr></table>"
                bd td_prop txt
            else txt
          in
          let txt = txt ^ Dag.image_txt conf base p in
          (2 * ncol - 1, CenterA, TDitem txt)
      | None -> (1, LeftA, TDnothing) ]
    in
    [td :: tdl]
  in
  let spouses_txt v tdl po =
    let tdl =
      if tdl = [] then [] else [(1, LeftA, TDnothing) :: tdl]
    in
    match po with
    [ Some (p, auth) when Array.length (get_family p) > 0 ->
        let rec loop tdl i =
          if i = Array.length (get_family p) then tdl
          else
            let ifam = (get_family p).(i) in
            let tdl =
              if i > 0 then [(1, LeftA, TDtext "...") :: tdl] else tdl
            in
            let td =
              let fam = foi base ifam in
              let ncol = 
                if v > 1 then fam_nb_column 0 (v - 1) fam
                else 1
              in
              let s =
                let sp = pget conf base (spouse (get_key_index p) fam) in
                let txt = person_title_text conf base sp in
                let txt = reference conf base sp txt in
                let txt =
                  if auth then txt ^ Date.short_dates_text conf base sp
                  else txt
                in
                "&amp;" ^
                  (if auth then
                     Date.short_marriage_date_text conf base fam p sp
                   else "") ^
                  "&nbsp;" ^ txt ^ Dag.image_txt conf base sp
              in
              let s =
                if bd > 0 || td_prop <> "" then
                  Printf.sprintf
                    "<table border=\"%d\"><tr>\
                     <td align=\"center\"%s>%s</td></tr></table>" bd td_prop s
                else s
              in
              (2 * ncol - 1, CenterA, TDitem s)
            in
            loop [td :: tdl] (i + 1)
        in
        loop tdl 0
    | _ -> [(1, LeftA, TDnothing) :: tdl] ]
  in
  let next_gen gen =
    List.fold_right
      (fun po gen ->
         match po with
         [ Some (p, auth) ->
             if Array.length (get_family p) = 0 then [None :: gen]
             else
               List.fold_right
                 (fun ifam gen ->
                    let des = foi base ifam in
                    if Array.length (get_children des) = 0 then [None :: gen]
                    else
                      let age_auth =
                        List.for_all
                          (fun ip ->
                             authorized_age conf base (pget conf base ip))
                          (Array.to_list (get_children des))
                      in
                      List.fold_right
                        (fun iper gen ->
                           let g = (pget conf base iper, age_auth) in
                           [Some g :: gen])
                        (Array.to_list (get_children des)) gen)
                 (Array.to_list (get_family p)) gen
         | None -> [None :: gen] ])
      gen []
  in
  let hts =
    let tdal =
      loop [] [] [Some (p, True)] (gv + 1)
      where rec loop tdal prev_gen gen v =
        let tdal =
          if prev_gen <> [] then
            [children_vertical_bars v gen; horizontal_bars v prev_gen;
             spouses_vertical_bar (v + 1) prev_gen :: tdal]
          else tdal
        in
        let tdal =
          let tdl = List.fold_left (person_txt v) [] gen in
          [Array.of_list (List.rev tdl) :: tdal]
        in
        let tdal =
          let tdl = List.fold_left (spouses_txt v) [] gen in
          [Array.of_list (List.rev tdl) :: tdal] 
        in
        if v > 1 then
          loop tdal gen (next_gen gen) (v - 1)
        else tdal
    in
    Array.of_list (List.rev tdal)
  in
  hts
;

value print_tree conf base v p =
  let gv = min (limit_by_tree conf) v in
  let page_title =
    Printf.sprintf "%s: %s" (capitale (transl conf "tree"))
      (person_text_no_html conf base p)
  in
  let hts = make_tree_hts conf base gv p in
  Dag.print_slices_menu_or_dag_page conf base page_title hts ""
;

value print_aboville conf base max_level p =
  let max_level = min (Perso.limit_desc conf) max_level in
  do {
    Hutil.header conf (descendants_title conf base p);
    print_link_to_welcome conf True;
    Wserver.wprint "%s.<br><p>" (capitale (text_to conf max_level));
    let rec loop_ind lev lab p =
      do {
        Wserver.wprint "<tt>%s</tt>\n" lab;
        Wserver.wprint "%s%s\n" (referenced_person_title_text conf base p)
          (Date.short_dates_text conf base p);
        let u = p in
        if lev < max_level then
          for i = 0 to Array.length (get_family u) - 1 do {
            let cpl = foi base (get_family u).(i) in
            let spouse =
              pget conf base (Gutil.spouse (get_key_index p) cpl)
            in
            let mdate =
              if authorized_age conf base p &&
                 authorized_age conf base spouse then
                let fam = foi base (get_family u).(i) in
                match Adef.od_of_codate (get_marriage fam) with
                [ Some (Dgreg d _) ->
                    "<font size=\"-2\"><em>" ^ Date.year_text d ^ "</em></font>"
                | _ -> "" ]
              else ""
            in
            Wserver.wprint "&amp;%s %s%s\n" mdate
              (referenced_person_title_text conf base spouse)
              (Date.short_dates_text conf base spouse)
          }
        else ();
        Wserver.wprint "<br>\n";
        if lev < max_level then
          let rec loop_fam cnt_chil i =
            if i = Array.length (get_family u) then ()
            else
              let des = foi base (get_family u).(i) in
              let rec loop_chil cnt_chil j =
                if j = Array.length (get_children des) then
                  loop_fam cnt_chil (i + 1)
                else do {
                  loop_ind (lev + 1) (lab ^ string_of_int cnt_chil ^ ".")
                    (pget conf base (get_children des).(j));
                  loop_chil (cnt_chil + 1) (j + 1)
                }
              in
              loop_chil cnt_chil 0
          in
          loop_fam 1 0
        else ()
      }
    in
    loop_ind 0 "" p;
    Hutil.trailer conf
  }
;

value desmenu_print = Perso.interp_templ "desmenu";

value print conf base p =
  let templ =
    match p_getenv conf.env "t" with
    [ Some ("F" | "L" | "M") -> "deslist"
    | Some _ -> ""
    | _ -> "desmenu" ]
  in
  if templ <> "" then Perso.interp_templ templ conf base p
  else
    match (p_getenv conf.env "t", p_getint conf.env "v") with
    [ (Some "A", Some v) -> print_aboville conf base v p
    | (Some "S", Some v) -> display_descendants_level conf base v p
    | (Some "H", Some v) -> display_descendant_with_table conf base v p
    | (Some "N", Some v) -> display_descendants_with_numbers conf base v p
    | (Some "G", Some v) -> display_descendant_index conf base v p
    | (Some "C", Some v) -> display_spouse_index conf base v p
    | (Some "T", Some v) -> print_tree conf base v p
    | _ -> desmenu_print conf base p ]
;
