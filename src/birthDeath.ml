(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: birthDeath.ml,v 3.15 2000-06-21 21:13:05 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Gutil;
open Util;
open Config;

value before_date (_, d, _) (_, d1, _) =
  if d1.year < d.year then True
  else if d1.year > d.year then False
  else if d1.month < d.month then True
  else if d1.month > d.month then False
  else if d1.prec > d.prec then True
  else if d1.prec < d.prec then False
  else if d1.day < d.day then True
  else if d1.day > d.day then False
  else True
;

value select conf base get_date find_oldest =
  let module Q =
    Pqueue.Make
      (struct
         type t = (Def.person * Def.dmy * Def.calendar);
         value leq x y =
           if find_oldest then before_date x y
           else before_date y x;
       end)
  in
  let n =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ ->
        try int_of_string (List.assoc "latest_event" conf.base_env) with
        [ Not_found | Failure _ -> 20 ] ]
  in
  let n = min (max 0 n) base.data.persons.len in
  loop Q.empty 0 0 where rec loop q len i =
    if i = base.data.persons.len then
      loop [] q where rec loop list q =
        if Q.is_empty q then (list, len)
        else
          let (e, q) = Q.take q in
          loop [e :: list] q
    else
      let p = base.data.persons.get i in
      match get_date p with
      [ Some (Dgreg d cal) ->
          let e = (p, d, cal) in
          if len < n then loop (Q.add e q) (len + 1) (i + 1)
          else loop (snd (Q.take (Q.add e q))) len (i + 1)
      | _ -> loop q len (i + 1) ]
;

value select_family conf base get_date =
  let module QF =
    Pqueue.Make
      (struct
         type t = (Def.family * Def.dmy * Def.calendar);
         value leq x y = before_date y x;
       end)
  in
  let n =
    match p_getint conf.env "k" with
    [ Some x -> x
    | _ ->
        try int_of_string (List.assoc "latest_event" conf.base_env) with
        [ Not_found | Failure _ -> 20 ] ]
  in
  let n = min (max 0 n) base.data.families.len in
  loop QF.empty 0 0 where rec loop q len i =
    if i = base.data.families.len then
      loop [] q where rec loop list q =
        if QF.is_empty q then (list, len)
        else
          let (e, q) = QF.take q in
          loop [e :: list] q
    else
      let p = base.data.families.get i in
      let (q, len) =
        if p.relation == Married then
          match get_date p with
          [ Some (Dgreg d cal) ->
              let e = (p, d, cal) in
              if len < n then (QF.add e q, len + 1)
              else (snd (QF.take (QF.add e q)), len)
          | _ -> (q, len) ]
        else (q, len)
      in
      loop q len (i + 1)
;

value print_birth conf base =
  let (list, len) =
    select conf base (fun p -> Adef.od_of_codate p.birth) False
  in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d births")) len
  in
  do header conf title;
     Wserver.wprint "<ul>\n";
     let _ = List.fold_left
       (fun (last_month_txt, was_future) (p, d, cal) ->
          let month_txt =
            let d = {(d) with day = 0} in
            capitale (Date.string_of_date conf (Dgreg d cal))
          in
          let future = strictement_apres_dmy d conf.today in
          do if not future && was_future then
               do Wserver.wprint "</ul>\n</ul>\n<p>\n<ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else if month_txt <> last_month_txt then
               do if last_month_txt = "" then ()
                  else Wserver.wprint "</ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else ();
             Wserver.wprint "<li>\n";
             Wserver.wprint "<strong>\n";
             afficher_personne_referencee conf base p;
             Wserver.wprint "</strong>,\n";
             if future then
               Wserver.wprint "<em>%s</em>.\n"
                 (Date.string_of_date conf (Dgreg d cal))
             else
               Wserver.wprint "%s <em>%s</em>.\n"
                 (transl_nth conf "born" (index_of_sex p.sex))
                 (Date.string_of_ondate conf (Dgreg d cal));
          return (month_txt, future))
       ("", False) list
     in ();
     Wserver.wprint "</ul>\n</ul>\n";
     trailer conf;
  return ()
;

value get_death p =
  match p.death with
  [ Death _ cd -> Some (Adef.date_of_cdate cd)
  | _ -> None ]
;

value print_death conf base =
  let (list, len) = select conf base get_death False in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d deaths")) len
  in
  do header conf title;
     Wserver.wprint "<ul>\n";
     let _ = List.fold_left
       (fun last_month_txt (p, d, cal) ->
          let month_txt =
            let d = {(d) with day = 0} in
            capitale (Date.string_of_date conf (Dgreg d cal))
          in
          do if month_txt <> last_month_txt then
               do if last_month_txt = "" then ()
                  else Wserver.wprint "</ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else ();
             Wserver.wprint "<li>\n";
             Wserver.wprint "<strong>\n";
             afficher_personne_referencee conf base p;
             Wserver.wprint "</strong>,\n";
             Wserver.wprint "%s <em>%s</em>"
               (transl_nth conf "died" (index_of_sex p.sex))
               (Date.string_of_ondate conf (Dgreg d cal));
             let sure d = d.prec = Sure in
             match Adef.od_of_codate p.birth with
             [ Some (Dgreg d1 _) ->
                 if sure d1 && sure d && d1 <> d then
                   let a = temps_ecoule d1 d in
                   do Wserver.wprint " <em>(";
                      Date.print_age conf a;
                      Wserver.wprint ")</em>";
                   return ()
                 else ()
             | _ -> () ];
             Wserver.wprint "\n";
          return month_txt)
       "" list
     in ();
     Wserver.wprint "</ul>\n</ul>\n";
     trailer conf;
  return ()
;

value print_oldest_alive conf base =
  let limit =
    match p_getint conf.env "lim" with
    [ Some x -> x
    | None -> 150 ]
  in
  let get_oldest_alive p =
    match p.death with
    [ NotDead -> Adef.od_of_codate p.birth
    | DontKnowIfDead ->
        match Adef.od_of_codate p.birth with
        [ Some (Dgreg d _) as x when conf.today.year - d.year <= limit -> x
        | _ -> None ]
    | _ -> None ]
  in
  let (list, len) = select conf base get_oldest_alive True in
  let title _ =
    Wserver.wprint
      (fcapitale (ftransl conf "the %d oldest perhaps still alive")) len
  in
  do header conf title;
     Wserver.wprint "<ul>\n";
     let _ = List.fold_left
       (fun last_month_txt (p, d, cal) ->
          let month_txt =
            let d = {(d) with day = 0} in
            capitale (Date.string_of_date conf (Dgreg d cal))
          in
          do if month_txt <> last_month_txt then
               do if last_month_txt = "" then ()
                  else Wserver.wprint "</ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else ();
             Wserver.wprint "<li>\n";
             Wserver.wprint "<strong>\n";
             afficher_personne_referencee conf base p;
             Wserver.wprint "</strong>,\n";
             Wserver.wprint "%s <em>%s</em>"
               (transl_nth conf "born" (index_of_sex p.sex))
               (Date.string_of_ondate conf (Dgreg d cal));
             if p.death = NotDead && d.prec = Sure then
               let a = temps_ecoule d conf.today in
               do Wserver.wprint " <em>(";
                  Date.print_age conf a;
                  Wserver.wprint ")</em>";
               return ()
             else ();
             Wserver.wprint ".\n";
          return month_txt)
       "" list
     in ();
     Wserver.wprint "</ul>\n</ul>\n";
     trailer conf;
  return ()
;

value print_marriage conf base =
  let (list, len) =
    select_family conf base (fun fam -> Adef.od_of_codate fam.marriage)
  in
  let title _ =
    Wserver.wprint (fcapitale (ftransl conf "the latest %d marriages")) len
  in
  do header conf title;
     Wserver.wprint "<ul>\n";
     let _ = List.fold_left
       (fun (last_month_txt, was_future) (fam, d, cal) ->
          let month_txt =
            let d = {(d) with day = 0} in
            capitale (Date.string_of_date conf (Dgreg d cal))
          in
          let cpl = coi base fam.fam_index in
          let future = strictement_apres_dmy d conf.today in
          do if not future && was_future then
               do Wserver.wprint "</ul>\n</ul>\n<p>\n<ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else if month_txt <> last_month_txt then
               do if last_month_txt = "" then ()
                  else Wserver.wprint "</ul>\n";
                  Wserver.wprint "<li>%s\n" month_txt;
                  Wserver.wprint "<ul>\n";
               return ()
             else ();
             Wserver.wprint "<li>\n";
             Wserver.wprint "<strong>\n";
             afficher_personne_referencee conf base (poi base cpl.father);
             Wserver.wprint "</strong>\n";
             Wserver.wprint "%s" (transl conf "and");
             Wserver.wprint "<strong>\n";
             afficher_personne_referencee conf base (poi base cpl.mother);
             Wserver.wprint "</strong>,\n";
             if future then
               Wserver.wprint "<em>%s</em>.\n"
                 (Date.string_of_date conf (Dgreg d cal))
             else
               Wserver.wprint "%s <em>%s</em>.\n"
                 (match fam.relation with
                  [ NotMarried -> transl_nth conf "relation/relations" 0
                  | Married -> transl conf "married"
                  | Engaged -> transl conf "engaged" ])
                 (Date.string_of_ondate conf (Dgreg d cal));
          return (month_txt, future))
       ("", False) list
     in ();
     Wserver.wprint "</ul>\n</ul>\n";
     trailer conf;
  return ()
;
