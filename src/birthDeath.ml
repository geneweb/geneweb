(* camlp4r ./def.syn.cmo ./pa_html.cmo *)
(* $Id: birthDeath.ml,v 3.11 2000-05-14 19:59:33 ddr Exp $ *)
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

module Q =
  Pqueue.Make
    (struct
       type t = (Def.person * Def.dmy * Def.calendar);
       value leq x y = before_date y x;
     end)
;

module QF =
  Pqueue.Make
    (struct
       type t = (Def.family * Def.dmy * Def.calendar);
       value leq x y = before_date y x;
     end)
;

value select conf base get_date =
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
      match get_date p with
      [ Some (Dgreg d cal) ->
          let e = (p, d, cal) in
          if len < n then loop (QF.add e q) (len + 1) (i + 1)
          else loop (snd (QF.take (QF.add e q))) len (i + 1)
      | _ -> loop q len (i + 1) ]
;

value print_birth conf base =
  let (list, len) = select conf base (fun p -> Adef.od_of_codate p.birth) in
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
  let (list, len) = select conf base get_death in
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
             Wserver.wprint "%s <em>%s</em>.\n"
               (match fam.relation with
                [ NotMarried -> transl_nth conf "relation/relations" 0
                | Married -> transl_nth conf "married" 1
                | Engaged -> transl_nth conf "engaged" 1 ])
               (Date.string_of_ondate conf (Dgreg d cal));
          return (month_txt, future))
       ("", False) list
     in ();
     Wserver.wprint "</ul>\n</ul>\n";
     trailer conf;
  return ()
;
