(* camlp4r ./pa_html.cmo *)
(* $Id: merge.ml,v 4.1 2001-04-03 20:21:48 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;
open Gutil;
open Util;

value print_someone conf base p =
  Wserver.wprint "%s%s %s" (p_first_name base p)
    (if p.occ == 0 then ""else "." ^ string_of_int p.occ)
    (p_surname base p)
;

value print conf base p =
  let title h =
    do Wserver.wprint "%s" (capitale (transl_decline conf "merge" ""));
       if h then ()
       else do Wserver.wprint ": "; print_someone conf base p; return ();
    return ()
  in
  let list = Gutil.find_same_name base p in
  let list =
    List.fold_right
      (fun p1 pl -> if p1.cle_index = p.cle_index then pl else [p1 :: pl])
      list []
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=GET action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MRG_IND>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "%s " (capitale (transl_decline conf "with" ""));
       if list <> [] then
         Wserver.wprint
           ":<br>\n<input type=radio name=select value=input checked>\n"
       else ();
       Wserver.wprint "(%s . %s %s):\n"
         (transl_nth conf "first name/first names" 0)
         (transl conf "number") (transl_nth conf "surname/surnames" 0);
       Wserver.wprint "<input name=n size=30 maxlength=200>\n";
       Wserver.wprint "<br>\n";
       if list <> [] then
         Wserver.wprint "<table border=0 cellspacing=0 cellpadding=0>\n"
       else ();
       List.iter
         (fun p ->
            do Wserver.wprint "<tr><td valign=top>\n";
               Wserver.wprint "<input type=radio name=select value=%d>\n"
                 (Adef.int_of_iper p.cle_index);
               Wserver.wprint "<td>\n";
               stag "a" "href=\"%s%s\"" (commd conf) (acces conf base p) begin
                 Wserver.wprint "%s.%d %s" (sou base p.first_name) p.occ
                   (sou base p.surname);
               end;
               Wserver.wprint "%s" (Date.short_dates_text conf base p);
               match main_title base p with
               [ Some t -> Wserver.wprint "%s" (one_title_text conf base p t)
               | None -> () ];
               match (aoi base p.cle_index).parents with
               [ Some ifam ->
                   let cpl = coi base ifam in
                   Wserver.wprint ",\n%s"
                     (transl_decline2 conf "%1 of %2"
                        (transl_nth conf "son/daughter/child"
                           (index_of_sex p.sex))
                        (person_title_text conf base (poi base cpl.father) ^
                         " " ^ transl conf "and" ^ " " ^
                         person_title_text conf base (poi base cpl.mother)))
               | None -> () ];
               Wserver.wprint "\n<br>\n";
            return ())
         list;
       if list <> [] then Wserver.wprint "</table>\n" else ();
       Wserver.wprint "<p>\n<input type=submit value=Ok>\n";
     end;
     trailer conf;
  return ()
;
