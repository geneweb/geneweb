(* camlp4r ./pa_html.cmo *)
(* $Id: merge.ml,v 3.2 2000-06-03 21:08:03 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

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
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=GET action=\"%s\"" conf.command begin
       Util.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MRG_IND>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "%s " (capitale (transl_decline conf "with" ""));
       Wserver.wprint "(%s . %s %s):\n"
         (transl_nth conf "first name/first names" 0)
         (transl conf "number") (transl_nth conf "surname/surnames" 0);
       Wserver.wprint "<input name=n size=30 maxlength=200>\n";
       Wserver.wprint "=&gt;\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     trailer conf;
  return ()
;
