(* camlp4r ./pa_html.cmo *)
(* $Id: merge.ml,v 1.2 1998-09-30 14:04:42 ddr Exp $ *)

open Def;
open Config;
open Gutil;
open Util;

value print_someone conf base p =
  Wserver.wprint "%s%s %s" (coa conf (sou base p.first_name))
    (if p.occ == 0 then ""else "." ^ string_of_int p.occ)
    (coa conf (sou base p.surname))
;

value print conf base p =
  let title h =
    do Wserver.wprint "%s" (capitale (transl conf "merge"));
       if h then ()
       else do Wserver.wprint ": "; print_someone conf base p; return ();
    return ()
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=GET action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MRG_IND>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "%s " (capitale (transl conf "with"));
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
