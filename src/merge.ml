(* camlp4r ./pa_html.cmo *)
(* $Id: merge.ml,v 1.1.1.1 1998-09-01 14:32:10 ddr Exp $ *)

open Def;
open Config;
open Gutil;
open Util;

value print_someone base p =
  Wserver.wprint "%s%s %s" (sou base p.first_name)
    (if p.occ == 0 then ""else "." ^ string_of_int p.occ)
    (sou base p.surname)
;

(*
value print_person conf base (first_name, surname, occ) =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" begin
        Wserver.wprint "<input name=p2 size=23 maxlength=200";
        Wserver.wprint " value=\"%s\">" first_name;
      end;
      tag "td" "align=right" begin
        let s = capitale (transl conf "number") in
        let s = if String.length s > 3 then String.sub s 0 3 else s in
        Wserver.wprint "%s" s;
      end;
      tag "td" begin
        Wserver.wprint "<input name=oc2 size=5 maxlength=8%s>\n"
          (if occ == 0 then "" else " value=" ^ string_of_int occ);
      end;
    end;
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=3" begin
        Wserver.wprint
          "<input name=n2 size=40 maxlength=200 value=\"%s\">\n"
          surname;
      end;
    end;
  end
;
*)

value print conf base p =
  let title h =
    do Wserver.wprint "%s" (capitale (transl conf "merge"));
       if h then ()
       else do Wserver.wprint ": "; print_someone base p; return ();
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
