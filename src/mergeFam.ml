(* camlp4r ./pa_html.cmo *)
(* $Id: mergeFam.ml,v 1.4 1999-01-11 14:35:55 ddr Exp $ *)

open Config;
open Def;
open Util;
open Gutil;

value print_differences conf base branches fam1 fam2 =
  let string_field str_orig title name proj =
    let x1 = proj fam1 in
    let x2 = proj fam2 in
    if x1 <> "" && x2 <> "" && x1 <> x2 then
      do Wserver.wprint "<h4>%s</h4>\n" (capitale title);
         tag "ul" begin
           Wserver.wprint "<li>\n";
           Wserver.wprint "<input type=radio name=\"%s\" value=1 checked>\n"
             name;
           Wserver.wprint "%s\n" (if str_orig then coa conf x1 else x1);
           Wserver.wprint "<li>\n";
           Wserver.wprint "<input type=radio name=\"%s\" value=2>\n" name;
           Wserver.wprint "%s\n" (if str_orig then coa conf x2 else x2);
         end;
      return ()
    else ()
  in
  tag "form" "method=POST action=\"%s\"" conf.command begin
    Srcfile.hidden_env conf;
    Wserver.wprint "<input type=hidden name=m value=MRG_FAM_OK>\n";
    Wserver.wprint "<input type=hidden name=f1 value=%d>\n"
      (Adef.int_of_ifam fam1.fam_index);
    Wserver.wprint "<input type=hidden name=f2 value=%d>\n"
      (Adef.int_of_ifam fam2.fam_index);
    loop branches where rec loop =
      fun
      [ [(ip1, ip2)] ->
          do Wserver.wprint "<input type=hidden name=ini1 value=%d>\n"
               (Adef.int_of_iper ip1);
             Wserver.wprint "<input type=hidden name=ini2 value=%d>\n"
               (Adef.int_of_iper ip2);
          return ()
      | [_ :: branches] -> loop branches
      | _ -> () ];
    Wserver.wprint "<p>\n";
    string_field False (transl_nth conf "marriage/marriages" 0) "marriage"
      (fun fam ->
         match Adef.od_of_codate fam.marriage with
         [ None -> ""
         | Some d -> Date.string_of_ondate conf d ]);
    string_field True
      (transl_nth conf "marriage/marriages" 0 ^ " / " ^ transl conf "place")
      "marriage_place" (fun fam -> sou base fam.marriage_place);
    string_field False (transl conf "divorce") "divorce"
      (fun fam ->
         match fam.divorce with
         [ NotDivorced -> ""
         | Divorced cod ->
             match Adef.od_of_codate cod with
             [ Some d -> Date.string_of_ondate conf d
             | None -> "" ] ]);
    Wserver.wprint "<p>\n";
    Wserver.wprint "<input type=submit value=Ok>\n";
  end
;

value merge_fam1 conf base fam1 fam2 =
  let title h =
    let s = transl_nth conf "family/families" 1 in
    Wserver.wprint "%s" (capitale (transl_concat conf "merge" s))
  in
  do header conf title;
     print_differences conf base [] fam1 fam2;
     trailer conf;
  return ()
;

value merge_fam conf base fam1 fam2 =
  let cpl1 = coi base fam1.fam_index in
  let cpl2 = coi base fam2.fam_index in
  if cpl1.father = cpl2.father && cpl1.mother = cpl2.mother then
    merge_fam1 conf base fam1 fam2
  else incorrect_request conf
;

value print conf base =
  match (p_getint conf.env "f1", p_getint conf.env "f2") with
  [ (Some f1, Some f2) ->
      let fam1 = base.data.families.get f1 in
      let fam2 = base.data.families.get f2 in
      merge_fam conf base fam1 fam2
  | _ -> incorrect_request conf ]
;
