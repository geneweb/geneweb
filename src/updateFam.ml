(* camlp4r ./pa_html.cmo *)
(* $Id: updateFam.ml,v 1.2 1998-09-29 12:22:44 ddr Exp $ *)

open Def;
open Gutil;
open Util;
open Config;

value bogus_family_index = Adef.ifam_of_int (-1);

type create = [ Create of sexe | Link ];
type str_indi = (string * string * int * create);

value person_key base ip =
  let p = poi base ip in
  let first_name = sou base p.first_name in
  let surname = sou base p.surname in
  let occ =
    if first_name = "?" || surname = "?" then Adef.int_of_iper ip else p.occ
  in
  (first_name, surname, occ, Link)
;

value string_family_of base fam cpl =
  let sfam = Gutil.map_family_ps (person_key base) (sou base) fam in
  let scpl = Gutil.map_couple_p (person_key base) cpl in
  (sfam, scpl)
;

type family_member = [ Father | Mother | Child ];

value print_person conf base var fmem (first_name, surname, occ, create) =
  tag "table" "border=1" begin
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "first name/first names" 0));
      end;
      tag "td" begin
        Wserver.wprint "<input name=%s_first_name size=23 maxlength=200" var;
        Wserver.wprint " value=\"%s\">" first_name;
      end;
      tag "td" "align=right" begin
        let s = capitale (transl conf "number") in
        let s = if String.length s > 3 then String.sub s 0 3 else s in
        Wserver.wprint "%s" s;
      end;
      tag "td" begin
        Wserver.wprint "<input name=%s_occ size=5 maxlength=8%s>" var
          (if occ == 0 then "" else " value=" ^ string_of_int occ);
      end;
      tag "td" begin
        tag "select" "name=%s_p" var begin
          Wserver.wprint "<option value=link%s>%s\n"
            (if create = Link then " selected" else "")
            (capitale (transl conf "link"));
          Wserver.wprint "<option value=create%s>%s\n"
            (match create with [ Create Neutre -> " selected" | _ -> "" ])
            (capitale (transl conf "create"));
          if fmem = Child then
            do Wserver.wprint "<option value=create_M%s>%s %s\n"
                 (match create with
                  [ Create Masculin -> " selected" | _ -> "" ])
                 (capitale (transl conf "create"))
                 (transl_nth conf "M/F" 0);
               Wserver.wprint "<option value=create_F%s>%s %s\n"
                 (match create with
                  [ Create Feminin -> " selected" | _ -> "" ])
                 (capitale (transl conf "create"))
                 (transl_nth conf "M/F" 1);
            return ()
          else ();
        end;
      end;
    end;
    Wserver.wprint "\n";
    tag "tr" begin
      tag "td" begin
        Wserver.wprint "%s"
          (capitale (transl_nth conf "surname/surnames" 0));
      end;
      tag "td" "colspan=4" begin
        Wserver.wprint
          "<input name=%s_surname size=40 maxlength=200 value=\"%s\">"
          var surname;
      end;
    end;
  end
;

value print_father conf base fam =
  do tag "h4" begin
       Wserver.wprint " %s "
         (capitale (transl_nth conf "him/her" 0));
     end;
     Wserver.wprint "\n";
     print_person conf base "his" Father fam.father;
     Wserver.wprint "\n";
  return ()
;

value print_mother conf base fam =
  do tag "h4" begin
       Wserver.wprint " %s "
         (capitale (transl_nth conf "him/her" 1));
     end;
     Wserver.wprint "\n";
     print_person conf base "her" Mother fam.mother;
     Wserver.wprint "\n";
  return ()
;

value print_marriage conf base fam =
  do tag "h4" begin
       Wserver.wprint "%s"
         (capitale (transl_nth conf "marriage/marriages" 0));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "%s\n" (capitale (transl conf "place"));
         end;
         tag "td" begin
           Wserver.wprint
             "<input name=marriage_place size=40 maxlength=200%s>\n"
             (if fam.marriage_place = "" then ""
             else " value=\"" ^ fam.marriage_place ^ "\"");
         end;
       end;
     end;
     tag "table" "border=1" begin
       Update.print_date conf base (capitale (transl conf "date")) "marriage"
         (Adef.od_of_codate fam.marriage);
     end;
     Update.print_src conf "marr_src" fam.marriage_src;
  return ()
;

value print_divorce conf base fam =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "divorce" 0));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "tr" "colspan=3" begin
           Wserver.wprint "<input type=radio name=divorce value=not_divorced%s>"
             (match fam.divorce with [ NotDivorced -> " checked" | _ -> "" ]);
           Wserver.wprint "%s\n" (capitale (transl conf "not divorced"));
           Wserver.wprint "<input type=radio name=divorce value=divorced%s>"
             (match fam.divorce with [ Divorced _ -> " checked" | _ -> "" ]);
           Wserver.wprint "%s\n" (capitale (transl conf "divorced"));
         end;
       end;
       Update.print_date conf base (capitale (transl conf "date")) "divorce"
         (match fam.divorce with
          [ Divorced d -> Adef.od_of_codate d
          | _ -> None ]);
     end;
  return ()
;

value print_add_child conf base cnt =
  do Wserver.wprint "<li>\n"; return
  tag "table" "border=1" begin
    tag "tr" begin
      let var = "add_child" ^ string_of_int cnt in
      tag "td" begin
        Wserver.wprint "%s / %s <input type=checkbox name=%s>"
          (capitale (transl conf "insert"))
          (capitale (transl_nth conf "child/children" 0))
          var;
      end;
    end;
  end
;

value print_child conf base cnt n =
  do Wserver.wprint "\n<li>\n";
     print_person conf base ("child" ^ string_of_int cnt) Child n;
     print_add_child conf base cnt;
  return ()
;

value print_children conf base fam cpl force_children_surnames =
  let children =
    match Array.to_list fam.children with
    [ [] -> [("", "", 0, Create Neutre)]
    | ipl ->
        let (_, father_surname, _, _) = cpl.father in
        List.map
          (fun (first_name, surname, occ, create) ->
             let surname =
               if not force_children_surnames && surname = father_surname then
                 ""
               else surname
             in
             (first_name, surname, occ, create))
          ipl ]
  in
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "child/children" 1));
     end;
     Wserver.wprint "\n";
     tag "ul" begin
       print_add_child conf base 0;
       let _ = List.fold_left
          (fun cnt n -> do print_child conf base cnt n; return cnt + 1)
          1 children
       in ();
     end;
  return ()
;

value print_comment conf base fam =
  do stag "h4" begin
       Wserver.wprint "%s" (capitale (transl conf "comment"));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=comment size=50 maxlength=200%s>\n"
             (match fam.comment with
              [ s when s <> "" -> " value=\"" ^ s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_source conf base field =
  do tag "h4" begin
       Wserver.wprint "%s" (capitale (transl_nth conf "source/sources" 0));
     end;
     Wserver.wprint "\n";
     tag "table" "border=1" begin
       tag "tr" begin
         tag "td" begin
           Wserver.wprint "<input name=src size=50 maxlength=200%s>\n"
             (match field with
              [ s when s <> "" -> " value=\"" ^ quote_escaped s ^ "\""
              | _ -> "" ]);
         end;
       end;
     end;
  return ()
;

value print_family conf base fam cpl force_children_surnames =
  do print_father conf base cpl;
     Wserver.wprint "\n";
     print_mother conf base cpl;
     Wserver.wprint "\n";
     print_marriage conf base fam;
     Wserver.wprint "\n";
     print_divorce conf base fam;
     Wserver.wprint "\n";
     print_comment conf base fam;
     Wserver.wprint "\n";
     print_children conf base fam cpl force_children_surnames;
     Wserver.wprint "\n";
     print_source conf base fam.fsources;
  return ()
;

value print_mod1 conf base fam cpl digest =
  let title _ =
    Wserver.wprint "%s / %s # %d" (capitale (transl conf "modify"))
      (capitale (transl_nth conf "family/families" 0))
      (Adef.int_of_ifam fam.fam_index)
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=MOD_FAM_OK>\n";
       Wserver.wprint "<input type=hidden name=i value=%d>\n"
         (Adef.int_of_ifam fam.fam_index);
       Wserver.wprint "<input type=hidden name=digest value=\"%s\">\n" digest;
       print_family conf base fam cpl False;
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_del1 conf base fam =
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "delete"))
      (capitale (transl_nth conf "family/families" 0))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_ifam fam.fam_index);
       Wserver.wprint "<input type=hidden name=m value=DEL_FAM_OK>\n";
       Wserver.wprint "\n";
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_swi1 conf base p fam1 fam2 =
  let title _ =
    Wserver.wprint "%s" (capitale (transl conf "switch"))
  in
  let cpl1 = coi base fam1.fam_index in
  let cpl2 = coi base fam2.fam_index in
  do header conf title;
     Wserver.wprint "%s:"
       (capitale (transl conf "switch the order of the following families"));
     tag "ul" begin
       Wserver.wprint "<li>\n";
       Update.print_someone base (poi base cpl1.father);
       Wserver.wprint " %s " (transl conf "and");
       Update.print_someone base (poi base cpl1.mother);
       Wserver.wprint "<li>\n";
       Update.print_someone base (poi base cpl2.father);
       Wserver.wprint " %s " (transl conf "and");
       Update.print_someone base (poi base cpl2.mother);
     end;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=i value=%d>\n\n"
         (Adef.int_of_iper p.cle_index);
       Wserver.wprint "<input type=hidden name=f value=%d>\n\n"
         (Adef.int_of_ifam fam2.fam_index);
       Wserver.wprint "<input type=hidden name=m value=SWI_FAM_OK>\n";
       Wserver.wprint "\n";
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add1 conf base fam cpl force_children_surnames =
  let title _ =
    Wserver.wprint "%s / %s" (capitale (transl conf "add"))
      (capitale (transl_nth conf "family/families" 0))
  in
  do header conf title;
     Wserver.wprint "\n";
     tag "form" "method=POST action=\"%s\"" conf.command begin
       Srcfile.hidden_env conf;
       Wserver.wprint "<input type=hidden name=m value=ADD_FAM_OK>\n";
       print_family conf base fam cpl force_children_surnames;
       Wserver.wprint "\n<p>\n";
       Wserver.wprint "<input type=submit value=Ok>\n";
     end;
     Wserver.wprint "\n";
     trailer conf;
  return ()
;

value print_add conf base =
  let (fath, moth) =
    match p_getint conf.env "i" with
    [ Some i ->
        let p = base.persons.get i in
        let fath =
          match p.sexe with
          [ Masculin | Neutre -> person_key base p.cle_index
          | Feminin -> ("", "", 0, Create Neutre) ]
        in
        let moth =
          match p.sexe with
          [ Feminin -> person_key base p.cle_index
          | Masculin | Neutre -> ("", "", 0, Create Neutre) ]
        in
        (fath, moth)
    | None -> (("", "", 0, Create Neutre), ("", "", 0, Create Neutre)) ]
  in
  let fam =
    {marriage = Adef.codate_None; marriage_place = "";
     marriage_src = "";
     divorce = NotDivorced; children = [| |];
     comment = ""; origin_file = ""; fsources = "";
     fam_index = bogus_family_index}
  and cpl =
    {father = fath; mother = moth}
  in
  print_add1 conf base fam cpl False
;

value print_add_parents conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let p = base.persons.get i in
      let fam =
        {marriage = Adef.codate_None; marriage_place = "";
         marriage_src = "";
         divorce = NotDivorced;
         children =
           [| (sou base p.first_name, sou base p.surname, p.occ, Link) |];
         comment = ""; origin_file = ""; fsources = "";
         fam_index = bogus_family_index}
      and cpl =
        {father = ("", sou base p.surname, 0, Create Neutre);
         mother = ("", "", 0, Create Neutre)}
      in
      print_add1 conf base fam cpl True
  | _ -> incorrect_request conf ]
;

value print_mod conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      let cpl = coi base (Adef.ifam_of_int i) in
      let (sfam, scpl) = string_family_of base fam cpl in
      print_mod1 conf base sfam scpl (Update.digest_family fam)
  | _ -> incorrect_request conf ]
;

value print_del conf base =
  match p_getint conf.env "i" with
  [ Some i ->
      let fam = foi base (Adef.ifam_of_int i) in
      print_del1 conf base fam
  | _ -> incorrect_request conf ]
;

value rec find_families ifam =
  fun
  [ [ifam1; ifam2 :: ifaml] ->
      if ifam2 = ifam then Some (ifam1, ifam2)
      else find_families ifam [ifam2 :: ifaml]
  | _ -> None ]
;

value print_swi conf base =
  match (p_getint conf.env "i", p_getint conf.env "f") with
  [ (Some ip, Some ifam)  ->
      let p = base.persons.get ip in
      match find_families (Adef.ifam_of_int ifam) (Array.to_list p.family) with
      [ Some (ifam1, ifam2) ->
          print_swi1 conf base p (foi base ifam1) (foi base ifam2)
      | _ -> incorrect_request conf ]
  | _ -> incorrect_request conf ]
;
